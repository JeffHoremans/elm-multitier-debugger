module Multitier.Debugger
  exposing
    ( program
    , Model, ServerModel
    , Msg, ServerMsg )

import Html exposing (Html)
import Html.Attributes exposing (checked, style, disabled, size, value, type_, selected, id)
import Html.Events exposing (onClick, onCheck, on)
import Svg
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, r, cx, cy, fill)
import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)
import Dict exposing (Dict)
import WebSocket

import Multitier exposing (Config, MultitierCmd, MultitierProgram, (!!), performOnServer, performOnClient)
import Multitier.RPC as RPC exposing (RPC, rpc)
import Multitier.Error exposing (Error)
import Multitier.Server.WebSocket as ServerWebSocket exposing (WebSocket, ClientId)
-- import Multitier.Server.Console as Console
import Multitier.LowLevel exposing (toJSON, fromJSONString)
-- import Multitier.Server.Console as Console

type ResumeStrategy = FromPrevious | FromPaused | FromBackground

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused, FromBackground]

type AppState appModel appMsg =
  Running (RunningState appModel appMsg) |
  Paused (PausedState appModel appMsg)

type alias RunningState appModel appMsg =
  { appModel : appModel
  , events : Array (appMsg, appModel) }

type alias PausedState appModel appMsg =
  { pausedModel : appModel
  , pausedEvents : Array (appMsg, appModel)
  , previousIndex : Int
  , background : RunningState appModel appMsg }

-- PROGRAM

program :
  { config: Config
  , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
  , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  , serverState : serverModel -> (serverState, serverModel, Cmd serverMsg)
  , serverRPCs : remoteServerMsg -> RPC serverModel msg serverMsg
  , initServer: (serverModel, Cmd serverMsg)
  , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
  , serverSubscriptions : serverModel -> Sub serverMsg
  }
  -> MultitierProgram (Model model msg serverModel serverMsg remoteServerMsg) (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg)
program stuff = Multitier.program
    { config = stuff.config
    , init = wrapInit stuff.init
    , update = wrapUpdate stuff.update
    , subscriptions = wrapSubscriptions stuff.subscriptions
    , view = wrapView stuff.view
    , serverState = wrapServerState stuff.serverState
    , serverRPCs = wrapServerRPCs stuff.serverRPCs
    , initServer = wrapInitServer stuff.initServer
    , updateServer = wrapUpdateServer stuff.updateServer
    , serverSubscriptions = wrapServerSubscriptions stuff.serverSubscriptions
    }

-- SERVER-MODEL

type alias ServerModel serverModel serverMsg remoteServerMsg model msg =
  { socket: Maybe WebSocket
  , clients: Dict String ClientId
  , debugger: ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg }

type alias ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg =
  { appState: AppState serverModel (ServerEvent serverMsg remoteServerMsg)
  , clientStates : Dict String (ClientId, ClientDebuggerModel model msg) }

type ServerEvent serverMsg remoteServerMsg = InitServer | ServerMsgEvent serverMsg | StateEvent | RPCevent remoteServerMsg

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg remoteServerMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) = (ServerModel Maybe.Nothing Dict.empty (ServerDebuggerModel (Running (RunningState serverModel (Array.fromList [(InitServer,serverModel)]))) Dict.empty), Cmd.map ServerAppMsg cmd)

type ServerMsg serverMsg = ServerAppMsg serverMsg |
                           OnSocketOpen WebSocket | OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger in
    let (newServerModel, newCmds) = case serverMsg of
      ServerAppMsg serverAppMsg -> case serverModel.debugger.appState of
        Running state -> let (newAppModel, cmd) = updateServer serverAppMsg state.appModel
          in  { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel (Array.push ((ServerMsgEvent serverAppMsg), newAppModel) state.events)) }} ! [Cmd.map ServerAppMsg cmd]
        Paused state -> let (newAppModel, cmd) = updateServer serverAppMsg state.background.appModel
          in { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel (Array.push ((ServerMsgEvent serverAppMsg), newAppModel) state.background.events) }}} ! [Cmd.map ServerAppMsg cmd]

      OnSocketOpen socket -> { serverModel | socket = Just socket } ! []
      OnClientConnect cid -> serverModel ! [initializeClient serverModel cid]
      OnClientDisconnect cid -> {serverModel | clients = Dict.remove (toString cid) serverModel.clients } ! []

      Nothing -> serverModel ! []
    in newServerModel ! [newCmds, sendDebuggerModel newServerModel]

sendDebuggerModel : ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendDebuggerModel serverModel = serverModel.clients
  |> Dict.values
  |> multicastSocketMsg serverModel.socket (SetServerModel serverModel.debugger)

initializeClient : ServerModel serverModel serverMsg remoteServerMsg model msg -> ClientId -> Cmd (ServerMsg serverMsg)
initializeClient serverModel cid =
  let paused = case serverModel.debugger.appState of
    Running _ -> False
    _ -> True in
    sendSocketMsg serverModel.socket cid (InitializeClient cid paused)

sendSocketMsg : Maybe WebSocket -> ClientId -> SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendSocketMsg maybeSocket cid msg = case maybeSocket of
  Just socket -> ServerWebSocket.send socket cid (Encode.encode 0 (toJSON msg))
  _ -> Cmd.none

broadcastPauseAction : Maybe WebSocket -> Cmd (ServerMsg serverMsg)
broadcastPauseAction socket = broadcastSocketMsg socket (PauseClient)

broadcastResumeAction : Maybe WebSocket -> Cmd (ServerMsg serverMsg)
broadcastResumeAction socket = broadcastSocketMsg socket (ResumeClient)

broadcastSocketMsg : Maybe WebSocket -> SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastSocketMsg maybeSocket msg = case maybeSocket of
  Just socket -> ServerWebSocket.broadcast socket (Encode.encode 0 (toJSON msg))
  _ -> Cmd.none

multicastSocketMsg : Maybe WebSocket -> SocketMsg serverModel serverMsg remoteServerMsg model msg -> List ClientId -> Cmd (ServerMsg serverMsg)
multicastSocketMsg maybeSocket msg ids = case maybeSocket of
  Just socket -> ServerWebSocket.multicast socket ids (Encode.encode 0 (toJSON msg))
  _ -> Cmd.none

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int |

  SetClientDebuggerModel ClientId (ClientDebuggerModel model msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  -- SetState cid state -> rpc HandleSetState (\serverModel -> (serverModel, Task.succeed (), Cmd.none))

  StartDebugView cid -> rpc HandleStartDebugView (\serverModel -> ({serverModel | clients = Dict.insert (toString cid) cid serverModel.clients }, Task.succeed (serverModel.debugger), Cmd.none))
  StopDebugView cid -> rpc HandleStopDebugView (\serverModel -> ({ serverModel | clients = Dict.remove (toString cid) serverModel.clients }, Task.succeed (), Cmd.none))

  SetClientDebuggerModel cid model -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      ({ serverModel | debugger = { debugger | clientStates = Dict.insert (toString cid) (cid, model) debugger.clientStates }}, Task.succeed (), Cmd.none))

  PauseDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.appState of
      Running state ->
        let debugger = serverModel.debugger in
          let newServerModel = { serverModel | debugger = { debugger | appState = Paused (PausedState state.appModel state.events ((Array.length state.events) - 1) (RunningState state.appModel Array.empty)) }} in
            (newServerModel, Task.succeed (), Cmd.batch [broadcastPauseAction serverModel.socket, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  ResumeDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.appState of
      Paused state ->
        let debugger = serverModel.debugger in
          let newServerModel = { serverModel | debugger = { debugger | appState = Running (RunningState state.background.appModel (Array.append state.pausedEvents state.background.events)) }} in
            (newServerModel, Task.succeed (), Cmd.batch [broadcastResumeAction serverModel.socket, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  GoBackDebugger index -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in case serverModel.debugger.appState of
      Running state ->
        let newServerModel = { serverModel | debugger = { debugger | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.events) state.events index (RunningState state.appModel Array.empty))  }} in
          (newServerModel, Task.succeed (), sendDebuggerModel newServerModel)
      Paused state ->
        let newServerModel = { serverModel | debugger = { debugger | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedEvents) state.pausedEvents index state.background) }} in
          (newServerModel, Task.succeed (), sendDebuggerModel newServerModel))


  RemoteServerAppMsg msg ->
    RPC.map AppMsg ServerAppMsg
      (\appModel serverModel ->
        let debugger = serverModel.debugger in
          case serverModel.debugger.appState of
            Running state ->
              let newServerModel = { serverModel | debugger = { debugger | appState = Running (RunningState appModel (Array.push (RPCevent msg, appModel) state.events))}} in
                newServerModel ! [sendDebuggerModel newServerModel]
            Paused state ->
              let background = state.background in
                let newServerModel = { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState appModel (Array.push (RPCevent msg, appModel) state.background.events) }}} in
                  newServerModel ! [sendDebuggerModel newServerModel])
          (\serverModel -> case serverModel.debugger.appState of
            Running state -> state.appModel
            Paused state -> state.background.appModel)
          (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appServerState: serverState }

wrapServerState : (serverModel -> (serverState, serverModel, Cmd serverMsg)) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerState serverState, ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapServerState serverState = \serverModel ->
  let wrapState = \appModel newAppState ->
    let (appServerState, newAppModel, newCmd) = serverState appModel in
      let debugger = serverModel.debugger in
        let (wrappedServerModel, wrappedCmd) = { serverModel | debugger = { debugger | appState = newAppState newAppModel }} ! [Cmd.map ServerAppMsg newCmd] in
          (ServerState appServerState, wrappedServerModel, Cmd.batch [wrappedCmd]) in
  case serverModel.debugger.appState of
    Running state -> wrapState state.appModel (\newAppModel -> (Running (RunningState newAppModel (Array.push (StateEvent, newAppModel) state.events))))
    Paused state -> wrapState state.background.appModel (\newAppModel -> (Paused { state | background = RunningState newAppModel (Array.push (StateEvent, newAppModel) state.background.events) }))

  -- let (appServerState, newAppModel, newCmd) = case serverModel.debugger.appState of
  --   Running state -> serverState state.appModel
  --   Paused state -> serverState state.background.appModel in
  -- let debugger = serverModel.debugger in
  --   let (newWrappedServerModel, newWrappedCmd) = case serverModel.debugger.appState of
  --     Running state ->
  --       { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel (Array.push (StateEvent, newAppModel) state.events)) }} ! [Cmd.map ServerAppMsg newCmd]
  --     Paused state ->
  --       { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel (Array.push (StateEvent, newAppModel) state.background.events) }}} ! [Cmd.map ServerAppMsg newCmd]
  --   in (ServerState True appServerState, newWrappedServerModel, Cmd.batch [newWrappedCmd])



-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = case serverModel.debugger.appState of
      Running state -> Sub.map ServerAppMsg (serverSubscriptions state.appModel)
      Paused state -> Sub.map ServerAppMsg (serverSubscriptions state.background.appModel)
    in Sub.batch [appSubs, ServerWebSocket.listen "debugger" OnSocketOpen OnClientConnect OnClientDisconnect (always Nothing)]

-- MODEL

type Model model msg serverModel serverMsg remoteServerMsg =
  Uninitialized (model, MultitierCmd remoteServerMsg msg) |
  ClientDebugger ClientId (ClientDebuggerModel model msg) |
  Switching ClientId (ClientDebuggerModel model msg) |
  ServerDebugger ClientId (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) (ClientDebuggerModel model msg)

type alias ClientDebuggerModel appModel appMsg  =
  { appState : AppState appModel (ClientEvent appMsg)
  --, runInBackground : Bool
  --, resume : ResumeStrategy
  }

type ClientEvent appMsg = Init | MsgEvent appMsg

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
wrapInit init = \serverState -> Uninitialized (init serverState.appServerState) !! []

type Msg model msg serverModel serverMsg remoteServerMsg =
  AppMsg msg |

  OnSocketMsg String |

  Pause | Resume | GoBack Int |
  ToggleRunInBackground Bool | SetResume Int |

  SwitchDebugger |

  Handle (Result Error ()) |
  HandleStartDebugView (Result Error (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg)) | HandleStopDebugView (Result Error ())

type SocketMsg serverModel serverMsg remoteServerMsg model msg =
  InitializeClient ClientId Bool |
  SetServerModel (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) |

  PauseClient | ResumeClient

-- TODO split in submodules

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg remoteServerMsg -> Model model msg serverModel serverMsg remoteServerMsg -> ( Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) ))
wrapUpdate update = \msg model -> case model of

  Uninitialized (appModel, cmd) -> case msg of
    OnSocketMsg data -> case (fromJSONString data) of
      InitializeClient clientId paused ->
        let newcmodel = case paused of
          False -> (ClientDebuggerModel (Running (RunningState appModel (Array.fromList [(Init, appModel)]))) --True FromBackground
                                                                                                                                        )
          True -> (ClientDebuggerModel (Paused (PausedState appModel (Array.fromList [(Init, appModel)]) 0 (RunningState appModel Array.empty)))) in
        ClientDebugger clientId newcmodel !! [Multitier.map RemoteServerAppMsg AppMsg cmd, performOnServer (SetClientDebuggerModel clientId newcmodel)]
      _ -> model !! []
    _ -> model !! []

  ClientDebugger cid cmodel -> case msg of
    AppMsg appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid cmodel in ClientDebugger cid newcmodel !! [cmd]

    -- AppMsg appMsg -> case cmodel.appState of
    --   Running state -> let (newAppModel, cmd) = update appMsg state.appModel
    --     in  ClientDebugger cid { cmodel | appState = Running (RunningState newAppModel (Array.push ((MsgEvent appMsg), newAppModel) state.events))} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
    --   Paused state -> case cmodel.runInBackground of
    --     True -> let (newAppModel, cmd) = update appMsg state.background.appModel
    --       in  ClientDebugger cid { cmodel | appState = Paused { state | background = RunningState newAppModel (Array.push ((MsgEvent appMsg), newAppModel) state.background.events) }} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
    --     False -> model !! []
    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]

      -- Pause -> case cmodel.appState of
      --   Running state -> ClientDebugger cid { cmodel | resume = if cmodel.runInBackground then cmodel.resume else FromPaused
      --                            , appState = Paused (PausedState state.appModel state.events ((Array.length state.events) - 1) (RunningState state.appModel Array.empty)) } !! []
      --   _ -> model !! []
      ResumeClient -> let (newcmodel, cmd) = resumeClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]
      _ -> model !! []
      -- Resume -> case cmodel.appState of
      --   Paused state -> case cmodel.resume of
      --     FromPrevious -> ClientDebugger cid { cmodel | appState = Running (RunningState state.pausedModel (getPreviousEvents state.previousIndex state.pausedEvents)) } !! []
      --     FromPaused -> ClientDebugger cid { cmodel | appState = Running (RunningState (getPreviousAppModel state.pausedModel ((Array.length state.pausedEvents) - 1) state.pausedEvents) state.pausedEvents) } !! []
      --     FromBackground -> ClientDebugger cid { cmodel | appState = Running (RunningState state.background.appModel (Array.append state.pausedEvents state.background.events)) } !! []
      --   _ -> model !! []

    -- GoBack index -> case cmodel.appState of
    --   Running state -> ClientDebugger cid { cmodel | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.events) state.events index (RunningState state.appModel Array.empty))  } !! []
    --   Paused state -> ClientDebugger cid { cmodel | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedEvents) state.pausedEvents index state.background) } !! []
    --
    -- ToggleRunInBackground runInBackground -> ClientDebugger cid { cmodel | runInBackground = runInBackground } !! []
    -- SetResume index -> case Array.get index resumeStrategies of
    --   Just resume -> ClientDebugger cid { cmodel | resume = resume } !! []
    --   _ -> model !! []

    SwitchDebugger -> Switching cid cmodel !! [performOnServer (StartDebugView cid)]

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  Switching cid cmodel -> case msg of
    AppMsg appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid cmodel in Switching cid newcmodel !! [cmd]
    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cid cmodel in Switching cid newcmodel !! [cmd]
      ResumeClient -> let (newcmodel, cmd) = resumeClient cid cmodel in Switching cid newcmodel !! [cmd]
      _ -> model !! []

    HandleStartDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO handle error in view
      Ok serverDebuggerModel ->  ServerDebugger cid serverDebuggerModel cmodel !! []

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  ServerDebugger cid smodel cmodel -> case msg of
    AppMsg appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid cmodel in ServerDebugger cid smodel newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      SetServerModel serverModel -> ServerDebugger cid serverModel cmodel !! []
      PauseClient -> let (newcmodel, cmd) = pauseClient cid cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClient -> let (newcmodel, cmd) = resumeClient cid cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      _ -> model !! []

    Pause -> model !! [performOnServer PauseDebugger]
    Resume -> model !! [performOnServer ResumeDebugger]
    GoBack index -> model !! [performOnServer (GoBackDebugger index)]
    -- ToggleRunInBackground runInBackground -> TODO
    -- SetResume index -> TODO

    SwitchDebugger -> model !! [performOnServer (StopDebugView cid)]
    HandleStopDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO
      _ -> ClientDebugger cid cmodel !! []
    _ -> model !! []

pauseClient : ClientId -> ClientDebuggerModel model msg -> (ClientDebuggerModel model msg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
pauseClient cid cmodel = case cmodel.appState of
  Running state -> let newcmodel = { cmodel | appState = Paused (PausedState state.appModel state.events ((Array.length state.events) - 1) (RunningState state.appModel Array.empty)) } in
    newcmodel !! [performOnServer (SetClientDebuggerModel cid newcmodel)]
  _ -> cmodel !! []

resumeClient : ClientId -> ClientDebuggerModel model msg -> (ClientDebuggerModel model msg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClient cid cmodel = case cmodel.appState of
  Paused state -> let newcmodel = { cmodel | appState = Running (RunningState state.background.appModel (Array.append state.pausedEvents state.background.events)) } in
    newcmodel !! [performOnServer (SetClientDebuggerModel cid newcmodel)]
  _ -> cmodel !! []


updateAppModel : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ClientId -> ClientDebuggerModel model msg -> (ClientDebuggerModel model msg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModel update appMsg cid cmodel = let (newcmodel, cmd) =
  case cmodel.appState of
    Running state -> let (newAppModel, cmd) = update appMsg state.appModel
      in  { cmodel | appState = Running (RunningState newAppModel (Array.push ((MsgEvent appMsg), newAppModel) state.events))} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
    Paused state -> --case cmodel.runInBackground of
      -- True ->
        let (newAppModel, cmd) = update appMsg state.background.appModel
        in  { cmodel | appState = Paused { state | background = RunningState newAppModel (Array.push ((MsgEvent appMsg), newAppModel) state.background.events) }} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
      -- False -> cmodel !! []
      in
    (newcmodel, Multitier.batch [cmd, performOnServer (SetClientDebuggerModel cid newcmodel)])

getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index events = case Array.get index events of
  Just (_, model) -> model
  _ -> appModel

getPreviousEvents : Int -> Array (appMsg, appModel) -> Array (appMsg, appModel)
getPreviousEvents index events = events |> Array.slice 0 (index+1)

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let appSubs = \cmodel -> case cmodel.appState of
    Running state -> Sub.map AppMsg (subscriptions state.appModel)
    Paused state -> -- case cmodel.runInBackground of
      -- True ->
        Sub.map AppMsg (subscriptions state.pausedModel)
      -- False -> Sub.none
  in let subs = case model of
    Uninitialized _ -> Sub.none
    ClientDebugger cid cmodel -> appSubs cmodel
    Switching cid cmodel -> appSubs cmodel
    ServerDebugger cid smodel cmodel -> appSubs cmodel
  in Sub.batch [subs, WebSocket.listen "ws://localhost:8081/debugger" OnSocketMsg]

-- VIEW

-- custom decoder for getting the selected index
targetSelectedIndex : Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

selectResume: ResumeStrategy -> Bool -> List (Html (Msg model msg serverModel serverMsg remoteServerMsg))
selectResume currentResume runInBackground =
  resumeStrategies
    |> Array.map (\resume -> Html.option [selected (currentResume == resume), disabled (if resume == FromBackground && not runInBackground then True else False)] [ Html.text (resumeToString resume)])
    |> Array.toList

resumeToString : ResumeStrategy -> String
resumeToString resume = case resume of
  FromPrevious -> "previous selected state"
  FromPaused -> "paused state"
  FromBackground -> "current state running in background"

wrapView : (model -> Html msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Html (Msg model msg serverModel serverMsg remoteServerMsg))
wrapView appView = \model -> case model of
  Uninitialized _ -> Html.text "Registering on server..."

  ClientDebugger _ cmodel ->
    let view appModel events previousIndex divAtt =
      Html.div [] [
        Html.div divAtt [
          Html.map AppMsg (appView appModel)],
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"],
          eventsView appModel events previousIndex]]
      in case cmodel.appState of
        Running state ->
          view state.appModel state.events ((Array.length state.events) - 1) [] -- (ActionProps Pause "Pause" False True)
        Paused state ->
          view state.pausedModel state.pausedEvents state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] -- (ActionProps Resume "Resume" True False)

  Switching cid smodel -> Html.text "Switching to server debugger..."

  ServerDebugger cid smodel _ ->
    let view appModel events previousIndex divAtt actionProps =
      Html.div [] [
        Html.button [onClick SwitchDebugger] [Html.text "Switch back to client"],
        serverActions smodel actionProps,
        serverEventsView appModel events previousIndex,
        clientEventsView smodel.clientStates,
        timelineView appModel events previousIndex]
      in case smodel.appState of
        Running state ->
          view state.appModel state.events ((Array.length state.events) - 1) [] (ActionProps Pause "Pause" False True)
        Paused state ->
          view state.pausedModel state.pausedEvents state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] (ActionProps Resume "Resume" True False)

timelineView : serverModel -> Array ((ServerEvent serverMsg remoteServerMsg), serverModel) -> Int -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
timelineView appModel events previousIndex =
  let offset = 10
      eventSpacing = 25
      circles = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) ->
      Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] [])
    |> Array.toList
  in
  Html.div [id "timeline", style [("overflow-x", "auto")]] [
    Svg.svg [ width (toString ((((Array.length events) - 1) * eventSpacing) + (offset * 2))), height "40"]
      (List.concat [
        [Svg.line [x1 (toString offset), y1 "20", x2 "100%", y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []],
        circles ])]

clientEventsView : Dict String (ClientId, ClientDebuggerModel model msg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
clientEventsView clientStates =
  clientStates
    |> Dict.toList
    |> List.map (\(_, (cid,cmodel)) -> let appModel = case cmodel.appState of
        Running state -> state.appModel
        Paused state -> state.pausedModel
      in Html.div [] [Html.text (toString appModel)] )
    |> Html.div []

serverEventsView : serverModel -> Array ((ServerEvent serverMsg remoteServerMsg), serverModel) -> Int -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverEventsView appModel events previousIndex =
  let options = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [--onClick (GoBack index),
                                                          selected (previousIndex == index)] [Html.text (serverEventView msg)])
    |> Array.toList
    |> List.reverse
  in Html.div [] [
    Html.select [size 15] options,
    Html.pre [] [Html.text (toString appModel)]]

eventsView : model -> Array ((ClientEvent msg), model) -> Int -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
eventsView appModel events previousIndex =
  let options = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [--onClick (GoBack index),
                                                          selected (previousIndex == index)] [Html.text (clientEventView msg)])
    |> Array.toList
    |> List.reverse
  in Html.div [] [
    Html.select [size 15] options,
    Html.pre [] [Html.text (toString appModel)]]

type alias ActionProps msg =
  { btnAction: msg
  , btnText: String
  , hideRunInBackground: Bool
  , hideResumeFrom: Bool }

clientEventView : ClientEvent msg -> String
clientEventView event = case event of
  Init -> "[Init]"
  MsgEvent msg -> "[Msg] " ++ (toString msg)

serverEventView : ServerEvent serverMsg remoteServerMsg -> String
serverEventView event = case event of
  InitServer -> "[Init]"
  ServerMsgEvent msg -> "[Msg] " ++ (toString msg)
  StateEvent -> "[Server-state requested]"
  RPCevent msg -> "[RPC] " ++ (toString msg)


-- actions : ClientDebuggerModel model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
-- actions model props = Html.div [] [
--   Html.button [onClick props.btnAction] [Html.text props.btnText],
--   Html.br [] [],
--   Html.text "Run in background when paused",
--   Html.input [disabled props.hideRunInBackground, checked model.runInBackground, type_ "checkbox", onCheck ToggleRunInBackground] [],
--   Html.br [] [],
--   Html.text "Resume from: ",
--   Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume model.runInBackground)]

serverActions : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverActions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText]]
