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
import Multitier.Server.WebSocket as ServerWebSocket exposing (ClientId)
import Multitier.LowLevel exposing (toJSON, fromJSONString)

type ResumeStrategy = FromPrevious | FromPaused

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused]

type AppState event model msg =
  Running (State event model msg) |
  Paused (State event model msg)

type alias EventStream event model cmd = Array (event, model, cmd)

type alias State event model cmd =
  { appModel : model
  , events : EventStream event model cmd
  , previous : Maybe (PreviousState model cmd) }

type alias PreviousState model cmd =
  { appModel : model
  , cmd : cmd
  , index : Int }

getPreviousAppModel : model -> cmd -> Int -> EventStream event model cmd -> (model, cmd)
getPreviousAppModel appModel appCmd index events = case Array.get index events of
  Just (_, model,cmd) -> (model, cmd)
  _ -> (appModel, appCmd)

getPreviousEvents : Int -> EventStream event model cmd -> EventStream event model cmd
getPreviousEvents index events = events |> Array.slice 0 (index+1)

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
  { clients: Dict String ClientId
  , debugger: ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg }

type alias ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg =
  { appState: AppState (ServerEvent serverMsg remoteServerMsg) serverModel (Cmd serverMsg)
  , resume : ResumeStrategy
  , clientStates : Dict String (ClientId, ClientDebuggerModel model msg remoteServerMsg) }

type ServerEvent serverMsg remoteServerMsg = InitServer | ServerMsgEvent serverMsg | StateEvent | RPCevent remoteServerMsg

socketPath : String
socketPath = "debug"

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg remoteServerMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) =
  { clients = Dict.empty
  , debugger =
    { appState = Running (State serverModel (Array.fromList [(InitServer,serverModel, cmd)]) Maybe.Nothing)
    , clientStates = Dict.empty
    , resume = FromPaused }} ! [Cmd.map ServerAppMsg cmd]

type ServerMsg serverMsg = ServerAppMsg serverMsg |
                           OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger in
    let (newServerModel, newCmds) = case serverMsg of
      ServerAppMsg serverAppMsg -> case serverModel.debugger.appState of
        Running state -> let (newAppModel, cmd) = updateServer serverAppMsg state.appModel
          in  { serverModel | debugger = { debugger | appState = Running (State newAppModel (Array.push ((ServerMsgEvent serverAppMsg), newAppModel, cmd) state.events) state.previous) }} ! [Cmd.map ServerAppMsg cmd]
        Paused state -> serverModel ! []
      OnClientConnect cid -> serverModel ! [initializeClient serverModel cid]
      OnClientDisconnect cid -> {serverModel | clients = Dict.remove (toString cid) serverModel.clients } ! []

      Nothing -> serverModel ! []
    in newServerModel ! [newCmds, sendDebuggerModel newServerModel]

sendDebuggerModel : ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendDebuggerModel serverModel = serverModel.clients
  |> Dict.values
  |> multicastSocketMsg (SetServerModel serverModel.debugger)

initializeClient : ServerModel serverModel serverMsg remoteServerMsg model msg -> ClientId -> Cmd (ServerMsg serverMsg)
initializeClient serverModel cid =
  let paused = case serverModel.debugger.appState of
    Running _ -> False
    _ -> True in
    sendSocketMsg cid (InitializeClient { cid= cid, paused= paused })

sendSocketMsg : ClientId -> SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendSocketMsg cid msg = ServerWebSocket.send socketPath cid (Encode.encode 0 (toJSON msg))

broadcastPauseAction : Cmd (ServerMsg serverMsg)
broadcastPauseAction = broadcastSocketMsg (PauseClient)

broadcastResumeAction : Cmd (ServerMsg serverMsg)
broadcastResumeAction = broadcastSocketMsg (ResumeClient)

broadcastGoBackAction : Cmd (ServerMsg serverMsg)
broadcastGoBackAction = broadcastSocketMsg (GoBackClient)

broadcastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastSocketMsg msg = ServerWebSocket.broadcast socketPath (Encode.encode 0 (toJSON msg))

multicastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> List ClientId -> Cmd (ServerMsg serverMsg)
multicastSocketMsg msg ids = ServerWebSocket.multicast socketPath ids (Encode.encode 0 (toJSON msg))

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int |
  SetDebuggerResumeStrategy ResumeStrategy |

  SetClientDebuggerModel ClientId (ClientDebuggerModel model msg remoteServerMsg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  StartDebugView cid -> rpc HandleStartDebugView (\serverModel -> ({serverModel | clients = Dict.insert (toString cid) cid serverModel.clients }, Task.succeed (serverModel.debugger), Cmd.none))
  StopDebugView cid -> rpc HandleStopDebugView (\serverModel -> ({ serverModel | clients = Dict.remove (toString cid) serverModel.clients }, Task.succeed (), Cmd.none))

  SetClientDebuggerModel cid model -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      ({ serverModel | debugger = { debugger | clientStates = Dict.insert (toString cid) (cid, model) debugger.clientStates }}, Task.succeed (), Cmd.none))

  PauseDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.appState of
      Running state ->
        let debugger = serverModel.debugger in
          let newServerModel = { serverModel | debugger = { debugger | appState = Paused state }} in
            (newServerModel, Task.succeed (), Cmd.batch [broadcastPauseAction, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  ResumeDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.appState of
      Paused state ->
        let debugger = serverModel.debugger in
          let (newServerModel, newServerCmd) = case serverModel.debugger.resume of
            FromPaused -> { serverModel | debugger = { debugger | appState = Running (State state.appModel state.events Maybe.Nothing) }} ! []
            FromPrevious -> case state.previous of
              Just previous -> { serverModel | debugger = { debugger | appState = Running (State previous.appModel (getPreviousEvents previous.index state.events) Maybe.Nothing) }} ! [Cmd.map ServerAppMsg previous.cmd]
              _ -> serverModel ! [] in
           (newServerModel, Task.succeed (), Cmd.batch [newServerCmd, broadcastResumeAction, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  SetDebuggerResumeStrategy resume -> rpc Handle (\serverModel -> let debugger = serverModel.debugger in ({ serverModel | debugger = { debugger | resume = resume }}, Task.succeed (), Cmd.none))

  GoBackDebugger index -> rpc Handle
    (\serverModel ->
      let debugger = serverModel.debugger in case serverModel.debugger.appState of
        Running state ->
          let (newServerModel, newCmd) =
            case (index == Array.length state.events) of
              True -> { serverModel | debugger = { debugger | appState = Running (State state.appModel state.events Maybe.Nothing) }} ! []
              False -> let (previousAppModel, previousCmd) = (getPreviousAppModel state.appModel Cmd.none index state.events) in
                { serverModel | debugger = { debugger | appState = Running (State state.appModel state.events (Just (PreviousState previousAppModel previousCmd index))) }} ! []
          in (newServerModel, Task.succeed (), Cmd.batch [newCmd, sendDebuggerModel newServerModel]) -- TODO goback to client
        Paused state ->
          let (newServerModel, newCmd) =
            case (index == Array.length state.events) of
              True -> { serverModel | debugger = { debugger | appState = Paused (State state.appModel state.events Maybe.Nothing) }} ! []
              False -> let (previousAppModel, previousCmd) = (getPreviousAppModel state.appModel Cmd.none index state.events) in
                { serverModel | debugger = { debugger | appState = Paused (State state.appModel state.events (Just (PreviousState previousAppModel previousCmd index))) }} ! []
          in (newServerModel, Task.succeed (), Cmd.batch [newCmd, sendDebuggerModel newServerModel])) -- TODO goback to client


  RemoteServerAppMsg msg ->
    RPC.map AppMsg ServerAppMsg
      (\appModel serverModel ->
        let debugger = serverModel.debugger in
          case serverModel.debugger.appState of
            Running state ->
              let newServerModel = { serverModel | debugger = { debugger | appState = Running (State appModel (Array.push (RPCevent msg, appModel, Cmd.none) state.events) state.previous)}} in
                newServerModel ! [sendDebuggerModel newServerModel]
            Paused state -> serverModel ! [])
          (\serverModel -> case serverModel.debugger.appState of
            Running state -> state.appModel
            Paused state -> state.appModel)
          (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appServerState: serverState }

wrapServerState : (serverModel -> (serverState, serverModel, Cmd serverMsg)) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerState serverState, ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapServerState serverState = \serverModel -> case serverModel.debugger.appState of
  Running state ->
    let (appServerState, newAppModel, newCmd) = serverState state.appModel in
      let debugger = serverModel.debugger in
        let (wrappedServerModel, wrappedCmd) = { serverModel | debugger = { debugger | appState = Running (State newAppModel (Array.push (StateEvent, newAppModel, newCmd) state.events) state.previous) }} ! [Cmd.map ServerAppMsg newCmd] in
          (ServerState appServerState, wrappedServerModel,wrappedCmd)
  Paused state ->
    let (appServerState,_,_) = serverState state.appModel in
      (ServerState appServerState, serverModel, Cmd.none)




-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = case serverModel.debugger.appState of
      Running state -> Sub.map ServerAppMsg (serverSubscriptions state.appModel)
      Paused state -> Sub.none
    in Sub.batch [appSubs, ServerWebSocket.keepAliveAndMonitor socketPath OnClientConnect OnClientDisconnect]

-- MODEL

type Model model msg serverModel serverMsg remoteServerMsg =
  Uninitialized (model, MultitierCmd remoteServerMsg msg) |
  ClientDebugger ClientId (ClientDebuggerModel model msg remoteServerMsg) |
  Switching ClientId (ClientDebuggerModel model msg remoteServerMsg) |
  ServerDebugger ClientId (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) (ClientDebuggerModel model msg remoteServerMsg)

type alias ClientDebuggerModel appModel appMsg remoteServerMsg =
  { appState : AppState (ClientEvent appMsg) appModel (MultitierCmd remoteServerMsg appMsg) }

type ClientEvent appMsg = Init | MsgEvent appMsg

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
wrapInit init = \serverState -> Uninitialized (init serverState.appServerState) !! []

type Msg model msg serverModel serverMsg remoteServerMsg =
  AppMsg msg |

  OnSocketMsg String |

  Pause | Resume | GoBack Int |
  SetResume Int |

  SwitchDebugger |

  Handle (Result Error ()) |
  HandleStartDebugView (Result Error (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg)) | HandleStopDebugView (Result Error ())

type alias InitData =
  { cid: ClientId
  , paused: Bool }

type SocketMsg serverModel serverMsg remoteServerMsg model msg =
  InitializeClient InitData |
  SetServerModel (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) |

  PauseClient | ResumeClient | GoBackClient

-- TODO split in submodules

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg remoteServerMsg -> Model model msg serverModel serverMsg remoteServerMsg -> ( Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) ))
wrapUpdate update = \msg model -> case model of

  Uninitialized (appModel, cmd) -> case msg of
    OnSocketMsg data -> case (fromJSONString data) of
      InitializeClient initData ->
        let newAppState = case initData.paused of
          False -> Running (State appModel (Array.fromList [(Init, appModel, cmd)]) Maybe.Nothing)
          True -> Paused (State appModel (Array.fromList [(Init, appModel, cmd)]) (Just (PreviousState appModel Multitier.none 0))) in
         let newcmodel = ClientDebuggerModel newAppState in
            ClientDebugger initData.cid newcmodel !! [Multitier.map RemoteServerAppMsg AppMsg cmd, performOnServer (SetClientDebuggerModel initData.cid newcmodel)]
      _ -> model !! []
    _ -> model !! []

  ClientDebugger cid cmodel -> case msg of
    AppMsg appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid cmodel in ClientDebugger cid newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClient -> let (newcmodel, cmd) = resumeClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]
      GoBackClient -> let (newcmodel, cmd) = goBackClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]
      _ -> model !! []

    -- GoBack index -> case cmodel.appState of
    --   Running state -> ClientDebugger cid { cmodel | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.events) state.events index (RunningState state.appModel Array.empty))  } !! []
    --   Paused state -> ClientDebugger cid { cmodel | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedEvents) state.pausedEvents index state.background) } !! []

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
      GoBackClient -> let (newcmodel, cmd) = goBackClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]
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
      GoBackClient -> let (newcmodel, cmd) = goBackClient cid cmodel in ClientDebugger cid newcmodel !! [cmd]
      _ -> model !! []

    Pause -> model !! [performOnServer PauseDebugger]
    Resume -> model !! [performOnServer ResumeDebugger]
    GoBack index -> model !! [performOnServer (GoBackDebugger index)]

    SetResume index -> case Array.get index resumeStrategies of
      Just resume -> model !! [performOnServer (SetDebuggerResumeStrategy resume)]
      _ -> model !! []

    SwitchDebugger -> model !! [performOnServer (StopDebugView cid)]
    HandleStopDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO
      _ -> ClientDebugger cid cmodel !! []
    _ -> model !! []

pauseClient : ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
pauseClient cid cmodel = case cmodel.appState of
  Running state -> let newcmodel = { cmodel | appState = Paused state } in
    newcmodel !! [performOnServer (SetClientDebuggerModel cid newcmodel)]
  _ -> cmodel !! []

resumeClient : ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClient cid cmodel = case cmodel.appState of
  Paused state -> let newcmodel = { cmodel | appState = Running (State state.appModel state.events state.previous) } in
    newcmodel !! [performOnServer (SetClientDebuggerModel cid newcmodel)]
  _ -> cmodel !! []

goBackClient : ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
goBackClient cid cmodel = case cmodel.appState of
  Running state -> cmodel !! []
  Paused state -> cmodel !! []

updateAppModel : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModel update appMsg cid cmodel =
  case cmodel.appState of
    Running state ->
      let (newcmodel, cmd) =
        let (newAppModel, cmd) = update appMsg state.appModel in
          { cmodel | appState = Running (State newAppModel (Array.push ((MsgEvent appMsg), newAppModel, cmd) state.events) state.previous)} !! [Multitier.map RemoteServerAppMsg AppMsg cmd] in
            newcmodel !! [cmd, performOnServer (SetClientDebuggerModel cid newcmodel)]
    Paused state -> cmodel !! []

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let appSubs = \cmodel -> case cmodel.appState of
    Running state -> Sub.map AppMsg (subscriptions state.appModel)
    Paused state -> Sub.none
  in let subs = case model of
    Uninitialized _ -> Sub.none
    ClientDebugger cid cmodel -> appSubs cmodel
    Switching cid cmodel -> appSubs cmodel
    ServerDebugger cid smodel cmodel -> appSubs cmodel
  in Sub.batch [subs, WebSocket.listen ("ws://localhost:8081/" ++ socketPath ) OnSocketMsg]

-- VIEW

-- custom decoder for getting the selected index
targetSelectedIndex : Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

selectResume: ResumeStrategy -> List (Html (Msg model msg serverModel serverMsg remoteServerMsg))
selectResume currentResume =
  resumeStrategies
    |> Array.map (\resume -> Html.option [selected (currentResume == resume)] [ Html.text (resumeToString resume)])
    |> Array.toList

resumeToString : ResumeStrategy -> String
resumeToString resume = case resume of
  FromPrevious -> "previous selected state"
  FromPaused -> "paused state"

wrapView : (model -> Html msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Html (Msg model msg serverModel serverMsg remoteServerMsg))
wrapView appView = \model -> case model of
  Uninitialized _ -> Html.text "Registering on server..."

  ClientDebugger _ cmodel ->
    let view state divAtt =
      Html.div [] [
        Html.div divAtt [
          Html.map AppMsg (appView state.appModel)], -- TODO if previous then previousModel
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"],
          Html.pre [] [
            Html.text (toString (case state.previous of
              Just previous -> previous.appModel
              _ -> state.appModel))],
          eventsView state]]
      in case cmodel.appState of
        Running state ->
          view state []
        Paused state ->
          view state [disabled True, style [("opacity", "0.25")]]

  Switching cid smodel -> Html.text "Switching to server debugger..."

  ServerDebugger cid smodel _ ->
    let view state actionProps =
      Html.div [] [
        Html.button [onClick SwitchDebugger] [Html.text "Switch back to client"],
        serverActions smodel actionProps,
        Html.pre [] [
          Html.text (toString (case state.previous of
            Just previous -> previous.appModel
            _ -> state.appModel))],
        serverEventsView state,
        clientEventsView smodel.clientStates,
        timelineView state]
      in case smodel.appState of
        Running state ->
          view state (ActionProps Pause "Pause" True)
        Paused state ->
          view state (ActionProps Resume "Resume" False)

timelineView : State (ServerEvent serverMsg remoteServerMsg) serverModel (Cmd serverMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
timelineView state =
  let
    previousIndex = case state.previous of
      Just previous -> previous.index
      _ -> (Array.length state.events) - 1
    offset = 10
    eventSpacing = 25
    circles = state.events
      |> Array.indexedMap (,)
      |> Array.map (\(index, (msg, model, cmd)) ->
        Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] [])
      |> Array.toList
  in
  Html.div [id "timeline", style [("overflow-x", "auto")]] [
    Svg.svg [ width (toString ((((Array.length state.events) - 1) * eventSpacing) + (offset * 2))), height "40"]
      (List.concat [
        [Svg.line [x1 (toString offset), y1 "20", x2 "100%", y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []],
        circles ])]

clientEventsView : Dict String (ClientId, ClientDebuggerModel model msg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
clientEventsView clientStates =
  clientStates
    |> Dict.toList
    |> List.map (\(_, (cid,cmodel)) -> let appModel = case cmodel.appState of
        Running state -> state.appModel
        Paused state -> state.appModel
      in Html.div [] [Html.text (toString appModel)] )
    |> Html.div []

serverEventsView : State (ServerEvent serverMsg remoteServerMsg) serverModel (Cmd serverMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverEventsView state =
  let
    previousIndex = case state.previous of
      Just previous -> previous.index
      _ -> (Array.length state.events) - 1
    options = state.events
      |> Array.indexedMap (,)
      |> Array.map (\(index, (msg, model, cmd)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (serverEventView msg)])
      |> Array.toList
      |> List.reverse
  in Html.div [] [
    Html.select [size 15] options]

eventsView : State (ClientEvent msg) model (MultitierCmd remoteServerMsg msg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
eventsView state =
  let
    previousIndex = case state.previous of
      Just previous -> previous.index
      _ -> (Array.length state.events) - 1
    options = state.events
      |> Array.indexedMap (,)
      |> Array.map (\(index, (msg, model, cmd)) -> Html.option [selected (previousIndex == index)] [Html.text (clientEventView msg)])
      |> Array.toList
      |> List.reverse
  in Html.div [] [
    Html.select [size 15] options]

type alias ActionProps msg =
  { btnAction: msg
  , btnText: String
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

serverActions : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverActions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
    Html.text "Resume from: ",
    Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume)]
