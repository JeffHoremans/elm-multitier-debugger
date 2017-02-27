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

import Multitier.Debugger.EventStream as EventStream exposing (EventStream, Event(..))

type ResumeStrategy = FromPrevious | FromPaused

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused]

type DebuggerState = Running | Paused

type alias PreviousState model cmd =
  { appModel : model
  , cmd : cmd
  , index : Int }

-- PROGRAM

program :
  { config: Config
  , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
  , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  , serverState : serverModel -> serverState
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
  { debugger: ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg
  , clients :
    { debugging: Dict String ClientId
    , connected: Dict String ClientId }}

type alias ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg =
  { state: DebuggerState
  , appModel : serverModel
  , events : EventStream (ServerEvent serverMsg remoteServerMsg) serverModel (Cmd serverMsg) (ClientEvent msg) model (MultitierCmd remoteServerMsg msg)
  , previous : Maybe (PreviousState serverModel (Cmd serverMsg))
  , resume : ResumeStrategy
  , clientIds : List String }

type ServerEvent serverMsg remoteServerMsg = InitServer | ServerMsgEvent serverMsg | StateEvent | RPCevent remoteServerMsg

socketPath : String
socketPath = "debug"

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg remoteServerMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) =
  { debugger =
    { state = Running
    , appModel = serverModel
    , events = EventStream.empty (serverModel, cmd) |> EventStream.pushServerEvent (InitServer, serverModel, cmd)
    , previous = Maybe.Nothing
    , resume = FromPaused
    , clientIds = [] }
  , clients =
    { debugging = Dict.empty
    , connected = Dict.empty }} ! [Cmd.map ServerAppMsg cmd]

type ServerMsg serverMsg = ServerAppMsg serverMsg |
                           OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger
      clients = serverModel.clients in
  let (newServerModel, newCmds) = case serverMsg of
    ServerAppMsg serverAppMsg ->
      case serverModel.debugger.state of
        Running ->
          let (newAppModel, cmd) = updateServer serverAppMsg debugger.appModel in
            { serverModel | debugger = { debugger | appModel = newAppModel, events = EventStream.pushServerEvent ((ServerMsgEvent serverAppMsg), newAppModel, cmd) debugger.events }} ! [Cmd.map ServerAppMsg cmd]
        Paused -> serverModel ! []

    OnClientConnect cid ->
      { serverModel | debugger =  { debugger | clientIds = (toString cid) :: debugger.clientIds },
                      clients =   { clients  | connected = Dict.insert (toString cid) cid clients.connected }}
        ! [initializeClient serverModel cid]
    OnClientDisconnect cid ->
      { serverModel | clients =   { clients  | debugging = Dict.remove (toString cid) clients.debugging, connected = Dict.remove (toString cid) clients.connected }} ! []

    Nothing -> serverModel ! []

  in newServerModel ! [newCmds, sendDebuggerModel newServerModel]

sendDebuggerModel : ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendDebuggerModel serverModel = serverModel.clients.debugging
  |> Dict.values
  |> multicastSocketMsg (SetServerModel serverModel.debugger)

initializeClient : ServerModel serverModel serverMsg remoteServerMsg model msg -> ClientId -> Cmd (ServerMsg serverMsg)
initializeClient serverModel cid =
  let paused =
    case serverModel.debugger.state of
      Paused -> True
      Running -> False
  in sendSocketMsg cid (InitializeClient { cid= cid, paused= paused })

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

  AddClientEvent ClientId (ClientEvent msg) model (MultitierCmd remoteServerMsg msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  StartDebugView cid ->
    rpc HandleStartDebugView (\serverModel -> let clients = serverModel.clients in
      ({serverModel | clients = { clients | debugging = Dict.insert (toString cid) cid clients.debugging }}, Task.succeed (serverModel.debugger), Cmd.none))
  StopDebugView cid ->
    rpc HandleStopDebugView (\serverModel -> let clients = serverModel.clients in
      ({ serverModel | clients = { clients | debugging = Dict.remove (toString cid) clients.debugging }}, Task.succeed (), Cmd.none))

  AddClientEvent cid event model cmd -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      case debugger.state of
        Running -> ({ serverModel | debugger = { debugger | events = EventStream.pushClientEvent cid (event,model,cmd) debugger.events }}, Task.succeed (), Cmd.none)
        Paused -> (serverModel, Task.succeed (), Cmd.none)) -- TODO what to do in this case?

  PauseDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.state of
      Running ->
        let debugger = serverModel.debugger in
          let newServerModel = { serverModel | debugger = { debugger | state = Paused }} in
            (newServerModel, Task.succeed (), Cmd.batch [broadcastPauseAction, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  ResumeDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.state of
      Paused ->
        let debugger = serverModel.debugger in
          let (newServerModel, newServerCmd) = case serverModel.debugger.resume of
            FromPaused -> { serverModel | debugger = { debugger | state = Running }} ! []
            FromPrevious -> case debugger.previous of
              Just previous ->
                { serverModel | debugger = { debugger | state = Running
                                                      , appModel = previous.appModel
                                                      , events = EventStream.goBack previous.index debugger.events
                                                      , previous = Maybe.Nothing }} ! [Cmd.map ServerAppMsg previous.cmd]
              _ -> serverModel ! [] in
           (newServerModel, Task.succeed (), Cmd.batch [newServerCmd, broadcastResumeAction, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  SetDebuggerResumeStrategy resume -> rpc Handle (\serverModel -> let debugger = serverModel.debugger in ({ serverModel | debugger = { debugger | resume = resume }}, Task.succeed (), Cmd.none))

  GoBackDebugger index -> rpc Handle
    (\serverModel ->
      let debugger = serverModel.debugger in
        let (newServerModel, newCmd) =
          case (index == EventStream.length debugger.events) of
            True -> { serverModel | debugger = { debugger | previous = Maybe.Nothing }} ! []
            False -> let (previousAppModel, previousCmd) = (EventStream.previousServerState (debugger.appModel,Cmd.none) index debugger.events) in
              { serverModel | debugger = { debugger | previous = (Just (PreviousState previousAppModel previousCmd index)) }} ! []
          in (newServerModel, Task.succeed (), Cmd.batch [newCmd, sendDebuggerModel newServerModel])) -- TODO goback to client


  RemoteServerAppMsg msg ->
    RPC.map AppMsg ServerAppMsg
      (\appModel serverModel ->
        let debugger = serverModel.debugger in
          case debugger.state of
            Running ->
              let newServerModel = { serverModel | debugger = { debugger | appModel = appModel, events = EventStream.pushServerEvent (RPCevent msg, appModel, Cmd.none) debugger.events }} in
                newServerModel ! [sendDebuggerModel newServerModel]
            Paused -> serverModel ! [])
          (\serverModel -> serverModel.debugger.appModel)
          (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appServerState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> ServerState serverState)
wrapServerState serverState = \serverModel -> ServerState (serverState serverModel.debugger.appModel)

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = case serverModel.debugger.state of
      Running -> Sub.map ServerAppMsg (serverSubscriptions serverModel.debugger.appModel)
      Paused -> Sub.none
    in Sub.batch [appSubs, ServerWebSocket.keepAliveAndMonitor socketPath OnClientConnect OnClientDisconnect]

-- MODEL

type Model model msg serverModel serverMsg remoteServerMsg =
  Uninitialized (model, MultitierCmd remoteServerMsg msg) |
  ClientDebugger ClientId (ClientDebuggerModel model msg remoteServerMsg) |
  Switching ClientId (ClientDebuggerModel model msg remoteServerMsg) |
  ServerDebugger ClientId (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) (ClientDebuggerModel model msg remoteServerMsg)

type alias ClientDebuggerModel model msg remoteServerMsg =
  { state : DebuggerState
  , appModel : model
  , previous : Maybe (PreviousState model (MultitierCmd remoteServerMsg msg)) }

type ClientEvent msg = Init | MsgEvent msg

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
        let newcmodel = case initData.paused of
          True -> ClientDebuggerModel Paused appModel (Just (PreviousState appModel Multitier.none 0))
          False -> ClientDebuggerModel Running appModel Maybe.Nothing in
            ClientDebugger initData.cid newcmodel !! [Multitier.map RemoteServerAppMsg AppMsg cmd, performOnServer (AddClientEvent initData.cid Init appModel cmd)]
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
pauseClient cid cmodel = case cmodel.state of
  Running -> let newcmodel = { cmodel | state = Paused } in
    newcmodel !! []
  _ -> cmodel !! []

resumeClient : ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClient cid cmodel = case cmodel.state of
  Paused -> let newcmodel = { cmodel | state = Running } in
    newcmodel !! []
  _ -> cmodel !! []

goBackClient : ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
goBackClient cid cmodel = case cmodel.state of
  Running -> cmodel !! []
  Paused -> cmodel !! []

updateAppModel : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ClientId -> ClientDebuggerModel model msg remoteServerMsg -> (ClientDebuggerModel model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModel update appMsg cid cmodel =
  case cmodel.state of
    Running ->
      let (newAppModel, newCmd) = update appMsg cmodel.appModel in
        { cmodel | appModel = newAppModel } !! [Multitier.map RemoteServerAppMsg AppMsg newCmd, performOnServer (AddClientEvent cid (MsgEvent appMsg) newAppModel newCmd)]
    Paused -> cmodel !! []

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let appSubs = \cmodel -> case cmodel.state of
    Running -> Sub.map AppMsg (subscriptions cmodel.appModel)
    Paused -> Sub.none
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
    let view divAtt =
      Html.div [] [
        Html.div divAtt [
          Html.map AppMsg (appView cmodel.appModel)], -- TODO if previous then previousModel
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"],
          Html.pre [] [
            Html.text (toString (case cmodel.previous of
              Just previous -> previous.appModel
              _ -> cmodel.appModel))]
          --,eventsView cmodel
          ]]
      in case cmodel.state of
        Running ->
          view []
        Paused ->
          view [disabled True, style [("opacity", "0.25")]]

  Switching cid smodel -> Html.text "Switching to server debugger..."

  ServerDebugger cid smodel _ ->
    let view actionProps =
      Html.div [] [
        Html.button [onClick SwitchDebugger] [Html.text "Switch back to client"],
        serverActions smodel actionProps,
        Html.pre [] [
          Html.text (toString (case smodel.previous of
            Just previous -> previous.appModel
            _ -> smodel.appModel))],
        eventsView smodel,
        timelineView smodel]
      in case smodel.state of
        Running ->
          view (ActionProps Pause "Pause" True)
        Paused ->
          view (ActionProps Resume "Resume" False)

timelineView : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
timelineView smodel =
  let
    previousIndex = case smodel.previous of
      Just previous -> previous.index
      _ -> (EventStream.length smodel.events) - 1
    offset = 10
    eventSpacing = 25
    circles =
      EventStream.view smodel.events
        |> List.filter (\(_,event) -> case event of
          ServerEvent _ -> True
          _ -> False)
        |> List.map (\(index, event) -> Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] [])

  in
  Html.div [id "timeline", style [("overflow-x", "auto")]] [
    Svg.svg [ width (toString ((((EventStream.length smodel.events) - 1) * eventSpacing) + (offset * 2))), height "40"]
      (List.concat [
        [Svg.line [x1 (toString offset), y1 "20", x2 "100%", y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []],
        circles ])]

eventsView : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
eventsView smodel =
  let
    previousIndex = case smodel.previous of
      Just previous -> previous.index
      _ -> (EventStream.length smodel.events) - 1
    options = EventStream.view smodel.events
      |> List.map (\(index, event) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (eventView event)])
      |> List.reverse
  in Html.div [] [
    Html.select [size 15] options]

type alias ActionProps msg =
  { btnAction: msg
  , btnText: String
  , hideResumeFrom: Bool }

eventView : Event (ServerEvent serverMsg remoteServerMsg) (ClientEvent msg) -> String
eventView event = case event of
  ClientEvent cid cevent -> clientEventView cevent
  ServerEvent sevent -> serverEventView sevent

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
