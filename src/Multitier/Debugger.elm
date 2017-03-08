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

import Multitier exposing (Config, MultitierCmd(..), MultitierProgram, (!!), performOnServer, performOnClient)
import Multitier.RPC as RPC exposing (RPC, rpc)
import Multitier.Error exposing (Error)
import Multitier.Server.WebSocket as ServerWebSocket exposing (ClientId)
import Multitier.LowLevel exposing (toJSON, fromJSONString)

import Multitier.Debugger.TimeLine as TimeLine exposing (TimeLine, RunCycleID, Event(..), ServerEventType(..), ClientEventType(..))

type ResumeStrategy = FromPrevious | FromPaused

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused]

type ServerDebuggerState = Running | Paused

type alias PreviousState serverModel serverCmd model cmd =
  { appModel : serverModel
  , cmd : serverCmd
  , clients : Dict String (ClientId, (model,cmd, Int))
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
    { debugging: Dict String ClientId }}

type alias ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg =
  { state: ServerDebuggerState
  , appModel : serverModel
  , timeline : TimeLine serverModel serverMsg remoteServerMsg model msg
  , previous : Maybe (PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg))
  , resume : ResumeStrategy
  , clientIds : List String
  , runCycleID : RunCycleID
 }

socketPath : String
socketPath = "debug"

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg remoteServerMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) =
  { debugger =
    { state = Running
    , appModel = serverModel
    , timeline = TimeLine.empty (serverModel, cmd) |> TimeLine.pushServerEvent 0 (InitServer, serverModel, cmd)
    , previous = Maybe.Nothing
    , resume = FromPaused
    , clientIds = []
    , runCycleID = 0 }
  , clients =
    { debugging = Dict.empty }} ! [Cmd.map (ServerAppMsg 0) cmd]

type ServerMsg serverMsg = ServerAppMsg RunCycleID serverMsg |
                           OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger
      clients = serverModel.clients in
  let (newServerModel, newCmds) = case serverMsg of
    ServerAppMsg runCycleID serverAppMsg ->
      case serverModel.debugger.state of
        Running ->
          let (newAppModel, cmd) = updateServer serverAppMsg debugger.appModel in
            { serverModel | debugger = { debugger | appModel = newAppModel, timeline = TimeLine.pushServerEvent debugger.runCycleID ((ServerMsgEvent serverAppMsg), newAppModel, cmd) debugger.timeline }} ! [Cmd.map (ServerAppMsg debugger.runCycleID) cmd]
        Paused -> serverModel ! []

    OnClientConnect cid ->
      { serverModel | debugger =  { debugger | clientIds = (toString cid) :: debugger.clientIds }} ! [initializeClient serverModel cid]
    OnClientDisconnect cid ->
      { serverModel | clients =   { clients  | debugging = Dict.remove (toString cid) clients.debugging }} ! []

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
  in sendSocketMsg cid (InitializeClient { cid= cid, paused= paused, runCycleID= serverModel.debugger.runCycleID })

sendSocketMsg : ClientId -> SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendSocketMsg cid msg = ServerWebSocket.send socketPath cid (Encode.encode 0 (toJSON msg))

broadcastPauseAction : Cmd (ServerMsg serverMsg)
broadcastPauseAction = broadcastSocketMsg (PauseClient)

broadcastResumeFromPausedAction : Cmd (ServerMsg serverMsg)
broadcastResumeFromPausedAction = broadcastSocketMsg ResumeClientFromPaused

broadcastResumeFromPreviousAction : RunCycleID -> PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) -> ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastResumeFromPreviousAction rcid previous serverModel =
  TimeLine.clients serverModel.debugger.timeline
    |> List.map (\cid -> case Dict.member (toString cid) previous.clients of
        True -> sendSocketMsg cid (ResumeClientFromPrevious rcid (Dict.get (toString cid) previous.clients))
        False -> sendSocketMsg cid (ResumeClientFromPrevious rcid Maybe.Nothing))
    |> Cmd.batch
-- TODO repop clients + delete when disconnecting...

broadcastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastSocketMsg msg = ServerWebSocket.broadcast socketPath (Encode.encode 0 (toJSON msg))

multicastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> List ClientId -> Cmd (ServerMsg serverMsg)
multicastSocketMsg msg ids = ServerWebSocket.multicast socketPath ids (Encode.encode 0 (toJSON msg))

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg ClientId RunCycleID Int remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int |
  SetDebuggerResumeStrategy ResumeStrategy |

  AddClientEvent ClientId Int (ClientEventType msg) model (MultitierCmd remoteServerMsg msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  StartDebugView cid ->
    rpc HandleStartDebugView (\serverModel -> let clients = serverModel.clients in
      ({serverModel | clients = { clients | debugging = Dict.insert (toString cid) cid clients.debugging }}, Task.succeed (serverModel.debugger), Cmd.none))
  StopDebugView cid ->
    rpc HandleStopDebugView (\serverModel -> let clients = serverModel.clients in
      ({ serverModel | clients = { clients | debugging = Dict.remove (toString cid) clients.debugging }}, Task.succeed (), Cmd.none))

  AddClientEvent cid rpcid event model cmd -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      case debugger.state of
        Running ->
          let newServerModel = { serverModel | debugger = { debugger | timeline = TimeLine.pushClientEvent cid debugger.runCycleID rpcid (event,model,cmd) debugger.timeline }} in
            (newServerModel, Task.succeed (), sendDebuggerModel newServerModel)
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
            FromPaused -> { serverModel | debugger = { debugger | state = Running }} ! [broadcastResumeFromPausedAction]
            FromPrevious -> case debugger.previous of
              Just previous -> let newRunCyleID = debugger.runCycleID + 1 in
                { serverModel | debugger = { debugger | state = Running
                                                      , appModel = previous.appModel
                                                      , timeline = TimeLine.goBack previous.index debugger.timeline
                                                      , runCycleID = newRunCyleID
                                                      , previous = Maybe.Nothing }} ! [Cmd.map (ServerAppMsg newRunCyleID) previous.cmd, broadcastResumeFromPreviousAction newRunCyleID previous serverModel]
              _ -> { serverModel | debugger = { debugger | state = Running }} ! [broadcastResumeFromPausedAction] in
           (newServerModel, Task.succeed (), Cmd.batch [newServerCmd, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  SetDebuggerResumeStrategy resume -> rpc Handle (\serverModel -> let debugger = serverModel.debugger in ({ serverModel | debugger = { debugger | resume = resume }}, Task.succeed (), Cmd.none))

  GoBackDebugger index -> rpc Handle
    (\serverModel ->
      let debugger = serverModel.debugger in
        let (newServerModel, newCmd) =
          case (index == TimeLine.length debugger.timeline) of
            True -> { serverModel | debugger = { debugger | previous = Maybe.Nothing }} ! []
            False -> let (previousAppModel, previousCmd, previousClients) = (TimeLine.previousState index debugger.timeline) in
              { serverModel | debugger = { debugger | previous = (Just (PreviousState previousAppModel previousCmd previousClients index)) }} ! []
          in (newServerModel, Task.succeed (), Cmd.batch [newCmd, sendDebuggerModel newServerModel]))


  RemoteServerAppMsg cid rcid rpcid msg ->
    RPC.map (AppMsg rcid (Just rpcid)) (ServerAppMsg rcid)
      (\appModel serverModel ->
        let debugger = serverModel.debugger in
          case debugger.state of
            Running ->
              let newServerModel = { serverModel | debugger = { debugger | appModel = appModel, timeline = TimeLine.pushServerEvent rcid (RPCevent cid rpcid msg, appModel, Cmd.none) debugger.timeline }} in
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
    let appSubs = Sub.map (ServerAppMsg serverModel.debugger.runCycleID) (serverSubscriptions serverModel.debugger.appModel)
    in Sub.batch [appSubs, ServerWebSocket.keepAliveAndMonitor socketPath OnClientConnect OnClientDisconnect]

-- MODEL

type Model model msg serverModel serverMsg remoteServerMsg =
  Uninitialized (model, MultitierCmd remoteServerMsg msg) |
  ClientDebugger ClientId (ClientDebuggerModel model) |
  Switching ClientId (ClientDebuggerModel model) |
  ServerDebugger ClientId (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) (ClientDebuggerModel model)

type ClientDebuggerState = ClientRunning | ClientPaused | ClientUnvalid

type alias ClientDebuggerModel model =
  { state : ClientDebuggerState
  , appModel : model
  , rpccounter : Int
  , runCycleID : RunCycleID }

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
wrapInit init = \serverState -> Uninitialized (init serverState.appServerState) !! []

type Msg model msg serverModel serverMsg remoteServerMsg =
  AppMsg RunCycleID (Maybe Int) msg |

  OnSocketMsg String |

  Pause | Resume | GoBack Int |
  SetResume Int |

  SwitchDebugger |

  Handle (Result Error ()) |
  HandleStartDebugView (Result Error (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg)) | HandleStopDebugView (Result Error ())

type alias InitData =
  { cid: ClientId
  , paused: Bool
  , runCycleID : RunCycleID }

type SocketMsg serverModel serverMsg remoteServerMsg model msg =
  InitializeClient InitData |
  SetServerModel (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) |

  PauseClient | ResumeClientFromPaused | ResumeClientFromPrevious RunCycleID (Maybe (ClientId, (model,(MultitierCmd remoteServerMsg msg), Int)))

-- TODO split in submodules

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg remoteServerMsg -> Model model msg serverModel serverMsg remoteServerMsg -> ( Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) ))
wrapUpdate update = \msg model -> case model of

  Uninitialized (appModel, cmd) -> case msg of
    OnSocketMsg data -> case (fromJSONString data) of
      InitializeClient initData ->
        let newcmodel = case initData.paused of
          True -> ClientDebuggerModel ClientPaused appModel 0 initData.runCycleID
          False -> ClientDebuggerModel ClientRunning appModel 0 initData.runCycleID in
            let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds initData.cid newcmodel cmd in
              ClientDebugger initData.cid rpcWrappedModel !! [rpcWrappedCmds, performOnServer (AddClientEvent initData.cid newcmodel.rpccounter (Init rpcIds) appModel cmd) ]
      _ -> model !! []
    _ -> model !! []

  ClientDebugger cid cmodel -> case msg of
    AppMsg rcid rpcid appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid rpcid cmodel in ClientDebugger cid newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClientFromPaused -> let (newcmodel, cmd) = resumeClientFromPaused cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClientFromPrevious rcid prev -> let (newcmodel, cmd) = resumeClientFromPrevious cid rcid prev cmodel in ClientDebugger cid newcmodel !! [cmd]
      _ -> model !! []

    SwitchDebugger -> Switching cid cmodel !! [performOnServer (StartDebugView cid)]

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  Switching cid cmodel -> case msg of
    AppMsg rcid rpcid appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid rpcid cmodel in Switching cid newcmodel !! [cmd]
    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in Switching cid newcmodel !! [cmd]
      ResumeClientFromPaused -> let (newcmodel, cmd) = resumeClientFromPaused cmodel in Switching cid newcmodel !! [cmd]
      ResumeClientFromPrevious rcid prev -> let (newcmodel, cmd) = resumeClientFromPrevious cid rcid prev cmodel in Switching cid newcmodel !! [cmd]
      _ -> model !! []

    HandleStartDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO handle error in view
      Ok serverDebuggerModel ->  ServerDebugger cid serverDebuggerModel cmodel !! []

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  ServerDebugger cid smodel cmodel -> case msg of
    AppMsg rcid rpcid appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg cid rpcid cmodel in ServerDebugger cid smodel newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      SetServerModel serverModel -> ServerDebugger cid serverModel cmodel !! []
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClientFromPaused -> let (newcmodel, cmd) = resumeClientFromPaused cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClientFromPrevious rcid prev -> let (newcmodel, cmd) = resumeClientFromPrevious cid rcid prev cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
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

pauseClient : ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
pauseClient cmodel = { cmodel | state = ClientPaused } !! []

resumeClientFromPaused : ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClientFromPaused cmodel = { cmodel | state = ClientRunning } !! []

resumeClientFromPrevious : ClientId -> RunCycleID -> Maybe (ClientId, (model,(MultitierCmd remoteServerMsg msg), Int)) -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClientFromPrevious cid rcid prev cmodel = case prev of
  Just (_,(prevModel,prevCmd, prevCounter)) -> let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid { cmodel | rpccounter = prevCounter} prevCmd in
    { rpcWrappedModel | state = ClientRunning, appModel = prevModel, runCycleID = rcid } !! [rpcWrappedCmds]
  _ -> { cmodel | state = ClientUnvalid } !! []


updateAppModel : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ClientId -> Maybe Int -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModel update appMsg cid rpcid cmodel =
  case cmodel.state of
    ClientRunning ->
      let (newAppModel, newCmd) = update appMsg cmodel.appModel in
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid cmodel newCmd in
          { rpcWrappedModel | appModel = newAppModel } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (MsgEvent rpcid rpcIds appMsg) newAppModel newCmd)]
    _ -> cmodel !! []

wrapRPCcmds : ClientId -> ClientDebuggerModel model -> MultitierCmd remoteServerMsg msg -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg), List Int)
wrapRPCcmds cid model cmd = case cmd of
  ServerCmd remoteServerMsg -> ({ model | rpccounter = model.rpccounter + 1 }, Multitier.map (RemoteServerAppMsg cid model.runCycleID model.rpccounter) (AppMsg model.runCycleID Maybe.Nothing) cmd, [model.rpccounter])
  ClientCmd _ -> (model, Multitier.map (RemoteServerAppMsg cid model.runCycleID model.rpccounter) (AppMsg model.runCycleID Maybe.Nothing) cmd, [])
  Batch cmds -> List.foldl (\currentCmd (prevModel, prevCmd, prevRPCids) -> let (newModel, newCmd, newRPCids) = (wrapRPCcmds cid prevModel currentCmd) in
                                                                    (newModel, Multitier.batch [newCmd,prevCmd], List.append prevRPCids newRPCids)) (model,Multitier.none,[]) cmds

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let appSubs = \cmodel -> case cmodel.state of
    ClientRunning -> Sub.map (AppMsg cmodel.runCycleID Maybe.Nothing) (subscriptions cmodel.appModel)
    ClientPaused -> Sub.map (AppMsg cmodel.runCycleID Maybe.Nothing) (subscriptions cmodel.appModel)
    _ -> Sub.none
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
          Html.map ((AppMsg cmodel.runCycleID) Maybe.Nothing) (appView cmodel.appModel)], -- TODO if previous then previousModel
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"],
          Html.pre [] [
            Html.text (toString cmodel.appModel)]
          --,eventsView cmodel
          ]]
      in case cmodel.state of
        ClientRunning ->
          view []
        ClientPaused ->
          view [disabled True, style [("opacity", "0.25")]]
        ClientUnvalid -> Html.text "Client is not valid anymore... Refresh to start a new client session."

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
    clients = TimeLine.clients smodel.timeline |> List.indexedMap (\index cid -> ((toString cid), index)) in
  let
    clientIndices = clients |> Dict.fromList in
  let
    previousIndex = case smodel.previous of
      Just previous -> previous.index
      _ -> (TimeLine.length smodel.timeline) - 1
    offset = 10
    eventSpacing = 25
    circles =
      TimeLine.view smodel.timeline
        |> List.map (\(index, event) -> case event of
          ServerEvent rcid sserverEvent ->
            [Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] []]
          ClientEvent cid rcid clientEvent ->
            let clientIndex = Maybe.withDefault 0 (Dict.get (toString cid) clientIndices) in
              let circle = Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy (toString ((clientIndex * 40) + 60)), onClick (GoBack index), style [("cursor", "pointer")]] [] in
                let (maybeRPCid, rpcIds) = case clientEvent of
                  Init ids -> (Maybe.Nothing, ids)
                  MsgEvent id ids _ -> (id,ids) in
                    let rpcLines = rpcIds |> List.map (\rpcid ->
                      let serverEventIndex = TimeLine.getRPCeventIndex cid rpcid smodel.timeline in
                        case serverEventIndex of
                          Just serverIndex -> [Svg.line [x1 (toString ((index * eventSpacing) + offset )), y1 (toString ((clientIndex * 40) + 60)), x2 (toString ((serverIndex * eventSpacing) + offset )), y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []]
                          _ -> []) in
                            case maybeRPCid of
                              Just msgRPCid -> let rpcLine = let serverEventIndex = TimeLine.getRPCeventIndex cid msgRPCid smodel.timeline in
                                case serverEventIndex of
                                  Just serverIndex -> [Svg.line [x1 (toString ((index * eventSpacing) + offset )) , y1 (toString ((clientIndex * 40) + 60)), x2 (toString ((serverIndex * eventSpacing) + offset )), y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []]
                                  _ -> [] in
                                circle :: (List.append rpcLine (List.concat rpcLines))
                              _ -> circle :: (List.concat rpcLines))

        |> List.concat


    serverLine = Svg.line [x1 (toString offset), y1 "20", x2 "100%", y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []
    clientLines =
      clients
        |> List.map (\(_,index) -> Svg.line [x1 (toString offset), y1 (toString ((index * 40) + 60)), x2 "100%", y2 (toString ((index * 40) + 60)), style [("stroke", "black"), ("stroke-width", "3")]] [])
  in
  Html.div [id "timeline", style [("overflow-x", "auto")]] [
    Svg.svg [ width (toString ((((TimeLine.length smodel.timeline) - 1) * eventSpacing) + (offset * 2))), height (toString (40 * ((TimeLine.numberOfClients smodel.timeline) + 1)))]
      (List.concat [
        [serverLine],
        clientLines,
        circles ])]

eventsView : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
eventsView smodel =
  let
    previousIndex = case smodel.previous of
      Just previous -> previous.index
      _ -> (TimeLine.length smodel.timeline) - 1
    options = TimeLine.view smodel.timeline
      |> List.map (\(index, event) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (eventView event)])
      |> List.reverse
  in Html.div [] [
    Html.select [size 15] options]

type alias ActionProps msg =
  { btnAction: msg
  , btnText: String
  , hideResumeFrom: Bool }

eventView : Event serverMsg remoteServerMsg msg -> String
eventView event = case event of
  ClientEvent cid rcid cevent -> clientEventView cid rcid cevent
  ServerEvent rcid sevent -> serverEventView rcid sevent

clientEventView : ClientId -> RunCycleID -> ClientEventType msg -> String
clientEventView cid rcid event = let result =
  case event of
    Init rpcids -> "[Init-"++ (toString cid) ++ "-" ++ (toString rpcids) ++"]"
    MsgEvent maybeRPCid rpcids msg -> case maybeRPCid of
      Just rpcid -> "[Msg-"++ (toString cid) ++"-"++ (toString rpcid) ++ "-" ++ (toString rpcids) ++"] " ++ (toString msg)
      _ -> "[Msg-"++ (toString cid) ++ "-" ++ (toString rpcids) ++"] " ++ (toString msg)
   in ("("++(toString rcid)++")" ++ result)

serverEventView : RunCycleID -> ServerEventType serverMsg remoteServerMsg -> String
serverEventView rcid event = let result =
  case event of
    InitServer -> "[Init-Server]"
    ServerMsgEvent msg -> "[ServerMsg] " ++ (toString msg)
    RPCevent cid rpcid msg -> "[RP-" ++ (toString cid) ++"-" ++ (toString rpcid) ++"] " ++ (toString msg)
   in ("("++(toString rcid)++")" ++ result)

serverActions : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverActions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
    Html.text "Resume from: ",
    Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume)]
