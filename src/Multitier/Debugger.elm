module Multitier.Debugger
  exposing
    ( program
    , Model, ServerModel
    , Msg, ServerMsg )

import Html exposing (Html)
import Html.Attributes exposing (checked, style, disabled, size, value, type_, selected, id)
import Html.Events exposing (onClick, onCheck, on)
import Svg
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, r, cx, cy, fill, stroke, d)
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

import Multitier.Debugger.TimeLine as TimeLine exposing (..)

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
    , initServer = wrapInitServer stuff.updateServer stuff.update stuff.initServer
    , updateServer = wrapUpdateServer stuff.updateServer
    , serverSubscriptions = wrapServerSubscriptions stuff.serverSubscriptions
    }

-- SERVER-MODEL

type alias ServerModel serverModel serverMsg remoteServerMsg model msg =
  { debugger: ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg
  , updateClient : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
  , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
  , clients :
    { debugging: Dict String ClientId }}

type alias ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg =
  { state: ServerDebuggerState
  , appModel : serverModel
  , timeline : TimeLine serverModel serverMsg remoteServerMsg model msg
  , previous : Maybe (PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg))
  , messagesReceivedDuringPaused : Array (RunCycle, PausedServerMessage serverModel serverMsg remoteServerMsg msg)
  , resume : ResumeStrategy
  , clientIds : List String
  , msgCount : Int
  , rpcMsgCount : Int
  , runCycle : RunCycle
  , showParentage : Bool
 }

type alias PreviousState serverModel serverCmd model cmd =
   { appModel : serverModel
   , cmd : serverCmd
   , parentMsg : ParentServerMsg
   , rpcMsgCount : Int
   , msgCount : Int
   , clients : Dict String (ClientId, (model,cmd, ParentMsg, Int, Int))
   , index : Int }

type PausedServerMessage serverModel serverMsg remoteServerMsg msg =
  PausedServerAppMsg ParentServerMsg serverMsg |
  PausedRemoteServerAppMsg ClientId (serverModel -> (serverModel, Cmd serverMsg)) Int remoteServerMsg |
  PausedClientAppMsg (PausedClientMsg msg)

type ResumeStrategy = FromPrevious | FromPaused

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused]

type ServerDebuggerState = Running | Paused



socketPath : String
socketPath = "debug"

wrapInitServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg remoteServerMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer updateServer updateClient (serverModel, cmd) =
  let msgCount = 0
      rpcMsgCount = 0
      runCycle = 0
   in
  { debugger =
    { state = Running
    , appModel = serverModel
    , timeline = TimeLine.empty serverModel |> TimeLine.pushServerEvent runCycle rpcMsgCount (msgCount + 1) (InitServerEvent, serverModel, cmd, None)
    , previous = Maybe.Nothing
    , messagesReceivedDuringPaused = Array.empty
    , resume = FromPaused
    , clientIds = []
    , msgCount = msgCount + 1
    , rpcMsgCount = rpcMsgCount
    , runCycle = runCycle
    , showParentage = True }
  , updateClient = updateClient
  , updateServer = updateServer
  , clients =
    { debugging = Dict.empty }} ! [Cmd.map (ServerAppMsg (RegularServerMsg msgCount) runCycle) cmd]

type ServerMsg serverMsg = ServerAppMsg ParentServerMsg RunCycle serverMsg |

                           OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger
      clients = serverModel.clients in
  let (newServerModel, newCmds) = case serverMsg of
    ServerAppMsg parent runCycle serverAppMsg ->
      if runCycle < debugger.runCycle then
        if TimeLine.isServerParentMember runCycle parent debugger.timeline then
          case serverModel.debugger.state of
            Running -> updateServerAppModel parent serverAppMsg serverModel
            Paused -> storePausedMessage updateServer runCycle parent serverAppMsg serverModel
        else let test = Debug.log "Message discarded because of previous runCycle and removed parent:" (toString serverAppMsg) in serverModel ! [] -- Message discarded...
      else case serverModel.debugger.state of
        Running -> updateServerAppModel parent serverAppMsg serverModel
        Paused -> storePausedMessage updateServer runCycle parent serverAppMsg serverModel
    OnClientConnect cid ->
      { serverModel | debugger =  { debugger | clientIds = (toString cid) :: debugger.clientIds }} ! [initializeClient serverModel cid]
    OnClientDisconnect cid ->
      { serverModel | clients =   { clients  | debugging = Dict.remove (toString cid) clients.debugging }} ! []

    Nothing -> serverModel ! []

  in newServerModel ! [newCmds, sendDebuggerModel newServerModel]

updateServerAppModel : ParentServerMsg -> serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
updateServerAppModel parent serverAppMsg serverModel = let debugger = serverModel.debugger in
  let (newAppModel, cmd) = serverModel.updateServer serverAppMsg debugger.appModel in
    case parent of
      None ->
        { serverModel | debugger = { debugger | appModel = newAppModel, msgCount = debugger.msgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount (debugger.msgCount + 1) (((ServerMsgEvent (NewServerMsg debugger.msgCount)) serverAppMsg), newAppModel, cmd, (RegularServerMsg debugger.msgCount)) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (RegularServerMsg debugger.msgCount) debugger.runCycle) cmd]
      RegularServerMsg parentID ->
        { serverModel | debugger = { debugger | appModel = newAppModel, msgCount = debugger.msgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount (debugger.msgCount + 1) (((ServerMsgEvent (ServerChildMsg parentID debugger.msgCount)) serverAppMsg), newAppModel, cmd, (RegularServerMsg debugger.msgCount)) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (RegularServerMsg debugger.msgCount) debugger.runCycle) cmd]
      ServerRPC cid rpcid ->
        { serverModel | debugger = { debugger | appModel = newAppModel, rpcMsgCount = debugger.rpcMsgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle (debugger.rpcMsgCount + 1) debugger.msgCount (((ServerMsgEvent (RPCserverMsg cid rpcid debugger.rpcMsgCount)) serverAppMsg), newAppModel, cmd, (ServerRPCmsg (cid, rpcid, debugger.rpcMsgCount))) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (ServerRPCmsg (cid, rpcid, debugger.rpcMsgCount)) debugger.runCycle) cmd]
      ServerRPCmsg (cid,rpcid,rpcmsgid) ->
        { serverModel | debugger = { debugger | appModel = newAppModel, msgCount = debugger.msgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount (debugger.msgCount + 1) (((ServerMsgEvent (RPCchildServerMsg (cid,rpcid,rpcmsgid) debugger.msgCount)) serverAppMsg), newAppModel, cmd, (RegularServerMsg debugger.msgCount)) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (RegularServerMsg debugger.msgCount) debugger.runCycle) cmd]

storePausedMessage : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> RunCycle -> ParentServerMsg -> serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
storePausedMessage updateServer runCycle parent serverAppMsg serverModel = let debugger = serverModel.debugger in
  case parent of
    None -> serverModel ! [] -- message discarded TODO check for certain
    _ -> { serverModel | debugger = { debugger | messagesReceivedDuringPaused = Array.push (runCycle, PausedServerAppMsg parent serverAppMsg) debugger.messagesReceivedDuringPaused}} ! []

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
  in sendSocketMsg cid (InitializeClient { cid= cid, paused= paused, runCycle= serverModel.debugger.runCycle })

sendSocketMsg : ClientId -> SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendSocketMsg cid msg = ServerWebSocket.send socketPath cid (Encode.encode 0 (toJSON msg))

broadcastPauseAction : Cmd (ServerMsg serverMsg)
broadcastPauseAction = broadcastSocketMsg (PauseClient)

broadcastResumeFromPausedAction : Cmd (ServerMsg serverMsg)
broadcastResumeFromPausedAction = broadcastSocketMsg ResumeClientFromPaused

broadcastResumeFromPreviousAction : PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) -> ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastResumeFromPreviousAction previous serverModel =
  TimeLine.clients serverModel.debugger.timeline
    |> List.map (\cid -> case Dict.member (toString cid) previous.clients of
        True -> sendSocketMsg cid (ResumeClientFromPrevious (Dict.get (toString cid) previous.clients))
        False -> sendSocketMsg cid (ResumeClientFromPrevious Maybe.Nothing))
    |> Cmd.batch
-- TODO repop clients + delete when disconnecting...

broadcastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastSocketMsg msg = ServerWebSocket.broadcast socketPath (Encode.encode 0 (toJSON msg))

multicastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> List ClientId -> Cmd (ServerMsg serverMsg)
multicastSocketMsg msg ids = ServerWebSocket.multicast socketPath ids (Encode.encode 0 (toJSON msg))

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg RunCycle ClientId Int remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int |
  SetDebuggerResumeStrategy ResumeStrategy | ToggleShowParentageDebugger Bool |

  AddClientEvent ClientId Int Int (ClientEventType msg) model (MultitierCmd remoteServerMsg msg) ParentMsg |
  AddPausedClientEvent RunCycle (PausedClientMsg msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  StartDebugView cid ->
    rpc HandleStartDebugView (\serverModel -> let clients = serverModel.clients in
      ({serverModel | clients = { clients | debugging = Dict.insert (toString cid) cid clients.debugging }}, Task.succeed (serverModel.debugger), Cmd.none))
  StopDebugView cid ->
    rpc HandleStopDebugView (\serverModel -> let clients = serverModel.clients in
      ({ serverModel | clients = { clients | debugging = Dict.remove (toString cid) clients.debugging }}, Task.succeed (), Cmd.none))

  AddClientEvent cid rpcCount msgCount event model cmd parentMsg -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      case debugger.state of
        Running ->
          let newServerModel = { serverModel | debugger = { debugger | timeline = TimeLine.pushClientEvent debugger.runCycle cid rpcCount msgCount (event,model,cmd,parentMsg) debugger.timeline }} in
            (newServerModel, Task.succeed (), sendDebuggerModel newServerModel)
        Paused -> (serverModel, Task.succeed (), Cmd.none)) -- TODO what to do in this case? check runcycle?

  AddPausedClientEvent runCycle (cid,parentMsg,maybeRPCid,msg) -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      case debugger.state of
        Running -> (serverModel, Task.succeed (), Cmd.none) -- TODO what to do in this case?
        Paused -> let newServerModel = { serverModel | debugger = { debugger | messagesReceivedDuringPaused = Array.push (runCycle, PausedClientAppMsg (cid,parentMsg,maybeRPCid,msg)) debugger.messagesReceivedDuringPaused }} in
          (newServerModel, Task.succeed (), Cmd.none))

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
        let (newServerModel, newServerCmd) =
          case serverModel.debugger.resume of
            FromPaused -> resumeServerFromPaused serverModel
            FromPrevious -> case serverModel.debugger.previous of
              Just previous -> resumeServerFromPrevious previous serverModel
              _ -> resumeServerFromPaused serverModel in
         (newServerModel, Task.succeed (), Cmd.batch [newServerCmd, sendDebuggerModel newServerModel])
      _ -> (serverModel, Task.succeed (), Cmd.none))

  SetDebuggerResumeStrategy resume ->
    rpc Handle (\serverModel ->
      let debugger = serverModel.debugger in
        let newServerModel = { serverModel | debugger = { debugger | resume = resume }} in (newServerModel, Task.succeed (), sendDebuggerModel newServerModel))
  ToggleShowParentageDebugger showParentage ->
    rpc Handle (\serverModel ->
      let debugger = serverModel.debugger in
        let newServerModel = { serverModel | debugger = { debugger | showParentage = showParentage}} in (newServerModel, Task.succeed (), sendDebuggerModel newServerModel))

  GoBackDebugger index -> rpc Handle
    (\serverModel ->
      let debugger = serverModel.debugger in
        let (newServerModel, newCmd) =
          case (index == TimeLine.length debugger.timeline) of
            True -> { serverModel | debugger = { debugger | previous = Maybe.Nothing }} ! []
            False -> let (previousAppModel, previousCmd, previousParentMsg, previousRpcMsgCount, previousMsgCount, previousClients) = (TimeLine.previousState index debugger.timeline) in
              { serverModel | debugger = { debugger | previous = (Just (PreviousState previousAppModel previousCmd previousParentMsg previousRpcMsgCount previousMsgCount previousClients index)) }} ! []
          in (newServerModel, Task.succeed (), Cmd.batch [newCmd, sendDebuggerModel newServerModel]))


  RemoteServerAppMsg runCycle cid rpcid msg ->
    RPC.map (AppMsg NoParentMsg runCycle (Just rpcid))
      (\updateAppModel serverModel -> let debugger = serverModel.debugger in
        -- if runCycle < debugger.runCycle then
        --   if TimeLine.isClientParentMember runCycle cid (parent) debugger.timeline then
        --     case debugger.state of
        --       Running -> updateServerAppModelFromRPC updateAppModel cid rpcid msg serverModel
        --       Paused -> storePausedRPCMessage updateAppModel runCycle cid rpcid msg serverModel
        --   else let test = Debug.log "RPC Message discarded because of previous runCycle and removed parent:" (toString msg) in serverModel ! [] -- Message discarded...
        -- else case debugger.state of
        --   Running -> updateServerAppModelFromRPC updateAppModel cid rpcid msg serverModel
        --   Paused -> storePausedRPCMessage updateAppModel runCycle cid rpcid msg serverModel) TODO connect clientmsg -> rpc -> clientmsg
        case serverModel.debugger.state of
          Running -> updateServerAppModelFromRPC updateAppModel cid rpcid msg serverModel
          Paused -> storePausedRPCMessage updateAppModel runCycle cid rpcid msg serverModel)
      (\serverModel -> serverModel.debugger.appModel)
      (serverRPCs msg)

updateServerAppModelFromRPC : (serverModel -> (serverModel,Cmd serverMsg)) -> ClientId -> Int -> remoteServerMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
updateServerAppModelFromRPC updateAppModel cid rpcid msg serverModel = let debugger = serverModel.debugger in
  let (newAppModel,newAppCmd) = updateAppModel debugger.appModel in
    let newServerModel = { serverModel | debugger = { debugger | appModel = newAppModel, timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount debugger.msgCount (ServerRPCevent cid rpcid msg, newAppModel, Cmd.none, (ServerRPC cid rpcid)) debugger.timeline }} in
      newServerModel ! [Cmd.map (ServerAppMsg (ServerRPC cid rpcid) debugger.runCycle) newAppCmd, sendDebuggerModel newServerModel]

storePausedRPCMessage : (serverModel -> (serverModel,Cmd serverMsg)) -> RunCycle -> ClientId -> Int -> remoteServerMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
storePausedRPCMessage updateAppModel runCycle cid rpcid msg serverModel = { serverModel | debugger = let debugger = serverModel.debugger in
  { debugger | messagesReceivedDuringPaused = Array.push (runCycle, PausedRemoteServerAppMsg cid updateAppModel rpcid msg) debugger.messagesReceivedDuringPaused}} ! []

resumeServerFromPaused : ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
resumeServerFromPaused serverModel = updateServerWithPausedMessages (Array.toList serverModel.debugger.messagesReceivedDuringPaused) (serverModel, Cmd.none)

updateServerWithPausedMessages : List (RunCycle, PausedServerMessage serverModel serverMsg remoteServerMsg msg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)) -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
updateServerWithPausedMessages messages (serverModel, cmd) = let debugger = serverModel.debugger in
  case messages of
    [] -> { serverModel | debugger = { debugger | state = Running, messagesReceivedDuringPaused = Array.empty }} ! [cmd, broadcastResumeFromPausedAction]
    (runCycle,message) :: otherMessages ->
      let (newServerModel,newServerCmd) = case message of
        PausedServerAppMsg parentMsg serverAppMsg -> updateServerAppModel parentMsg serverAppMsg serverModel
        PausedRemoteServerAppMsg cid updateAppModel rpcid remoteServerMsg -> updateServerAppModelFromRPC updateAppModel cid rpcid remoteServerMsg serverModel
        PausedClientAppMsg (cid,parentMsg,maybeRPCid,msg) -> (serverModel,cmd) in -- TODO
       updateServerWithPausedMessages otherMessages (newServerModel ! [cmd,newServerCmd])

resumeServerFromPrevious : PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
resumeServerFromPrevious previous serverModel = let debugger = serverModel.debugger in
  let (newTimeline, eventsToCheck) = TimeLine.goBack previous.index debugger.timeline
      messagesReceivedDuringPaused = debugger.messagesReceivedDuringPaused |> Array.toList in
   let newServerModel = { serverModel | debugger = { debugger | timeline = newTimeline, appModel = previous.appModel, runCycle = debugger.runCycle + 1, msgCount = previous.msgCount, rpcMsgCount = previous.rpcMsgCount }} in
    checkEvents eventsToCheck previous.index newServerModel
      |> (\model -> (model,Cmd.none))
      |> checkPaused previous (Array.toList serverModel.debugger.messagesReceivedDuringPaused)

checkEvents : List (RunCycle, Event serverMsg remoteServerMsg msg) -> Int -> ServerModel serverModel serverMsg remoteServerMsg model msg -> ServerModel serverModel serverMsg remoteServerMsg model msg
checkEvents eventsToCheck goBackIndex serverModel = case eventsToCheck of
  [] -> serverModel
  (runCycle,event) :: otherEvents ->
    let newServerModel =
      case event of
        ServerEvent eventType ->
          let handleEvent parent serverMsg =
            if TimeLine.isServerParentMember runCycle parent serverModel.debugger.timeline || TimeLine.isServerParentMember serverModel.debugger.runCycle parent serverModel.debugger.timeline then
              case TimeLine.getServerEventParentIndex eventType serverModel.debugger.timeline of
                Just index -> case goBackIndex == index of
                  False -> let (newServerModel,_) = updateServerAppModel parent serverMsg serverModel in newServerModel
                  True -> let test = Debug.log "Message discarded because parent is the go back point:" (toString serverMsg) in serverModel -- Message discarded...
                _ -> serverModel -- Not possible
            else let test = Debug.log "Message discarded because parent does not exist in new timeline:" (toString serverMsg) in serverModel  -- Message discarded...
          in case eventType of
            ServerMsgEvent msgType serverMsg -> case msgType of
              NewServerMsg msgid -> handleEvent None serverMsg
              ServerChildMsg parentid msgid -> handleEvent (RegularServerMsg parentid) serverMsg
              RPCserverMsg cid rpcid rpcmsgid -> handleEvent (ServerRPC cid rpcid) serverMsg
              RPCchildServerMsg (cid,rpcid,rpcmsgid) msgid -> handleEvent (ServerRPCmsg (cid,rpcid,rpcmsgid)) serverMsg
            ServerRPCevent cid rpcid remoteServerMsg -> serverModel -- TODO check client parent still exists
            _ -> serverModel
        ClientEvent cid eventType -> serverModel --TODO
          -- case eventType of
          --   Init ids -> (serverModel,cmd)
          --   MsgEvent msgType maybeRPCid ids msg -> case msgType of
          --     NewClientMsg msgid -> (serverModel,cmd)
          --     ClientChildMsg parentid msgid -> (serverModel,cmd)
    in checkEvents otherEvents goBackIndex newServerModel

checkPaused :
  PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) ->
  List (RunCycle, PausedServerMessage serverModel serverMsg remoteServerMsg msg) ->
  (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)) ->
  (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
checkPaused previous messages (serverModel,cmd) = let debugger = serverModel.debugger in
  case messages of
    [] -> { serverModel | debugger = { debugger | state = Running, previous = Maybe.Nothing }}
      ! [Cmd.map (ServerAppMsg previous.parentMsg debugger.runCycle) previous.cmd, cmd, broadcastResumeFromPreviousAction previous serverModel]
    (runCycle,message) :: otherMessages -> case message of
      PausedServerAppMsg parentMsg serverAppMsg ->
        if TimeLine.isServerParentMember runCycle parentMsg debugger.timeline then
          let (newServerModel,newServerCmd) = updateServerAppModel parentMsg serverAppMsg serverModel in
            newServerModel ! [cmd,newServerCmd]
        else let test = Debug.log "Message discarded because parent does not exist in new timeline:" (toString serverAppMsg) in (serverModel,cmd) -- Message discarded...
      PausedRemoteServerAppMsg cid updateAppModel rpcid remoteServerMsg ->
        let (newServerModel,newServerCmd) = updateServerAppModelFromRPC updateAppModel cid rpcid remoteServerMsg serverModel in -- TODO check client parent still exists
          newServerModel ! [cmd,newServerCmd]
      PausedClientAppMsg (cid,parentMsg,maybeRPCid,msg) -> (serverModel,cmd) -- TODO

-- SERVER-STATE

type alias ServerState serverState = { appServerState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> ServerState serverState)
wrapServerState serverState = \serverModel -> ServerState (serverState serverModel.debugger.appModel)

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = Sub.map (ServerAppMsg None serverModel.debugger.runCycle) (serverSubscriptions serverModel.debugger.appModel)
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
  , runCycle : RunCycle
  , rpccounter : Int
  , msgCount : Int }

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
wrapInit init = \serverState -> Uninitialized (init serverState.appServerState) !! []

type Msg model msg serverModel serverMsg remoteServerMsg =
  AppMsg ParentMsg RunCycle (Maybe Int) msg |

  OnSocketMsg String |

  Pause | Resume | GoBack Int |
  SetResume Int | ToggleShowParentage Bool |

  SwitchDebugger |

  Handle (Result Error ()) |
  HandleStartDebugView (Result Error (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg)) | HandleStopDebugView (Result Error ())

type alias InitData =
  { cid: ClientId
  , paused: Bool
  , runCycle: RunCycle }

type alias PausedClientMsg msg = (ClientId,ParentMsg,(Maybe Int),msg)

type SocketMsg serverModel serverMsg remoteServerMsg model msg =
  InitializeClient InitData |
  SetServerModel (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) |

  PauseClient | ResumeClientFromPaused | ResumeClientFromPrevious (Maybe (ClientId, (model,(MultitierCmd remoteServerMsg msg), ParentMsg, Int, Int)))

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg remoteServerMsg -> Model model msg serverModel serverMsg remoteServerMsg -> ( Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) ))
wrapUpdate update = \msg model -> case model of

  Uninitialized (appModel, cmd) -> case msg of
    OnSocketMsg data -> case (fromJSONString data) of
      InitializeClient initData ->
        let msgCount = 0
            rpcCount = 0 in
          let newcmodel = case initData.paused of
            True -> ClientDebuggerModel ClientPaused appModel initData.runCycle rpcCount (msgCount + 1)
            False -> ClientDebuggerModel ClientRunning appModel initData.runCycle rpcCount (msgCount + 1) in
              let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds initData.cid (RegularMsg msgCount) newcmodel cmd in
                ClientDebugger initData.cid rpcWrappedModel !! [rpcWrappedCmds, performOnServer (AddClientEvent initData.cid newcmodel.rpccounter (msgCount + 1) (Init rpcIds) appModel cmd (RegularMsg msgCount)) ]
      _ -> model !! []
    _ -> model !! []

  ClientDebugger cid cmodel -> case msg of
    AppMsg parent runCycle rpcid appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg parent cid rpcid cmodel in ClientDebugger cid newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClientFromPaused -> let (newcmodel, cmd) = resumeClientFromPaused cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClientFromPrevious prev -> let (newcmodel, cmd) = resumeClientFromPrevious cid prev cmodel in ClientDebugger cid newcmodel !! [cmd]
      _ -> model !! []

    SwitchDebugger -> Switching cid cmodel !! [performOnServer (StartDebugView cid)]

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  Switching cid cmodel -> case msg of
    AppMsg parent runCycle rpcid appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg parent cid rpcid cmodel in Switching cid newcmodel !! [cmd]
    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in Switching cid newcmodel !! [cmd]
      ResumeClientFromPaused -> let (newcmodel, cmd) = resumeClientFromPaused cmodel in Switching cid newcmodel !! [cmd]
      ResumeClientFromPrevious prev -> let (newcmodel, cmd) = resumeClientFromPrevious cid prev cmodel in Switching cid newcmodel !! [cmd]
      _ -> model !! []

    HandleStartDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO handle error in view
      Ok serverDebuggerModel ->  ServerDebugger cid serverDebuggerModel cmodel !! []

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  ServerDebugger cid smodel cmodel -> case msg of
    AppMsg parent runCycle rpcid appMsg -> let (newcmodel, cmd) = updateAppModel update appMsg parent cid rpcid cmodel in ServerDebugger cid smodel newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      SetServerModel serverModel -> ServerDebugger cid serverModel cmodel !! []
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClientFromPaused -> let (newcmodel, cmd) = resumeClientFromPaused cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClientFromPrevious prev -> let (newcmodel, cmd) = resumeClientFromPrevious cid prev cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      _ -> model !! []

    Pause -> model !! [performOnServer PauseDebugger]
    Resume -> model !! [performOnServer ResumeDebugger]
    GoBack index -> model !! [performOnServer (GoBackDebugger index)]

    SetResume index -> case Array.get index resumeStrategies of
      Just resume -> model !! [performOnServer (SetDebuggerResumeStrategy resume)]
      _ -> model !! []
    ToggleShowParentage runInBackground -> model !! [performOnServer (ToggleShowParentageDebugger runInBackground)]

    SwitchDebugger -> model !! [performOnServer (StopDebugView cid)]
    HandleStopDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO
      _ -> ClientDebugger cid cmodel !! []
    _ -> model !! []

pauseClient : ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
pauseClient cmodel = { cmodel | state = ClientPaused } !! []

resumeClientFromPaused : ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClientFromPaused cmodel = { cmodel | state = ClientRunning } !! []

resumeClientFromPrevious : ClientId -> Maybe (ClientId, (model,(MultitierCmd remoteServerMsg msg), ParentMsg, Int, Int)) -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClientFromPrevious cid prev cmodel = case prev of
  Just (_,(prevModel,prevCmd, prevParentMsg, prevRpcCount, prevMsgCount)) ->
    let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid prevParentMsg { cmodel | rpccounter = prevRpcCount, msgCount = prevMsgCount } prevCmd in
      { rpcWrappedModel | state = ClientRunning, appModel = prevModel } !! [rpcWrappedCmds]
  _ -> { cmodel | state = ClientUnvalid } !! []


updateAppModel : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ParentMsg -> ClientId -> Maybe Int -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModel update appMsg parentMsg cid rpcid cmodel =
  case cmodel.state of
    ClientRunning ->
      let (newAppModel, newCmd) = update appMsg cmodel.appModel in
        case parentMsg of
          NoParentMsg ->
            let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.msgCount) cmodel newCmd in
              { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (NewClientMsg cmodel.msgCount) rpcid rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.msgCount))]
          RegularMsg parentId ->
            let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.msgCount) cmodel newCmd in
              { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (ClientChildMsg parentId cmodel.msgCount) rpcid rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.msgCount))]
    _ -> cmodel !! [performOnServer (AddPausedClientEvent cmodel.runCycle (cid,parentMsg,rpcid,appMsg))]

wrapRPCcmds : ClientId -> ParentMsg -> ClientDebuggerModel model -> MultitierCmd remoteServerMsg msg -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg), List Int)
wrapRPCcmds cid parentMsg model cmd = case cmd of
  ServerCmd remoteServerMsg -> ({ model | rpccounter = model.rpccounter + 1 }, Multitier.map (RemoteServerAppMsg model.runCycle cid model.rpccounter) (AppMsg parentMsg model.runCycle Maybe.Nothing) cmd, [model.rpccounter])
  ClientCmd _ -> (model, Multitier.map (RemoteServerAppMsg model.runCycle cid model.rpccounter) (AppMsg parentMsg model.runCycle Maybe.Nothing) cmd, [])
  Batch cmds -> List.foldl (\currentCmd (prevModel, prevCmd, prevRPCids) -> let (newModel, newCmd, newRPCids) = (wrapRPCcmds cid parentMsg prevModel currentCmd) in
                                                                    (newModel, Multitier.batch [newCmd,prevCmd], List.append prevRPCids newRPCids)) (model,Multitier.none,[]) cmds

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let appSubs = \cmodel -> case cmodel.state of
    ClientRunning -> Sub.map (AppMsg NoParentMsg cmodel.runCycle Maybe.Nothing) (subscriptions cmodel.appModel)
    ClientPaused -> Sub.map (AppMsg NoParentMsg cmodel.runCycle Maybe.Nothing) (subscriptions cmodel.appModel)
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
          Html.map ((AppMsg NoParentMsg cmodel.runCycle) Maybe.Nothing) (appView cmodel.appModel)], -- TODO if previous then previousModel
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
        Html.pre [] [Html.text (toString (Array.map (\(runCycle,message) -> case message of
          PausedServerAppMsg parent serverMsg -> ("PausedServerAppMsg parent:(" ++ (toString parent) ++")-" ++ (toString serverMsg))
          PausedRemoteServerAppMsg cid _ rpcid remoteServerMsg -> ("PausedRemoteServerAppMsg cid:(" ++ (toString cid) ++ "," ++ (toString rpcid) ++")-" ++ (toString remoteServerMsg))
          PausedClientAppMsg (cid,parentMsg,maybeRPCid,msg) -> ("PausedClientAppMsg cid:(" ++ (toString cid) ++ ",parent:"  ++ (toString parentMsg) ++ ",rpc:" ++ (toString maybeRPCid) ++ ")-" ++ (toString msg))
           ) smodel.messagesReceivedDuringPaused))],
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
        |> List.map (\(index, (runCycle,event)) -> case event of
          ServerEvent serverEventType ->
            let parentLine = if smodel.showParentage then
              case TimeLine.getServerEventParentIndex serverEventType smodel.timeline of
                Just parentIndex ->
                  let px1 = (parentIndex * eventSpacing) + offset
                      px2 = (index * eventSpacing) + offset
                      qx = px1 + ((px2 - px1) // 2) in
                    [Svg.path [d ("M "++(toString px1)++" 20 Q "++(toString qx)++ " 0 "++(toString px2)++" 20"), stroke "black", fill "transparent"] []]
                _ -> []
              else []
            in List.append parentLine
              [Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] []]
          ClientEvent cid clientEvent ->
            let clientIndex = Maybe.withDefault 0 (Dict.get (toString cid) clientIndices) in
              let circle =
                let parentLine = if smodel.showParentage then
                  case TimeLine.getClientEventParentIndex cid clientEvent smodel.timeline of
                    Just parentIndex ->
                      let px1 = (parentIndex * eventSpacing) + offset
                          py1 = (clientIndex * 40) + 60
                          px2 = (index * eventSpacing) + offset
                          py2 = (clientIndex * 40) + 60
                          qx = px1 + ((px2 - px1) // 2)
                          qy = ((clientIndex * 40) + 60) - 20 in
                        [Svg.path [d ("M "++(toString px1)++" "++(toString py1)++" Q "++(toString qx)++ " "++(toString qy)++" "++(toString px2)++" "++(toString py2)), stroke "black", fill "transparent"] []]
                    _ -> []
                  else []
                 in List.append parentLine [Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy (toString ((clientIndex * 40) + 60)), onClick (GoBack index), style [("cursor", "pointer")]] []] in
                let (maybeRPCid, rpcIds) = case clientEvent of
                  Init ids -> (Maybe.Nothing, ids)
                  MsgEvent _ id ids _ -> (id,ids) in
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
                                List.append circle (List.append rpcLine (List.concat rpcLines))
                              _ -> List.append circle (List.concat rpcLines))

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
      |> List.map (\(index, (runCycle,event)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (eventView runCycle event)])
      |> List.reverse
  in Html.div [] [
    Html.select [size 15] options]

type alias ActionProps msg =
  { btnAction: msg
  , btnText: String
  , hideResumeFrom: Bool }

eventView : RunCycle -> Event serverMsg remoteServerMsg msg -> String
eventView runCycle event = case event of
  ClientEvent cid cevent -> clientEventView runCycle cid cevent
  ServerEvent sevent -> serverEventView runCycle sevent

clientEventView : RunCycle -> ClientId -> ClientEventType msg -> String
clientEventView runCycle cid event = "(" ++ (toString runCycle) ++ ")" ++
  case event of
    Init rpcids -> "[Init-"++ (toString cid) ++ "-" ++ (toString rpcids) ++"]"
    MsgEvent msgType maybeRPCid rpcids msg -> case maybeRPCid of
      Just rpcid -> let msgid = case msgType of
        NewClientMsg id -> id
        ClientChildMsg _ id -> id in -- this case should never happen
         "[RPC-Msg-"++ (toString cid) ++ "-" ++ (toString msgid) ++ "-" ++ (toString rpcid) ++ "-" ++ (toString rpcids) ++"] " ++ (toString msg)
      _ -> case msgType of
        NewClientMsg msgid -> "[NewMsg-"++ (toString cid) ++ "-" ++ (toString msgid) ++ "-" ++ (toString rpcids) ++"] " ++ (toString msg)
        ClientChildMsg parentid msgid -> "[ChildMsg-"++ (toString cid) ++ "-(" ++ (toString parentid) ++ "," ++ (toString msgid) ++ ")-" ++ (toString rpcids) ++"] " ++ (toString msg)

serverEventView : RunCycle -> ServerEventType serverMsg remoteServerMsg -> String
serverEventView runCycle event = "(" ++ (toString runCycle) ++ ")" ++
  case event of
    InitServerEvent -> "[Init-Server]"
    ServerMsgEvent msgtype msg -> case msgtype of
      NewServerMsg msgid -> "[NewServerMsg](" ++ (toString msgid) ++ ")" ++ (toString msg)
      ServerChildMsg parentid msgid -> "[ChildServerMsg](" ++ (toString parentid) ++ "," ++ (toString msgid) ++ ")" ++ (toString msg)
      RPCserverMsg cid rpcid rpcmsgid -> "[RPCserverMsg](" ++ (toString cid) ++ "," ++ (toString rpcid) ++ "," ++ (toString rpcmsgid) ++")" ++ (toString msg)
      RPCchildServerMsg (cid,rpcid, rpcmsgid) msgid -> "[RPCchildServerMsg]((" ++ (toString cid) ++ "," ++ (toString rpcid) ++ "," ++ (toString rpcmsgid) ++")," ++ (toString msgid) ++")" ++ (toString msg)
    ServerRPCevent cid rpcid msg -> "[RP-" ++ (toString cid) ++"-" ++ (toString rpcid) ++"] " ++ (toString msg)

serverActions : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverActions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
    Html.text "Show parentage: ",
    Html.input [checked model.showParentage, type_ "checkbox", onCheck ToggleShowParentage] [],
    Html.text "Resume from: ",
    Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume)]
