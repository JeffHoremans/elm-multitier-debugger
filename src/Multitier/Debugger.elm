module Multitier.Debugger
  exposing
    ( program
    , Model, ServerModel
    , Msg, ServerMsg )

import Html exposing (Html)
import Html.Attributes exposing (checked, style, disabled, size, value, type_, selected, id)
import Html.Events exposing (onClick, onCheck, on)
import Svg
import Svg.Attributes exposing (width, height, viewBox, x, y, x1, x2, y1, y2, r, cx, cy, fill, stroke, d)
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
  , messagesReceivedDuringPaused : Array (PausedServerMessage serverModel serverMsg remoteServerMsg msg)
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
  PausedRemoteServerAppMsg RunCycle ClientId (serverModel -> (serverModel, Cmd serverMsg)) Int Int remoteServerMsg |
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
    , timeline = TimeLine.empty serverModel |> TimeLine.pushServerEvent runCycle rpcMsgCount (msgCount + 1) (InitServerEvent, serverModel, cmd, RegularServerMsg runCycle msgCount)
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
    { debugging = Dict.empty }} ! [Cmd.map (ServerAppMsg (RegularServerMsg runCycle msgCount)) cmd]

type ServerMsg serverMsg = ServerAppMsg ParentServerMsg serverMsg |

                           OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger
      clients = serverModel.clients in
  let (newServerModel, newCmds) = case serverMsg of
    ServerAppMsg parent serverAppMsg ->
      case serverModel.debugger.state of
        Running -> case parent of
          None -> updateServerAppModel parent serverAppMsg serverModel
          _ -> if TimeLine.isServerParentMember parent debugger.timeline then
            updateServerAppModel parent serverAppMsg serverModel
          else serverModel ! [] -- Message discarded...
        Paused -> if TimeLine.isServerParentMember parent debugger.timeline then
          storePausedMessage updateServer parent serverAppMsg serverModel
        else serverModel ! [] -- Message discarded...
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
        , timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount (debugger.msgCount + 1) (((ServerMsgEvent (NewServerMsg debugger.msgCount)) serverAppMsg), newAppModel, cmd, (RegularServerMsg debugger.runCycle debugger.msgCount)) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (RegularServerMsg debugger.runCycle debugger.msgCount)) cmd]
      RegularServerMsg parentRunCycle parentID ->
        { serverModel | debugger = { debugger | appModel = newAppModel, msgCount = debugger.msgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount (debugger.msgCount + 1) (((ServerMsgEvent (ServerChildMsg parentRunCycle parentID debugger.msgCount)) serverAppMsg), newAppModel, cmd, (RegularServerMsg debugger.runCycle debugger.msgCount)) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (RegularServerMsg debugger.runCycle debugger.msgCount)) cmd]
      ServerRPC parentRunCycle cid rpcid ->
        { serverModel | debugger = { debugger | appModel = newAppModel, rpcMsgCount = debugger.rpcMsgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle (debugger.rpcMsgCount + 1) debugger.msgCount (((ServerMsgEvent (RPCserverMsg parentRunCycle cid rpcid debugger.rpcMsgCount)) serverAppMsg), newAppModel, cmd, (ServerRPCmsg debugger.runCycle (cid, rpcid, debugger.rpcMsgCount))) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (ServerRPCmsg debugger.runCycle (cid, rpcid, debugger.rpcMsgCount))) cmd]
      ServerRPCmsg parentRunCycle (cid,rpcid,rpcmsgid) ->
        { serverModel | debugger = { debugger | appModel = newAppModel, msgCount = debugger.msgCount + 1
        , timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount (debugger.msgCount + 1) (((ServerMsgEvent (RPCchildServerMsg parentRunCycle (cid,rpcid,rpcmsgid) debugger.msgCount)) serverAppMsg), newAppModel, cmd, (RegularServerMsg debugger.runCycle debugger.msgCount)) debugger.timeline }}
          ! [Cmd.map (ServerAppMsg (RegularServerMsg debugger.runCycle debugger.msgCount)) cmd]

storePausedMessage : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> ParentServerMsg -> serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
storePausedMessage updateServer parent serverAppMsg serverModel = let debugger = serverModel.debugger in
  { serverModel | debugger = { debugger | messagesReceivedDuringPaused = Array.push (PausedServerAppMsg parent serverAppMsg) debugger.messagesReceivedDuringPaused}} ! []

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

broadcastResumeFromPausedAction : Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)) -> ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastResumeFromPausedAction clients serverModel =
  TimeLine.clients serverModel.debugger.timeline
    |> List.map (\cid -> sendSocketMsg cid (ResumeClientFromPaused (Dict.get (toString cid) clients)))
    |> Cmd.batch

broadcastResumeFromPreviousAction : PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) -> List ClientId -> Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)) -> ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastResumeFromPreviousAction previous oldClients clients serverModel =
  let allClients = TimeLine.clients serverModel.debugger.timeline
      allClientsDict = allClients |> List.map (\cid -> ((toString cid),cid)) |> Dict.fromList in
    allClients
      |> List.map (\cid -> sendSocketMsg cid (ResumeClientFromPrevious (Dict.get (toString cid) clients)))
      |> List.append (List.map (\cid -> case Dict.member (toString cid) allClientsDict of
        False -> sendSocketMsg cid InvalidateClient
        _ -> Cmd.none) oldClients)
      |> Cmd.batch
-- TODO repop clients + delete when disconnecting... ++ handle init of new client when paused

broadcastGoBack : Maybe (Dict String (ClientId, (model,cmd, ParentMsg, Int, Int))) -> ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastGoBack maybePreviousClients serverModel =
  case maybePreviousClients of
      Just previousClients ->
        let allClients = TimeLine.clients serverModel.debugger.timeline in
          allClients
            |> List.map (\cid -> case Dict.get (toString cid) previousClients of
              Just (_,(model,_,_,_,_)) -> sendSocketMsg cid (GoBackClient (PreviousModel (Just model)))
              _ -> sendSocketMsg cid (GoBackClient TempInvalid))
            |> Cmd.batch
      _ -> broadcastSocketMsg (GoBackClient (PreviousModel Maybe.Nothing))

broadcastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
broadcastSocketMsg msg = ServerWebSocket.broadcast socketPath (Encode.encode 0 (toJSON msg))

multicastSocketMsg : SocketMsg serverModel serverMsg remoteServerMsg model msg -> List ClientId -> Cmd (ServerMsg serverMsg)
multicastSocketMsg msg ids = ServerWebSocket.multicast socketPath ids (Encode.encode 0 (toJSON msg))

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg RunCycle ClientId Int Int remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int |
  SetDebuggerResumeStrategy ResumeStrategy | ToggleShowParentageDebugger Bool |

  IsParentStillMember ClientId ParentMsg msg |
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

  IsParentStillMember cid parentmsg msg -> rpc (ParentStillMember cid parentmsg msg)
    (\serverModel -> let debugger = serverModel.debugger in
      if TimeLine.isClientParentMember cid parentmsg debugger.timeline then
        (serverModel, Task.succeed True, Cmd.none)
      else (serverModel, Task.succeed False, Cmd.none))

  AddClientEvent cid rpcCount msgCount event model cmd parentMsg -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      let newServerModel = { serverModel | debugger = { debugger | timeline = TimeLine.pushClientEvent debugger.runCycle cid rpcCount msgCount (event,model,cmd,parentMsg) debugger.timeline }} in
        (newServerModel, Task.succeed (), sendDebuggerModel newServerModel))

  AddPausedClientEvent runCycle (cid,parentMsg,msg) -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      case debugger.state of
        Running -> (serverModel, Task.succeed (), Cmd.none) -- TODO what to do in this case?
        Paused -> let newServerModel = { serverModel | debugger = { debugger | messagesReceivedDuringPaused = Array.push (PausedClientAppMsg (cid,parentMsg,msg)) debugger.messagesReceivedDuringPaused }} in
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
            True -> { serverModel | debugger = { debugger | previous = Maybe.Nothing }} ! [broadcastGoBack Maybe.Nothing serverModel ]
            False -> let (previousAppModel, previousCmd, previousParentMsg, previousRpcMsgCount, previousMsgCount, previousClients) = (TimeLine.previousState index debugger.timeline) in
              { serverModel | debugger = { debugger | previous = (Just (PreviousState previousAppModel previousCmd previousParentMsg previousRpcMsgCount previousMsgCount previousClients index)) }} ! [broadcastGoBack (Just previousClients) serverModel]
          in (newServerModel, Task.succeed (), Cmd.batch [newCmd, sendDebuggerModel newServerModel]))


  RemoteServerAppMsg parentRunCycle cid parentid rpcid msg ->
    RPC.map (AppMsg (ServerRPCparentMsg parentRunCycle parentid rpcid))
      (\updateAppModel serverModel -> let debugger = serverModel.debugger in
        if TimeLine.isClientParentMember cid (RegularMsg parentRunCycle parentid) debugger.timeline then
          case debugger.state of
            Running -> updateServerAppModelFromRPC updateAppModel parentRunCycle cid parentid rpcid msg serverModel
            Paused -> storePausedRPCMessage updateAppModel parentRunCycle cid parentid rpcid msg serverModel
        else serverModel ! []) -- Message discarded...
      (\serverModel -> serverModel.debugger.appModel)
      (serverRPCs msg)

updateServerAppModelFromRPC : (serverModel -> (serverModel,Cmd serverMsg)) -> RunCycle -> ClientId -> Int -> Int -> remoteServerMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
updateServerAppModelFromRPC updateAppModel parentRunCycle cid parentid rpcid msg serverModel = let debugger = serverModel.debugger in
  let (newAppModel,newAppCmd) = updateAppModel debugger.appModel in
    let newServerModel = { serverModel | debugger = { debugger | appModel = newAppModel, timeline = TimeLine.pushServerEvent debugger.runCycle debugger.rpcMsgCount debugger.msgCount (ServerRPCevent parentRunCycle cid parentid rpcid updateAppModel msg, newAppModel, Cmd.none, (ServerRPC debugger.runCycle cid rpcid)) debugger.timeline }} in
      newServerModel ! [Cmd.map (ServerAppMsg (ServerRPC debugger.runCycle cid rpcid)) newAppCmd, sendDebuggerModel newServerModel]

storePausedRPCMessage : (serverModel -> (serverModel,Cmd serverMsg)) -> RunCycle -> ClientId -> Int -> Int -> remoteServerMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
storePausedRPCMessage updateAppModel parentRunCycle cid parentid rpcid msg serverModel = { serverModel | debugger = let debugger = serverModel.debugger in
  { debugger | messagesReceivedDuringPaused = Array.push (PausedRemoteServerAppMsg parentRunCycle cid updateAppModel parentid rpcid msg) debugger.messagesReceivedDuringPaused}} ! []

resumeServerFromPaused : ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
resumeServerFromPaused serverModel =
  let ((newServerModel,newServerCmd), clients) = updateServerWithPausedMessages (Array.toList serverModel.debugger.messagesReceivedDuringPaused) ((serverModel, Cmd.none),Dict.empty) in
    let debugger = newServerModel.debugger in
      { newServerModel | debugger = { debugger | state = Running, messagesReceivedDuringPaused = Array.empty }} ! [newServerCmd, broadcastResumeFromPausedAction clients newServerModel]

updateServerWithPausedMessages :
  List (PausedServerMessage serverModel serverMsg remoteServerMsg msg) ->
  ((ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)), Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))) ->
  ((ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)), Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
updateServerWithPausedMessages messages ((serverModel, cmd), clients) = let debugger = serverModel.debugger in
  case messages of
    [] -> ((serverModel, cmd), clients)
    message :: otherMessages ->
      let ((newServerModel,newServerCmd), newClients) = case message of
        PausedServerAppMsg parentMsg serverAppMsg -> (updateServerAppModel parentMsg serverAppMsg serverModel, clients)
        PausedRemoteServerAppMsg parentRunCycle cid update parentid rpcid remoteServerMsg -> (updateServerAppModelFromRPC update parentRunCycle cid parentid rpcid remoteServerMsg serverModel, clients)
        PausedClientAppMsg (cid,parentMsg,msg) -> case Dict.get (toString cid) clients of
          Just (clientModel, clientCmd) ->
            let (newClientModel,newClientCmd) = updateAppModel serverModel.updateClient msg parentMsg cid clientModel in
              ((serverModel,Cmd.none), (Dict.insert (toString cid) (newClientModel,newClientCmd) clients))
          _ -> let (_, _, _,_, _, previousClients) = TimeLine.previousState ((TimeLine.length debugger.timeline) - 1) debugger.timeline in
            case Dict.get (toString cid) previousClients of
              Just (cid, (previousAppModel, _, _, previousRpcMsgCount, previousMsgCount)) ->
                let clientModel = ClientDebuggerModel ClientPaused previousAppModel debugger.runCycle previousRpcMsgCount previousMsgCount (PreviousModel Maybe.Nothing) in
                  let (newClientModel,newClientCmd) = updateAppModel serverModel.updateClient msg parentMsg cid clientModel in
                    ((serverModel,Cmd.none),(Dict.insert (toString cid) (newClientModel,newClientCmd) clients))
              _ -> ((serverModel,Cmd.none), clients)
      in updateServerWithPausedMessages otherMessages ((newServerModel ! [cmd,newServerCmd]), newClients)

resumeServerFromPrevious :
  PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) ->
  ServerModel serverModel serverMsg remoteServerMsg model msg ->
  (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg))
resumeServerFromPrevious previous serverModel = let debugger = serverModel.debugger in
  let (newTimeline, eventsToCheck) = TimeLine.goBack (debugger.runCycle + 1) previous.index debugger.timeline
      messagesReceivedDuringPaused = debugger.messagesReceivedDuringPaused |> Array.toList
      currentClients = TimeLine.clients debugger.timeline in
    let ((newServerModel,newServerCmd),newClients) =
      let modelToUse = { serverModel | debugger = { debugger | timeline = newTimeline, appModel = previous.appModel, runCycle = debugger.runCycle + 1, msgCount = previous.msgCount, rpcMsgCount = previous.rpcMsgCount }} in
      let clientsToUse = previous.clients
            |> Dict.map (\_ (cid, (previousAppModel, previousAppCmd, previousParentMsg, previousRpcMsgCount, previousMsgCount)) ->
               let clientModel = ClientDebuggerModel ClientPaused previousAppModel (debugger.runCycle + 1) previousRpcMsgCount previousMsgCount (PreviousModel Maybe.Nothing) in
                 let (rpcWrappedModel, rpcWrappedCmd, _) = wrapRPCcmds cid previousParentMsg previousMsgCount clientModel previousAppCmd in
                   (rpcWrappedModel !! [rpcWrappedCmd]))
      in checkEvents previous eventsToCheck previous.index (modelToUse, clientsToUse)
        |> (\(model,clients) -> ((model,Cmd.none),clients))
        |> checkPaused previous (Array.toList serverModel.debugger.messagesReceivedDuringPaused)
     in let newDebugger = newServerModel.debugger in
      { newServerModel | debugger = { newDebugger | state = Running, previous = Maybe.Nothing, messagesReceivedDuringPaused = Array.empty }}
        ! [Cmd.map (ServerAppMsg previous.parentMsg) previous.cmd, newServerCmd, broadcastResumeFromPreviousAction previous currentClients newClients newServerModel]

checkEvents :
  PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) ->
  List (RunCycle,Event serverModel serverMsg remoteServerMsg msg) ->
  Int ->
  (ServerModel serverModel serverMsg remoteServerMsg model msg, Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))) ->
  (ServerModel serverModel serverMsg remoteServerMsg model msg, Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
checkEvents previous eventsToCheck goBackIndex (serverModel, clients) = case eventsToCheck of
  [] -> (serverModel,clients)
  (runCycle,event) :: otherEvents -> let debugger = serverModel.debugger in
    let (newServerModel,newClients) =
      case event of
        ServerEvent eventType ->
          let handleServerEvent parent serverMsg =
                if TimeLine.isServerParentMember parent debugger.timeline then
                  let (newServerModel,_) = updateServerAppModel parent serverMsg serverModel in (newServerModel,clients)
                else (serverModel,clients)  -- Message discarded...
              handleServerRPCevent parentRunCycle cid parentid rpcid updateAppModel msg =
                if TimeLine.isClientParentMember cid (RegularMsg parentRunCycle parentid) debugger.timeline then
                  let (newServerModel,_) = updateServerAppModelFromRPC updateAppModel parentRunCycle cid parentid rpcid msg serverModel in (newServerModel,clients)
                else (serverModel,clients)  -- Message discarded...

          in case eventType of
            ServerMsgEvent msgType serverMsg -> case msgType of
              NewServerMsg msgid -> handleServerEvent None serverMsg
              ServerChildMsg parentRunCycle parentid msgid -> handleServerEvent (RegularServerMsg  parentRunCycle parentid) serverMsg
              RPCserverMsg parentRunCycle cid rpcid rpcmsgid -> handleServerEvent (ServerRPC parentRunCycle cid rpcid) serverMsg
              RPCchildServerMsg parentRunCycle (cid,rpcid,rpcmsgid) msgid -> handleServerEvent (ServerRPCmsg parentRunCycle (cid,rpcid,rpcmsgid)) serverMsg
            ServerRPCevent parentRunCycle cid parentid rpcid updateAppModel remoteServerMsg -> handleServerRPCevent parentRunCycle cid parentid rpcid updateAppModel remoteServerMsg
            _ -> (serverModel,clients)
        ClientCloseEvent cid -> (serverModel,clients) -- TODO push close event
        ClientEvent cid eventType ->
          let handleClientEvent parent msg =
            if TimeLine.isClientParentMember cid parent debugger.timeline then
              case Dict.get (toString cid) clients of
                Just (clientModel, clientCmd) ->
                  let (newClientModel,newClientCmd) = updateAppModelWithoutEffects serverModel.updateClient msg parent cid clientModel in
                    (serverModel, Dict.insert (toString cid) (newClientModel !! [clientCmd,newClientCmd]) clients)
                _ -> (serverModel, clients) -- TODO handle new clients
            else (serverModel,clients)  -- Message discarded...
           in case eventType of
            Init ids -> (serverModel,clients) -- TODO
            MsgEvent msgType ids msg -> case msgType of
              NewClientMsg msgid -> handleClientEvent NoParentMsg msg
              ClientChildMsg parentRunCycle parentid _ -> handleClientEvent (RegularMsg parentRunCycle parentid) msg
              ClientRPCchildMsg parentRunCycle parentid rpcid _ -> handleClientEvent (ServerRPCparentMsg parentRunCycle parentid rpcid) msg

    in checkEvents previous otherEvents goBackIndex (newServerModel,newClients)

checkPaused :
  PreviousState serverModel (Cmd serverMsg) model (MultitierCmd remoteServerMsg msg) ->
  List (PausedServerMessage serverModel serverMsg remoteServerMsg msg) ->
  ((ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)),Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))) ->
  ((ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)),Dict String (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
checkPaused previous messages ((serverModel,cmd),clients) = let debugger = serverModel.debugger in
  case messages of
    [] -> ((serverModel,cmd),clients)
    message :: otherMessages -> case message of
      PausedServerAppMsg parentMsg serverAppMsg ->
        if TimeLine.isServerParentMember parentMsg debugger.timeline then
          let (newServerModel,newServerCmd) = updateServerAppModel parentMsg serverAppMsg serverModel in
            checkPaused previous otherMessages ((newServerModel ! [cmd,newServerCmd]), clients)
        else checkPaused previous otherMessages ((serverModel,cmd),clients) -- Message discarded...

      PausedRemoteServerAppMsg parentRunCycle cid updateAppModel parentid rpcid remoteServerMsg ->
        if TimeLine.isClientParentMember cid (RegularMsg parentRunCycle parentid) debugger.timeline then
          let (newServerModel,newServerCmd) = updateServerAppModelFromRPC updateAppModel parentRunCycle cid parentid rpcid remoteServerMsg serverModel in
            checkPaused previous otherMessages ((newServerModel ! [cmd,newServerCmd]), clients)
        else checkPaused previous otherMessages ((serverModel,cmd),clients) -- Message discarded...
      PausedClientAppMsg (cid,parentMsg,msg) ->
        if TimeLine.isClientParentMember cid parentMsg debugger.timeline then
           case Dict.get (toString cid) clients of
            Just (clientModel, clientCmd) ->
              let (newClientModel,newClientCmd) = updateAppModel serverModel.updateClient msg parentMsg cid clientModel in
                ((serverModel,cmd), Dict.insert (toString cid) (newClientModel !! [clientCmd,newClientCmd]) clients)
            _ -> case Dict.get (toString cid) previous.clients of
                  Just (cid, (previousAppModel, _, _, previousRpcMsgCount, previousMsgCount)) ->
                    let clientModel = ClientDebuggerModel ClientPaused previousAppModel debugger.runCycle previousRpcMsgCount previousMsgCount (PreviousModel Maybe.Nothing) in
                      let (newClientModel,newClientCmd) = updateAppModel serverModel.updateClient msg parentMsg cid clientModel in
                        ((serverModel,cmd),Dict.insert (toString cid) (newClientModel !! [newClientCmd]) clients)
                  _ -> ((serverModel,cmd), clients)
        else checkPaused previous otherMessages ((serverModel,cmd),clients) -- Message discarded...

-- SERVER-STATE

type alias ServerState serverState = { appServerState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> ServerState serverState)
wrapServerState serverState = \serverModel -> ServerState (serverState serverModel.debugger.appModel)

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = Sub.map (ServerAppMsg None) (serverSubscriptions serverModel.debugger.appModel)
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
  , msgCount : Int
  , previous : Previous model }

type Previous model = PreviousModel (Maybe model) | TempInvalid

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
wrapInit init = \serverState -> Uninitialized (init serverState.appServerState) !! []

type Msg model msg serverModel serverMsg remoteServerMsg =
  AppMsg ParentMsg msg |

  OnSocketMsg String |

  Pause | Resume | GoBack Int |
  SetResume Int | ToggleShowParentage Bool |
  ParentStillMember ClientId ParentMsg msg (Result Error Bool) |

  SwitchDebugger |

  Handle (Result Error ()) |
  HandleStartDebugView (Result Error (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg)) | HandleStopDebugView (Result Error ())

type alias InitData =
  { cid: ClientId
  , paused: Bool
  , runCycle: RunCycle }

type alias PausedClientMsg msg = (ClientId,ParentMsg,msg)

type SocketMsg serverModel serverMsg remoteServerMsg model msg =
  InitializeClient InitData |
  SetServerModel (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg) |

  PauseClient | InvalidateClient | GoBackClient (Previous model) |
  ResumeClientFromPaused (Maybe (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))) |
  ResumeClientFromPrevious (Maybe (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg remoteServerMsg -> Model model msg serverModel serverMsg remoteServerMsg -> ( Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) ))
wrapUpdate update = \msg model -> case model of

  Uninitialized (appModel, cmd) -> case msg of
    OnSocketMsg data -> case (fromJSONString data) of
      InitializeClient initData ->
        let msgCount = 0
            rpcCount = 0 in
          let newcmodel = case initData.paused of
            True -> ClientDebuggerModel ClientPaused appModel initData.runCycle rpcCount (msgCount + 1) (PreviousModel Maybe.Nothing)
            False -> ClientDebuggerModel ClientRunning appModel initData.runCycle rpcCount (msgCount + 1) (PreviousModel Maybe.Nothing) in
              let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds initData.cid (RegularMsg initData.runCycle msgCount) msgCount newcmodel cmd in
                ClientDebugger initData.cid rpcWrappedModel !! [rpcWrappedCmds, performOnServer (AddClientEvent initData.cid newcmodel.rpccounter (msgCount + 1) (Init rpcIds) appModel cmd (RegularMsg initData.runCycle msgCount)) ]
      _ -> model !! []
    _ -> model !! []

  ClientDebugger cid cmodel -> case msg of
    AppMsg parent appMsg -> let (newcmodel, cmd) = handleAppMsg update appMsg parent cid cmodel in ClientDebugger cid newcmodel !! [cmd]
    ParentStillMember _ parentMsg appMsg result -> let (newcmodel, cmd) = handleParentStillMember update cid parentMsg appMsg result cmodel in ClientDebugger cid newcmodel !! [cmd]

    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClientFromPaused resume -> let (newcmodel, cmd) = resumeClientFromPaused resume cmodel in ClientDebugger cid newcmodel !! [cmd]
      ResumeClientFromPrevious resume -> let (newcmodel, cmd) = resumeClientFromPrevious cid resume cmodel in ClientDebugger cid newcmodel !! [cmd]
      InvalidateClient -> ClientDebugger cid { cmodel | state = ClientUnvalid } !! []
      GoBackClient maybeModel -> let (newcmodel,cmd) = goBackClient maybeModel cmodel in ClientDebugger cid newcmodel !! [cmd]
      _ -> model !! []

    SwitchDebugger -> Switching cid cmodel !! [performOnServer (StartDebugView cid)]

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  Switching cid cmodel -> case msg of
    AppMsg parent appMsg -> let (newcmodel, cmd) = handleAppMsg update appMsg parent cid cmodel in Switching cid newcmodel !! [cmd]
    ParentStillMember _ parentMsg appMsg result -> let (newcmodel, cmd) = handleParentStillMember update cid parentMsg appMsg result cmodel in Switching cid newcmodel !! [cmd]
    OnSocketMsg data -> case (fromJSONString data) of
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in Switching cid newcmodel !! [cmd]
      ResumeClientFromPaused resume -> let (newcmodel, cmd) = resumeClientFromPaused resume cmodel in Switching cid newcmodel !! [cmd]
      ResumeClientFromPrevious resume -> let (newcmodel, cmd) = resumeClientFromPrevious cid resume cmodel in Switching cid newcmodel !! [cmd]
      InvalidateClient -> Switching cid { cmodel | state = ClientUnvalid } !! []
      GoBackClient maybeModel -> let (newcmodel,cmd) = goBackClient maybeModel cmodel in Switching cid newcmodel !! [cmd]
      _ -> model !! []

    HandleStartDebugView result -> case result of
      Result.Err err -> model !! [] -- TODO handle error in view
      Ok serverDebuggerModel ->  ServerDebugger cid serverDebuggerModel cmodel !! []

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    _ -> model !! []

  ServerDebugger cid smodel cmodel -> case msg of
    AppMsg parent appMsg -> let (newcmodel, cmd) = handleAppMsg update appMsg parent cid cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
    ParentStillMember _ parentMsg appMsg result -> let (newcmodel, cmd) = handleParentStillMember update cid parentMsg appMsg result cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
    OnSocketMsg data -> case (fromJSONString data) of
      SetServerModel serverModel -> ServerDebugger cid serverModel cmodel !! []
      PauseClient -> let (newcmodel, cmd) = pauseClient cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClientFromPaused resume -> let (newcmodel, cmd) = resumeClientFromPaused resume cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      ResumeClientFromPrevious resume -> let (newcmodel, cmd) = resumeClientFromPrevious cid resume cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
      InvalidateClient -> ServerDebugger cid smodel { cmodel | state = ClientUnvalid } !! []
      GoBackClient maybeModel -> let (newcmodel,cmd) = goBackClient maybeModel cmodel in ServerDebugger cid smodel newcmodel !! [cmd]
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

goBackClient : Previous model -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
goBackClient previous cmodel = case cmodel.state of
  ClientUnvalid -> cmodel !! []
  _ -> { cmodel | previous = previous } !! []

pauseClient : ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
pauseClient cmodel = case cmodel.state of
  ClientUnvalid -> cmodel !! []
  _ -> { cmodel | state = ClientPaused } !! []

resumeClientFromPaused : Maybe (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)) -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClientFromPaused maybeNewModel cmodel = case cmodel.state of
  ClientUnvalid -> cmodel !! []
  _ -> case maybeNewModel of
    Just (newModel,newCmd) -> { newModel | state = ClientRunning } !! [newCmd]
    _ -> { cmodel | state = ClientRunning } !! []

resumeClientFromPrevious : ClientId  -> Maybe (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)) -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg))
resumeClientFromPrevious cid maybeNewModel cmodel = case cmodel.state of
  ClientUnvalid -> cmodel !! []
  _ -> case maybeNewModel of
    Just (newModel,newCmd) -> { newModel | state = ClientRunning } !! [newCmd]
    _ -> { cmodel | state = ClientRunning, runCycle = cmodel.runCycle + 1 } !! []

handleParentStillMember : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> ClientId -> ParentMsg -> msg -> Result Error Bool -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
handleParentStillMember update cid parentMsg appMsg result cmodel = case cmodel.state of
  ClientUnvalid -> cmodel !! []
  _ -> case result of
    Result.Ok stillMember -> case stillMember of
      True -> case cmodel.state of
        ClientRunning -> updateAppModel update appMsg parentMsg cid cmodel
        ClientPaused -> cmodel !! [performOnServer (AddPausedClientEvent cmodel.runCycle (cid,parentMsg,appMsg))]
        _ -> cmodel !! []
      False -> cmodel !! [] -- discard message
    Result.Err err -> cmodel !! [] -- TODO error in view

handleAppMsg : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ParentMsg -> ClientId -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
handleAppMsg update appMsg parentMsg cid cmodel = case cmodel.state of
  ClientUnvalid -> cmodel !! []
  ClientRunning -> case parentMsg of
    NoParentMsg -> updateAppModel update appMsg parentMsg cid cmodel
    _ -> cmodel !! [performOnServer (IsParentStillMember cid parentMsg appMsg)]
  ClientPaused -> cmodel !! [performOnServer (IsParentStillMember cid parentMsg appMsg)]

updateAppModel : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ParentMsg -> ClientId -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModel update appMsg parentMsg cid cmodel =
  let (newAppModel, newCmd) = update appMsg cmodel.appModel in
    case parentMsg of
      NoParentMsg ->
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.runCycle cmodel.msgCount) cmodel.msgCount cmodel newCmd in
          { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (NewClientMsg cmodel.msgCount) rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.runCycle cmodel.msgCount))]
      RegularMsg parentRunCycle parentId ->
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.runCycle cmodel.msgCount) cmodel.msgCount cmodel newCmd in
          { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (ClientChildMsg parentRunCycle parentId cmodel.msgCount) rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.runCycle cmodel.msgCount))]
      ServerRPCparentMsg parentRunCycle parentid rpcid ->
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.runCycle cmodel.msgCount) cmodel.msgCount cmodel newCmd in
          { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (ClientRPCchildMsg parentRunCycle parentid rpcid cmodel.msgCount) rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.runCycle cmodel.msgCount))]

updateAppModelWithoutEffects : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> msg -> ParentMsg -> ClientId -> ClientDebuggerModel model -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) )
updateAppModelWithoutEffects update appMsg parentMsg cid cmodel =
  let (newAppModel, newCmd) = update appMsg cmodel.appModel in
    case parentMsg of
      NoParentMsg ->
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.runCycle cmodel.msgCount) cmodel.msgCount cmodel Multitier.none in
          { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (NewClientMsg cmodel.msgCount) rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.runCycle cmodel.msgCount))]
      RegularMsg parentRunCycle parentId ->
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.runCycle cmodel.msgCount) cmodel.msgCount cmodel Multitier.none in
          { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (ClientChildMsg parentRunCycle parentId cmodel.msgCount) rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.runCycle cmodel.msgCount))]
      ServerRPCparentMsg parentRunCycle parentid rpcid ->
        let (rpcWrappedModel, rpcWrappedCmds, rpcIds) = wrapRPCcmds cid (RegularMsg cmodel.runCycle cmodel.msgCount) cmodel.msgCount cmodel Multitier.none in
          { rpcWrappedModel | appModel = newAppModel, msgCount = cmodel.msgCount + 1 } !! [rpcWrappedCmds, performOnServer (AddClientEvent cid cmodel.rpccounter (cmodel.msgCount + 1) (MsgEvent (ClientRPCchildMsg parentRunCycle parentid rpcid cmodel.msgCount) rpcIds appMsg) newAppModel newCmd (RegularMsg cmodel.runCycle cmodel.msgCount))]


wrapRPCcmds : ClientId -> ParentMsg -> Int -> ClientDebuggerModel model -> MultitierCmd remoteServerMsg msg -> (ClientDebuggerModel model, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg), List Int)
wrapRPCcmds cid parentMsg parentId model cmd =
  case cmd of
    ServerCmd remoteServerMsg -> ({ model | rpccounter = model.rpccounter + 1 }, Multitier.map (RemoteServerAppMsg model.runCycle cid parentId model.rpccounter) (AppMsg parentMsg) cmd, [model.rpccounter])
    ClientCmd _ -> (model, Multitier.map (RemoteServerAppMsg model.runCycle cid parentId model.rpccounter) (AppMsg parentMsg) cmd, [])
    Batch cmds -> List.foldl (\currentCmd (prevModel, prevCmd, prevRPCids) -> let (newModel, newCmd, newRPCids) = (wrapRPCcmds cid parentMsg parentId prevModel currentCmd) in
                                                                      (newModel, Multitier.batch [newCmd,prevCmd], List.append prevRPCids newRPCids)) (model,Multitier.none,[]) cmds

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let appSubs = \cmodel -> case cmodel.state of
    ClientRunning -> Sub.map (AppMsg NoParentMsg) (subscriptions cmodel.appModel)
    ClientPaused -> Sub.map (AppMsg NoParentMsg) (subscriptions cmodel.appModel)
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
    let view divAtt appModel =
      Html.div [] [
        Html.div divAtt [
          Html.map (AppMsg NoParentMsg) (appView appModel)],
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"],
          Html.pre [] [
            Html.text (toString appModel)]
          ]]
      in case cmodel.state of
        ClientRunning ->
          view [] cmodel.appModel
        ClientPaused ->
          case cmodel.previous of
            PreviousModel maybePrevModel -> case maybePrevModel of
              Just prevModel -> view [disabled True, style [("opacity", "0.25")]] prevModel
              _ -> view [disabled True, style [("opacity", "0.25")]] cmodel.appModel
            TempInvalid -> Html.div [] [
              Html.text "Client was not started yet at this point in the timeline... ",
              Html.br [] [],
              Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"]]
        ClientUnvalid -> Html.div [] [
          Html.text "Client is not valid anymore... Refresh to start a new client session or...",
          Html.br [] [],
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server debugger"]]

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
    offset = 100
    labelOffset = 10
    eventSpacing = 25
    circles =
      TimeLine.view smodel.timeline
        |> List.map (\(index, (runCycle,event)) -> case event of

          ServerEvent serverEventType ->
            let parentLine = if smodel.showParentage then
              case serverEventType of
                ServerRPCevent _ _ _ _ _ _ -> []
                _ -> case TimeLine.getServerEventParentIndex serverEventType smodel.timeline of
                  Just parentIndex ->
                    let px1 = (parentIndex * eventSpacing) + offset
                        px2 = (index * eventSpacing) + offset
                        qx = px1 + ((px2 - px1) // 2) in
                      [Svg.path [d ("M "++(toString px1)++" 20 Q "++(toString qx)++ " 0 "++(toString px2)++" 20"), stroke "black", fill "transparent"] []]
                  _ -> []
              else []
            in List.append parentLine
              [Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] []]
          ClientCloseEvent cid -> []
          ClientEvent cid clientEvent ->
            let clientIndex = Maybe.withDefault 0 (Dict.get (toString cid) clientIndices) in
              let circle =
                let parentLine = if smodel.showParentage then
                  case clientEvent of
                    Init _ -> []
                    MsgEvent msgType _ _ -> case msgType of
                      ClientRPCchildMsg _ _ _ _ -> []
                      _ -> case TimeLine.getClientEventParentIndex cid clientEvent smodel.timeline of
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
                  MsgEvent msgType ids _ ->
                    case msgType of
                      ClientRPCchildMsg _ _ rpcid _ -> (Just rpcid,ids)
                      _ -> (Maybe.Nothing, ids) in
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


    serverLabel = Svg.text_ [x (toString labelOffset), y "20"] [Html.text "Server"]
    serverLine = Svg.line [x1 (toString offset), y1 "20", x2 "100%", y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []
    clientLabels = clients |> List.map (\(_,index) -> Svg.text_ [x (toString labelOffset), y (toString ((index * 40) + 60))] [Html.text ("Client " ++ (toString (index + 1)))])
    clientLines =
      TimeLine.clients smodel.timeline
        |> List.indexedMap (\index cid -> (index,cid))
        |> List.map (\(index,cid) -> let startIndex = case TimeLine.getFirstClientIndex cid smodel.timeline of
          Just firstIndex -> (firstIndex * eventSpacing) + offset
          _ -> offset
            in Svg.line [x1 (toString startIndex), y1 (toString ((index * 40) + 60)), x2 "100%", y2 (toString ((index * 40) + 60)), style [("stroke", "black"), ("stroke-width", "3")]] [])
  in
  Html.div [id "timeline", style [("overflow-x", "auto")]] [
    Svg.svg [ width (toString ((((TimeLine.length smodel.timeline) - 1) * eventSpacing) + (offset * 2))), height (toString (40 * ((TimeLine.numberOfClients smodel.timeline) + 1)))]
      (List.concat [
        [serverLabel],
        [serverLine],
        clientLabels,
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

eventView : RunCycle -> Event serverModel serverMsg remoteServerMsg msg -> String
eventView runCycle event = case event of
  ClientEvent cid cevent -> clientEventView runCycle cid cevent
  ClientCloseEvent cid -> "[Close-"++ (toString cid) ++"]"
  ServerEvent sevent -> serverEventView runCycle sevent

clientEventView : RunCycle -> ClientId -> ClientEventType msg -> String
clientEventView runCycle cid event = --"(" ++ (toString runCycle) ++ ")" ++
  case event of
    Init rpcids -> "[Init]"
    MsgEvent msgType rpcids msg -> "[Msg] " ++ (toString msg)

serverEventView : RunCycle -> ServerEventType serverModel serverMsg remoteServerMsg -> String
serverEventView runCycle event = --"(" ++ (toString runCycle) ++ ")" ++
  case event of
    InitServerEvent -> "[Init-Server]"
    ServerMsgEvent msgtype msg -> "[Server-Msg] " ++ (toString msg)
    ServerRPCevent parentRunCycle cid parentid rpcid _ msg -> "[RPC] " ++ (toString msg)

serverActions : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverActions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
    Html.text "Show parentage: ",
    Html.input [checked model.showParentage, type_ "checkbox", onCheck ToggleShowParentage] [],
    Html.text "Resume from: ",
    Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume)]
