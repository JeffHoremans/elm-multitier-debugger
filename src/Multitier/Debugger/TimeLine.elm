module Multitier.Debugger.TimeLine
  exposing
    ( TimeLine
    , Event(..)
    , RunCycle
    , ServerEventType(..)
    , ServerMsgType(..)
    , ParentServerMsg(..)
    , ClientEventType(..)
    , ClientMsgType(..)
    , ParentMsg(..)
    , empty
    , pushServerEvent
    , pushClientEvent
    , previousState
    , goBack
    , length
    , clients
    , numberOfClients
    , getRPCeventIndex
    , getServerEventParentIndex
    , isServerParentMember
    , getClientEventParentIndex
    , isClientParentMember
    , view )

import Array exposing (Array)
import Dict exposing (Dict)
import Multitier.Server.WebSocket exposing (ClientId)
import Multitier exposing (MultitierCmd(..))


type alias RunCycle = Int

type TimeLine serverModel serverMsg remoteServerMsg model msg = TimeLine (Model serverModel serverMsg remoteServerMsg model msg)

type alias Model serverModel serverMsg remoteServerMsg model msg =
  { events : Array (RunCycle, Event serverModel serverMsg remoteServerMsg msg, EventRecoveryState serverModel serverMsg remoteServerMsg model msg)
  , server : (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int))
  , rpcindices : Dict (String,Int) Int
  , clientParentIndices : Dict String (Dict Int (RunCycle, Int))
  , serverParentIndices :
    { regular : Dict Int (RunCycle, Int)
    , rpc : Dict (String,Int) (RunCycle, Int)
    , rpcchild: Dict (String,Int,Int) (RunCycle, Int) }}

type alias EventRecoveryState serverModel serverMsg remoteServerMsg model msg =
  { server : (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int))
  , rpcindices : Dict (String,Int) Int
  , clientParentIndices : Dict String (Dict Int (RunCycle, Int))
  , serverParentIndices :
    { regular : Dict Int (RunCycle, Int)
    , rpc : Dict (String,Int) (RunCycle, Int)
    , rpcchild: Dict (String,Int,Int) (RunCycle, Int) }}

type Event serverModel serverMsg remoteServerMsg msg =
  ServerEvent (ServerEventType serverModel serverMsg remoteServerMsg) |
  ClientEvent ClientId (ClientEventType msg)

type ClientEventType msg = Init (List Int) | MsgEvent ClientMsgType (List Int) msg

type ClientMsgType =
  NewClientMsg Int |
  ClientChildMsg RunCycle Int Int |
  ClientRPCchildMsg RunCycle Int Int

type ParentMsg =
  NoParentMsg |
  RegularMsg RunCycle Int |
  ServerRPCparentMsg RunCycle Int

type ServerEventType serverModel serverMsg remoteServerMsg =
  InitServerEvent |
  ServerMsgEvent ServerMsgType serverMsg |
  ServerRPCevent RunCycle ClientId Int Int (serverModel -> (serverModel,Cmd serverMsg)) remoteServerMsg

type ServerMsgType =
  NewServerMsg Int |
  ServerChildMsg RunCycle Int Int |
  RPCserverMsg RunCycle ClientId Int Int |
  RPCchildServerMsg RunCycle (ClientId,Int,Int) Int

type ParentServerMsg =
  None |
  RegularServerMsg RunCycle Int |
  ServerRPC RunCycle ClientId Int |
  ServerRPCmsg RunCycle (ClientId,Int,Int)

empty : serverModel -> TimeLine serverModel serverMsg remoteServerMsg model msg
empty serverModel = TimeLine (Model Array.empty (serverModel,Cmd.none,None,0,0) Dict.empty Dict.empty Dict.empty {regular=Dict.empty, rpc=Dict.empty,rpcchild=Dict.empty})

pushServerEvent : RunCycle -> Int -> Int -> ((ServerEventType serverModel serverMsg remoteServerMsg), serverModel, Cmd serverMsg, ParentServerMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushServerEvent runCycle rpcMsgCount msgCount (serverEvent, serverModel, serverCmd, parentMsg) (TimeLine {events, clients, rpcindices, clientParentIndices, serverParentIndices}) =
  let currentIndex = Array.length events in
  let value = (runCycle,currentIndex) in
  let newServer = (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount)
      newRPCindices = case serverEvent of
        ServerRPCevent _ cid parentid rpcid _ _ -> Dict.insert ((toString cid), rpcid) currentIndex rpcindices
        _ -> rpcindices
      newParentIndices = case serverEvent of
         InitServerEvent -> { serverParentIndices | regular = Dict.insert 0 value serverParentIndices.regular}
         ServerRPCevent _ cid parentid rpcid _ _ -> { serverParentIndices | rpc = Dict.insert ((toString cid),rpcid) value serverParentIndices.rpc}
         ServerMsgEvent serverMsgType _ -> case serverMsgType of
           NewServerMsg msgid -> { serverParentIndices | regular = Dict.insert msgid value serverParentIndices.regular}
           ServerChildMsg _ _ msgid -> { serverParentIndices | regular = Dict.insert msgid value serverParentIndices.regular}
           RPCserverMsg _ cid rpcid rpcmsgid -> { serverParentIndices | rpcchild = Dict.insert ((toString cid),rpcid,rpcmsgid) value serverParentIndices.rpcchild}
           RPCchildServerMsg _ _ msgid -> { serverParentIndices | regular = Dict.insert msgid value serverParentIndices.regular} in
  let newEvents = Array.push (runCycle, ServerEvent serverEvent, EventRecoveryState newServer clients newRPCindices clientParentIndices newParentIndices) events in
    TimeLine (Model newEvents newServer clients newRPCindices clientParentIndices newParentIndices)

pushClientEvent : RunCycle -> ClientId -> Int -> Int -> ((ClientEventType msg), model, MultitierCmd remoteServerMsg msg, ParentMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushClientEvent runCycle cid rpcCount msgCount (clientEvent, model, cmd, parentMsg) (TimeLine {events, server, clients, rpcindices,clientParentIndices,serverParentIndices}) =
  let currentIndex = Array.length events in
  let value = (runCycle,currentIndex) in
  let parentIndices = case Dict.get (toString cid) clientParentIndices of
        Just result -> result
        _ -> Dict.empty in
  let newClients = Dict.insert (toString cid) (cid, (model, cmd, parentMsg, rpcCount, msgCount)) clients
      newClientParentIndices = case clientEvent of
         Init _ -> Dict.insert (toString cid) (Dict.insert 0 value parentIndices) clientParentIndices
         MsgEvent msgType _ _ -> case msgType of
           NewClientMsg msgid -> Dict.insert (toString cid) (Dict.insert msgid value parentIndices) clientParentIndices
           ClientChildMsg _ _ msgid -> Dict.insert (toString cid) (Dict.insert msgid value parentIndices) clientParentIndices
           ClientRPCchildMsg _ _ msgid -> Dict.insert (toString cid) (Dict.insert msgid value parentIndices) clientParentIndices in
  let newEvents = Array.push (runCycle, ClientEvent cid clientEvent, EventRecoveryState server newClients rpcindices newClientParentIndices serverParentIndices) events in
    TimeLine (Model newEvents server newClients rpcindices newClientParentIndices serverParentIndices)

previousState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int, Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int)))
previousState index (TimeLine {events, server}) =
  case Array.get index events of
    Just (_,_, recoveryState) -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = recoveryState.server in (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount, recoveryState.clients)
    _ -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = server in (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount, Dict.empty)

previousRecoveryState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> EventRecoveryState serverModel serverMsg remoteServerMsg model msg
previousRecoveryState index (TimeLine {events, server, clients, rpcindices, clientParentIndices, serverParentIndices}) =
  case Array.get index events of
    Just (_,_, recoveryState) -> recoveryState
    _ -> EventRecoveryState server clients rpcindices clientParentIndices serverParentIndices

goBack : RunCycle -> Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> (TimeLine serverModel serverMsg remoteServerMsg model msg, List (RunCycle, Event serverModel serverMsg remoteServerMsg msg))
goBack newRunCycle index (TimeLine model) =
  let newEvents = model.events |> Array.slice 0 (index+1)
      eventsToCheck = model.events |> Array.slice (index+1) (Array.length model.events) |> Array.map (\(runCycle,event,_) -> (runCycle,event)) |> Array.toList
      recovery = previousRecoveryState index (TimeLine model) in
    let newTimeLine = TimeLine (Model newEvents recovery.server recovery.clients recovery.rpcindices recovery.clientParentIndices recovery.serverParentIndices)
      |> updateRunCycle newRunCycle index
    in (newTimeLine, eventsToCheck)

updateRunCycle : RunCycle -> Int ->  TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
updateRunCycle newRunCycle index (TimeLine {events, server, clients, rpcindices, clientParentIndices, serverParentIndices}) =
  case Array.get index events of
    Just (_,event,_) ->
      let value = (newRunCycle,index) in
        let (newServer,newClients,newServerParentIndices,newClientParentIndices) =
          case event of
            ServerEvent eventType -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = server in
              let updatedServerParentIndices = case eventType of
                InitServerEvent -> { serverParentIndices | regular = Dict.insert 0 value serverParentIndices.regular}
                ServerRPCevent _ cid parentid rpcid _ _ -> { serverParentIndices | rpc = Dict.insert ((toString cid),rpcid) value serverParentIndices.rpc}
                ServerMsgEvent serverMsgType _ -> case serverMsgType of
                  NewServerMsg msgid -> { serverParentIndices | regular = Dict.insert msgid value serverParentIndices.regular}
                  ServerChildMsg _ _ msgid -> { serverParentIndices | regular = Dict.insert msgid value serverParentIndices.regular}
                  RPCserverMsg _ cid rpcid rpcmsgid -> { serverParentIndices | rpcchild = Dict.insert ((toString cid),rpcid,rpcmsgid) value serverParentIndices.rpcchild}
                  RPCchildServerMsg _ _ msgid -> { serverParentIndices | regular = Dict.insert msgid value serverParentIndices.regular}
              in let updatedParentMsg = case parentMsg of
                  None -> parentMsg
                  RegularServerMsg _ msgid -> RegularServerMsg newRunCycle msgid
                  ServerRPC _ cid rpcid -> ServerRPC newRunCycle cid rpcid
                  ServerRPCmsg _ parentid -> ServerRPCmsg newRunCycle parentid
              in ((serverModel,serverCmd,updatedParentMsg,rpcMsgCount,msgCount),clients,updatedServerParentIndices, clientParentIndices)
            ClientEvent cid eventType -> let parentIndices = Maybe.withDefault Dict.empty (Dict.get (toString cid) clientParentIndices) in
              let updatedClientParentIndices =
                case eventType of
                  Init _ -> Dict.insert (toString cid) (Dict.insert 0 value parentIndices) clientParentIndices
                  MsgEvent msgType _ _ -> case msgType of
                    NewClientMsg msgid -> Dict.insert (toString cid) (Dict.insert msgid value parentIndices) clientParentIndices
                    ClientChildMsg _ _ msgid -> Dict.insert (toString cid) (Dict.insert msgid value parentIndices) clientParentIndices
                    ClientRPCchildMsg _ _ msgid -> Dict.insert (toString cid) (Dict.insert msgid value parentIndices) clientParentIndices

              in let updatedClients = clients
                |> Dict.update (toString cid) (\maybeClient -> case maybeClient of
                  Just (cid, (model, cmd, parentMsg, rpcCount, msgCount)) ->
                    let updateParentMsg =
                      case parentMsg of
                        NoParentMsg -> parentMsg
                        RegularMsg _ msgid -> RegularMsg newRunCycle msgid
                        ServerRPCparentMsg _ msgid -> ServerRPCparentMsg newRunCycle msgid
                    in (Just (cid, (model, cmd, updateParentMsg, rpcCount, msgCount)))
                  _ -> Nothing )

              in (server,clients,serverParentIndices,updatedClientParentIndices) in
          let newEvents = Array.set index (newRunCycle,event,EventRecoveryState server clients rpcindices newClientParentIndices newServerParentIndices) events in
            TimeLine (Model newEvents server clients rpcindices newClientParentIndices newServerParentIndices)
    _ -> TimeLine (Model events server clients rpcindices clientParentIndices serverParentIndices)

getRPCeventIndex : ClientId -> Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getRPCeventIndex cid rpcid (TimeLine {rpcindices}) = Dict.get ((toString cid),rpcid) rpcindices

getServerEventParentIndex : ServerEventType serverModel serverMsg remoteServerMsg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getServerEventParentIndex eventType timeline = Maybe.map (\(_,index) -> index) (getServerEventParentIndexHelp eventType timeline)

getServerEventParentIndexHelp : ServerEventType serverModel serverMsg remoteServerMsg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe (RunCycle,Int)
getServerEventParentIndexHelp eventType (TimeLine {serverParentIndices, clientParentIndices}) = case eventType of
  ServerMsgEvent serverMsgType _ -> case serverMsgType of
    ServerChildMsg parentRunCycle parentid _ -> Dict.get parentid serverParentIndices.regular
    RPCserverMsg parentRunCycle cid rpcid rpcmsgid -> Dict.get ((toString cid),rpcid) serverParentIndices.rpc
    RPCchildServerMsg parentRunCycle (cid,rpcid,rpcmsgid) msgid -> Dict.get ((toString cid),rpcid,rpcmsgid) serverParentIndices.rpcchild
    _ -> Nothing
  ServerRPCevent parentRunCycle cid parentid rpcid _ _ -> case Dict.get (toString cid) clientParentIndices of
    Just parentIndices -> Dict.get parentid parentIndices
    _ -> Nothing
  _ -> Nothing

getClientEventParentIndex : ClientId -> ClientEventType msg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getClientEventParentIndex cid eventType timeline = Maybe.map (\(_,index) -> index) (getClientEventParentIndexHelp cid eventType timeline)

getClientEventParentIndexHelp : ClientId -> ClientEventType msg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe (RunCycle, Int)
getClientEventParentIndexHelp cid eventType (TimeLine {clientParentIndices,serverParentIndices}) = case eventType of
  MsgEvent msgType _ _ -> case msgType of
    ClientChildMsg parentRunCycle parentid _ -> case Dict.get (toString cid) clientParentIndices of
      Just parentIndices -> Dict.get parentid parentIndices
      _ -> Nothing
    ClientRPCchildMsg parentRunCycle rpcid msgid -> Dict.get ((toString cid),rpcid) serverParentIndices.rpc
    _ -> Nothing
  _ -> Nothing

isServerParentMember : ParentServerMsg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Bool
isServerParentMember parent (TimeLine {serverParentIndices}) =
  let checkRunCycle parentRunCycle maybeIndex =
    case maybeIndex of
      Just (runCycle,_) -> parentRunCycle == runCycle
      _ -> False in
  case parent of
    None -> False
    RegularServerMsg parentRunCycle msgid -> checkRunCycle parentRunCycle (Dict.get msgid serverParentIndices.regular)
    ServerRPC parentRunCycle cid rpcid -> checkRunCycle parentRunCycle (Dict.get ((toString cid),rpcid) serverParentIndices.rpc)
    ServerRPCmsg parentRunCycle (cid,rpcid,rpcmsgid) -> checkRunCycle parentRunCycle (Dict.get ((toString cid),rpcid,rpcmsgid) serverParentIndices.rpcchild)


isClientParentMember : ClientId -> ParentMsg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Bool
isClientParentMember cid parent (TimeLine {clientParentIndices, serverParentIndices}) =
  let checkRunCycle parentRunCycle maybeIndex =
    case maybeIndex of
      Just (runCycle,_) -> parentRunCycle == runCycle
      _ -> False in
  case parent of
    NoParentMsg -> False
    RegularMsg parentRunCycle msgid -> case Dict.get (toString cid) clientParentIndices of
      Just parentIndices -> checkRunCycle parentRunCycle (Dict.get msgid parentIndices)
      _ -> False
    ServerRPCparentMsg parentRunCycle rpcid -> checkRunCycle parentRunCycle (Dict.get ((toString cid), rpcid) serverParentIndices.rpc)

length : TimeLine serverModel serverMsg remoteServerMsg model msg -> Int
length (TimeLine model) = Array.length model.events

numberOfClients : TimeLine serverModel serverMsg remoteServerMsg model msg -> Int
numberOfClients (TimeLine {clients}) = Dict.size clients

clients : TimeLine serverModel serverMsg remoteServerMsg model msg -> List ClientId
clients (TimeLine {clients}) = clients |> Dict.values |> List.map (\(cid,_) -> cid)

view : TimeLine serverModel serverMsg remoteServerMsg model msg -> List (Int, (RunCycle,Event serverModel serverMsg remoteServerMsg msg))
view (TimeLine model) =
  model.events
    |> Array.map (\(runCycle,event,_) -> (runCycle,event))
    |> Array.indexedMap (,)
    |> Array.toList
