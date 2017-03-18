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
    , view )

import Array exposing (Array)
import Dict exposing (Dict)
import Multitier.Server.WebSocket exposing (ClientId)
import Multitier exposing (MultitierCmd(..))


type alias RunCycle = Int

type TimeLine serverModel serverMsg remoteServerMsg model msg = TimeLine (Model serverModel serverMsg remoteServerMsg model msg)

type alias Model serverModel serverMsg remoteServerMsg model msg =
  { timeline : Array (Event serverMsg remoteServerMsg msg, EventRecoveryState serverModel serverMsg remoteServerMsg model msg)
  , server : (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int))
  , rpcindices : Dict (String,Int) Int
  , parentIndices :
    { regular : Dict Int Int
    , rpc : Dict (String,Int) Int
    , rpcchild: Dict (String,Int,Int) Int }}

type alias EventRecoveryState serverModel serverMsg remoteServerMsg model msg =
  { server : (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int))
  , rpcindices : Dict (String,Int) Int
  , parentIndices :
    { regular : Dict Int Int
    , rpc : Dict (String,Int) Int
    , rpcchild: Dict (String,Int,Int) Int }}

type Event serverMsg remoteServerMsg msg =
  ServerEvent (ServerEventType serverMsg remoteServerMsg) |
  ClientEvent ClientId (ClientEventType msg)

type ClientEventType msg = Init (List Int) | MsgEvent ClientMsgType (Maybe Int) (List Int) msg

type ClientMsgType =
  NewClientMsg Int |
  ClientChildMsg Int Int

type ParentMsg =
  NoParentMsg |
  RegularMsg Int

type ServerEventType serverMsg remoteServerMsg =
  InitServerEvent |
  ServerMsgEvent ServerMsgType serverMsg |
  ServerRPCevent ClientId Int remoteServerMsg

type ServerMsgType =
  NewServerMsg Int |
  ServerChildMsg Int Int |
  RPCserverMsg ClientId Int Int |
  RPCchildServerMsg (ClientId,Int,Int) Int

type ParentServerMsg =
  None |
  RegularServerMsg Int |
  ServerRPC ClientId Int |
  ServerRPCmsg (ClientId,Int,Int)

getRPCeventIndex : ClientId -> Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getRPCeventIndex cid rpcid (TimeLine {rpcindices}) = Dict.get ((toString cid),rpcid) rpcindices

getServerEventParentIndex : ServerEventType serverMsg remoteServerMsg -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getServerEventParentIndex eventType (TimeLine {parentIndices}) = case eventType of
  ServerMsgEvent serverMsgType serverMsg -> case serverMsgType of
    ServerChildMsg parentid _ -> Dict.get parentid parentIndices.regular
    RPCserverMsg cid rpcid rpcmsgid -> Dict.get ((toString cid),rpcid) parentIndices.rpc
    RPCchildServerMsg (cid,rpcid,rpcmsgid) msgid -> Dict.get ((toString cid),rpcid,rpcmsgid) parentIndices.rpcchild
    _ -> Nothing
  _ -> Nothing

empty : serverModel -> TimeLine serverModel serverMsg remoteServerMsg model msg
empty serverModel = TimeLine (Model Array.empty (serverModel,Cmd.none,None,0,0) Dict.empty Dict.empty {regular=Dict.empty, rpc=Dict.empty,rpcchild=Dict.empty})

pushServerEvent : Int -> Int -> ((ServerEventType serverMsg remoteServerMsg), serverModel, Cmd serverMsg, ParentServerMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushServerEvent rpcMsgCount msgCount (serverEvent, serverModel, serverCmd, parentMsg) (TimeLine {timeline, clients, rpcindices, parentIndices}) =
  let currentIndex = (Array.length timeline) in
  let newServer = (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount)
      newRPCindices = case serverEvent of
        ServerRPCevent cid rpcid _ -> Dict.insert ((toString cid), rpcid) currentIndex rpcindices
        _ -> rpcindices
      newParentIndices = case serverEvent of
         InitServerEvent -> { parentIndices | regular = Dict.insert 0 currentIndex parentIndices.regular}
         ServerRPCevent cid rpcid _ -> { parentIndices | rpc = Dict.insert ((toString cid),rpcid) currentIndex parentIndices.rpc}
         ServerMsgEvent serverMsgType _ -> case serverMsgType of
           NewServerMsg msgid -> { parentIndices | regular = Dict.insert msgid currentIndex parentIndices.regular}
           ServerChildMsg _ msgid -> { parentIndices | regular = Dict.insert msgid currentIndex parentIndices.regular}
           RPCserverMsg cid rpcid rpcmsgid -> { parentIndices | rpcchild = Dict.insert ((toString cid),rpcid,rpcmsgid) currentIndex parentIndices.rpcchild}
           RPCchildServerMsg _ msgid -> { parentIndices | regular = Dict.insert msgid currentIndex parentIndices.regular} in
  let newTimeline = Array.push (ServerEvent serverEvent, EventRecoveryState newServer clients newRPCindices newParentIndices) timeline in
    TimeLine (Model newTimeline newServer clients newRPCindices newParentIndices)

pushClientEvent : ClientId -> Int -> Int -> ((ClientEventType msg), model, MultitierCmd remoteServerMsg msg, ParentMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushClientEvent cid rpcCount msgCount (clientEvent, model, cmd, parentMsg) (TimeLine {timeline, server, clients, rpcindices,parentIndices}) =
  let newClients = Dict.insert (toString cid) (cid, (model, cmd, parentMsg, rpcCount, msgCount)) clients
      newTimeline = Array.push (ClientEvent cid clientEvent, EventRecoveryState server newClients rpcindices parentIndices) timeline in
    TimeLine (Model newTimeline server newClients rpcindices parentIndices)

previousState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int, Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int)))
previousState index (TimeLine {timeline, server}) =
  case Array.get index timeline of
    Just (_, recoveryState) -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = recoveryState.server in (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount, recoveryState.clients)
    _ -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = server in (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount, Dict.empty)

previousRecoveryState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> EventRecoveryState serverModel serverMsg remoteServerMsg model msg
previousRecoveryState index (TimeLine {timeline, server, clients, rpcindices, parentIndices}) =
  case Array.get index timeline of
    Just (_, recoveryState) -> recoveryState
    _ -> EventRecoveryState server clients rpcindices parentIndices

goBack : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
goBack index (TimeLine model) =
  let newTimeline = model.timeline |> Array.slice 0 (index+1)
      recovery = previousRecoveryState index (TimeLine model) in
    TimeLine (Model newTimeline recovery.server recovery.clients recovery.rpcindices recovery.parentIndices)

length : TimeLine serverModel serverMsg remoteServerMsg model msg -> Int
length (TimeLine model) = Array.length model.timeline

numberOfClients : TimeLine serverModel serverMsg remoteServerMsg model msg -> Int
numberOfClients (TimeLine {clients}) = Dict.size clients

clients : TimeLine serverModel serverMsg remoteServerMsg model msg -> List ClientId
clients (TimeLine {clients}) = clients |> Dict.values |> List.map (\(cid,_) -> cid)

view : TimeLine serverModel serverMsg remoteServerMsg model msg -> List (Int, Event serverMsg remoteServerMsg msg)
view (TimeLine model) =
  model.timeline
    |> Array.map (\(event,_) -> event)
    |> Array.indexedMap (,)
    |> Array.toList
