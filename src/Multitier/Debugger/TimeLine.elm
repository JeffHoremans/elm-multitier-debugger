module Multitier.Debugger.TimeLine
  exposing
    ( TimeLine
    , Event(..)
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
    , view )

import Array exposing (Array)
import Dict exposing (Dict)
import Multitier.Server.WebSocket exposing (ClientId)
import Multitier exposing (MultitierCmd(..))

type TimeLine serverModel serverMsg remoteServerMsg model msg = TimeLine (Model serverModel serverMsg remoteServerMsg model msg)

type alias Model serverModel serverMsg remoteServerMsg model msg =
  { timeline : Array (Event serverMsg remoteServerMsg msg, EventRecoveryState serverModel serverMsg remoteServerMsg model msg)
  , server : (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int))
  , rpcindices : Dict (String,Int) Int }

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

type alias EventRecoveryState serverModel serverMsg remoteServerMsg model msg =
  { server : (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int))
  , rpcindices : Dict (String,Int) Int }

getRPCeventIndex : ClientId -> Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getRPCeventIndex cid rpcid (TimeLine {rpcindices}) = Dict.get ((toString cid),rpcid) rpcindices

empty : serverModel -> TimeLine serverModel serverMsg remoteServerMsg model msg
empty serverModel = TimeLine (Model Array.empty (serverModel,Cmd.none,None,0,0) Dict.empty Dict.empty)

pushServerEvent : Int -> Int -> ((ServerEventType serverMsg remoteServerMsg), serverModel, Cmd serverMsg, ParentServerMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushServerEvent rpcMsgCount msgCount (serverEvent, serverModel, serverCmd, parentMsg) (TimeLine {timeline, clients, rpcindices}) =
  let newServer = (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount)
      newRPCindices = case serverEvent of
        ServerRPCevent cid rpcid _ -> Dict.insert ((toString cid), rpcid) (Array.length timeline) rpcindices
        _ -> rpcindices in
  let newTimeline = Array.push (ServerEvent serverEvent, EventRecoveryState newServer clients newRPCindices) timeline in
    TimeLine (Model newTimeline newServer clients newRPCindices)

pushClientEvent : ClientId -> Int -> Int -> ((ClientEventType msg), model, MultitierCmd remoteServerMsg msg, ParentMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushClientEvent cid rpcCount msgCount (clientEvent, model, cmd, parentMsg) (TimeLine {timeline, server, clients, rpcindices}) =
  let newClients = Dict.insert (toString cid) (cid, (model, cmd, parentMsg, rpcCount, msgCount)) clients
      newTimeline = Array.push (ClientEvent cid clientEvent, EventRecoveryState server newClients rpcindices) timeline in
    TimeLine (Model newTimeline server newClients rpcindices)

previousState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> (serverModel, Cmd serverMsg, ParentServerMsg, Int, Int, Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, ParentMsg, Int, Int)))
previousState index (TimeLine {timeline, server}) =
  case Array.get index timeline of
    Just (_, recoveryState) -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = recoveryState.server in (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount, recoveryState.clients)
    _ -> let (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount) = server in (serverModel, serverCmd, parentMsg, rpcMsgCount, msgCount, Dict.empty)

previousRecoveryState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> EventRecoveryState serverModel serverMsg remoteServerMsg model msg
previousRecoveryState index (TimeLine {timeline, server, clients, rpcindices}) =
  case Array.get index timeline of
    Just (_, recoveryState) -> recoveryState
    _ -> EventRecoveryState server clients rpcindices

goBack : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
goBack index (TimeLine model) =
  let newTimeline = model.timeline |> Array.slice 0 (index+1)
      recovery = previousRecoveryState index (TimeLine model) in
    TimeLine (Model newTimeline recovery.server recovery.clients recovery.rpcindices)

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
