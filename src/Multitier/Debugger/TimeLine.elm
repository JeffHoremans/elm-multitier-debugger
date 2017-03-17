module Multitier.Debugger.TimeLine
  exposing
    ( TimeLine
    , Event(..)
    , ServerEventType(..)
    , ServerMsgType(..)
    , ClientEventType(..)
    , ClientMsgType(..)
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
  , server : (serverModel, Cmd serverMsg)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, Int))
  , rpcindices : Dict (String,Int) Int }

type Event serverMsg remoteServerMsg msg =
  ServerEvent (ServerEventType serverMsg remoteServerMsg) |
  ClientEvent ClientId (ClientEventType msg)

type ClientEventType msg = Init (List Int) | MsgEvent ClientMsgType (Maybe Int) (List Int) msg

type ClientMsgType =
  NewClientMsg Int |
  ClientChildMsg Int Int


type ServerEventType serverMsg remoteServerMsg =
  InitServerEvent |
  ServerMsgEvent ServerMsgType serverMsg |
  ServerRPCevent ClientId Int remoteServerMsg

type ServerMsgType =
  NewServerMsg Int |
  ServerChildMsg Int Int |
  RPCserverMsg ClientId Int Int |
  RPCchildServerMsg (ClientId,Int,Int) Int

type alias EventRecoveryState serverModel serverMsg remoteServerMsg model msg =
  { server : (serverModel, Cmd serverMsg)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, Int))
  , rpcindices : Dict (String,Int) Int }

getRPCeventIndex : ClientId -> Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getRPCeventIndex cid rpcid (TimeLine {rpcindices}) = Dict.get ((toString cid),rpcid) rpcindices

empty : (serverModel, Cmd serverMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg
empty serverState = TimeLine (Model Array.empty serverState Dict.empty Dict.empty)

pushServerEvent : ((ServerEventType serverMsg remoteServerMsg), serverModel, Cmd serverMsg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushServerEvent (serverEvent, serverModel, serverCmd) (TimeLine {timeline, clients, rpcindices}) =
  let newServer = (serverModel, serverCmd)
      newRPCindices = case serverEvent of
        ServerRPCevent cid rpcid _ -> Dict.insert ((toString cid), rpcid) (Array.length timeline) rpcindices
        _ -> rpcindices in
  let newTimeline = Array.push (ServerEvent serverEvent, EventRecoveryState (serverModel, serverCmd) clients newRPCindices) timeline in
    TimeLine (Model newTimeline newServer clients newRPCindices)

pushClientEvent : ClientId -> Int -> ((ClientEventType msg), model, MultitierCmd remoteServerMsg msg) -> TimeLine serverModel serverMsg remoteServerMsg model msg -> TimeLine serverModel serverMsg remoteServerMsg model msg
pushClientEvent cid rpcid (clientEvent, model, cmd) (TimeLine {timeline, server, clients, rpcindices}) =
  let newClients = Dict.insert (toString cid) (cid, (model, cmd, rpcid)) clients
      newTimeline = Array.push (ClientEvent cid clientEvent, EventRecoveryState server newClients rpcindices) timeline in
    TimeLine (Model newTimeline server newClients rpcindices)

previousState : Int -> TimeLine serverModel serverMsg remoteServerMsg model msg -> (serverModel, Cmd serverMsg, Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, Int)))
previousState index (TimeLine {timeline, server}) =
  case Array.get index timeline of
    Just (_, recoveryState) -> let (serverModel, serverCmd) = recoveryState.server in (serverModel, serverCmd, recoveryState.clients)
    _ -> let (serverModel, serverCmd) = server in (serverModel, serverCmd, Dict.empty)

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
