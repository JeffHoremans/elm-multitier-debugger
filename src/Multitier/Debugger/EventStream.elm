module Multitier.Debugger.EventStream
  exposing
    ( EventStream
    , Event(..)
    , ServerEventType(..)
    , ClientEventType(..)
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

type EventStream serverModel serverMsg remoteServerMsg model msg = EventStream (Model serverModel serverMsg remoteServerMsg model msg)

type alias Model serverModel serverMsg remoteServerMsg model msg =
  { stream : Array (Event serverMsg remoteServerMsg msg, EventRecoveryState serverModel serverMsg remoteServerMsg model msg)
  , server : (serverModel, Cmd serverMsg)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, Int))
  , rpcindices : Dict (String,Int) Int }

type Event serverMsg remoteServerMsg msg =
  ServerEvent (ServerEventType serverMsg remoteServerMsg) |
  ClientEvent ClientId (ClientEventType msg)

type ClientEventType msg = Init (List Int) | MsgEvent (Maybe Int) (List Int) msg
type ServerEventType serverMsg remoteServerMsg = InitServer | ServerMsgEvent serverMsg | RPCevent ClientId Int remoteServerMsg

type alias EventRecoveryState serverModel serverMsg remoteServerMsg model msg =
  { server : (serverModel, Cmd serverMsg)
  , clients : Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, Int))
  , rpcindices : Dict (String,Int) Int}

getRPCeventIndex : ClientId -> Int -> EventStream serverModel serverMsg remoteServerMsg model msg -> Maybe Int
getRPCeventIndex cid rpcid (EventStream {rpcindices}) = Dict.get ((toString cid),rpcid) rpcindices

empty : (serverModel, Cmd serverMsg) -> EventStream serverModel serverMsg remoteServerMsg model msg
empty serverState = EventStream (Model Array.empty serverState Dict.empty Dict.empty)

pushServerEvent : ((ServerEventType serverMsg remoteServerMsg), serverModel, Cmd serverMsg) -> EventStream serverModel serverMsg remoteServerMsg model msg -> EventStream serverModel serverMsg remoteServerMsg model msg
pushServerEvent (serverEvent, serverModel, serverCmd) (EventStream {stream, clients, rpcindices}) =
  let newServer = (serverModel, serverCmd)
      newRPCindices = case serverEvent of
        RPCevent cid rpcid _ -> Dict.insert ((toString cid),rpcid) (Array.length stream) rpcindices
        _ -> rpcindices in
  let newStream = Array.push (ServerEvent serverEvent, EventRecoveryState (serverModel, serverCmd) clients newRPCindices) stream in
    EventStream (Model newStream newServer clients newRPCindices)

pushClientEvent : ClientId -> Int -> ((ClientEventType msg), model, MultitierCmd remoteServerMsg msg) -> EventStream serverModel serverMsg remoteServerMsg model msg -> EventStream serverModel serverMsg remoteServerMsg model msg
pushClientEvent cid rpcid (clientEvent, model, cmd) (EventStream {stream, server, clients, rpcindices}) =
  let newClients = Dict.insert (toString cid) (cid, (model, cmd, rpcid)) clients
      newStream = Array.push (ClientEvent cid clientEvent, EventRecoveryState server newClients rpcindices) stream in
    EventStream (Model newStream server newClients rpcindices)

previousState : Int -> EventStream serverModel serverMsg remoteServerMsg model msg -> (serverModel, Cmd serverMsg, Dict String (ClientId, (model, MultitierCmd remoteServerMsg msg, Int)))
previousState index (EventStream {stream, server}) =
  case Array.get index stream of
    Just (_, recoveryState) -> let (serverModel, serverCmd) = recoveryState.server in (serverModel, serverCmd, recoveryState.clients)
    _ -> let (serverModel, serverCmd) = server in (serverModel, serverCmd, Dict.empty)

previousRecoveryState : Int -> EventStream serverModel serverMsg remoteServerMsg model msg -> EventRecoveryState serverModel serverMsg remoteServerMsg model msg
previousRecoveryState index (EventStream {stream, server, clients, rpcindices}) =
  case Array.get index stream of
    Just (_, recoveryState) -> recoveryState
    _ -> EventRecoveryState server clients rpcindices

goBack : Int -> EventStream serverModel serverMsg remoteServerMsg model msg -> EventStream serverModel serverMsg remoteServerMsg model msg
goBack index (EventStream model) =
  let newStream = model.stream |> Array.slice 0 (index+1)
      recovery = previousRecoveryState index (EventStream model) in
    EventStream (Model newStream recovery.server recovery.clients recovery.rpcindices)

length : EventStream serverModel serverMsg remoteServerMsg model msg -> Int
length (EventStream model) = Array.length model.stream

numberOfClients : EventStream serverModel serverMsg remoteServerMsg model msg -> Int
numberOfClients (EventStream {clients}) = Dict.size clients

clients : EventStream serverModel serverMsg remoteServerMsg model msg -> List ClientId
clients (EventStream {clients}) = clients |> Dict.values |> List.map (\(cid,_) -> cid)

view : EventStream serverModel serverMsg remoteServerMsg model msg -> List (Int, Event serverMsg remoteServerMsg msg)
view (EventStream model) =
  model.stream
    |> Array.map (\(event,_) -> event)
    |> Array.indexedMap (,)
    |> Array.toList
