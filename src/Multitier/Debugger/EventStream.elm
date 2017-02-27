module Multitier.Debugger.EventStream
  exposing
    ( EventStream
    , Event(..)
    , empty
    , pushServerEvent
    , pushClientEvent
    , previousServerState
    , previousClientState
    , goBack
    , length
    , view )

import Array exposing (Array)
import Dict exposing (Dict)
import Multitier.Server.WebSocket exposing (ClientId)

type EventStream serverEvent serverModel serverCmd clientEvent model cmd = EventStream (Model serverEvent serverModel serverCmd clientEvent model cmd)

type alias Model serverEvent serverModel serverCmd clientEvent model cmd =
  { stream : Array (Event serverEvent clientEvent, EventRecoveryState serverModel serverCmd model cmd)
  , server : (serverModel, serverCmd)
  , clients : Dict String (ClientId, (model, cmd))}

type Event serverEvent clientEvent =
  ServerEvent serverEvent |
  ClientEvent ClientId clientEvent

type alias EventRecoveryState serverModel serverCmd model cmd =
  { server : (serverModel, serverCmd)
  , clients : Dict String (ClientId, (model, cmd)) }


empty : (serverModel, serverCmd) -> EventStream serverEvent serverModel serverCmd clientEvent model cmd
empty serverState = EventStream (Model Array.empty serverState Dict.empty)

pushServerEvent : (serverEvent, serverModel, serverCmd) -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> EventStream serverEvent serverModel serverCmd clientEvent model cmd
pushServerEvent (serverEvent, serverModel, serverCmd) (EventStream {stream, clients}) =
  let newStream = Array.push (ServerEvent serverEvent, EventRecoveryState (serverModel, serverCmd) clients) stream
      newServer = (serverModel, serverCmd) in
    EventStream (Model newStream newServer clients)

pushClientEvent : ClientId -> (clientEvent, model, cmd) -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> EventStream serverEvent serverModel serverCmd clientEvent model cmd
pushClientEvent cid (clientEvent, model, cmd) (EventStream {stream, server, clients}) =
  let newClients = Dict.insert (toString cid) (cid, (model, cmd)) clients in
  let newStream = Array.push (ClientEvent cid clientEvent, EventRecoveryState server newClients) stream in
    EventStream (Model newStream server newClients)

previousServerState : (serverModel,serverCmd) -> Int -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> (serverModel, serverCmd)
previousServerState default index (EventStream {stream}) =
  case Array.get index stream of
    Just (_, recoveryState) -> recoveryState.server
    _ -> default

previousClientState : ClientId -> (model,cmd) -> Int -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> (model, cmd)
previousClientState cid default index (EventStream {stream}) =
  case Array.get index stream of
    Just (_, recoveryState) -> case Dict.get (toString cid) recoveryState.clients of
      Just (_,result) -> result
      _ -> default
    _ -> default

goBack : Int -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> EventStream serverEvent serverModel serverCmd clientEvent model cmd
goBack index (EventStream model) =
  let newStream = model.stream |> Array.slice 0 (index+1)
      newServer = previousServerState model.server index (EventStream model)
      newClients = Dict.map (\key (cid,current) -> (cid, (previousClientState cid current index (EventStream model)))) model.clients
  in EventStream (Model newStream newServer newClients)

length : EventStream serverEvent serverModel serverCmd clientEvent model cmd -> Int
length (EventStream model) = Array.length model.stream

view : EventStream serverEvent serverModel serverCmd clientEvent model cmd -> List (Int, Event serverEvent clientEvent)
view (EventStream model) =
  model.stream
    |> Array.map (\(event,_) -> event)
    |> Array.indexedMap (,)
    |> Array.toList
