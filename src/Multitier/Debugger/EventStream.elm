module Multitier.Debugger.EventStream
  exposing
    ( EventStream
    , Event(..)
    , empty
    , pushServerEvent
    , pushClientEvent
    , previousState
    , goBack
    , length
    , clients
    , numberOfClients
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

previousState : Int -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> (serverModel, serverCmd, Dict String (ClientId, (model,cmd)))
previousState index (EventStream {stream, server}) =
  case Array.get index stream of
    Just (_, recoveryState) -> let (serverModel, serverCmd) = recoveryState.server in (serverModel, serverCmd, recoveryState.clients)
    _ -> let (serverModel, serverCmd) = server in (serverModel, serverCmd, Dict.empty)

goBack : Int -> EventStream serverEvent serverModel serverCmd clientEvent model cmd -> EventStream serverEvent serverModel serverCmd clientEvent model cmd
goBack index (EventStream model) =
  let newStream = model.stream |> Array.slice 0 (index+1)
      (newServerModel, newServerCmd, newClients) = previousState index (EventStream model)
  in EventStream (Model newStream (newServerModel, newServerCmd) newClients)

length : EventStream serverEvent serverModel serverCmd clientEvent model cmd -> Int
length (EventStream model) = Array.length model.stream

numberOfClients : EventStream serverEvent serverModel serverCmd clientEvent model cmd -> Int
numberOfClients (EventStream {clients}) = Dict.size clients

clients : EventStream serverEvent serverModel serverCmd clientEvent model cmd -> List ClientId
clients (EventStream {clients}) = clients |> Dict.values |> List.map (\(cid,_) -> cid)

view : EventStream serverEvent serverModel serverCmd clientEvent model cmd -> List (Int, Event serverEvent clientEvent)
view (EventStream model) =
  model.stream
    |> Array.map (\(event,_) -> event)
    |> Array.indexedMap (,)
    |> Array.toList
