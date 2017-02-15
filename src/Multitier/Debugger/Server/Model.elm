module Multitier.Debugger.Server.Model
  exposing
    ( ServerModel, wrapInitServer )

import Dict exposing (Dict)
import Array exposing (Array)

import Multitier.Server.WebSocket exposing (ClientId, WebSocket)

import Multitier.Debugger.Server.Msg exposing (ServerMsg(..))
import Multitier.Debugger.General.DebuggerModel exposing (ServerDebuggerModel)
import Multitier.Debugger.General.AppState exposing (AppState(..), RunningState)
import Multitier.Debugger.General.Event exposing (ServerEvent(..))

type alias ServerModel serverModel serverMsg remoteServerMsg model msg =
  { socket: Maybe WebSocket
  , clients: Dict String ClientId
  , debugger: ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg }

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg remoteServerMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) =
  (ServerModel
    Maybe.Nothing Dict.empty
    (ServerDebuggerModel (Running (RunningState serverModel (Array.fromList [(InitServer,serverModel)]))) Dict.empty)
  , Cmd.map ServerAppMsg cmd)
