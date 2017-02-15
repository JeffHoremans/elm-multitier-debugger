module Multitier.Debugger.Client.Model
  exposing (Model(..), wrapInit)

import Array

import Multitier exposing (MultitierCmd)
import Multitier.Server.WebSocket exposing (ClientId)
import Multitier.Debugger.Client.Msg exposing (Msg(..))
import Multitier.Debugger.Server.RPC exposing (RemoteServerMsg(..))
import Multitier.Debugger.Server.State exposing (ServerState)
import Multitier.Debugger.General.DebuggerModel exposing (ClientDebuggerModel, ServerDebuggerModel)
import Multitier.Debugger.General.AppState exposing (AppState(..), RunningState)
import Multitier.Debugger.General.Event exposing (ClientEvent(..))
import Multitier.Debugger.General.ResumeStrategy exposing (ResumeStrategy(..))

type Model model msg serverModel serverMsg remoteServerMsg =
  ClientDebugger (Maybe ClientId) (ClientDebuggerModel model msg) |
  ServerDebugger ClientId (Maybe (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg))

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.appState in (ClientDebugger Maybe.Nothing (ClientDebuggerModel (Running (RunningState model (Array.fromList [(Init,model)]))) True FromBackground), Multitier.map RemoteServerAppMsg AppMsg cmd)
