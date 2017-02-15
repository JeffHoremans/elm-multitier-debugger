module Multitier.Debugger.Client.Subscriptions
  exposing
    (wrapSubscriptions)

import WebSocket
import Multitier.Debugger.Client.Model exposing (Model(..))
import Multitier.Debugger.Client.Msg as ServerMsg exposing (Msg(..))
import Multitier.Debugger.General.AppState exposing (AppState(..))

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Sub (Msg model msg serverModel serverMsg remoteServerMsg))
wrapSubscriptions subscriptions = \model ->
  let subs = case model of
    ClientDebugger _ cmodel ->
      case cmodel.appState of
        Running state -> Sub.map AppMsg (subscriptions state.appModel)
        Paused state -> case cmodel.runInBackground of
          True -> Sub.map AppMsg (subscriptions state.pausedModel)
          False -> Sub.none
    ServerDebugger _ smodel -> Sub.none
  in Sub.batch [subs, WebSocket.listen "ws://localhost:8081/debugger" OnServerSocketMsg]
