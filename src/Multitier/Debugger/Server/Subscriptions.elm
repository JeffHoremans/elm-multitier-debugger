module Multitier.Debugger.Server.Subscriptions
  exposing
    (wrapServerSubscriptions)

import Multitier.Server.WebSocket as ServerWebSocket
import Multitier.Debugger.Server.Model exposing (ServerModel)
import Multitier.Debugger.Server.Msg as ServerMsg exposing (ServerMsg(..))
import Multitier.Debugger.General.AppState exposing (AppState(..))

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = case serverModel.debugger.appState of
      Running state -> Sub.map ServerAppMsg (serverSubscriptions state.appModel)
      Paused state -> Sub.map ServerAppMsg (serverSubscriptions state.background.appModel)
    in Sub.batch [appSubs, ServerWebSocket.listen "debugger" OnSocketOpen OnClientConnect OnClientDisconnect (always ServerMsg.Nothing)]
