module Multitier.Debugger.Server.Msg
  exposing
    (ServerMsg(..))

import Multitier.Server.WebSocket exposing (ClientId, WebSocket)

type ServerMsg serverMsg =
  ServerAppMsg serverMsg |
  OnSocketOpen WebSocket | OnClientConnect ClientId | OnClientDisconnect ClientId |
  Nothing
