module Multitier.Debugger.General.Event
  exposing
    ( ClientEvent(..)
    , ServerEvent(..) )

type ClientEvent appMsg = Init | MsgEvent appMsg

type ServerEvent serverMsg remoteServerMsg = InitServer | ServerMsgEvent serverMsg | StateEvent | RPCevent remoteServerMsg
