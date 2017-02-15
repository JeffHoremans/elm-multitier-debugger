module Multitier.Debugger.Client.Msg
  exposing
    ( Msg(..)
    , SocketMsg(..))

import Multitier.Server.WebSocket exposing (ClientId)
import Multitier.Error exposing (Error)
import Multitier.Debugger.General.DebuggerModel exposing (ServerDebuggerModel)

type Msg model msg serverModel serverMsg remoteServerMsg =
  AppMsg msg |

  OnServerSocketMsg String |

  Pause | Resume | GoBack Int |
  ToggleRunInBackground Bool | SetResume Int |

  SwitchDebugger |

  Handle (Result Error ()) |
  HandleStartDebugView (Result Error (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg))

type SocketMsg serverModel serverMsg remoteServerMsg model msg = SetClientId ClientId | SetServerModel (ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg)
