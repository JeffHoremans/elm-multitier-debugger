module Multitier.Debugger
  exposing
    ( program )

import Html exposing (Html)

import Multitier exposing (Config, MultitierCmd, MultitierProgram)
import Multitier.RPC as RPC exposing (RPC)

import Multitier.Debugger.Client.Model as Model exposing (Model(..))
import Multitier.Debugger.Client.View as View
import Multitier.Debugger.Client.Msg exposing (Msg(..), SocketMsg(..))
import Multitier.Debugger.Client.Update as Update exposing (wrapUpdate)
import Multitier.Debugger.Client.Subscriptions as Subscriptions exposing (wrapSubscriptions)
import Multitier.Debugger.Client.Update as Update exposing (wrapUpdate)

import Multitier.Debugger.Server.Model as ServerModel exposing (ServerModel)
import Multitier.Debugger.Server.Update as ServerUpdate exposing (wrapUpdateServer)
import Multitier.Debugger.Server.Msg as ServerMsg exposing (ServerMsg(..))
import Multitier.Debugger.Server.State as ServerState exposing (ServerState)
import Multitier.Debugger.Server.RPC as ServerRPC exposing (RemoteServerMsg(..))
import Multitier.Debugger.Server.Subscriptions as ServerSubscriptions exposing (wrapServerSubscriptions)

-- PROGRAM

program :
  { config: Config
  , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
  , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  , serverState : serverModel -> (serverState, serverModel, Cmd serverMsg)
  , serverRPCs : remoteServerMsg -> RPC serverModel msg serverMsg
  , initServer: (serverModel, Cmd serverMsg)
  , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
  , serverSubscriptions : serverModel -> Sub serverMsg
  }
  -> MultitierProgram (Model model msg serverModel serverMsg remoteServerMsg) (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg)
program stuff = Multitier.program
    { config = stuff.config
    , init = Model.wrapInit stuff.init
    , update = Update.wrapUpdate stuff.update
    , subscriptions = Subscriptions.wrapSubscriptions stuff.subscriptions
    , view = View.wrapView stuff.view
    , serverState = ServerState.wrapServerState stuff.serverState
    , serverRPCs = ServerRPC.wrapServerRPCs stuff.serverRPCs
    , initServer = ServerModel.wrapInitServer stuff.initServer
    , updateServer = ServerUpdate.wrapUpdateServer stuff.updateServer
    , serverSubscriptions = ServerSubscriptions.wrapServerSubscriptions stuff.serverSubscriptions
    }
