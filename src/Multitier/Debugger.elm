module Multitier.Debugger exposing (..)

import Html exposing (Html)
import Multitier exposing (Config, MultitierCmd, MultitierProgram)
import Multitier.RPC as RPC exposing (RPC)

program :
  { config: Config
  , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
  , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  , serverState : serverModel -> serverState
  , serverRPCs : remoteServerMsg -> RPC serverModel msg serverMsg
  , initServer: (serverModel, Cmd serverMsg)
  , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
  , serverSubscriptions : serverModel -> Sub serverMsg
  }
  -> MultitierProgram (Model model) (ServerModel serverModel) (Msg msg) (ServerMsg serverMsg)
program stuff = Multitier.program
    { config = stuff.config
    , init = wrapInit stuff.init
    , update = wrapUpdate stuff.update
    , subscriptions = wrapSubscriptions stuff.subscriptions
    , view = wrapView stuff.view
    , serverState = wrapServerState stuff.serverState
    , serverRPCs = wrapServerRPCs stuff.serverRPCs
    , initServer = wrapInitServer stuff.initServer
    , updateServer = wrapUpdateServer stuff.updateServer
    , serverSubscriptions = wrapServerSubscriptions stuff.serverSubscriptions
    }

-- SERVER-MODEL

type alias ServerModel serverModel = { userModel: serverModel }

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) = (ServerModel serverModel, Cmd.map UserServerMsg cmd)

type ServerMsg serverMsg = UserServerMsg serverMsg

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel -> (ServerModel serverModel, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel -> case serverMsg of
  UserServerMsg userServerMsg ->
    let (userServerModel, cmd) = updateServer userServerMsg serverModel.userModel
    in  (ServerModel userServerModel, Cmd.map UserServerMsg cmd)

-- PROCEDURE

type RemoteServerMsg remoteServerMsg = UserRemoteServerMsg remoteServerMsg

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg -> RPC (ServerModel serverModel) (Msg msg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of
  UserRemoteServerMsg msg -> RPC.map UserMsg UserServerMsg (\userModel serverModel -> { serverModel | userModel = userModel}) (\serverModel -> serverModel.userModel) (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { userState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel -> ServerState serverState)
wrapServerState serverState = \serverModel -> let userState = serverState serverModel.userModel in ServerState userState

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions = \serverModel -> Sub.map UserServerMsg (serverSubscriptions serverModel.userModel)

-- MODEL

type alias Model model = { userModel: model }

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.userState in (Model model, Multitier.map UserRemoteServerMsg UserMsg cmd)

type Msg msg = UserMsg msg

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg msg -> Model model -> ( Model model, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg) ))
wrapUpdate update = \msg model -> case msg of
  UserMsg userMsg ->
    let (userModel, cmd) = update userMsg model.userModel
    in  (Model userModel, Multitier.map UserRemoteServerMsg UserMsg cmd)

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model -> Sub (Msg msg))
wrapSubscriptions subscriptions = \model -> Sub.map UserMsg (subscriptions model.userModel)

-- VIEW

wrapView : (model -> Html msg) -> (Model model -> Html (Msg msg))
wrapView view = \model -> Html.map UserMsg (view model.userModel)
