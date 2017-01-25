module Multitier.Debugger
  exposing
    ( program
    , Model, ServerModel
    , Msg, ServerMsg )

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Multitier exposing (Config, MultitierCmd, MultitierProgram, (!!))
import Multitier.RPC as RPC exposing (RPC)

-- PROGRAM

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
  -> MultitierProgram (Model model msg remoteServerMsg) (ServerModel serverModel) (Msg msg) (ServerMsg serverMsg)
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

type Model userModel userMsg userRemoteServerMsg =
  Running userModel |
  Paused userModel userModel (MultitierCmd userRemoteServerMsg userMsg)

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.userState in (Running model, Multitier.map UserRemoteServerMsg UserMsg cmd)

type Msg msg = UserMsg msg | Pause | Resume

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg msg -> Model model msg remoteServerMsg -> ( Model model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg) ))
wrapUpdate update = \msg model -> case msg of
  UserMsg userMsg -> case model of
    Running userModel ->
      let (newUserModel, cmd) = update userMsg userModel
      in  (Running newUserModel, Multitier.map UserRemoteServerMsg UserMsg cmd)
    Paused pausedModel userModel cmd ->
      let (newUserModel, newCmd) = update userMsg userModel
      in  (Paused pausedModel newUserModel (Multitier.batch [cmd, newCmd]), Multitier.none)
  Pause -> case model of
    Running userModel -> Paused userModel userModel Multitier.none !! []
    _ -> model !! []
  Resume -> case model of
    Paused pausedModel userModel cmd -> Running userModel !! [Multitier.map UserRemoteServerMsg UserMsg cmd]
    _ -> model !! []


-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg remoteServerMsg -> Sub (Msg msg))
wrapSubscriptions subscriptions = \model -> case model of
  Running userModel -> Sub.map UserMsg (subscriptions userModel)
  Paused pausedModel _ _ -> Sub.map UserMsg (subscriptions pausedModel)

-- VIEW

wrapView : (model -> Html msg) -> (Model model msg remoteServerMsg -> Html (Msg msg))
wrapView view = \model -> case model of
  Running userModel ->
    Html.div [] [
      Html.map UserMsg (view userModel),
      Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
        Html.button [onClick Pause] [Html.text "Pause"]]
    ]
  Paused pausedModel _ _ ->
    Html.div [] [
      Html.map UserMsg (view pausedModel),
      Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
        Html.button [onClick Resume] [Html.text "Resume"]]
    ]
