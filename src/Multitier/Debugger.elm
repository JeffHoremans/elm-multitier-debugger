module Multitier.Debugger
  exposing
    ( program
    , Model, ServerModel
    , Msg, ServerMsg )

import Html exposing (Html)
import Html.Attributes exposing (style, disabled, size, value, type_)
import Html.Events exposing (onClick, onCheck)
import Array exposing (Array)

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

type alias ServerModel serverModel = { appModel: serverModel }

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) = (ServerModel serverModel, Cmd.map ServerAppMsg cmd)

type ServerMsg serverMsg = ServerAppMsg serverMsg

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel -> (ServerModel serverModel, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel -> case serverMsg of
  ServerAppMsg serverAppMsg ->
    let (serverAppModel, cmd) = updateServer serverAppMsg serverModel.appModel
    in  (ServerModel serverAppModel, Cmd.map ServerAppMsg cmd)

-- PROCEDURE

type RemoteServerMsg remoteServerMsg = RemoteServerAppMsg remoteServerMsg

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg -> RPC (ServerModel serverModel) (Msg msg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of
  RemoteServerAppMsg msg -> RPC.map AppMsg ServerAppMsg (\appModel serverModel -> { serverModel | appModel = appModel}) (\serverModel -> serverModel.appModel) (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel -> ServerState serverState)
wrapServerState serverState = \serverModel -> let appState = serverState serverModel.appModel in ServerState appState

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions = \serverModel -> Sub.map ServerAppMsg (serverSubscriptions serverModel.appModel)

-- MODEL

type alias Model appModel appMsg remoteServerAppMsg =
  { appState : AppState appModel appMsg remoteServerAppMsg
  , resumeFromPaused : Bool
  }

type AppState appModel appMsg remoteServerAppMsg =
  Running appModel (Array (appMsg, appModel)) |
  Paused appModel (Array (appMsg, appModel)) appModel (Array (appMsg, appModel)) (MultitierCmd remoteServerAppMsg appMsg)

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.appState in (Model (Running model Array.empty) False, Multitier.map RemoteServerAppMsg AppMsg cmd)

type Msg msg = AppMsg msg | Pause | Resume | GoBack Int | ToggleResumeFromPaused Bool

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg msg -> Model model msg remoteServerMsg -> ( Model model msg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg) ))
wrapUpdate update = \msg model -> case msg of
  AppMsg appMsg -> case model.appState of
    Running appModel messages ->
      let (newAppModel, cmd) = update appMsg appModel
      in  { model | appState = Running newAppModel (Array.push (appMsg, newAppModel) messages)} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
    Paused pausedModel pausedMessages appModel messages cmd ->
      let (newAppModel, newCmd) = update appMsg appModel
      in  { model | appState = Paused pausedModel pausedMessages newAppModel (Array.push (appMsg, newAppModel) messages) (Multitier.batch [cmd, newCmd])} !! []
  Pause -> case model.appState of
    Running appModel messages -> { model | appState = Paused appModel messages appModel Array.empty Multitier.none } !! []
    _ -> model !! []
  Resume -> case model.appState of
    Paused pausedModel pausedMessages appModel messages cmd -> case model.resumeFromPaused of
      True -> { model | appState = Running pausedModel pausedMessages } !! []
      False -> { model | appState = Running appModel (Array.append pausedMessages messages)} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
    _ -> model !! []
  GoBack index -> case model.appState of
    Running appModel messages -> { model | appState = Paused (getPreviousAppModel appModel index messages) messages appModel Array.empty Multitier.none } !! []
    Paused pausedModel pausedMessages appModel messages cmd -> { model | appState = Paused (getPreviousAppModel pausedModel index pausedMessages) pausedMessages appModel messages cmd } !! []
  ToggleResumeFromPaused _ -> { model | resumeFromPaused = not model.resumeFromPaused } !! []


getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index messages = case Array.get index messages of
  Just (_, model) -> model
  _ -> appModel

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg remoteServerMsg -> Sub (Msg msg))
wrapSubscriptions subscriptions = \model -> case model.appState of
  Running appModel _-> Sub.map AppMsg (subscriptions appModel)
  Paused pausedModel _ _ _ _ -> Sub.none --Sub.map AppMsg (subscriptions pausedModel)

-- VIEW

wrapView : (model -> Html msg) -> (Model model msg remoteServerMsg -> Html (Msg msg))
wrapView appView = \model ->
  let view appModel messages divAtt btnAction btnText =
    Html.div [] [
      Html.div divAtt [
        Html.map AppMsg (appView appModel)],
      Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
        Html.button [onClick btnAction] [Html.text btnText],
        Html.br [] [],
        Html.text "Resume from current paused model",
        Html.input [value (toString model.resumeFromPaused), type_ "checkbox", onCheck ToggleResumeFromPaused] [],
        Html.br [] [],
        messageView messages,
        Html.pre [] [Html.text (toString appModel)]]]
    in case model.appState of
      Running appModel messages ->
        view appModel messages [] Pause "Pause"
      Paused pausedModel pausedMessages _ _ _ ->
        view pausedModel pausedMessages [disabled True, onClick Resume, style [("opacity", "0.25")]] Resume "Resume"

messageView : Array (msg, model) -> Html (Msg msg)
messageView messages =
  let options = messages
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [onClick (GoBack index)] [Html.text (toString msg)])
    |> Array.toList
    |> List.reverse
  in Html.select [size 15] options
