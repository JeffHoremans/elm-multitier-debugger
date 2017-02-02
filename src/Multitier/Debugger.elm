module Multitier.Debugger
  exposing
    ( program
    , Model, ServerModel
    , Msg, ServerMsg )

import Html exposing (Html)
import Html.Attributes exposing (checked, style, disabled, size, value, type_, selected)
import Html.Events exposing (onClick, onCheck, on)
import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)

import Multitier exposing (Config, MultitierCmd, MultitierProgram, (!!))
import Multitier.RPC as RPC exposing (RPC)

type ResumeStrategy = FromPrevious | FromPaused | FromBackground

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused, FromBackground]

type AppState appModel appMsg =
  Running (RunningState appModel appMsg) |
  Paused (PausedState appModel appMsg)

type alias RunningState appModel appMsg =
  { appModel : appModel
  , messages : Array (appMsg, appModel) }

type alias PausedState appModel appMsg =
  { pausedModel : appModel
  , pausedMessages : Array (appMsg, appModel)
  , previousIndex : Int
  , background : RunningState appModel appMsg }

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
  -> MultitierProgram (Model model msg) (ServerModel serverModel serverMsg) (Msg msg) (ServerMsg serverMsg)
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

type alias ServerModel serverModel serverMsg = { appState: AppState serverModel serverMsg }

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) = (ServerModel (Running (RunningState serverModel Array.empty)), Cmd.map ServerAppMsg cmd)

type ServerMsg serverMsg = ServerAppMsg serverMsg | PauseServer | ResumeServer | GoBackServer Int

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg -> (ServerModel serverModel serverMsg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel -> case serverMsg of
  ServerAppMsg serverAppMsg -> case serverModel.appState of
    Running state -> let (newAppModel, cmd) = updateServer serverAppMsg state.appModel
      in { serverModel | appState = Running (RunningState newAppModel (Array.push (serverAppMsg, newAppModel) state.messages)) } ! [Cmd.map ServerAppMsg cmd]
    Paused state -> let (newAppModel, cmd) = updateServer serverAppMsg state.background.appModel
      in { serverModel | appState = Paused { state | background = RunningState newAppModel (Array.push (serverAppMsg, newAppModel) state.background.messages) }} ! [Cmd.map ServerAppMsg cmd]

  PauseServer -> case serverModel.appState of
    Running state -> { serverModel | appState = Paused (PausedState state.appModel state.messages ((Array.length state.messages) - 1) (RunningState state.appModel Array.empty)) } ! []
    _ -> serverModel ! []
  ResumeServer -> case serverModel.appState of
    Paused state -> { serverModel | appState = Running (RunningState state.background.appModel (Array.append state.pausedMessages state.background.messages)) } ! []
    _ -> serverModel ! []
  GoBackServer index -> case serverModel.appState of
    Running state -> { serverModel | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.messages) state.messages index (RunningState state.appModel Array.empty))  } ! []
    Paused state -> { serverModel | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedMessages) state.pausedMessages index state.background) } ! []

-- PROCEDURE

type RemoteServerMsg remoteServerMsg = RemoteServerAppMsg remoteServerMsg
wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg -> RPC (ServerModel serverModel serverMsg) (Msg msg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of
  RemoteServerAppMsg msg ->
    RPC.map AppMsg ServerAppMsg
      (\appModel serverModel -> case serverModel.appState of
        Running state -> { serverModel | appState = Running { state | appModel = appModel }}
        Paused state -> let background = state.background in { serverModel | appState = Paused { state | background = { background | appModel = appModel }}})
      (\serverModel -> case serverModel.appState of
        Running state -> state.appModel
        Paused state -> state.background.appModel)
      (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel serverMsg -> ServerState serverState)
wrapServerState serverState = \serverModel -> let appState = case serverModel.appState of
  Running state -> serverState state.appModel
  Paused state -> serverState state.background.appModel in ServerState appState

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions = \serverModel -> case serverModel.appState of
  Running state -> Sub.map ServerAppMsg (serverSubscriptions state.appModel)
  Paused state -> Sub.map ServerAppMsg (serverSubscriptions state.background.appModel)


-- MODEL

type alias Model appModel appMsg  =
  { appState : AppState appModel appMsg
  , runInBackground : Bool
  , resume : ResumeStrategy }

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.appState in (Model (Running (RunningState model Array.empty)) True FromBackground, Multitier.map RemoteServerAppMsg AppMsg cmd)

type Msg msg = AppMsg msg | Pause | Resume | GoBack Int | ToggleRunInBackground Bool | SetResume Int

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg msg -> Model model msg -> ( Model model msg, MultitierCmd (RemoteServerMsg remoteServerMsg) (Msg msg) ))
wrapUpdate update = \msg model -> case msg of
  AppMsg appMsg -> case model.appState of
    Running state -> let (newAppModel, cmd) = update appMsg state.appModel
      in  { model | appState = Running (RunningState newAppModel (Array.push (appMsg, newAppModel) state.messages))} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
    Paused state -> case model.runInBackground of
      True -> let (newAppModel, cmd) = update appMsg state.background.appModel
        in  { model | appState = Paused { state | background = RunningState newAppModel (Array.push (appMsg, newAppModel) state.background.messages) }} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
      False -> model !! []

  Pause -> case model.appState of
    Running state -> { model | resume = if model.runInBackground then model.resume else FromPaused
                             , appState = Paused (PausedState state.appModel state.messages ((Array.length state.messages) - 1) (RunningState state.appModel Array.empty)) } !! []
    _ -> model !! []
  Resume -> case model.appState of
    Paused state -> case model.resume of
      FromPrevious -> { model | appState = Running (RunningState state.pausedModel (getPreviousMessages state.previousIndex state.pausedMessages)) } !! []
      FromPaused -> { model | appState = Running (RunningState (getPreviousAppModel state.pausedModel ((Array.length state.pausedMessages) - 1) state.pausedMessages) state.pausedMessages) } !! []
      FromBackground -> { model | appState = Running (RunningState state.background.appModel (Array.append state.pausedMessages state.background.messages)) } !! []
    _ -> model !! []
  GoBack index -> case model.appState of
    Running state -> { model | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.messages) state.messages index (RunningState state.appModel Array.empty))  } !! []
    Paused state -> { model | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedMessages) state.pausedMessages index state.background) } !! []

  ToggleRunInBackground runInBackground -> { model | runInBackground = runInBackground } !! []
  SetResume index -> case Array.get index resumeStrategies of
    Just resume -> { model | resume = resume } !! []
    _ -> model !! []

getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index messages = case Array.get index messages of
  Just (_, model) -> model
  _ -> appModel

getPreviousMessages : Int -> Array (appMsg, appModel) -> Array (appMsg, appModel)
getPreviousMessages index messages = messages |> Array.slice 0 (index+1)

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg -> Sub (Msg msg))
wrapSubscriptions subscriptions = \model -> case model.appState of
  Running state -> Sub.map AppMsg (subscriptions state.appModel)
  Paused state -> case model.runInBackground of
    True -> Sub.map AppMsg (subscriptions state.pausedModel)
    False -> Sub.none

-- VIEW

-- custom decoder for getting the selected index
targetSelectedIndex : Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

selectResume: ResumeStrategy -> Bool -> List (Html (Msg msg))
selectResume currentResume runInBackground =
  resumeStrategies
    |> Array.map (\resume -> Html.option [selected (currentResume == resume), disabled (if resume == FromBackground && not runInBackground then True else False)] [ Html.text (resumeToString resume)])
    |> Array.toList

resumeToString : ResumeStrategy -> String
resumeToString resume = case resume of
  FromPrevious -> "previous selected state"
  FromPaused -> "paused state"
  FromBackground -> "current state running in background"

wrapView : (model -> Html msg) -> (Model model msg -> Html (Msg msg))
wrapView appView = \model ->
  let view appModel messages previousIndex divAtt btnAction btnText hideRunInBackground hideResumeFrom =
    Html.div [] [
      Html.div divAtt [
        Html.map AppMsg (appView appModel)],
      Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
        actions model (ActionsProperties btnAction btnText hideRunInBackground hideResumeFrom),
        Html.br [] [],
        messageView appModel messages previousIndex]]
    in case model.appState of
      Running state ->
        view state.appModel state.messages ((Array.length state.messages) - 1) [] Pause "Pause" False True
      Paused state ->
        view state.pausedModel state.pausedMessages state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] Resume "Resume" True False

messageView : model -> Array (msg, model) -> Int -> Html (Msg msg)
messageView appModel messages previousIndex =
  let options = messages
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (toString msg)])
    |> Array.toList
    |> List.reverse
  in Html.div [] [
    Html.select [size 15] options,
    Html.pre [] [Html.text (toString appModel)]]

type alias ActionsProperties msg =
  { btnAction: msg
  , btnText: String
  , hideRunInBackground: Bool
  , hideResumeFrom: Bool }

actions : Model model msg -> ActionsProperties (Msg msg) -> Html (Msg msg)
actions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
  Html.text "Run in background when paused",
  Html.input [disabled props.hideRunInBackground, checked model.runInBackground, type_ "checkbox", onCheck ToggleRunInBackground] [],
  Html.br [] [],
  Html.text "Resume from: ",
  Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume model.runInBackground)]

-- SERVER-VIEW
-- rendered on client

serverView : ServerModel serverModel serverMsg -> Html (Msg msg)
serverView serverModel = Html.div [] []
