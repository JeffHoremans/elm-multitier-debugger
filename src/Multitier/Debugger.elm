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
-- import Task exposing (Task)
import Dict exposing (Dict)
import WebSocket

import Multitier exposing (Config, MultitierCmd, MultitierProgram, (!!), performOnServer)
import Multitier.RPC as RPC exposing (RPC, rpc)
import Multitier.Error exposing (Error)
import Multitier.Server.WebSocket as ServerWebSocket exposing (WebSocket, ClientId)
-- import Multitier.Server.Console as Console

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
  -> MultitierProgram (Model model msg serverModel serverMsg) (ServerModel serverModel serverMsg model msg) (Msg model msg serverModel serverMsg) (ServerMsg serverMsg)
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

type alias ServerModel serverModel serverMsg model msg =
  { socket: Maybe WebSocket
  , debuggerCid: Maybe ClientId
  , debugger: ServerDebuggerModel serverModel serverMsg model msg }

type alias ServerDebuggerModel serverModel serverMsg model msg =
  { appState: AppState serverModel serverMsg
  , clientStates : Dict ClientId (AppState model msg)}

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) = (ServerModel Maybe.Nothing Maybe.Nothing (ServerDebuggerModel (Running (RunningState serverModel Array.empty)) Dict.empty), Cmd.map ServerAppMsg cmd)

type ServerMsg serverMsg = ServerAppMsg serverMsg |
                           PauseServer | ResumeServer | GoBackServer Int |
                           OnSocketOpen WebSocket |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg model msg -> (ServerModel serverModel serverMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel -> let debugger = serverModel.debugger in case serverMsg of
  ServerAppMsg serverAppMsg -> case serverModel.debugger.appState of
    Running state -> let (newAppModel, cmd) = updateServer serverAppMsg state.appModel
      in  { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel (Array.push (serverAppMsg, newAppModel) state.messages)) }} ! [Cmd.map ServerAppMsg cmd]
    Paused state -> let (newAppModel, cmd) = updateServer serverAppMsg state.background.appModel
      in { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel (Array.push (serverAppMsg, newAppModel) state.background.messages) }}} ! [Cmd.map ServerAppMsg cmd]

  PauseServer -> case serverModel.debugger.appState of
    Running state -> { serverModel | debugger = { debugger | appState = Paused (PausedState state.appModel state.messages ((Array.length state.messages) - 1) (RunningState state.appModel Array.empty)) }} ! []
    _ -> serverModel ! []
  ResumeServer -> case serverModel.debugger.appState of
    Paused state -> { serverModel | debugger = { debugger | appState = Running (RunningState state.background.appModel (Array.append state.pausedMessages state.background.messages)) }} ! []
    _ -> serverModel ! []
  GoBackServer index -> case serverModel.debugger.appState of
    Running state -> { serverModel | debugger = { debugger | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.messages) state.messages index (RunningState state.appModel Array.empty))  }} ! []
    Paused state -> { serverModel | debugger = { debugger | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedMessages) state.pausedMessages index state.background) }} ! []

  OnSocketOpen socket -> { serverModel | socket = Just socket } ! []
  Nothing -> serverModel ! []

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg remoteServerMsg --|
  -- StartServerDebugView ClientId |
  -- SetState Int (AppState model msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg model msg) (Msg model msg serverModel serverMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  -- StartServerDebugView cid -> rpc HandleStartServerDebugView (\serverModel -> ({ serverModel | debuggerCid = Just cid }, Task.succeed serverModel.debugger, Cmd.none))
  -- SetState cid state -> rpc HandleSetState (\serverModel -> (serverModel, Task.succeed (), Cmd.none))

  RemoteServerAppMsg msg ->
    RPC.map AppMsg ServerAppMsg
      (\appModel serverModel -> let debugger = serverModel.debugger in case serverModel.debugger.appState of
        Running state -> { serverModel | debugger = { debugger | appState = Running { state | appModel = appModel }}}
        Paused state -> let background = state.background in { serverModel | debugger = { debugger | appState = Paused { state | background = { background | appModel = appModel }}}})
      (\serverModel -> case serverModel.debugger.appState of
        Running state -> state.appModel
        Paused state -> state.background.appModel)
      (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appState: serverState }

wrapServerState : (serverModel -> serverState) -> (ServerModel serverModel serverMsg model msg -> ServerState serverState)
wrapServerState serverState = \serverModel -> let appState = case serverModel.debugger.appState of
  Running state -> serverState state.appModel
  Paused state -> serverState state.background.appModel in ServerState appState

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = case serverModel.debugger.appState of
      Running state -> Sub.map ServerAppMsg (serverSubscriptions state.appModel)
      Paused state -> Sub.map ServerAppMsg (serverSubscriptions state.background.appModel)
    in Sub.batch [appSubs, ServerWebSocket.listen "debugger" OnSocketOpen (always Nothing) (always Nothing) (always Nothing)]

-- MODEL

type Model model msg serverModel serverMsg = ClientDebugger (ClientModel model msg) | ServerDebugger (Maybe (ServerDebuggerModel serverModel serverMsg model msg))

type alias ClientModel appModel appMsg  =
  { appState : AppState appModel appMsg
  , runInBackground : Bool
  , resume : ResumeStrategy }

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.appState in (ClientDebugger (ClientModel (Running (RunningState model Array.empty)) True FromBackground), Multitier.map RemoteServerAppMsg AppMsg cmd)

type Msg model msg serverModel serverMsg =
  AppMsg msg |

  Pause | Resume | GoBack Int |
  ToggleRunInBackground Bool | SetResume Int |

  SwitchDebugger |

  SetServerModel String |
  HandleSetState (Result Error ()) |
  HandleStartServerDebugView (Result Error (ServerDebuggerModel serverModel serverMsg model msg))

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg -> Model model msg serverModel serverMsg -> ( Model model msg serverModel serverMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg) ))
wrapUpdate update = \msg model -> case model of
  ClientDebugger cmodel -> case msg of
    AppMsg appMsg -> case cmodel.appState of
      Running state -> let (newAppModel, cmd) = update appMsg state.appModel
        in  ClientDebugger { cmodel | appState = Running (RunningState newAppModel (Array.push (appMsg, newAppModel) state.messages))} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
      Paused state -> case cmodel.runInBackground of
        True -> let (newAppModel, cmd) = update appMsg state.background.appModel
          in  ClientDebugger { cmodel | appState = Paused { state | background = RunningState newAppModel (Array.push (appMsg, newAppModel) state.background.messages) }} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
        False -> model !! []

    Pause -> case cmodel.appState of
      Running state -> ClientDebugger { cmodel | resume = if cmodel.runInBackground then cmodel.resume else FromPaused
                               , appState = Paused (PausedState state.appModel state.messages ((Array.length state.messages) - 1) (RunningState state.appModel Array.empty)) } !! []
      _ -> model !! []
    Resume -> case cmodel.appState of
      Paused state -> case cmodel.resume of
        FromPrevious -> ClientDebugger { cmodel | appState = Running (RunningState state.pausedModel (getPreviousMessages state.previousIndex state.pausedMessages)) } !! []
        FromPaused -> ClientDebugger { cmodel | appState = Running (RunningState (getPreviousAppModel state.pausedModel ((Array.length state.pausedMessages) - 1) state.pausedMessages) state.pausedMessages) } !! []
        FromBackground -> ClientDebugger { cmodel | appState = Running (RunningState state.background.appModel (Array.append state.pausedMessages state.background.messages)) } !! []
      _ -> model !! []
    GoBack index -> case cmodel.appState of
      Running state -> ClientDebugger { cmodel | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.messages) state.messages index (RunningState state.appModel Array.empty))  } !! []
      Paused state -> ClientDebugger { cmodel | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedMessages) state.pausedMessages index state.background) } !! []

    ToggleRunInBackground runInBackground -> ClientDebugger { cmodel | runInBackground = runInBackground } !! []
    SetResume index -> case Array.get index resumeStrategies of
      Just resume -> ClientDebugger { cmodel | resume = resume } !! []
      _ -> model !! []

    SwitchDebugger -> ServerDebugger Maybe.Nothing !! []

    HandleSetState result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []

    SetServerModel data -> model !! []
    _ -> model !! []

  _ -> model !! []

getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index messages = case Array.get index messages of
  Just (_, model) -> model
  _ -> appModel

getPreviousMessages : Int -> Array (appMsg, appModel) -> Array (appMsg, appModel)
getPreviousMessages index messages = messages |> Array.slice 0 (index+1)

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg -> Sub (Msg model msg serverModel serverMsg))
wrapSubscriptions subscriptions = \model -> case model of
  ClientDebugger cmodel ->
    case cmodel.appState of
      Running state -> Sub.map AppMsg (subscriptions state.appModel)
      Paused state -> case cmodel.runInBackground of
        True -> Sub.map AppMsg (subscriptions state.pausedModel)
        False -> Sub.none
  ServerDebugger smodel -> WebSocket.listen "ws://localhost:8081/debugger" SetServerModel

-- VIEW

-- custom decoder for getting the selected index
targetSelectedIndex : Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

selectResume: ResumeStrategy -> Bool -> List (Html (Msg model msg serverModel serverMsg))
selectResume currentResume runInBackground =
  resumeStrategies
    |> Array.map (\resume -> Html.option [selected (currentResume == resume), disabled (if resume == FromBackground && not runInBackground then True else False)] [ Html.text (resumeToString resume)])
    |> Array.toList

resumeToString : ResumeStrategy -> String
resumeToString resume = case resume of
  FromPrevious -> "previous selected state"
  FromPaused -> "paused state"
  FromBackground -> "current state running in background"

wrapView : (model -> Html msg) -> (Model model msg serverModel serverMsg -> Html (Msg model msg serverModel serverMsg))
wrapView appView = \model -> case model of
  ClientDebugger cmodel ->
    let view appModel messages previousIndex divAtt actionProps =
      Html.div [] [
        Html.div divAtt [
          Html.map AppMsg (appView appModel)],
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          actions cmodel actionProps,
          messageView appModel messages previousIndex]]
      in case cmodel.appState of
        Running state ->
          view state.appModel state.messages ((Array.length state.messages) - 1) [] (ActionProps Pause "Pause" False True)
        Paused state ->
          view state.pausedModel state.pausedMessages state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] (ActionProps Resume "Resume" True False)
  ServerDebugger smodel -> Html.div [] []

messageView : model -> Array (msg, model) -> Int -> Html (Msg model msg serverModel serverMsg)
messageView appModel messages previousIndex =
  let options = messages
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (toString msg)])
    |> Array.toList
    |> List.reverse
  in Html.div [] [
    Html.select [size 15] options,
    Html.pre [] [Html.text (toString appModel)]]

type alias ActionProps msg =
  { btnAction: msg
  , btnText: String
  , hideRunInBackground: Bool
  , hideResumeFrom: Bool }

actions : ClientModel model msg -> ActionProps (Msg model msg serverModel serverMsg) -> Html (Msg model msg serverModel serverMsg)
actions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
  Html.text "Run in background when paused",
  Html.input [disabled props.hideRunInBackground, checked model.runInBackground, type_ "checkbox", onCheck ToggleRunInBackground] [],
  Html.br [] [],
  Html.text "Resume from: ",
  Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume model.runInBackground)]
