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
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)
import Dict exposing (Dict)
import WebSocket

import Multitier exposing (Config, MultitierCmd, MultitierProgram, (!!), performOnServer, performOnClient)
import Multitier.RPC as RPC exposing (RPC, rpc)
import Multitier.Error exposing (Error)
import Multitier.Server.WebSocket as ServerWebSocket exposing (WebSocket, ClientId)
-- import Multitier.Server.Console as Console
import Multitier.LowLevel exposing (toJSON, fromJSONString)
-- import Multitier.Server.Console as Console

type ResumeStrategy = FromPrevious | FromPaused | FromBackground

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused, FromBackground]

type AppState appModel appMsg =
  Running (RunningState appModel appMsg) |
  Paused (PausedState appModel appMsg)

type alias RunningState appModel appMsg =
  { appModel : appModel
  , events : Array (appMsg, appModel) }

type alias PausedState appModel appMsg =
  { pausedModel : appModel
  , pausedEvents : Array (appMsg, appModel)
  , previousIndex : Int
  , background : RunningState appModel appMsg }

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
  , client: Maybe ClientId
  , debugger: ServerDebuggerModel serverModel serverMsg model msg }

type alias ServerDebuggerModel serverModel serverMsg model msg =
  { appState: AppState serverModel serverMsg
  , clientStates : Dict ClientId (AppState model msg) }

wrapInitServer : ((serverModel, Cmd serverMsg)) -> (((ServerModel serverModel serverMsg model msg), Cmd (ServerMsg serverMsg)))
wrapInitServer (serverModel, cmd) = (ServerModel Maybe.Nothing Maybe.Nothing (ServerDebuggerModel (Running (RunningState serverModel Array.empty)) Dict.empty), Cmd.map ServerAppMsg cmd)

type ServerMsg serverMsg = ServerAppMsg serverMsg |
                           OnSocketOpen WebSocket | OnClientConnect ClientId | OnClientDisconnect ClientId |
                           Nothing

wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg model msg -> (ServerModel serverModel serverMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger in
    let (newServerModel, newCmds) = case serverMsg of
      ServerAppMsg serverAppMsg -> case serverModel.debugger.appState of
        Running state -> let (newAppModel, cmd) = updateServer serverAppMsg state.appModel
          in  { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel (Array.push (serverAppMsg, newAppModel) state.events)) }} ! [Cmd.map ServerAppMsg cmd]
        Paused state -> let (newAppModel, cmd) = updateServer serverAppMsg state.background.appModel
          in { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel (Array.push (serverAppMsg, newAppModel) state.background.events) }}} ! [Cmd.map ServerAppMsg cmd]

      OnSocketOpen socket -> { serverModel | socket = Just socket } ! []
      OnClientConnect cid -> case serverModel.socket of
        Just socket -> serverModel ! [ServerWebSocket.send socket cid (Encode.encode 0 (toJSON (SetClientId cid)))]
        _ -> serverModel ! []
      OnClientDisconnect cid -> serverModel ! []

      Nothing -> serverModel ! []
    in newServerModel ! [newCmds, sendDebuggerModel newServerModel]

sendDebuggerModel : ServerModel serverModel serverMsg model msg -> Cmd (ServerMsg serverMsg)
sendDebuggerModel serverModel = case (serverModel.socket, serverModel.client) of
  (Just socket, Just cid) -> ServerWebSocket.send socket cid (Encode.encode 0 (toJSON (SetServerModel serverModel.debugger)))
  _ -> Cmd.none

-- PROCEDURE

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int
  -- SetState Int (AppState model msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg model msg) (Msg model msg serverModel serverMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  -- SetState cid state -> rpc HandleSetState (\serverModel -> (serverModel, Task.succeed (), Cmd.none))

  StartDebugView cid -> rpc Handle (\serverModel -> ({serverModel | client = Just cid }, Task.succeed (), Cmd.none))
  StopDebugView cid -> rpc Handle (\serverModel -> ({ serverModel | client = Maybe.Nothing }, Task.succeed (), Cmd.none))

  PauseDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.appState of
      Running state ->
        let debugger = serverModel.debugger in
          let newServerModel = { serverModel | debugger = { debugger | appState = Paused (PausedState state.appModel state.events ((Array.length state.events) - 1) (RunningState state.appModel Array.empty)) }} in
            (newServerModel, Task.succeed (), sendDebuggerModel newServerModel)
      _ -> (serverModel, Task.succeed (), Cmd.none))

  ResumeDebugger -> rpc Handle
    (\serverModel -> case serverModel.debugger.appState of
      Paused state ->
        let debugger = serverModel.debugger in
          let newServerModel = { serverModel | debugger = { debugger | appState = Running (RunningState state.background.appModel (Array.append state.pausedEvents state.background.events)) }} in
            (newServerModel, Task.succeed (), sendDebuggerModel newServerModel)
      _ -> (serverModel, Task.succeed (), Cmd.none))

  GoBackDebugger index -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in case serverModel.debugger.appState of
      Running state ->
        let newServerModel = { serverModel | debugger = { debugger | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.events) state.events index (RunningState state.appModel Array.empty))  }} in
          (newServerModel, Task.succeed (), sendDebuggerModel newServerModel)
      Paused state ->
        let newServerModel = { serverModel | debugger = { debugger | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedEvents) state.pausedEvents index state.background) }} in
          (newServerModel, Task.succeed (), sendDebuggerModel newServerModel))


  RemoteServerAppMsg msg ->
    RPC.map AppMsg ServerAppMsg
      (\appModel serverModel ->
        let debugger = serverModel.debugger in
          case serverModel.debugger.appState of
            Running state ->
              let newServerModel = { serverModel | debugger = { debugger | appState = Running { state | appModel = appModel }}} in
                newServerModel ! [sendDebuggerModel newServerModel]
            Paused state ->
              let background = state.background in
                let newServerModel = { serverModel | debugger = { debugger | appState = Paused { state | background = { background | appModel = appModel }}}} in
                  newServerModel ! [sendDebuggerModel newServerModel])
          (\serverModel -> case serverModel.debugger.appState of
            Running state -> state.appModel
            Paused state -> state.background.appModel)
          (serverRPCs msg)

-- SERVER-STATE

type alias ServerState serverState = { appState: serverState }

wrapServerState : (serverModel -> (serverState, serverModel, Cmd serverMsg)) -> (ServerModel serverModel serverMsg model msg -> (ServerState serverState, ServerModel serverModel serverMsg model msg, Cmd (ServerMsg serverMsg)))
wrapServerState serverState = \serverModel ->
  let (appState, newAppModel, newCmd) = case serverModel.debugger.appState of
    Running state -> serverState state.appModel
    Paused state -> serverState state.background.appModel in
  let debugger = serverModel.debugger in
    let (newWrappedServerModel, newWrappedCmd) = case serverModel.debugger.appState of
      Running state ->
        { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel --(Array.push (serverAppMsg, newAppModel) state.events) TODO add serverstate debug event
                                                                                              state.events) }} ! [Cmd.map ServerAppMsg newCmd]
      Paused state ->
        { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel --(Array.push (serverAppMsg, newAppModel) state.background.events)
                                                                                                                  state.background.events }}} ! [Cmd.map ServerAppMsg newCmd]
    in (ServerState appState, newWrappedServerModel, Cmd.batch [newWrappedCmd])

-- SERVER-SUBSCRIPTIONS

wrapServerSubscriptions : (serverModel -> Sub serverMsg) -> (ServerModel serverModel serverMsg model msg -> Sub (ServerMsg serverMsg))
wrapServerSubscriptions serverSubscriptions =
  \serverModel ->
    let appSubs = case serverModel.debugger.appState of
      Running state -> Sub.map ServerAppMsg (serverSubscriptions state.appModel)
      Paused state -> Sub.map ServerAppMsg (serverSubscriptions state.background.appModel)
    in Sub.batch [appSubs, ServerWebSocket.listen "debugger" OnSocketOpen OnClientConnect OnClientDisconnect (always Nothing)]

-- MODEL

type Model model msg serverModel serverMsg = ClientDebugger (Maybe ClientId) (ClientModel model msg) | ServerDebugger ClientId (Maybe (ServerDebuggerModel serverModel serverMsg model msg))

type alias ClientModel appModel appMsg  =
  { appState : AppState appModel (ClientEvent appMsg)
  , runInBackground : Bool
  , resume : ResumeStrategy }

type ClientEvent appMsg = Init | MsgEvent appMsg

wrapInit : (serverState -> (model, MultitierCmd remoteServerMsg msg)) -> (ServerState serverState -> (Model model msg serverModel serverMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg)))
wrapInit init = \serverState -> let (model, cmd) = init serverState.appState in (ClientDebugger Maybe.Nothing (ClientModel (Running (RunningState model Array.empty)) True FromBackground), Multitier.map RemoteServerAppMsg AppMsg cmd)

type Msg model msg serverModel serverMsg =
  AppMsg msg |

  OnServerSocketMsg String |

  Pause | Resume | GoBack Int |
  ToggleRunInBackground Bool | SetResume Int |

  SwitchDebugger |

  Handle (Result Error ())

type ServerSocketMsg serverModel serverMsg model msg = SetClientId ClientId | SetServerModel (ServerDebuggerModel serverModel serverMsg model msg)

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg -> Model model msg serverModel serverMsg -> ( Model model msg serverModel serverMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg) ))
wrapUpdate update = \msg model -> case model of
  ClientDebugger cid cmodel -> case msg of
    OnServerSocketMsg data -> case (fromJSONString data) of
      SetClientId clientId -> ClientDebugger (Just clientId) cmodel !! []
      _ -> model !! []

    AppMsg appMsg -> case cmodel.appState of
      Running state -> let (newAppModel, cmd) = update appMsg state.appModel
        in  ClientDebugger cid { cmodel | appState = Running (RunningState newAppModel (Array.push ((MsgEvent appMsg), newAppModel) state.events))} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
      Paused state -> case cmodel.runInBackground of
        True -> let (newAppModel, cmd) = update appMsg state.background.appModel
          in  ClientDebugger cid { cmodel | appState = Paused { state | background = RunningState newAppModel (Array.push ((MsgEvent appMsg), newAppModel) state.background.events) }} !! [Multitier.map RemoteServerAppMsg AppMsg cmd]
        False -> model !! []

    Pause -> case cmodel.appState of
      Running state -> ClientDebugger cid { cmodel | resume = if cmodel.runInBackground then cmodel.resume else FromPaused
                               , appState = Paused (PausedState state.appModel state.events ((Array.length state.events) - 1) (RunningState state.appModel Array.empty)) } !! []
      _ -> model !! []
    Resume -> case cmodel.appState of
      Paused state -> case cmodel.resume of
        FromPrevious -> ClientDebugger cid { cmodel | appState = Running (RunningState state.pausedModel (getPreviousEvents state.previousIndex state.pausedEvents)) } !! []
        FromPaused -> ClientDebugger cid { cmodel | appState = Running (RunningState (getPreviousAppModel state.pausedModel ((Array.length state.pausedEvents) - 1) state.pausedEvents) state.pausedEvents) } !! []
        FromBackground -> ClientDebugger cid { cmodel | appState = Running (RunningState state.background.appModel (Array.append state.pausedEvents state.background.events)) } !! []
      _ -> model !! []
    GoBack index -> case cmodel.appState of
      Running state -> ClientDebugger cid { cmodel | appState = Paused (PausedState (getPreviousAppModel state.appModel index state.events) state.events index (RunningState state.appModel Array.empty))  } !! []
      Paused state -> ClientDebugger cid { cmodel | appState = Paused (PausedState (getPreviousAppModel state.pausedModel index state.pausedEvents) state.pausedEvents index state.background) } !! []

    ToggleRunInBackground runInBackground -> ClientDebugger cid { cmodel | runInBackground = runInBackground } !! []
    SetResume index -> case Array.get index resumeStrategies of
      Just resume -> ClientDebugger cid { cmodel | resume = resume } !! []
      _ -> model !! []

    SwitchDebugger -> case cid of
      Just clientId -> ServerDebugger clientId Maybe.Nothing !! [performOnServer (StartDebugView clientId)]
      _ -> model !! [] -- TODO report error

    Handle result -> case result of
      Result.Err err -> model !! [] -- TODO error in view
      _ -> model !! []


  ServerDebugger cid sm -> case sm of
    Just smodel -> case msg of
      OnServerSocketMsg data -> case (fromJSONString data) of
        SetServerModel serverModel -> ServerDebugger cid (Just serverModel) !! []
        _ -> model !! []
      Pause -> model !! [performOnServer PauseDebugger]
      Resume -> model !! [performOnServer ResumeDebugger]
      GoBack index -> model !! [performOnServer (GoBackDebugger index)]
      SwitchDebugger -> model !! []

      _ -> model !! []
    _ -> case msg of
      OnServerSocketMsg data -> case (fromJSONString data) of
        SetServerModel serverModel -> ServerDebugger cid (Just serverModel) !! []
        _ -> model !! []
      _ -> model !! []

getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index events = case Array.get index events of
  Just (_, model) -> model
  _ -> appModel

getPreviousEvents : Int -> Array (appMsg, appModel) -> Array (appMsg, appModel)
getPreviousEvents index events = events |> Array.slice 0 (index+1)

-- SUBSCRIPTIONS

wrapSubscriptions : (model -> Sub msg) -> (Model model msg serverModel serverMsg -> Sub (Msg model msg serverModel serverMsg))
wrapSubscriptions subscriptions = \model ->
  let subs = case model of
    ClientDebugger _ cmodel ->
      case cmodel.appState of
        Running state -> Sub.map AppMsg (subscriptions state.appModel)
        Paused state -> case cmodel.runInBackground of
          True -> Sub.map AppMsg (subscriptions state.pausedModel)
          False -> Sub.none
    ServerDebugger _ smodel -> Sub.none
  in Sub.batch [subs, WebSocket.listen "ws://localhost:8081/debugger" OnServerSocketMsg]

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
  ClientDebugger _ cmodel ->
    let view appModel events previousIndex divAtt actionProps =
      Html.div [] [
        Html.div divAtt [
          Html.map AppMsg (appView appModel)],
        Html.div [style [("position", "fixed"), ("bottom", "0"), ("width", "100%")]] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to server"],
          actions cmodel actionProps,
          eventsView appModel events previousIndex]]
      in case cmodel.appState of
        Running state ->
          view state.appModel state.events ((Array.length state.events) - 1) [] (ActionProps Pause "Pause" False True)
        Paused state ->
          view state.pausedModel state.pausedEvents state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] (ActionProps Resume "Resume" True False)

  ServerDebugger _ sm -> case sm of
    Just smodel ->
      let view appModel events previousIndex divAtt actionProps =
        Html.div [] [
          Html.button [onClick SwitchDebugger] [Html.text "Switch to client"],
          -- actions smodel actionProps,
          serverEventsView appModel events previousIndex]
        in case smodel.appState of
          Running state ->
            view state.appModel state.events ((Array.length state.events) - 1) [] (ActionProps Pause "Pause" False True)
          Paused state ->
            view state.pausedModel state.pausedEvents state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] (ActionProps Resume "Resume" True False)
    _ -> Html.text "loading..."

serverEventsView : serverModel -> Array (serverMsg, serverModel) -> Int -> Html (Msg model msg serverModel serverMsg)
serverEventsView appModel events previousIndex =
  let options = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (toString msg)])
    |> Array.toList
    |> List.reverse
  in Html.div [] [
    Html.select [size 15] options,
    Html.pre [] [Html.text (toString appModel)]]

eventsView : model -> Array ((ClientEvent msg), model) -> Int -> Html (Msg model msg serverModel serverMsg)
eventsView appModel events previousIndex =
  let options = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (clientEventView msg)])
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

clientEventView : ClientEvent msg -> String
clientEventView event = case event of
  Init -> "[INIT]"
  MsgEvent msg -> "[MSG] " ++ (toString msg)


actions : ClientModel model msg -> ActionProps (Msg model msg serverModel serverMsg) -> Html (Msg model msg serverModel serverMsg)
actions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
  Html.text "Run in background when paused",
  Html.input [disabled props.hideRunInBackground, checked model.runInBackground, type_ "checkbox", onCheck ToggleRunInBackground] [],
  Html.br [] [],
  Html.text "Resume from: ",
  Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume model.runInBackground)]
