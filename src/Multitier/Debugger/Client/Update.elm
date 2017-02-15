module Multitier.Debugger.Client.Update
  exposing (
    wrapUpdate
  )

import Array exposing (Array)

import Multitier exposing (MultitierCmd, (!!), performOnServer)
import Multitier.LowLevel exposing (fromJSONString)
import Multitier.Debugger.Client.Msg exposing (Msg(..), SocketMsg(..))
import Multitier.Debugger.Client.Model exposing (Model(..))
import Multitier.Debugger.Server.RPC exposing (RemoteServerMsg(..))

import Multitier.Debugger.General.AppState exposing (AppState(..), RunningState, PausedState)
import Multitier.Debugger.General.ResumeStrategy exposing (ResumeStrategy(..), resumeStrategies)
import Multitier.Debugger.General.Event exposing (ClientEvent(..))

wrapUpdate : (msg -> model -> ( model, MultitierCmd remoteServerMsg msg )) -> (Msg model msg serverModel serverMsg remoteServerMsg -> Model model msg serverModel serverMsg remoteServerMsg -> ( Model model msg serverModel serverMsg remoteServerMsg, MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) ))
wrapUpdate update = \msg model ->
  let (newModel, newCmd) = case model of
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

      HandleStartDebugView result -> model !! []


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
        HandleStartDebugView result -> case result of
          Result.Err err -> model !! [] -- TODO handle error in view
          Ok serverDebuggerModel ->  ServerDebugger cid (Just serverDebuggerModel) !! []
        _ -> model !! []
  in newModel !! [newCmd, sendClientDebuggerModelIfNeeded newModel]

getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index events = case Array.get index events of
  Just (_, model) -> model
  _ -> appModel

getPreviousEvents : Int -> Array (appMsg, appModel) -> Array (appMsg, appModel)
getPreviousEvents index events = events |> Array.slice 0 (index+1)

sendClientDebuggerModelIfNeeded : Model model msg serverModel serverMsg remoteServerMsg -> MultitierCmd (RemoteServerMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg)
sendClientDebuggerModelIfNeeded model = case model of
  ServerDebugger cid smodel -> Multitier.none
  ClientDebugger cid cmodel -> case cid of
    Just clientId -> performOnServer (SetClientDebuggerModel clientId cmodel)
    _ -> Multitier.none
