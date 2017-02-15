module Multitier.Debugger.Server.State
  exposing
    ( ServerState, wrapServerState )

import Array exposing (Array)

import Multitier.Debugger.Server.Msg exposing (ServerMsg(..))
import Multitier.Debugger.Server.Model exposing (ServerModel)
import Multitier.Debugger.General.AppState exposing (AppState(..), RunningState)
import Multitier.Debugger.General.Event exposing (ServerEvent(..))

type alias ServerState serverState = { appState: serverState }

wrapServerState : (serverModel -> (serverState, serverModel, Cmd serverMsg)) -> (ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerState serverState, ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapServerState serverState = \serverModel ->
  let (appState, newAppModel, newCmd) = case serverModel.debugger.appState of
    Running state -> serverState state.appModel
    Paused state -> serverState state.background.appModel in
  let debugger = serverModel.debugger in
    let (newWrappedServerModel, newWrappedCmd) = case serverModel.debugger.appState of
      Running state ->
        { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel (Array.push (StateEvent, newAppModel) state.events)) }} ! [Cmd.map ServerAppMsg newCmd]
      Paused state ->
        { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel (Array.push (StateEvent, newAppModel) state.background.events) }}} ! [Cmd.map ServerAppMsg newCmd]
    in (ServerState appState, newWrappedServerModel, Cmd.batch [newWrappedCmd])
