module Multitier.Debugger.Server.RPC
  exposing
    ( RemoteServerMsg(..)
    , wrapServerRPCs)

import Dict exposing (Dict)
import Task exposing (Task)
import Array exposing (Array)

import Multitier.RPC as RPC exposing (RPC, rpc)
import Multitier.Server.WebSocket exposing (ClientId)

import Multitier.Debugger.Client.Msg exposing (Msg(..))
import Multitier.Debugger.Server.Model exposing (ServerModel)
import Multitier.Debugger.Server.Msg exposing (ServerMsg(..))
import Multitier.Debugger.Server.Update exposing (sendDebuggerModel)
import Multitier.Debugger.General.DebuggerModel exposing (ClientDebuggerModel)
import Multitier.Debugger.General.Event exposing (ServerEvent(..))
import Multitier.Debugger.General.AppState exposing (AppState(..), RunningState, PausedState)

type RemoteServerMsg remoteServerMsg model msg =
  RemoteServerAppMsg remoteServerMsg |

  StartDebugView ClientId | StopDebugView ClientId |
  PauseDebugger | ResumeDebugger | GoBackDebugger Int |

  SetClientDebuggerModel ClientId (ClientDebuggerModel model msg)

wrapServerRPCs : (remoteServerMsg -> RPC serverModel msg serverMsg) -> (RemoteServerMsg remoteServerMsg model msg -> RPC (ServerModel serverModel serverMsg remoteServerMsg model msg) (Msg model msg serverModel serverMsg remoteServerMsg) (ServerMsg serverMsg))
wrapServerRPCs serverRPCs = \remoteServerMsg -> case remoteServerMsg of

  -- SetState cid state -> rpc HandleSetState (\serverModel -> (serverModel, Task.succeed (), Cmd.none))

  StartDebugView cid -> rpc HandleStartDebugView (\serverModel -> ({serverModel | clients = Dict.insert (toString cid) cid serverModel.clients }, Task.succeed (serverModel.debugger), Cmd.none))
  StopDebugView cid -> rpc Handle (\serverModel -> ({ serverModel | clients = Dict.remove (toString cid) serverModel.clients }, Task.succeed (), Cmd.none))

  SetClientDebuggerModel cid model -> rpc Handle
    (\serverModel -> let debugger = serverModel.debugger in
      ({ serverModel | debugger = { debugger | clientStates = Dict.insert (toString cid) (cid, model) debugger.clientStates }}, Task.succeed (), Cmd.none))

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
              let newServerModel = { serverModel | debugger = { debugger | appState = Running (RunningState appModel (Array.push (RPCevent msg, appModel) state.events))}} in
                newServerModel ! [sendDebuggerModel newServerModel]
            Paused state ->
              let background = state.background in
                let newServerModel = { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState appModel (Array.push (RPCevent msg, appModel) state.background.events) }}} in
                  newServerModel ! [sendDebuggerModel newServerModel])
          (\serverModel -> case serverModel.debugger.appState of
            Running state -> state.appModel
            Paused state -> state.background.appModel)
          (serverRPCs msg)

getPreviousAppModel : appModel -> Int -> Array (appMsg, appModel) -> appModel
getPreviousAppModel appModel index events = case Array.get index events of
  Just (_, model) -> model
  _ -> appModel
