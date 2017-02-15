module Multitier.Debugger.Server.Update
  exposing
    ( wrapUpdateServer
    , sendDebuggerModel )

import Dict exposing (Dict)
import Array exposing (Array)
import Json.Encode as Encode

import Multitier.Server.WebSocket as ServerWebSocket exposing (ClientId)
import Multitier.LowLevel exposing (toJSON)

import Multitier.Debugger.Client.Msg exposing (SocketMsg(..))
import Multitier.Debugger.Server.Msg as ServerMsg exposing (ServerMsg(..))
import Multitier.Debugger.Server.Model exposing (ServerModel)
import Multitier.Debugger.General.AppState exposing (AppState(..), RunningState)
import Multitier.Debugger.General.Event exposing (ServerEvent(..))


wrapUpdateServer : (serverMsg -> serverModel -> (serverModel, Cmd serverMsg)) -> (ServerMsg serverMsg -> ServerModel serverModel serverMsg remoteServerMsg model msg -> (ServerModel serverModel serverMsg remoteServerMsg model msg, Cmd (ServerMsg serverMsg)))
wrapUpdateServer updateServer = \serverMsg serverModel ->
  let debugger = serverModel.debugger in
    let (newServerModel, newCmds) = case serverMsg of
      ServerAppMsg serverAppMsg -> case serverModel.debugger.appState of
        Running state -> let (newAppModel, cmd) = updateServer serverAppMsg state.appModel
          in  { serverModel | debugger = { debugger | appState = Running (RunningState newAppModel (Array.push ((ServerMsgEvent serverAppMsg), newAppModel) state.events)) }} ! [Cmd.map ServerAppMsg cmd]
        Paused state -> let (newAppModel, cmd) = updateServer serverAppMsg state.background.appModel
          in { serverModel | debugger = { debugger | appState = Paused { state | background = RunningState newAppModel (Array.push ((ServerMsgEvent serverAppMsg), newAppModel) state.background.events) }}} ! [Cmd.map ServerAppMsg cmd]

      OnSocketOpen socket -> { serverModel | socket = Just socket } ! []
      OnClientConnect cid -> case serverModel.socket of
        Just socket -> serverModel ! [ServerWebSocket.send socket cid (Encode.encode 0 (toJSON (SetClientId cid)))]
        _ -> serverModel ! []
      OnClientDisconnect cid -> {serverModel | clients = Dict.remove (toString cid) serverModel.clients } ! []

      ServerMsg.Nothing -> serverModel ! []
    in newServerModel ! [newCmds, sendDebuggerModel newServerModel]

sendDebuggerModel : ServerModel serverModel serverMsg remoteServerMsg model msg -> Cmd (ServerMsg serverMsg)
sendDebuggerModel serverModel = case serverModel.socket of
  Just socket ->
    serverModel.clients
      |> Dict.toList
      |> List.map (\(_,cid) -> ServerWebSocket.send socket cid (Encode.encode 0 (toJSON (SetServerModel serverModel.debugger))))
      |> Cmd.batch
  _ -> Cmd.none
