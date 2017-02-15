module Multitier.Debugger.General.DebuggerModel
  exposing
    ( ClientDebuggerModel
    , ServerDebuggerModel )

import Dict exposing (Dict)

import Multitier.Server.WebSocket exposing (ClientId)

import Multitier.Debugger.General.AppState exposing (AppState)
import Multitier.Debugger.General.Event exposing (ClientEvent, ServerEvent)
import Multitier.Debugger.General.ResumeStrategy exposing (ResumeStrategy)

type alias ClientDebuggerModel appModel appMsg  =
  { appState : AppState appModel (ClientEvent appMsg)
  , runInBackground : Bool
  , resume : ResumeStrategy }

type alias ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg =
  { appState: AppState serverModel (ServerEvent serverMsg remoteServerMsg)
  , clientStates : Dict String (ClientId, ClientDebuggerModel model msg) }
