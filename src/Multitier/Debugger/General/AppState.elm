module Multitier.Debugger.General.AppState
  exposing
    ( AppState (..)
    , RunningState
    , PausedState )

import Array exposing (Array)

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
