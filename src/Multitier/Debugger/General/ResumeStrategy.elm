module Multitier.Debugger.General.ResumeStrategy
  exposing
    ( ResumeStrategy(..)
    , resumeStrategies )

import Array exposing (Array)

type ResumeStrategy = FromPrevious | FromPaused | FromBackground

resumeStrategies : Array ResumeStrategy
resumeStrategies = Array.fromList [FromPrevious, FromPaused, FromBackground]
