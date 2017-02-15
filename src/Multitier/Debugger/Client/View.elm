module Multitier.Debugger.Client.View
  exposing ( wrapView )

import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html)
import Html.Attributes exposing (style, disabled, selected, size, checked, type_)
import Html.Events exposing (onClick, onCheck, on)
import Array exposing (Array)
import Dict exposing (Dict)
import Svg
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, r, cx, cy, fill)

import Multitier.Server.WebSocket exposing (ClientId)

import Multitier.Debugger.Client.Msg exposing (Msg(..))
import Multitier.Debugger.Client.Model exposing (Model(..))
import Multitier.Debugger.General.DebuggerModel exposing (ClientDebuggerModel, ServerDebuggerModel)
import Multitier.Debugger.General.ResumeStrategy exposing (ResumeStrategy(..), resumeStrategies)
import Multitier.Debugger.General.AppState exposing (AppState(..))
import Multitier.Debugger.General.Event exposing (ClientEvent(..), ServerEvent(..))

-- custom decoder for getting the selected index
targetSelectedIndex : Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

selectResume: ResumeStrategy -> Bool -> List (Html (Msg model msg serverModel serverMsg remoteServerMsg))
selectResume currentResume runInBackground =
  resumeStrategies
    |> Array.map (\resume -> Html.option [selected (currentResume == resume), disabled (if resume == FromBackground && not runInBackground then True else False)] [ Html.text (resumeToString resume)])
    |> Array.toList

resumeToString : ResumeStrategy -> String
resumeToString resume = case resume of
  FromPrevious -> "previous selected state"
  FromPaused -> "paused state"
  FromBackground -> "current state running in background"

wrapView : (model -> Html msg) -> (Model model msg serverModel serverMsg remoteServerMsg -> Html (Msg model msg serverModel serverMsg remoteServerMsg))
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
          serverActions smodel actionProps,
          serverEventsView appModel events previousIndex,
          clientEventsView smodel.clientStates,
          timelineView appModel events previousIndex]
        in case smodel.appState of
          Running state ->
            view state.appModel state.events ((Array.length state.events) - 1) [] (ActionProps Pause "Pause" False True)
          Paused state ->
            view state.pausedModel state.pausedEvents state.previousIndex [disabled True, onClick Resume, style [("opacity", "0.25")]] (ActionProps Resume "Resume" True False)
    _ -> Html.text "loading..."

timelineView : serverModel -> Array ((ServerEvent serverMsg remoteServerMsg), serverModel) -> Int -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
timelineView appModel events previousIndex =
  let offset = 10
      eventSpacing = 25
      circles = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) ->
      Svg.circle [r "5", fill (if previousIndex == index then "gray" else "black"), cx (toString ((index * eventSpacing) + offset )), cy "20", onClick (GoBack index), style [("cursor", "pointer")]] [])
    |> Array.toList
  in
  Html.div [style [("overflow-x", "auto")]] [
    Svg.svg [ width (toString ((((Array.length events) - 1) * eventSpacing) + (offset * 2))), height "40"]
      (List.concat [
        [Svg.line [x1 (toString offset), y1 "20", x2 "100%", y2 "20", style [("stroke", "black"), ("stroke-width", "3")]] []],
        circles ])]


clientEventsView : Dict String (ClientId, ClientDebuggerModel model msg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
clientEventsView clientStates =
  clientStates
    |> Dict.toList
    |> List.map (\(_, (cid,cmodel)) -> let appModel = case cmodel.appState of
        Running state -> state.appModel
        Paused state -> state.pausedModel
      in Html.div [] [Html.text (toString appModel)] )
    |> Html.div []

serverEventsView : serverModel -> Array ((ServerEvent serverMsg remoteServerMsg), serverModel) -> Int -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverEventsView appModel events previousIndex =
  let options = events
    |> Array.indexedMap (,)
    |> Array.map (\(index, (msg, model)) -> Html.option [onClick (GoBack index), selected (previousIndex == index)] [Html.text (serverEventView msg)])
    |> Array.toList
    |> List.reverse
  in Html.div [] [
    Html.select [size 15] options,
    Html.pre [] [Html.text (toString appModel)]]

eventsView : model -> Array ((ClientEvent msg), model) -> Int -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
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
  Init -> "[Init]"
  MsgEvent msg -> "[Msg] " ++ (toString msg)

serverEventView : ServerEvent serverMsg remoteServerMsg -> String
serverEventView event = case event of
  InitServer -> "[Init]"
  ServerMsgEvent msg -> "[Msg] " ++ (toString msg)
  StateEvent -> "[Server-state requested]"
  RPCevent msg -> "[RPC] " ++ (toString msg)


actions : ClientDebuggerModel model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
actions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText],
  Html.br [] [],
  Html.text "Run in background when paused",
  Html.input [disabled props.hideRunInBackground, checked model.runInBackground, type_ "checkbox", onCheck ToggleRunInBackground] [],
  Html.br [] [],
  Html.text "Resume from: ",
  Html.select [disabled props.hideResumeFrom, on "change" (Decode.map SetResume targetSelectedIndex)] (selectResume model.resume model.runInBackground)]

serverActions : ServerDebuggerModel serverModel serverMsg remoteServerMsg model msg -> ActionProps (Msg model msg serverModel serverMsg remoteServerMsg) -> Html (Msg model msg serverModel serverMsg remoteServerMsg)
serverActions model props = Html.div [] [
  Html.button [onClick props.btnAction] [Html.text props.btnText]]
