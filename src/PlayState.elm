module PlayState exposing (PlayAction(..), PlayState, getTimeSinceStart, push)

import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (onClick)
import Time exposing (Posix)


type PlayAction
    = Stopped
    | Paused Float
    | Playing Float


type alias PlayState =
    { current : PlayAction, history : List PlayAction }


actionsSinceLastStop : List PlayAction -> List PlayAction
actionsSinceLastStop allActions =
    let
        go actions acc =
            case actions of
                [] ->
                    acc

                x :: xs ->
                    case x of
                        Stopped ->
                            acc

                        Paused t ->
                            go xs <| Paused t :: acc

                        Playing t ->
                            go xs <| Playing t :: acc
    in
    List.reverse (go allActions [])


getTimeSinceStart : Float -> PlayState -> Float
getTimeSinceStart currentTime { current, history } =
    let
        isStopped =
            case current of
                Stopped ->
                    True

                _ ->
                    False
    in
    if isStopped then
        0

    else
        let
            states =
                actionsSinceLastStop history

            step =
                \el ( prev, acc ) ->
                    case el of
                        Stopped ->
                            ( Nothing, 0 )

                        Paused t ->
                            case prev of
                                Nothing ->
                                    ( Just (Paused t), 0 )

                                Just last ->
                                    case last of
                                        Stopped ->
                                            ( Just (Paused t), 0 )

                                        Paused _ ->
                                            ( Just (Paused t), acc )

                                        Playing s ->
                                            ( Just (Paused t), acc + t - s )

                        Playing t ->
                            ( Just (Playing t), acc )

            ( _, timeSinceStart ) =
                List.foldr step ( Nothing, 0 ) (current :: states)
        in
        case current of
            Paused _ ->
                timeSinceStart

            Playing t ->
                timeSinceStart + currentTime - t

            _ ->
                0


push : PlayAction -> PlayState -> PlayState
push ps pi =
    { current = ps, history = pi.current :: pi.history }
