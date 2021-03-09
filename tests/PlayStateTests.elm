module PlayStateTests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import PlayState exposing (PlayAction(..), getTimeSinceStart)
import Test exposing (..)


all : Test
all =
    describe "getTimeSinceStart"
        [ test "example1" <|
            \_ ->
                let
                    playState =
                        { current = Playing 123
                        , history = [ Paused 111, Playing 100, Stopped, Playing 88 ]
                        }

                    currentTime =
                        134

                    expected =
                        (111 - 100) + (currentTime - 123)
                in
                Expect.equal expected <| getTimeSinceStart currentTime playState
        , test "example2" <|
            \_ ->
                let
                    playState =
                        { current = Paused 123
                        , history = [ Playing 111, Paused 100, Stopped, Playing 88 ]
                        }

                    currentTime =
                        134

                    expected =
                        123 - 111
                in
                Expect.equal expected <| getTimeSinceStart currentTime playState
        , test "example3" <|
            \_ ->
                let
                    playState =
                        { current = Paused 133
                        , history = [ Paused 123, Paused 111, Playing 100, Stopped, Playing 88 ]
                        }

                    currentTime =
                        160

                    expected =
                        111 - 100
                in
                Expect.equal expected <| getTimeSinceStart currentTime playState
        ]
