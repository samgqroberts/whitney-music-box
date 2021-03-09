module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float)
import Main exposing (dotPosition)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "dotPosition"
        [ fuzz float "at time=0 reports 0" <|
            \period -> Expect.equal (dotPosition period 0) 0
        , fuzz float "at time=period reports 0" <|
            \period -> Expect.equal (dotPosition period period) 0
        , fuzz (Fuzz.floatRange 0.00001 999999.9999) "at time=(period / 2) reports pi" <|
            \period -> Expect.within (Absolute 0.0000001) (dotPosition period (period / 2)) pi
        , fuzz (Fuzz.floatRange 0.00001 999999.9999) "at time=(15 * period / 8) reports 7pi/4" <|
            \period -> Expect.within (Absolute 0.0000001) (dotPosition period (15 * period / 8)) (7 * pi / 4)
        ]
