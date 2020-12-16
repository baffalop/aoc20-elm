module Day15.Recitation exposing (..)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import List.Extra


solve1 : String -> Maybe Int
solve1 =
    parse >> playUntil 2020 >> Maybe.andThen List.Extra.last


solve2 : String -> Maybe Int
solve2 =
    parse >> playUntil 30000000 >> Maybe.andThen List.Extra.last


playUntil : Int -> List Int -> Maybe (List Int)
playUntil limit input =
    let
        play : Int -> Int -> Dict Int Int -> List Int
        play index lastTurn memory =
            let
                thisTurn =
                    Dict.get lastTurn memory
                        |> Maybe.map ((-) (index - 1))
                        |> Maybe.withDefault 0

                newMemory =
                    Dict.insert lastTurn (index - 1) memory
            in
            if index > limit then
                []

            else
                thisTurn :: play (index + 1) thisTurn newMemory
    in
    List.Extra.unconsLast input
        |> Maybe.map
            (\( lastInput, rest ) ->
                input ++ play (List.length input + 1) lastInput (memorise rest)
            )


memorise : List Int -> Dict Int Int
memorise =
    List.indexedMap ((+) 1 >> flip Tuple.pair)
        >> List.foldl (uncurry Dict.insert) Dict.empty


parse =
    String.split "," >> List.filterMap String.toInt


puzzleInput =
    "5,2,8,16,18,0,1"
