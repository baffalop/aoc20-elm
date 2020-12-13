module Day13.Buses exposing (..)

import List.Extra


solve1 : String -> Maybe Int
solve1 =
    parse1
        >> Maybe.andThen
            (\{ start, buses } ->
                buses
                    |> List.map
                        (\x ->
                            { id = x
                            , wait = x - remainderBy x start
                            }
                        )
                    |> List.Extra.minimumBy .wait
                    |> Maybe.map (\{ id, wait } -> id * wait)
            )


solve2 : String -> Maybe Int
solve2 =
    parse2
        >> toConstraints
        >> (\constraints ->
                List.Extra.maximumBy .id constraints
                    |> Maybe.map
                        (\max ->
                            List.map (\c -> { c | offset = c.offset - max.offset }) constraints
                                |> satisfy max.id 1
                        )
           )


satisfy : Int -> Int -> List Constraint -> Int
satisfy bus mult input =
    if List.all (\{ id, offset } -> (((bus * mult) + offset) |> remainderBy id) == 0) input then
        bus * mult

    else
        satisfy bus (mult + 1) input


type alias Schedule =
    { start : Int
    , buses : List Int
    }


type alias Constraint =
    { id : Int
    , offset : Int
    }


parse1 : String -> Maybe Schedule
parse1 input =
    case String.lines input of
        line1 :: line2 :: [] ->
            String.toInt line1
                |> Maybe.map
                    (\start ->
                        { start = start
                        , buses =
                            line2
                                |> String.split ","
                                |> List.filterMap String.toInt
                        }
                    )

        _ ->
            Nothing


parse2 : String -> List (Maybe Int)
parse2 input =
    case String.lines input of
        _ :: line2 :: [] ->
            line2
                |> String.split ","
                |> List.map String.toInt

        _ ->
            []


toConstraints : List (Maybe Int) -> List Constraint
toConstraints =
    let
        process : Int -> List (Maybe Int) -> List Constraint
        process lastOffset input =
            case List.Extra.splitWhen ((/=) Nothing) input of
                Just ( xs, (Just bus) :: rest ) ->
                    let
                        offset =
                            lastOffset + List.length xs
                    in
                    { id = bus
                    , offset = offset
                    }
                        :: process offset rest

                _ ->
                    []
    in
    process 0


puzzleInput =
    """1006605
19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,883,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,797,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29"""
