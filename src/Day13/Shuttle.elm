module Day13.Shuttle exposing (..)

import List.Extra


solve1 : String -> Maybe Int
solve1 =
    parse
        >> Maybe.andThen
            (\{ start, shuttles } ->
                shuttles
                    |> List.map
                        (\x ->
                            { id = x
                            , wait = x - remainderBy x start
                            }
                        )
                    |> List.Extra.minimumBy .wait
                    |> Maybe.map (\{ id, wait } -> id * wait)
            )


type alias Constraints =
    { start : Int
    , shuttles : List Int
    }


parse : String -> Maybe Constraints
parse input =
    case String.lines input of
        line1 :: line2 :: [] ->
            String.toInt line1
                |> Maybe.map
                    (\start ->
                        { start = start
                        , shuttles =
                            line2
                                |> String.split ","
                                |> List.filterMap String.toInt
                        }
                    )

        _ ->
            Nothing


puzzleInput =
    """1006605
19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,883,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,797,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29"""
