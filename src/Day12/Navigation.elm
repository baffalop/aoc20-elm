module Day12.Navigation exposing (puzzleInput, solve1, solve2)

import Basics.Extra exposing (flip)
import Parser as P exposing ((|.), (|=), Parser)


solve1 : String -> Int
solve1 =
    parse >> navigateShip >> manhattanDistance


solve2 : String -> Int
solve2 =
    parse >> navigate >> .ship >> manhattanDistance


type alias Position a =
    { a
        | x : Int
        , y : Int
    }


type alias Ship =
    Position { bearing : Direction }


type alias Vessel =
    Position {}


type alias Navigation =
    { ship : Vessel
    , waypoint : Vessel
    }


type Instruction
    = Absolute Direction Int
    | Turn Turn Int
    | Forward Int


type Direction
    = North
    | South
    | East
    | West


type Turn
    = Left
    | Right


manhattanDistance : Position a -> Int
manhattanDistance =
    fork .x .y
        >> Tuple.mapBoth abs abs
        >> apply (+)


navigateShip : List Instruction -> Ship
navigateShip =
    List.foldl doForShip
        { x = 0
        , y = 0
        , bearing = East
        }


navigate : List Instruction -> Navigation
navigate =
    List.foldl do
        { ship = vessel 0 0
        , waypoint = vessel 10 -1
        }


vessel : Int -> Int -> Vessel
vessel x y =
    { x = x
    , y = y
    }


do : Instruction -> Navigation -> Navigation
do instruction ({ ship, waypoint } as nav) =
    case instruction of
        Absolute dir count ->
            { nav | waypoint = doMove dir count waypoint }

        Turn turn count ->
            { nav | waypoint = rotate turn count waypoint }

        Forward count ->
            { nav
                | ship =
                    { ship
                        | x = ship.x + (waypoint.x * count)
                        , y = ship.y + (waypoint.y * count)
                    }
            }


rotate : Turn -> Int -> Position a -> Position a
rotate turn inputCount ({ x, y } as pos) =
    let
        count =
            inputCount |> modBy 4
    in
    if count == 0 then
        pos

    else
        let
            mult =
                case turn of
                    Left ->
                        -1

                    Right ->
                        1
        in
        { pos
            | x = negate <| y * mult
            , y = x * mult
        }
            |> rotate turn (count - 1)


doForShip : Instruction -> Ship -> Ship
doForShip instruction =
    case instruction of
        Absolute dir count ->
            doMove dir count

        Turn turn count ->
            doTurn turn count

        Forward count ->
            goForward count


goForward : Int -> Ship -> Ship
goForward count ship =
    doMove ship.bearing count ship


doMove : Direction -> Int -> Position a -> Position a
doMove dir count ship =
    case dir of
        North ->
            { ship | y = ship.y - count }

        South ->
            { ship | y = ship.y + count }

        West ->
            { ship | x = ship.x - count }

        East ->
            { ship | x = ship.x + count }


doTurn : Turn -> Int -> Ship -> Ship
doTurn turn inputCount ship =
    let
        count =
            inputCount |> modBy 4
    in
    if count == 0 then
        ship

    else if turn == Left then
        doTurn Right (count * 3) ship

    else
        doTurn turn
            (count - 1)
            { ship
                | bearing =
                    case ship.bearing of
                        North ->
                            East

                        East ->
                            South

                        South ->
                            West

                        West ->
                            North
            }


fork : (a -> b) -> (a -> c) -> a -> ( b, c )
fork f g x =
    ( f x, g x )


apply : (a -> b -> c) -> ( a, b ) -> c
apply =
    Basics.Extra.uncurry



-- PARSING


parse : String -> List Instruction
parse =
    String.lines >> List.filterMap (parseInstruction >> Result.toMaybe)


parseInstruction : String -> Result (List P.DeadEnd) Instruction
parseInstruction =
    P.run <|
        orientationParser
            |= P.int
            |. P.end


orientationParser : Parser (Int -> Instruction)
orientationParser =
    charParser
        |> P.andThen
            (\c ->
                case c of
                    'N' ->
                        P.succeed <| Absolute North

                    'S' ->
                        P.succeed <| Absolute South

                    'E' ->
                        P.succeed <| Absolute East

                    'W' ->
                        P.succeed <| Absolute West

                    'L' ->
                        P.succeed <| (flip (//) 90 >> Turn Left)

                    'R' ->
                        P.succeed <| (flip (//) 90 >> Turn Right)

                    'F' ->
                        P.succeed <| Forward

                    _ ->
                        P.problem <| "Unrecognised instruction " ++ String.fromChar c
            )


charParser : Parser Char
charParser =
    P.chompIf (always True)
        |> P.getChompedString
        |> P.andThen
            (String.uncons
                >> Maybe.map (Tuple.first >> P.succeed)
                >> Maybe.withDefault (P.problem "charParser didn't chomp a char")
            )


puzzleInput =
    """N1
S4
L180
W5
S5
E2
N3
N5
E5
F25
S5
F88
L90
F8
R90
E2
L90
F72
S2
E2
F82
N1
W4
R90
W3
F29
L90
E2
L90
W5
L90
F82
R90
F36
R180
F19
W4
F40
W1
R270
E5
N3
W5
E5
L90
E4
R90
F50
E1
W2
R180
N3
W2
S4
F33
W1
L90
W3
R90
F51
R270
E3
R90
W5
S4
F72
R90
S4
E3
F66
R270
S3
E4
L90
E2
R90
F94
L90
F19
W2
L90
F68
N2
S3
F40
W3
F47
S5
W1
R90
F38
L180
F70
N2
L90
F13
R270
F57
E2
R90
N5
L90
F54
W3
S2
F27
S3
E2
S2
F29
F8
S5
F6
N4
L90
F94
W2
S5
R180
S5
F19
E3
N1
F39
N5
W3
R90
F28
L90
S1
L180
E4
N1
F16
R90
W4
S3
F24
W4
F31
E4
N5
F39
S3
F6
S2
L270
F2
R90
S5
E3
F61
S2
F12
E4
N3
R90
S5
F88
N3
E2
R90
E2
L90
W2
N3
R90
N1
L180
F8
E3
W2
F57
R90
E1
F69
N3
L90
S5
W2
L180
W4
L90
S4
L90
E3
S4
W3
F38
R90
S1
E1
F50
N5
E2
N5
L90
W2
F62
S4
R90
F41
W2
F77
L180
N5
L90
S1
W1
L90
W3
F89
N1
R90
S2
F50
S5
R180
E5
F23
E5
S2
F95
R90
F17
F13
R90
E2
S2
E2
S4
R90
W4
F19
F74
S4
R90
W5
S5
F5
N3
W5
R90
S5
F45
R270
E2
R90
W5
R90
E5
R90
E3
E4
F83
W2
F70
W1
F57
E5
S2
F95
S5
S4
E1
S5
L180
F18
N2
F55
W3
F22
W4
N3
F78
W1
F79
N3
F2
L90
W3
N5
L90
E1
L90
F80
W4
R180
W2
N4
F39
W2
F53
L180
W1
R90
F10
E1
S1
F74
N5
R90
N1
F77
L90
E2
R90
S2
R90
N4
W5
F54
S1
L90
W5
L90
N2
L90
S2
L90
S4
F16
N1
F26
R90
S4
R90
E5
L90
F83
N5
R90
S2
W4
R90
W4
L90
F90
S2
W2
F13
S3
F65
W1
L90
E5
R90
S4
E1
F49
W4
F37
W4
L180
W1
F72
S2
F28
L90
N1
E1
R90
F68
S1
W3
N1
F98
F18
E3
L180
F38
W5
L90
F39
W5
S4
L90
S2
L90
F80
W4
F75
F70
E5
N3
F21
L90
F4
W3
F11
N2
R90
F95
L90
W2
F28
E4
F53
N1
E4
L90
F22
N1
E2
N3
F48
W4
F22
E3
S1
F65
R180
F12
S3
W1
F86
E3
F81
S4
E2
F20
W4
L90
N1
F44
R90
S5
F51
W1
F81
N5
W5
S2
R90
S2
E4
R90
F65
L90
N4
F80
N5
R90
S1
L90
S4
R90
N5
F51
E2
L180
N3
R90
F16
E1
S1
L180
F62
L90
N2
F58
E1
R90
N3
L90
W2
R90
S1
E2
R180
F50
N1
W4
N1
F56
S1
W1
F93
N3
R180
S1
R90
N3
W1
S1
L90
N1
R90
E2
N5
W3
L270
N1
F9
N3
E2
S1
L180
F33
E1
F33
W5
F99
E4
F47
L180
S3
F35
N5
E4
N5
F25
R90
E5
F59
L180
F72
R180
F70
L180
S1
F83
E1
F79
R90
F36
E1
N4
F81
W2
L90
F97
R180
F72
S4
R90
F14
W2
F4
N2
N1
F88
R90
N5
F33
S3
E5
S5
E3
L90
F29
E1
F59
N3
R90
W2
F67
R90
E5
R90
N4
F77
R90
E4
S4
F43
R90
S2
W4
L180
N3
W4
L90
S4
L90
W3
N5
R90
S5
E5
R90
F4
F52
N2
F68
N1
R180
W3
S5
F91
W2
S2
L90
F36
R180
S3
L180
N1
L90
S2
E5
L90
F66
N1
E4
L90
F51
E1
S4
R180
F42
S4
L90
N1
E2
F100
R90
N4
F13
R270
W1
L90
E1
L270
W3
F86
W4
F37
W2
F57
N1
W3
F86
R90
F7
R90
S5
W5
E5
L270
W2
F39
L180
N2
F9
E4
R180
S2
F97
S1
R180
F56
W4
R90
W3
F56
L180
F3
E1
F39
N4
L90
F45
R270
F66
L90
F54
E5
L90
E4
N5
L90
S5
E2
N5
E4
F36
L180
F67
E2
N5
W3
L90
N1
E4
F44
E5
L180
W3
N5
R90
S3
R270
W2
L90
N3
L90
F46
N5
F95
E4
L90
F93
S3
W2
N1
F14
L90
F47
L90
N4
E5
F14
E5
S5
F32
W4
S5
R90
N4
F75
R90
W1
S5
N1
R90
E5
W1
F82
R180
N4
E2
S5
E4
S3
F60
L90
W2
F61
E2
L180
N2
F9
W2
L90
F20
W1
F59
S5
L180
F9
S2
S3
L90
E1
N2
W2
L90
W1
S5
F56
S4
F10
W2
N3
N5
F41
S4
L90
W2
S1
R90
F47"""
