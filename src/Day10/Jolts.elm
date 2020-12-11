module Day10.Jolts exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra
import List.Extra


solve1 : String -> Maybe Int
solve1 =
    parse
        >> differences
        >> (::) 3
        >> Dict.Extra.frequencies
        >> fork (Dict.get 1) (Dict.get 3)
        >> apply (Maybe.map2 (*))


solve2 =
    parse
        >> differences
        >> chunkWhen ((==) 1)
        >> List.map (List.length >> localChoices)
        >> List.product


differences : List Int -> List Int
differences =
    List.sort
        >> with ((::) 0)
        >> apply (List.map2 (-))


{-| Chunk contiguous elements that pass a test (discard the elements that don't)
-}
chunkWhen : (a -> Bool) -> List a -> List (List a)
chunkWhen test input =
    case List.Extra.dropWhile (not << test) input of
        [] ->
            []

        rest ->
            List.Extra.takeWhile test rest :: chunkWhen test (List.Extra.dropWhile test rest)


{-| From a set of adaptors n all 1 jolt apart, how many choices are there?
You always have to pick the rightmost because the next is 3 jolts away; so let n' be n - 1.
If n' <= 2, you can choose 0 to n'.
If n' = 3, you can choose at least 1 out of the 3.
...and in the puzzleInput there are no chunks of n > 4 so let's not think about it.
-}
localChoices : Int -> Int
localChoices n =
    n - 1 |> chooseAtLeast (max 0 <| n - 3)


{-| Number of unordered combinations of m-n elements from a set of n
In other words, the sum of n choose k where k=m to n
-}
chooseAtLeast : Int -> Int -> Int
chooseAtLeast m n =
    List.range m n
        |> List.map (flip choose n)
        |> List.sum


choose : Int -> Int -> Int
choose k n =
    factorial n // (factorial k * factorial (n - k))


factorial : Int -> Int
factorial =
    List.range 1 >> List.product


fork : (a -> b) -> (a -> c) -> a -> ( b, c )
fork f g x =
    ( f x, g x )


with : (a -> b) -> a -> ( a, b )
with =
    fork identity


apply : (a -> b -> c) -> ( a, b ) -> c
apply =
    -- I hate this name
    Basics.Extra.uncurry


parse : String -> List Int
parse =
    String.lines >> List.filterMap String.toInt


puzzleInput =
    """133
157
39
74
108
136
92
55
86
46
111
58
80
115
84
67
98
30
40
61
71
114
17
9
123
142
49
158
107
139
104
132
155
96
91
15
11
23
54
6
63
126
3
10
116
87
68
72
109
62
134
103
1
16
101
117
35
120
151
102
85
145
135
79
2
147
33
41
93
52
48
64
81
29
20
110
129
43
148
36
53
26
42
156
154
77
88
73
27
34
12
146
78
47
28
97"""
