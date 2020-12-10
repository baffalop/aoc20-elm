module Day10.Jolts exposing (..)

import Dict exposing (Dict)


solve1 : String -> Maybe Int
solve1 =
    parse
        >> List.sort
        >> differences
        >> (::) 3
        >> counts
        >> (\c -> Maybe.map2 (*) (Dict.get 1 c) (Dict.get 3 c))


differences : List Int -> List Int
differences input =
    List.map2 (-) input (0 :: input)


counts : List Int -> Dict Int Int
counts =
    List.foldl
        (\n -> Dict.update n (Maybe.withDefault 0 >> (+) n >> Just))
        Dict.empty


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
