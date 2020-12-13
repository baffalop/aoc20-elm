module Day11.Visualisation exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Browser
import Day11.Seating as Seating exposing (Seating, Tile(..))
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Matrix
import Time


main =
    Browser.document
        { init = \() -> init
        , view =
            \model ->
                { title = "Seating System"
                , body = List.singleton <| view model
                }
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { seating : Seating
    , mode : Mode
    , state : State
    }


type Mode
    = Neighbours
    | Sightlines


type State
    = NotStarted
    | Playing
    | Paused
    | Done
    | Editing String


init =
    { seating = initialSeating
    , mode = Neighbours
    , state = NotStarted
    }
        |> withNoCmd


initialSeating : Seating
initialSeating =
    Seating.puzzleInput |> Seating.parse


type Msg
    = StartWith Mode
    | Pause
    | Play
    | Reset
    | Tick
    | LaunchForm


subscriptions : Model -> Sub Msg
subscriptions { state } =
    if state == Playing then
        Time.every 200 <| always Tick

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            model |> withNoCmd
    in
    case msg of
        StartWith mode ->
            { model | mode = mode, state = Playing }
                |> withNoCmd

        Pause ->
            { model | state = Paused }
                |> withNoCmd

        Play ->
            { model | state = Playing }
                |> withNoCmd

        Reset ->
            init

        Tick ->
            if model.state /= Playing then
                noOp

            else
                let
                    advance =
                        case model.mode of
                            Neighbours ->
                                Seating.advanceByNeighbours

                            Sightlines ->
                                Seating.advanceBySightlines

                    next =
                        advance model.seating
                in
                { model
                    | seating = next
                    , state =
                        if next == model.seating then
                            Done

                        else
                            Playing
                }
                    |> withNoCmd

        LaunchForm ->
            Debug.todo "form"



-- VIEW


view : Model -> Html Msg
view { seating, mode, state } =
    Element.layout
        [ Element.centerX
        , Element.centerY
        , Element.Background.color <| rgb 15 15 35
        ]
    <|
        Element.column
            [ Element.centerX
            , Element.centerY
            , Element.width Element.shrink
            , Element.height Element.shrink
            , Element.spacing 23
            ]
            [ viewButtons state
            , case state of
                Editing value ->
                    viewInput value

                _ ->
                    Element.column
                        [ Element.width Element.shrink
                        , Element.height Element.shrink
                        , Element.spacing 12
                        ]
                        [ viewSeating seating
                        , Element.row
                            [ Element.spacing 30
                            ]
                            [ linkButton [] "Enter your own data" LaunchForm
                            , link [ Element.alignRight ]
                                "Source"
                                "https://github.com/baffalop/aoc20-elm/tree/master/src/Day11"
                            ]
                        ]
            ]


viewInput : String -> Element msg
viewInput value =
    Debug.todo "input"


viewButtons : State -> Element Msg
viewButtons state =
    Element.row
        [ Element.spacing 15
        , Element.centerX
        ]
    <|
        case state of
            NotStarted ->
                [ button "Start Part 1" <| StartWith Neighbours
                , button "Start Part 2" <| StartWith Sightlines
                ]

            Playing ->
                [ button "Pause" Pause
                , button "Reset" Reset
                ]

            Paused ->
                [ button "Play" Play
                , button "Reset" Reset
                ]

            Done ->
                [ button "Reset" Reset ]

            Editing _ ->
                [ button "Submit" Reset ]


button : String -> msg -> Element msg
button label onPress =
    Element.Input.button
        [ Element.width <| Element.px 110
        , Element.Background.color <| rgb 33 75 109
        , Element.Border.rounded 4
        , Element.padding 10
        , Element.Font.size 18
        , fontColor
        , helvetica
        , Element.Font.center
        , Element.mouseOver
            [ Element.Background.color <| rgb 45 96 138
            ]
        ]
        { onPress = Just onPress
        , label = Element.text label
        }


linkButton : List (Element.Attribute msg) -> String -> msg -> Element msg
linkButton attr text onPress =
    Element.Input.button
        (linkStyles ++ attr)
        { onPress = Just onPress
        , label = Element.text text
        }


link : List (Element.Attribute msg) -> String -> String -> Element msg
link attr text url =
    Element.link (linkStyles ++ attr)
        { url = url
        , label = Element.text text
        }


linkStyles =
    [ Element.Font.size 15
    , Element.Font.color <| rgb 168 193 218
    , helvetica
    , Element.Font.underline
    , Element.focused []
    ]



-- SEATING


viewSeating : Seating -> Element msg
viewSeating seating =
    printSeating seating
        |> Element.text
        |> Element.el
            [ Element.padding 10
            , Element.Background.color <| rgb 35 34 49
            , Element.Font.size 8
            , Element.Font.family [ Element.Font.monospace ]
            ]


printSeating : Seating -> String
printSeating seating =
    List.range 0 (Matrix.height seating - 1)
        |> List.filterMap (flip Matrix.getRow seating >> Result.toMaybe >> Maybe.map printRow)
        |> String.join "\n"


printRow : Array Seating.Tile -> String
printRow =
    Array.foldl (printTile >> (++)) ""


printTile : Seating.Tile -> String
printTile tile =
    case tile of
        Floor ->
            "▪️"

        Seat ->
            "\u{1FA91}"

        Person ->
            "\u{200D}🙎"



-- HELPERS


fontColor : Element.Attribute msg
fontColor =
    Element.Font.color <| rgb 225 240 255


helvetica : Element.Attribute msg
helvetica =
    Element.Font.family [ Element.Font.typeface "Helvetica", Element.Font.sansSerif ]


rgb : Int -> Int -> Int -> Element.Color
rgb r g b =
    Element.fromRgb255
        { red = r
        , green = g
        , blue = b
        , alpha = 1
        }


withCmd : Cmd msg -> Model -> ( Model, Cmd msg )
withCmd cmd model =
    ( model, cmd )


withNoCmd : Model -> ( Model, Cmd msg )
withNoCmd =
    withCmd Cmd.none
