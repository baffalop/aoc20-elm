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
    , initialSeating : Seating
    }


type Mode
    = Neighbours
    | Sightlines


type State
    = NotStarted
    | Playing
    | Paused
    | Done
    | Editing EditMode String


init =
    let
        seating =
            Seating.puzzleInput |> Seating.parse
    in
    { seating = seating
    , mode = Neighbours
    , state = NotStarted
    , initialSeating = seating
    }
        |> withNoCmd


type Msg
    = StartWith Mode
    | Pause
    | Play
    | Reset
    | Tick
    | Edit EditMode
    | Input String
    | Submit


type EditMode
    = New
    | Tweak


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
            case model.state of
                Editing Tweak _ ->
                    { model | state = Paused }
                        |> withNoCmd

                _ ->
                    { model
                        | state = NotStarted
                        , seating = model.initialSeating
                    }
                        |> withNoCmd

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

        Edit mode ->
            { model | state = Editing mode <| printSeating editTile model.seating }
                |> withNoCmd

        Input value ->
            case model.state of
                Editing mode _ ->
                    { model | state = Editing mode value }
                        |> withNoCmd

                _ ->
                    noOp

        Submit ->
            case model.state of
                Editing mode value ->
                    let
                        newSeating =
                            Seating.parse value
                    in
                    { model
                        | seating = newSeating
                        , state =
                            case mode of
                                New ->
                                    NotStarted

                                Tweak ->
                                    Paused
                        , initialSeating =
                            case mode of
                                New ->
                                    newSeating

                                Tweak ->
                                    model.initialSeating
                    }
                        |> withNoCmd

                _ ->
                    noOp



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
            , viewMain state seating
            ]


viewMain state seating =
    Element.column
        [ Element.width Element.shrink
        , Element.height Element.shrink
        , Element.spacing 12
        ]
        [ Element.el
            [ Element.padding 10
            , Element.Background.color <| rgb 35 34 49
            , fontColor
            , Element.Font.size 8
            , Element.Font.family [ Element.Font.monospace ]
            ]
          <|
            case state of
                Editing _ value ->
                    viewInput value

                _ ->
                    printSeating uiTile seating |> Element.text
        , Element.row [ Element.spacing 40 ]
            [ Element.link linkStyles
                { url = "https://github.com/baffalop/aoc20-elm/tree/master/src/Day11"
                , label = Element.text "Source"
                }
            ]
        ]


viewInput : String -> Element Msg
viewInput value =
    Element.Input.multiline
        [ Element.Background.color <| rgb 35 34 49
        , Element.width <| Element.px 980
        , Element.height <| Element.px 720
        , Element.Font.color <| rgb 188 192 78
        , Element.Font.size 15
        , Element.Border.width 3
        , Element.Border.color <| rgb 52 45 70
        , Element.focused [ Element.Border.color <| rgb 69 60 105 ]
        ]
        { onChange = Input
        , text = value
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Puzzle input"
        , spellcheck = False
        }


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
                , button "Edit" <| Edit New
                ]

            Playing ->
                [ button "Pause" Pause
                , button "Reset" Reset
                ]

            Paused ->
                [ button "Reset" Reset
                , button "Play" Play
                , button "Edit" <| Edit Tweak
                ]

            Done ->
                [ button "Reset" Reset ]

            Editing _ _ ->
                [ button "Submit" Submit
                , button "Cancel" Reset
                ]


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


linkStyles =
    [ Element.Font.size 15
    , Element.Font.color <| rgb 168 193 218
    , helvetica
    , Element.Font.underline
    , Element.focused []
    ]



-- SEATING


printSeating : (Seating.Tile -> String) -> Seating -> String
printSeating printer seating =
    List.range 0 (Matrix.height seating - 1)
        |> List.filterMap (flip Matrix.getRow seating >> Result.toMaybe >> Maybe.map (printRow printer))
        |> String.join "\n"


printRow : (Seating.Tile -> String) -> Array Seating.Tile -> String
printRow printer =
    Array.foldl (printer >> (++)) ""


uiTile : Seating.Tile -> String
uiTile tile =
    case tile of
        Floor ->
            "▪️"

        Seat ->
            "\u{1FA91}"

        Person ->
            "\u{200D}🙎"


editTile : Seating.Tile -> String
editTile tile =
    case tile of
        Floor ->
            "."

        Seat ->
            "L"

        Person ->
            "#"



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
