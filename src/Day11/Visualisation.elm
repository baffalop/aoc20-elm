module Day11.Visualisation exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Browser
import Browser.Dom
import Day11.Seating as Seating exposing (Seating, Tile(..))
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Matrix
import Task
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
    = NoOp
    | StartWith Mode
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
        NoOp ->
            noOp

        StartWith mode ->
            { model | mode = mode, state = Playing }
                |> withCmd (focus ids.pause)

        Pause ->
            { model | state = Paused }
                |> withCmd (focus ids.play)

        Play ->
            { model | state = Playing }
                |> withCmd (focus ids.pause)

        Reset ->
            case model.state of
                Editing Tweak _ ->
                    { model | state = Paused }
                        |> withCmd (focus ids.play)

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
                |> withCmd (focus ids.input)

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

                        { newState, initialSeating, focusId } =
                            case mode of
                                New ->
                                    { newState = NotStarted
                                    , initialSeating = newSeating
                                    , focusId = ids.part1
                                    }

                                Tweak ->
                                    { newState = Paused
                                    , initialSeating = model.initialSeating
                                    , focusId = ids.play
                                    }
                    in
                    { model
                        | seating = newSeating
                        , state = newState
                        , initialSeating = initialSeating
                    }
                        |> withCmd (focus focusId)

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
        , Element.paragraph linkStyles
            [ Element.link []
                { url = "https://adventofcode.com/2020/day/11"
                , label = Element.text "based on an Advent of Code challenge"
                }
            , Element.link (Element.alignRight :: linkStyles)
                { url = "https://github.com/baffalop/aoc20-elm/tree/master/src/Day11"
                , label = Element.text "source"
                }
            ]
        ]


viewInput : String -> Element Msg
viewInput value =
    Element.Input.multiline
        [ Element.Background.color <| rgb 35 34 49
        , Element.htmlAttribute <| Html.Attributes.id ids.input
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
                [ button Primary "Start Part 1" ids.part1 <| StartWith Neighbours
                , button Primary "Start Part 2" ids.part2 <| StartWith Sightlines
                , button Secondary "Edit" ids.edit <| Edit New
                ]

            Playing ->
                [ button Primary "Pause" ids.pause Pause
                , button Secondary "Reset" ids.play Reset
                ]

            Paused ->
                [ button Secondary "Reset" ids.reset Reset
                , button Primary "Play" ids.play Play
                , button Secondary "Edit" ids.edit <| Edit Tweak
                ]

            Done ->
                [ button Primary "Reset" ids.reset Reset ]

            Editing _ _ ->
                [ button Primary "Submit" ids.submit Submit
                , button Secondary "Cancel" ids.cancel Reset
                ]


type ButtonStyle
    = Primary
    | Secondary


button : ButtonStyle -> String -> String -> msg -> Element msg
button style label id onPress =
    Element.Input.button
        [ Element.htmlAttribute <| Html.Attributes.id id
        , Element.width <| Element.px 110
        , Element.Background.color <|
            case style of
                Primary ->
                    rgb 181 93 60

                Secondary ->
                    rgb 33 75 109
        , Element.Border.rounded 4
        , Element.padding 10
        , Element.Font.size 18
        , fontColor
        , helvetica
        , Element.Font.center
        , Element.mouseOver
            [ Element.Background.color <|
                case style of
                    Primary ->
                        rgb 201 110 76

                    Secondary ->
                        rgb 45 96 138
            ]
        ]
        { onPress = Just onPress
        , label = Element.text label
        }


linkStyles =
    [ Element.Font.size 15
    , Element.Font.color <| rgb 126 147 169
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
    Array.foldl (printer >> flip (++)) ""


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


ids =
    { input = "input"
    , play = "play"
    , pause = "pause"
    , reset = "reset"
    , part1 = "part1"
    , part2 = "part2"
    , edit = "edit"
    , submit = "submit"
    , cancel = "cancel"
    }



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


focus : String -> Cmd Msg
focus id =
    Browser.Dom.focus id |> Task.attempt (always NoOp)


withCmd : Cmd msg -> Model -> ( Model, Cmd msg )
withCmd cmd model =
    ( model, cmd )


withNoCmd : Model -> ( Model, Cmd msg )
withNoCmd =
    withCmd Cmd.none
