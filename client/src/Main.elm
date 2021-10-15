module Main exposing (main)

-- import Element.Events as Events

import Browser
import Coverage as C
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Html exposing (Html)
import Http
import Json.Decode as D



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    List C.CodeBlock


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = Fetch
    | GotData (Result Http.Error (List C.CodeBlock))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok block) ->
            ( block, Cmd.none )

        GotData (Err _) ->
            ( [], Cmd.none )

        Fetch ->
            ( model
            , Http.get
                { url = "code_blocks"
                , expect = Http.expectJson GotData (D.list C.decodeCodeBlock)
                }
            )



-- VIEW


view : Model -> Html Msg
view model =
    E.layout [ Background.color (E.rgb255 0x1A 0x1F 0x29) ]
        (mainView model)


mainView : Model -> E.Element Msg
mainView model =
    E.row [ E.alignTop, E.width E.fill, E.spacing 10, E.paddingEach { top = 10, right = 100, bottom = 100, left = 100 } ]
        [ getDataButton (Just Fetch) "load data"
        , E.column [ E.width E.fill, E.centerX, E.spacing 10 ] (List.map codeBlockWrapper model)
        ]


getDataButton : Maybe msg -> String -> E.Element msg
getDataButton attr title =
    EI.button
        [ Border.rounded 3
        , Border.width 2
        , Border.color (E.rgb255 0x69 0x53 0x80)
        , Background.color
            (E.rgb255 0x69 0x53 0x80)
        , Font.color (E.rgb255 0xCC 0xCA 0xC2)
        , E.alignTop
        , E.paddingXY 16 6
        , E.mouseDown [ Background.color (E.rgb255 0x49 0x33 0x60) ]
        , E.mouseOver [ Background.color (E.rgb255 0x59 0x43 0x70) ]
        , E.focused []
        ]
        { onPress = attr
        , label = E.text title
        }


codeBlockWrapper : C.CodeBlock -> E.Element msg
codeBlockWrapper block =
    E.el
        [ E.width E.fill
        , E.alignTop
        ]
        (C.viewCodeBlock block)
