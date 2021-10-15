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
import Style exposing (..)



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
    { blocks : List C.CodeBlock
    , counter_id : Maybe Int
    }


emptyModel : Model
emptyModel =
    Model [] Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = Fetch
    | GotData (Result Http.Error (List C.CodeBlock))
    | HoverCounterId (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok blocks) ->
            ( { model | blocks = blocks }, Cmd.none )

        GotData (Err _) ->
            ( model, Cmd.none )

        Fetch ->
            ( model
            , Http.get
                { url = "code_blocks"
                , expect = Http.expectJson GotData (D.list C.decodeCodeBlock)
                }
            )

        HoverCounterId id ->
            ( { model | counter_id = id }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    E.layout [ Background.color Style.bgDark ]
        (mainView model)


mainView : Model -> E.Element Msg
mainView model =
    E.row [ E.alignTop, E.width E.fill, E.spacing normalSpacing, E.paddingEach { top = 10, right = 100, bottom = 100, left = 100 } ]
        [ getDataButton (Just Fetch) "load data"
        , E.column
            [ E.width E.fill, E.centerX, E.spacing largeSpacing ]
            (List.map
                (\block -> codeBlockWrapper (\id -> HoverCounterId id) block model.counter_id)
                model.blocks
            )
        ]


getDataButton : Maybe msg -> String -> E.Element msg
getDataButton attr title =
    EI.button
        [ Border.width 2
        , Border.color actionColor
        , Background.color
            actionColor
        , Font.color fg
        , E.alignTop
        , E.paddingXY largeSpacing normalSpacing
        , E.mouseDown [ Background.color actionPressColor ]
        , E.mouseOver [ Background.color actionHoverColor ]
        , E.focused []
        ]
        { onPress = attr
        , label = E.text title
        }


codeBlockWrapper : (Maybe Int -> msg) -> C.CodeBlock -> Maybe Int -> E.Element msg
codeBlockWrapper msg block focused_id =
    E.el
        [ E.width E.fill
        , E.alignTop
        ]
        (C.viewCodeBlock msg block focused_id)
