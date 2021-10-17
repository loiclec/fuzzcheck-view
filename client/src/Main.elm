module Main exposing (main)

-- import Element.Events as Events

import API
import Array exposing (Array)
import Browser
import Coverage as C
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import FileSelect
import Html exposing (Html)
import Http
import Json.Decode as D
import MainModel exposing (..)
import Style exposing (..)
import Url
import Url.Builder as UrlB



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = FetchFiles
    | GotFiles (Result Http.Error (Array String))
    | FileSelectMsg FileSelect.Msg
    | FetchCodeBlocks
    | GotCodeBlocks (Result Http.Error (List C.CodeBlock))
    | HoverCounterId (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCodeBlocks (Ok blocks) ->
            ( { model | blocks = blocks }, Cmd.none )

        GotCodeBlocks (Err _) ->
            ( model, Cmd.none )

        FetchFiles ->
            ( model, Http.get { url = API.getListOfFiles, expect = Http.expectJson GotFiles (D.array D.string) } )

        FileSelectMsg m ->
            let
                files =
                    FileSelect.update m (FileSelect.Model model.all_files model.selected_file)
            in
            let
                newModel =
                    { model | all_files = files.all_files, selected_file = files.selected_file }
            in
            ( newModel
            , let
                optreq =
                    API.getCodeBlocksUrl newModel
              in
              case optreq of
                Just req ->
                    Http.get
                        { url = req
                        , expect = Http.expectJson GotCodeBlocks (D.list C.decodeCodeBlock)
                        }

                Nothing ->
                    Cmd.none
            )

        GotFiles (Ok files) ->
            ( { model | all_files = files }, Cmd.none )

        GotFiles (Err _) ->
            ( model, Cmd.none )

        FetchCodeBlocks ->
            ( model
            , let
                optreq =
                    API.getCodeBlocksUrl model
              in
              case optreq of
                Just req ->
                    Http.get
                        { url = req
                        , expect = Http.expectJson GotCodeBlocks (D.list C.decodeCodeBlock)
                        }

                Nothing ->
                    Cmd.none
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
    E.row [ E.width E.fill, E.alignTop, E.spacing normalSpacing, E.paddingEach { top = 10, right = 100, bottom = 100, left = 100 } ]
        [ E.column [ E.alignTop, E.spacing normalSpacing, E.width (E.px 150) ]
            [ getDataButton (Just FetchFiles) "load files"
            , getDataButton (Just FetchCodeBlocks) "load data"
            ]
        , E.column
            [ E.alignTop, E.width (E.px 800), E.spacing largeSpacing ]
            (E.map FileSelectMsg
                (E.el [ E.scrollbars, E.height (E.px 100) ]
                    (FileSelect.view
                        { all_files = model.all_files, selected_file = model.selected_file }
                    )
                )
                :: List.map
                    (\block -> codeBlockWrapper (\id -> HoverCounterId id) block model.counter_id)
                    model.blocks
            )
        ]


getDataButton : Maybe msg -> String -> E.Element msg
getDataButton attr title =
    EI.button
        [ E.width E.fill
        , Border.width 2
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
