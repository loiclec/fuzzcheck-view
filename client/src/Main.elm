module Main exposing (main)

-- import Element.Events as Events

import API
import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Coverage as C exposing (CodeBlock, Msg(..))
import Element as E exposing (layout)
import Element.Background as Background
import Element.Font as Font
import Helpers
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Json.Decode as D
import Layout exposing (Layout)
import ListSelect
import MainModel exposing (..)
import Style exposing (..)
import Task



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
    ( emptyModel, Cmd.batch [ Task.perform (\vp -> Resize (round vp.viewport.width) (round vp.viewport.height)) Browser.Dom.getViewport, Http.get { url = API.getListOfFiles, expect = Http.expectJson GotFiles (D.array D.string) } ] )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onKeyDown keyDecoder, Browser.Events.onResize Resize ]


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map
        (\key ->
            case key of
                "a" ->
                    PreviousFile

                "s" ->
                    NextFile

                "k" ->
                    PreviousFunction

                "l" ->
                    NextFunction

                _ ->
                    NoMsg
        )
        (D.field "key" D.string)



-- UPDATE


type Msg
    = Resize Int Int
    | PreviousFile
    | NextFile
    | PreviousFunction
    | NextFunction
    | NoMsg
    | FetchFiles
    | GotFiles (Result Http.Error (Array String))
    | GotFunctions (Result Http.Error (Array String))
    | FileSelect ListSelect.Msg
    | FunctionSelect ListSelect.Msg
    | FetchCodeBlock
    | GotCodeBlock (Result Http.Error C.CodeBlock)
    | HoverCounterId (Maybe Int)
    | GotBestInputForCounterId (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        Resize width height ->
            ( { model | layout = Layout.layoutForWidthAndHeight width height }, Cmd.none )

        PreviousFile ->
            let
                newModel =
                    { model
                        | selected_file = Helpers.prevInt model.selected_file
                    }
            in
            ( newModel
            , API.getListOfBlocksCmd GotFunctions newModel
            )

        NextFile ->
            let
                newModel =
                    { model
                        | selected_file = Helpers.nextInt (Array.length model.all_files) model.selected_file
                    }
            in
            ( newModel
            , API.getListOfBlocksCmd GotFunctions newModel
            )

        PreviousFunction ->
            let
                newModel =
                    { model
                        | selected_block = Helpers.prevInt model.selected_block
                    }
            in
            ( newModel
            , API.getCodeBlockCmd GotCodeBlock newModel
            )

        NextFunction ->
            let
                newModel =
                    { model
                        | selected_block = Helpers.nextInt (Array.length model.all_blocks) model.selected_block
                    }
            in
            ( newModel
            , API.getCodeBlockCmd GotCodeBlock newModel
            )

        GotCodeBlock (Ok block) ->
            ( { model | block = Just block }, Cmd.none )

        GotCodeBlock (Err _) ->
            ( { model | block = Nothing }, Cmd.none )

        FetchFiles ->
            ( model, Http.get { url = API.getListOfFiles, expect = Http.expectJson GotFiles (D.array D.string) } )

        GotFunctions (Ok functions) ->
            let
                newModel =
                    { model | all_blocks = functions, selected_block = Just 0 }
            in
            ( newModel, API.getCodeBlockCmd GotCodeBlock newModel )

        GotFunctions (Err _) ->
            ( { model | all_blocks = Array.empty }, Cmd.none )

        FileSelect m ->
            let
                files =
                    ListSelect.update m (MainModel.fileSelectModel model)
            in
            let
                newModel =
                    { model | all_files = files.all_items, selected_file = files.selected_item }
            in
            ( newModel
            , API.getListOfBlocksCmd GotFunctions newModel
            )

        FunctionSelect m ->
            let
                functions =
                    ListSelect.update m (MainModel.functionSelectModel model)
            in
            let
                newModel =
                    { model | selected_block = functions.selected_item }
            in
            ( newModel
            , API.getCodeBlockCmd GotCodeBlock newModel
            )

        GotFiles (Ok files) ->
            let
                newModel =
                    { model | all_files = files, selected_file = Just 0 }
            in
            ( newModel, API.getListOfBlocksCmd GotFunctions newModel )

        GotFiles (Err _) ->
            ( model, Cmd.none )

        FetchCodeBlock ->
            ( model
            , API.getCodeBlockCmd GotCodeBlock model
            )

        HoverCounterId optid ->
            case optid of
                Just id ->
                    ( { model | counter_id = optid, best_input = Nothing }
                    , Http.get
                        { url = API.getBestInputForCounter id
                        , expect = Http.expectJson GotBestInputForCounterId D.string
                        }
                    )

                Nothing ->
                    ( { model | counter_id = optid }, Cmd.none )

        GotBestInputForCounterId (Ok text) ->
            ( { model | best_input = Just text }, Cmd.none )

        GotBestInputForCounterId (Err _) ->
            ( { model | best_input = Just "error" }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    E.layout [ Background.color Style.bgDark ]
        (mainView model)


mainView : Model -> E.Element Msg
mainView model =
    E.column [ E.alignTop, E.width E.fill, E.spacing largeSpacing, E.padding model.layout.padding, E.height (E.shrink |> E.minimum (model.layout.height + 200)) ]
        [ E.row [ E.alignTop, E.width E.fill, E.spacing model.layout.column_sep ]
            [ E.column [ E.spacing normalSpacing, E.alignTop, E.width (E.px model.layout.column_width) ]
                [ E.row [ E.padding normalSpacing, E.width E.fill, Background.color fg, Font.family codeFontFamily, Font.color bgCode, Font.size largeFontSize ]
                    [ E.el [ E.alignLeft ] (E.text "Files"), E.el [ Font.size smallFontSize, E.alignRight ] (E.text "'a': previous    's': next") ]
                , E.el [ E.height (E.shrink |> E.maximum 140), E.width (E.fill |> E.minimum model.layout.column_width) ]
                    (E.map FileSelect
                        (ListSelect.view
                            (MainModel.fileSelectModel model)
                        )
                    )
                ]
            , E.column [ E.spacing normalSpacing, E.alignTop, E.width (E.px model.layout.column_width) ]
                [ E.row [ E.padding normalSpacing, E.width E.fill, Background.color fg, Font.family codeFontFamily, Font.color bgCode, Font.size largeFontSize ]
                    [ E.el [ E.alignLeft ] (E.text "Functions"), E.el [ Font.size smallFontSize, E.alignRight ] (E.text "'a': previous    's': next") ]
                , E.el [ E.height (E.shrink |> E.maximum 140), E.width (E.px model.layout.column_width) ]
                    (E.map FunctionSelect
                        (ListSelect.view
                            (MainModel.functionSelectModel
                                model
                            )
                        )
                    )
                ]
            ]
        , E.row [ E.alignTop, E.width E.fill, E.alignTop, E.spacing largeSpacing ]
            [ E.el [ E.alignTop, E.width E.fill ]
                (Maybe.withDefault
                    E.none
                    (Maybe.map (\block -> codeBlockWrapper { block = block, layout = model.layout, focused_id = model.counter_id }) model.block)
                )
            , E.paragraph [ E.alignTop, E.htmlAttribute (HA.style "position" "sticky"), E.htmlAttribute (HA.style "position" "-webkit-sticky"), E.htmlAttribute (HA.style "right" "0"), E.htmlAttribute (HA.style "top" "10px"), E.alignRight, E.alignTop, E.width E.fill, E.padding largeSpacing, Background.color fg, Font.color bgCode, Font.family codeFontFamily, Font.size largeFontSize, E.spacing normalSpacing ]
                [ E.text
                    (case model.best_input of
                        Just text ->
                            text

                        Nothing ->
                            ""
                    )
                ]
            ]
        , E.el [ E.height E.fill, E.width E.fill ] E.none
        ]


codeBlockWrapper : { a | block : CodeBlock, layout : Layout, focused_id : Maybe Int } -> E.Element Msg
codeBlockWrapper { block, layout, focused_id } =
    E.map
        (\msg ->
            case msg of
                SelectCounter id ->
                    HoverCounterId (Just id)

                UnselectCounter ->
                    HoverCounterId Nothing
        )
        (E.el
            [ E.width E.fill
            , E.alignTop
            ]
            (C.viewCodeBlock { block = block, layout = layout, focused_id = focused_id })
        )
