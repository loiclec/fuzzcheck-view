module Main exposing (main)

-- import Element.Events as Events

import API
import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Coverage as C exposing (FunctionCoverage, FunctionName, Msg(..))
import Element as E exposing (layout)
import Element.Background as Background
import Element.Font as Font
import Filters
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
    let
        model =
            emptyModel
    in
    ( model
    , Cmd.batch
        [ Task.perform (\vp -> Resize (round vp.viewport.width) (round vp.viewport.height)) Browser.Dom.getViewport
        , API.getFilesAndFunctionsCmd GotFunctions model
        , API.getListOfInputsCmd GotInputs
        ]
    )



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
    | FetchFunctions
    | GotFunctions (Result Http.Error (Array ( String, Array FunctionName )))
    | FileSelect ListSelect.Msg
    | FunctionSelect ListSelect.Msg
    | FetchCodeBlock
    | GotCodeBlock (Result Http.Error C.FunctionCoverage)
    | HoverCounterId (Maybe Int)
    | GotBestInputForCounterId (Result Http.Error String)
    | FetchInputs
    | GotInputs (Result Http.Error (Array InputInfo))
    | FetchInput String
    | GotPreviewInput String (Result Http.Error String)


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
            , Cmd.none
            )

        NextFile ->
            let
                newModel =
                    { model
                        | selected_file = Helpers.nextInt (Array.length model.all_files) model.selected_file
                    }
            in
            ( newModel
            , Cmd.none
            )

        PreviousFunction ->
            let
                newModel =
                    { model
                        | selected_function = Helpers.prevInt model.selected_function
                    }
            in
            ( newModel
            , API.getCoverageCmd GotCodeBlock newModel
            )

        NextFunction ->
            let
                newModel =
                    { model
                        | selected_function = Helpers.nextInt (Array.length model.all_files) model.selected_function
                    }
            in
            ( newModel
            , API.getCoverageCmd GotCodeBlock newModel
            )

        GotCodeBlock (Ok block) ->
            ( { model | block = Just block }, Cmd.none )

        GotCodeBlock (Err _) ->
            ( { model | block = Nothing }, Cmd.none )

        FetchFunctions ->
            ( model, Cmd.none )

        GotFunctions (Ok functions) ->
            let
                newModel =
                    { model | all_files = functions, selected_file = 0, selected_function = 0 }
            in
            ( newModel, API.getCoverageCmd GotCodeBlock newModel )

        GotFunctions (Err _) ->
            ( { model | all_files = Array.empty }, Cmd.none )

        FileSelect m ->
            let
                files =
                    ListSelect.update m (MainModel.fileSelectModel model)
            in
            let
                newModel =
                    { model | selected_file = files.selected_item }
            in
            ( newModel
            , Cmd.none
            )

        FunctionSelect m ->
            let
                functions =
                    ListSelect.update m (MainModel.functionSelectModel model)
            in
            let
                newModel =
                    { model | selected_function = functions.selected_item }
            in
            ( newModel
            , API.getCoverageCmd GotCodeBlock newModel
            )

        FetchCodeBlock ->
            ( model
            , API.getCoverageCmd GotCodeBlock model
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

        GotBestInputForCounterId (Ok name) ->
            ( { model | best_input = Just name }, API.getInputCmd (GotPreviewInput name) name )

        GotBestInputForCounterId (Err _) ->
            ( { model | best_input = Nothing }, Cmd.none )

        FetchInput name ->
            ( model, API.getInputCmd (GotPreviewInput name) name )

        GotPreviewInput name (Ok content) ->
            ( { model | previewed_input = Just ( name, content ) }, Cmd.none )

        GotPreviewInput _ (Err _) ->
            ( model, Cmd.none )

        FetchInputs ->
            ( model, API.getListOfInputsCmd GotInputs )

        GotInputs res ->
            case res of
                Ok inputs ->
                    ( { model | all_inputs = inputs, selected_input = 0 }, Cmd.none )

                Err _ ->
                    ( { model | all_inputs = Array.empty, selected_input = 0 }, Cmd.none )



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
                    [ E.el [ E.alignLeft ] (E.text "Functions"), E.el [ Font.size smallFontSize, E.alignRight ] (E.text "'k': previous    'l': next") ]
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
        , E.row [ E.alignTop, E.width E.fill, E.spacing model.layout.column_sep ]
            [ E.column [ E.spacing normalSpacing, E.alignTop, E.width (E.px model.layout.column_width) ]
                [ E.row [ E.padding normalSpacing, E.width E.fill, Background.color fg, Font.family codeFontFamily, Font.color bgCode, Font.size largeFontSize ]
                    [ E.el [ E.alignLeft ] (E.text "Filters") ]
                , E.el [ E.height (E.shrink |> E.maximum 140), E.width (E.px model.layout.column_width) ]
                    (E.map
                        (\m ->
                            case m of
                                Filters.ShowAllCoverage ->
                                    NoMsg

                                Filters.ShowInputCoverage _ ->
                                    NoMsg
                        )
                        (Filters.view
                            model
                        )
                    )
                ]
            , E.column [ E.spacing normalSpacing, E.alignTop, E.width (E.px model.layout.column_width) ]
                [ E.row [ E.padding normalSpacing, E.width E.fill, Background.color fg, Font.family codeFontFamily, Font.color bgCode, Font.size largeFontSize ]
                    [ E.el [ E.alignLeft ] (E.text "Inputs") ]
                , E.el [ E.height (E.shrink |> E.maximum 140), E.width (E.px model.layout.column_width) ]
                    (E.map
                        (\x ->
                            case x of
                                ListSelect.Select i ->
                                    NoMsg

                                ListSelect.UnSelect ->
                                    NoMsg

                                ListSelect.Hover i ->
                                    case Array.get i model.all_inputs of
                                        Just name ->
                                            FetchInput name.hash

                                        Nothing ->
                                            NoMsg

                                ListSelect.UnHover ->
                                    NoMsg
                        )
                        (ListSelect.view
                            { all_items = Array.map .hash model.all_inputs, selected_item = model.selected_input }
                        )
                    )
                ]
            ]
        , E.row [ E.alignTop, E.width E.fill, E.spacing model.layout.column_sep ]
            [ E.el [ E.alignTop, E.width (E.px model.layout.column_width) ]
                (Maybe.withDefault
                    E.none
                    (Maybe.map (\block -> codeBlockWrapper { block = block, layout = model.layout, focused_id = model.counter_id }) model.block)
                )
            , case model.previewed_input of
                Just ( name, text ) ->
                    E.column [ E.alignTop, E.alignTop, E.htmlAttribute (HA.style "position" "sticky"), E.htmlAttribute (HA.style "position" "-webkit-sticky"), E.htmlAttribute (HA.style "right" (String.fromInt model.layout.padding ++ "px")), E.htmlAttribute (HA.style "top" "10px"), E.width (E.px model.layout.column_width) ]
                        [ E.paragraph [ E.padding largeSpacing, Background.color fg, Font.color bgCode, Font.family codeFontFamily, Font.size largeFontSize, E.spacing normalSpacing, E.htmlAttribute (HA.style "white-space" "pre") ]
                            [ E.html (Html.text (name ++ ":\n\n" ++ text))
                            ]
                        ]

                Nothing ->
                    E.none
            ]
        , E.el [ E.height E.fill, E.width E.fill ] E.none
        ]


codeBlockWrapper : { a | block : FunctionCoverage, layout : Layout, focused_id : Maybe Int } -> E.Element Msg
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
