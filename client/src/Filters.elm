module Filters exposing (..)

import Array exposing (Array)
import Element as E
import Element.Input as EI
import ListSelect exposing (Msg(..))
import MainModel exposing (CoverageChoice(..))


type Msg
    = ShowAllCoverage
    | ShowInputCoverage (Maybe Int)


view : { a | coverage_choice : CoverageChoice, all_inputs : Array ( Int, String ), selected_input : Maybe Int } -> E.Element Msg
view model =
    E.column [ E.width E.fill ]
        [ EI.radio []
            { onChange = identity
            , selected =
                case model.coverage_choice of
                    All ->
                        Just ShowAllCoverage

                    Input _ ->
                        Just (ShowInputCoverage model.selected_input)
            , label = EI.labelAbove [] (E.text "Coverage")
            , options =
                [ EI.option ShowAllCoverage (E.text "All coverage")
                , EI.option (ShowInputCoverage model.selected_input) (E.text "Specific input")
                ]
            }
        , E.map
            (\x ->
                case x of
                    Select i ->
                        ShowInputCoverage (Just i)

                    UnSelect ->
                        ShowInputCoverage Nothing
            )
            (ListSelect.view
                { all_items = Array.map (\( _, y ) -> y) model.all_inputs, selected_item = model.selected_input }
            )
        ]
