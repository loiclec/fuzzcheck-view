module Filters exposing (..)

import Array exposing (Array)
import Element as E
import Element.Input as EI
import ListSelect exposing (Msg(..))
import MainModel exposing (InputFilter(..))


type Msg
    = ShowAllCoverage
    | ShowInputCoverage (Maybe Int)


view : { a | input_filter : InputFilter, selected_input : Maybe Int } -> E.Element Msg
view model =
    E.column [ E.scrollbars, E.width E.fill, E.height (E.shrink |> E.maximum 140) ]
        [ EI.radio []
            { onChange = identity
            , selected =
                case model.input_filter of
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
        ]
