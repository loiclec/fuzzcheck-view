module Filters exposing (..)

import Array exposing (Array)
import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as EI
import ListSelect exposing (Msg(..))
import MainModel exposing (InputFilter(..))
import Style exposing (..)


type Msg
    = ShowAllCoverage
    | ShowInputCoverage Int


view : { a | input_filter : InputFilter, selected_input : Int } -> E.Element Msg
view model =
    E.column [ Background.color bgCode, E.scrollbars, E.width E.fill, E.height (E.shrink |> E.maximum 140), Font.family codeFontFamily, Font.size normalFontSize, Font.color fg ]
        [ EI.radio []
            { onChange = identity
            , selected =
                case model.input_filter of
                    AllInputs ->
                        Just ShowAllCoverage

                    OnlyInput _ ->
                        Just (ShowInputCoverage model.selected_input)
            , label = EI.labelAbove [] (E.text "Coverage")
            , options =
                [ EI.option ShowAllCoverage (E.text "All coverage")
                , EI.option (ShowInputCoverage model.selected_input) (E.text "Specific input")
                ]
            }
        ]
