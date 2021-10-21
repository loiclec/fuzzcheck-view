module Filters exposing (..)

import Array exposing (Array)
import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as EI
import ListSelect exposing (Msg(..))
import MainModel exposing (CoverageKindFilter(..), FunctionFilter, InputFilter(..), getSelectedPoolIdx)
import Set exposing (Set)
import Style exposing (..)


type Msg
    = ChangeInputFilter InputFilter
    | ChangeCoverageKindFilter CoverageKindFilter
    | Exclude100 Bool
    | Exclude0 Bool


view : { a | all_inputs : Array { b | pool_idx : Int }, input_filter : InputFilter, coverage_kind_filter : CoverageKindFilter, function_filter : FunctionFilter } -> E.Element Msg
view model =
    E.column [ Background.color bgCode, E.scrollbars, E.width E.fill, E.height (E.shrink |> E.maximum 140), Font.family codeFontFamily, Font.size normalFontSize, Font.color fg ]
        [ EI.radio []
            { onChange = identity
            , selected = Just (ChangeInputFilter model.input_filter)
            , label = EI.labelAbove [] (E.text "Inputs")
            , options =
                [ EI.option (ChangeInputFilter AllInputs) (E.text "All inputs, combined")
                , EI.option (ChangeInputFilter OnlySelectedInput) (E.text "Specific input")
                ]
            }
        , EI.radio []
            { onChange = identity
            , selected = Just (ChangeCoverageKindFilter model.coverage_kind_filter)
            , label = EI.labelAbove [] (E.text "Coverage")
            , options =
                [ EI.option (ChangeCoverageKindFilter AllCoverageKind) (E.text "All")
                , EI.option (ChangeCoverageKindFilter LeastComplexCoverageKind) (E.text "Only for which the selected input is the least complex to reach the code region")
                ]
            }
        , EI.checkbox []
            { onChange = Exclude100
            , icon = EI.defaultCheckbox
            , checked = model.function_filter.exclude_100
            , label = EI.labelRight [] (E.text "Exclude functions with 100% coverage of the selected kind")
            }
        , EI.checkbox []
            { onChange = Exclude0
            , icon = EI.defaultCheckbox
            , checked = model.function_filter.exclude_0
            , label = EI.labelRight [] (E.text "Exclude functions with 0% coverage of the selected kind")
            }

        -- , EI.radio []
        --     { onChange = identity
        --     , selected = Just (ChangeFunctionFilter model.function_filter)
        --     , label = EI.labelAbove [] (E.text "Function")
        --     , options =
        --         [ EI.option (ChangeInputFilter AllInputs) (E.text "All functions")
        --         , EI.option (ChangeInputFilter OnlySelectedInput) (E.text "Specific input")
        --         ]
        --     }
        ]
