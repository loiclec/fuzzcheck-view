module Filters exposing (..)

import Array exposing (Array)
import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as EI
import ListSelect exposing (Msg(..))
import MainModel exposing (CoverageKindFilter(..), FunctionFilter, InputFilter(..))
import Style exposing (..)


type Msg
    = ChangeInputFilter InputFilter
    | ChangeCoverageKindFilter CoverageKindFilter
    | Exclude100 Bool
    | Exclude0 Bool


view : { a | all_inputs : Array { b | pool_idx : Int }, input_filter : InputFilter, coverage_kind_filter : CoverageKindFilter, function_filter : FunctionFilter } -> E.Element Msg
view model =
    E.column [ E.padding normalSpacing, E.spacing normalSpacing, Background.color bgCode, E.scrollbars, E.width E.fill, E.height (E.shrink |> E.maximum 140), Font.family codeFontFamily, Font.size normalFontSize, Font.color fg ]
        ([ EI.radio []
            { onChange = identity
            , selected = Just (ChangeInputFilter model.input_filter)
            , label = EI.labelLeft [ E.centerY ] (E.text "Show coverage for  ")
            , options =
                [ EI.option (ChangeInputFilter AllInputs) (E.text "all inputs combined")
                , EI.option (ChangeInputFilter OnlySelectedInput) (E.text "the selected input only")
                ]
            }
         , E.row []
            [ E.el [ E.centerY ] (E.text "Hide functions that have  ")
            , E.column []
                [ EI.checkbox []
                    { onChange = Exclude100
                    , icon = EI.defaultCheckbox
                    , checked = model.function_filter.exclude_100
                    , label =
                        EI.labelRight [ E.centerY ]
                            (case model.input_filter of
                                AllInputs ->
                                    E.text "100% coverage"

                                OnlySelectedInput ->
                                    E.text "100% coverage of the selected kind"
                            )
                    }
                , EI.checkbox []
                    { onChange = Exclude0
                    , icon = EI.defaultCheckbox
                    , checked = model.function_filter.exclude_0
                    , label =
                        EI.labelRight [ E.centerY ]
                            (case model.input_filter of
                                AllInputs ->
                                    E.text "0% coverage"

                                OnlySelectedInput ->
                                    E.text "0% coverage of the selected kind"
                            )
                    }
                ]
            ]
         ]
            ++ (case model.input_filter of
                    AllInputs ->
                        []

                    OnlySelectedInput ->
                        [ EI.radio []
                            { onChange = identity
                            , selected = Just (ChangeCoverageKindFilter model.coverage_kind_filter)
                            , label = EI.labelLeft [ E.centerY ] (E.text "    coverage kind:  ")
                            , options =
                                [ EI.option (ChangeCoverageKindFilter AllCoverageKind) (E.text "green (selected input reached the code region)")
                                , EI.option (ChangeCoverageKindFilter LeastComplexCoverageKind) (E.paragraph [ E.spacing 0 ] [ E.text "blue (selected input is the simplest input to reach the region)" ])
                                ]
                            }
                        ]
               )
        )
