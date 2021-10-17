module FileSelect exposing (..)

import Array exposing (Array)
import Element as E
import Element.Background as Background
import Element.Font as Font exposing (monospace)
import Element.Input as EI
import Set exposing (Set)
import Style exposing (..)


type alias Model =
    { all_files : Array String
    , selected_file : Maybe Int
    }


empty : Model
empty =
    { all_files = Array.empty
    , selected_file = Nothing
    }


type Msg
    = Select Int
    | UnSelect Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select i ->
            { model | selected_file = Just i }

        UnSelect i ->
            { model | selected_file = Nothing }


view : Model -> E.Element Msg
view model =
    E.column [ E.height (E.px 100), Font.color fg, Font.family [ monospace ], Font.size normalFontSize, E.spacing smallSpacing ]
        (Array.toList
            (Array.indexedMap
                (\i ->
                    \x ->
                        EI.checkbox
                            (E.padding smallSpacing
                                :: (if model.selected_file == Just i then
                                        [ Background.color (makeTransparent green 0.2), E.mouseOver [ Background.color (makeTransparent green 0.3) ] ]

                                    else
                                        [ Background.color (makeTransparent red 0.2), E.mouseOver [ Background.color (makeTransparent red 0.3) ] ]
                                   )
                            )
                            { onChange =
                                \checked ->
                                    if checked then
                                        Select i

                                    else
                                        UnSelect i
                            , icon =
                                \checked ->
                                    if checked then
                                        E.el [ Font.size normalFontSize ] (E.text "+")

                                    else
                                        E.el [ Font.size normalFontSize ] (E.text "-")
                            , checked = model.selected_file == Just i
                            , label =
                                EI.labelRight []
                                    (E.text x)
                            }
                )
                model.all_files
            )
        )
