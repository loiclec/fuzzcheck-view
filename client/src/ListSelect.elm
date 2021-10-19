module ListSelect exposing (..)

import Array exposing (Array)
import Element as E
import Element.Background as Background
import Element.Font as Font exposing (monospace)
import Element.Input as EI
import Style exposing (..)


type alias Model =
    { all_items : Array String
    , selected_item :
        Maybe Int
    }


empty : Model
empty =
    { all_items = Array.empty
    , selected_item =
        Nothing
    }


type Msg
    = Select Int
    | UnSelect


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select i ->
            { model
                | selected_item =
                    Just i
            }

        UnSelect ->
            { model
                | selected_item =
                    Nothing
            }


view : Model -> E.Element Msg
view model =
    E.column [ E.scrollbars, E.width E.fill, E.alignTop, E.height (E.shrink |> E.maximum 140), Font.color fg, Font.family [ monospace ], Font.size normalFontSize, E.spacing smallSpacing ]
        (Array.toList
            (Array.indexedMap
                (\i ->
                    \x ->
                        EI.checkbox
                            ([ E.width E.fill, E.padding smallSpacing ]
                                ++ (if
                                        model.selected_item
                                            == Just i
                                    then
                                        [ Background.color (makeTransparent green 0.3), E.mouseOver [ Background.color (makeTransparent green 0.5) ] ]

                                    else
                                        [ Background.color (makeTransparent red 0.3), E.mouseOver [ Background.color (makeTransparent red 0.5) ] ]
                                   )
                            )
                            { onChange =
                                \checked ->
                                    if checked then
                                        Select i

                                    else
                                        UnSelect
                            , icon =
                                \checked ->
                                    if checked then
                                        E.el [ Font.size normalFontSize ] (E.text "+")

                                    else
                                        E.el [ Font.size normalFontSize ] (E.text "-")
                            , checked =
                                model.selected_item
                                    == Just i
                            , label =
                                EI.labelRight [ E.width E.fill ]
                                    (E.paragraph [ E.spacing smallSpacing ] [ E.text x ])
                            }
                )
                model.all_items
            )
        )
