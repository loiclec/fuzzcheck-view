module ListSelect exposing (..)

import Array exposing (Array)
import Element as E
import Element.Background as Background
import Element.Events as EE
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
    | Hover Int
    | UnHover


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

        Hover _ ->
            model

        UnHover ->
            model


view : Model -> E.Element Msg
view model =
    E.column [ E.scrollbars, E.width E.fill, E.alignTop, E.height (E.shrink |> E.maximum 140), Font.color fg, Font.family [ monospace ], Font.size normalFontSize, E.spacing smallSpacing ]
        (Array.toList
            (Array.indexedMap
                (\i ->
                    \x ->
                        EI.checkbox
                            ([ E.width E.fill, E.padding smallSpacing, EE.onMouseEnter (Hover i), EE.onMouseLeave UnHover ]
                                ++ (if
                                        model.selected_item
                                            == Just i
                                    then
                                        [ Background.color (makeTransparent fg 1.0), Font.color bgCode ]

                                    else
                                        [ Background.color (makeTransparent bgCode 1), E.mouseOver [ Background.color (makeTransparent fg 0.3) ] ]
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
