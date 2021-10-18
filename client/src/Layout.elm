module Layout exposing (..)

import Style exposing (..)


type alias Layout =
    { width : Int
    , height : Int
    , column_width : Int
    , padding : Int
    , column_sep : Int
    }


initialLayout : Layout
initialLayout =
    Layout 0 0 0 0 0


layoutForWidthAndHeight : Int -> Int -> Layout
layoutForWidthAndHeight w h =
    let
        ( padding, column_sep ) =
            ( 10, normalSpacing )
    in
    Layout w h ((w - (padding * 2) - column_sep) // 2) padding column_sep
