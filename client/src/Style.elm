module Style exposing (..)

import Element as E exposing (Color)
import Element.Font as Font



-- Colors


fg : Color
fg =
    E.rgb255 0xCC 0xCA 0xC2


bgDark : Color
bgDark =
    E.rgb255 0x1A 0x1F 0x29


altBgDark : Color
altBgDark =
    E.rgba255 0x40 0x9F 0xFF 0.25


bgCode : Color
bgCode =
    E.rgb255 0x24 0x29 0x36


lighterBgCode : Color
lighterBgCode =
    E.rgba255 0x8A 0x91 0x99 0.4


red : Color
red =
    E.rgb255 0xFF 0x66 0x66


green : Color
green =
    E.rgb255 0x87 0xD9 0x6C


actionColor : Color
actionColor =
    E.rgb255 0x69 0x53 0x80


actionHoverColor : Color
actionHoverColor =
    E.rgb255 0x59 0x43 0x70


actionPressColor : Color
actionPressColor =
    E.rgb255 0x49 0x33 0x60


makeTransparent : Color -> Float -> Color
makeTransparent color opacity =
    let
        rgb =
            E.toRgb color
    in
    E.rgba rgb.red rgb.green rgb.blue opacity



-- Sizes


smallFontSize : Int
smallFontSize =
    11


normalFontSize : Int
normalFontSize =
    13


largeFontSize : Int
largeFontSize =
    15


smallSpacing : Int
smallSpacing =
    2


normalSpacing : Int
normalSpacing =
    6


largeSpacing : Int
largeSpacing =
    12



-- Font


codeFontFamily : List Font.Font
codeFontFamily =
    [ Font.monospace ]
