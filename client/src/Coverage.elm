module Coverage exposing (..)

import Element as E exposing (rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Html
import Json.Decode as D


type alias CodeBlock =
    { title : String
    , file : String
    , content : List CodeLine
    }


type alias CodeLine =
    { lineno : Int
    , spans : List CodeSpan
    }


type alias CodeSpan =
    { text : String
    , kind : CodeSpanKind
    }


type CodeSpanKind
    = Untracked
    | NotHit Int
    | Hit Int


emptyCodeBlock : CodeBlock
emptyCodeBlock =
    CodeBlock "empty" "" []


viewCodeBlock : CodeBlock -> E.Element msg
viewCodeBlock block =
    E.textColumn [ E.width E.fill, E.spacing 8 ]
        [ E.paragraph [ Background.color (E.rgb255 0xCC 0xCA 0xC2), Border.rounded 3, E.padding 10, Font.color (E.rgb255 0x24 0x29 0x36), Font.size 16, Font.family [ Font.monospace ] ] [ E.text (block.title ++ " in " ++ block.file) ]
        , E.paragraph
            [ E.width E.fill
            , Background.color (E.rgb255 0x24 0x29 0x36)
            , Border.rounded 3
            , E.padding 10
            , Font.color (E.rgb255 0xCC 0xCA 0xC2)
            , Font.size 13
            , Font.family
                [ Font.monospace
                ]
            ]
            (List.map
                viewCodeLine
                block.content
            )
        ]


viewCodeLine : CodeLine -> E.Element msg
viewCodeLine line =
    E.row []
        (E.el
            [ E.alignTop ]
            (E.text
                (String.fromInt line.lineno)
            )
            :: List.map
                viewCodeSpan
                line.spans
        )


viewCodeSpan : CodeSpan -> E.Element msg
viewCodeSpan span =
    case span.kind of
        Untracked ->
            let
                colour =
                    E.rgb255 0xCC 0xCA 0xC2
            in
            E.el
                [ Border.rounded 2, Font.color colour ]
                (E.text span.text)

        NotHit _ ->
            let
                colour =
                    E.rgb255 0xFF 0x66 0x66
            in
            E.el
                [ E.pointer, Border.rounded 2, E.padding 2, E.mouseOver [ Font.color (rgb255 220 220 220), Background.color colour ], Font.color colour ]
                (E.text span.text)

        Hit _ ->
            let
                colour =
                    E.rgb255 0x87 0xD9 0x6C
            in
            E.el
                [ E.pointer, Border.rounded 2, E.padding 2, E.mouseOver [ Font.color (rgb255 220 220 220), Background.color colour ], Font.color colour ]
                (E.text span.text)


{-| Decodes "Untracked", "NotHit", or "Hit" into the appropriate CodeSpanKind
-}
decodeCodeSpanKind : D.Decoder CodeSpanKind
decodeCodeSpanKind =
    D.oneOf
        [ D.string "Untracked" |> D.andThen (D.succeed Untracked)
        , D.field "NotHit" (D.field "id" D.int) |> D.andThen (D.succeed NotHit)
        , D.field "Hit" (D.field "id" D.int) |> D.andThen (D.succeed Hit)
        ]


{-| Decodes a CodeSpan
-}
decodeCodeSpan : D.Decoder CodeSpan
decodeCodeSpan =
    D.map2 CodeSpan
        (D.field
            "text"
            D.string
        )
        (D.field
            "kind"
            decodeCodeSpanKind
        )


{-| Decodes a CodeLine
-}
decodeCodeLine : D.Decoder CodeLine
decodeCodeLine =
    D.map2 CodeLine (D.field "lineno" D.int) (D.field "spans" (D.list decodeCodeSpan))


{-| Decodes a CodeBlock
-}
decodeCodeBlock : D.Decoder CodeBlock
decodeCodeBlock =
    D.map3 CodeBlock (D.field "title" D.string) (D.field "file" D.string) (D.field "content" (D.list decodeCodeLine))
