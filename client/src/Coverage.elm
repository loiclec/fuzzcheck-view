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
    E.column [ E.width E.fill ]
        [ E.paragraph
            [ Background.color (E.rgb255 0xCC 0xCA 0xC2), E.padding 10, Font.color (E.rgb255 0x24 0x29 0x36), Font.size 16, Font.family [ Font.monospace ] ]
            [ E.text (block.title ++ " in " ++ block.file) ]
        , viewCodeLines block.content

        -- E.row []
        --     [ E.column [ E.alignTop, Background.color (E.rgba255 0x8A 0x91 0x99 0.4) ]
        --         (List.map
        --             viewCodeLineNumber
        --             block.content
        --         )
        --     , E.column [ E.paddingXY 0 10, Background.color (E.rgb255 0x24 0x29 0x36), E.width E.fill ]
        --         (List.map
        --             viewCodeLine
        --             block.content
        --         )
        --     ]
        ]


viewCodeLines : List CodeLine -> E.Element msg
viewCodeLines lines =
    E.row
        [ E.width E.fill
        , Font.color (E.rgb255 0xCC 0xCA 0xC2)
        , Font.size 14
        , Font.family
            [ Font.monospace
            ]
        ]
        [ E.column [ E.paddingXY 2 10, E.width E.shrink, E.spacing 5, Background.color (E.rgba255 0x8A 0x91 0x99 0.4) ] (List.map viewCodeLineNumber lines)
        , E.column [ E.paddingXY 2 10, E.scrollbarX, E.width E.fill, E.spacing 5, Background.color (E.rgb255 0x24 0x29 0x36) ] (List.map viewCodeLine lines)
        ]


viewCodeLineNumber : CodeLine -> E.Element msg
viewCodeLineNumber line =
    E.el
        [ E.paddingXY 3 0 ]
        (E.text
            (String.fromInt line.lineno)
        )


viewCodeLine : CodeLine -> E.Element msg
viewCodeLine line =
    E.row
        [ E.width E.fill
        , Background.color (E.rgb255 0x24 0x29 0x36)
        ]
        (List.map
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
                [ Font.color colour ]
                (E.text span.text)

        NotHit _ ->
            let
                colour =
                    E.rgb255 0xFF 0x66 0x66
            in
            E.el
                [ E.pointer, E.mouseOver [ Font.color (rgb255 220 220 220), Background.color colour ], Font.color colour, Background.color (E.rgba (E.toRgb colour).red (E.toRgb colour).green (E.toRgb colour).blue 0.2) ]
                (E.text span.text)

        Hit _ ->
            let
                colour =
                    E.rgb255 0x87 0xD9 0x6C
            in
            E.el
                [ E.pointer, E.mouseOver [ Font.color (rgb255 220 220 220), Background.color colour ], Font.color colour ]
                (E.text span.text)


{-| Decodes "Untracked", "NotHit", or "Hit" into the appropriate CodeSpanKind
-}
decodeCodeSpanKind : D.Decoder CodeSpanKind
decodeCodeSpanKind =
    D.oneOf
        [ D.string
            |> D.andThen
                (\value ->
                    case value of
                        "Untracked" ->
                            D.succeed Untracked

                        _ ->
                            D.fail <| "failed to decode CodeSpanKind"
                )
        , D.field "NotHit" (D.field "id" D.int) |> D.andThen (\id -> D.succeed (NotHit id))
        , D.field "Hit" (D.field "id" D.int) |> D.andThen (\id -> D.succeed (Hit id))
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
    D.map3 CodeBlock (D.field "title" D.string) (D.field "file" D.string) (D.field "lines" (D.list decodeCodeLine))
