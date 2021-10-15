module Coverage exposing (..)

import Element as E exposing (rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Html
import Html.Events exposing (onMouseEnter)
import Json.Decode as D
import Style exposing (..)


type alias CodeBlock =
    { title : String
    , file : String
    , lines : List CodeLine
    , counter_ids : List Int
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
    CodeBlock "empty" "" [] []


viewCodeBlock : (Maybe Int -> msg) -> CodeBlock -> Maybe Int -> E.Element msg
viewCodeBlock msg block focused_id =
    E.column [ E.width E.fill ]
        [ E.paragraph
            [ Background.color fg, E.padding normalSpacing, Font.color bgCode, Font.size largeFontSize, Font.family codeFontFamily ]
            [ E.text (block.title ++ " in " ++ block.file) ]
        , viewCodeLines msg block.lines focused_id
        , viewCounterIds msg block focused_id
        ]


viewCounterIds : (Maybe Int -> msg) -> CodeBlock -> Maybe Int -> E.Element msg
viewCounterIds msg block focused_id =
    E.wrappedRow [ E.spacing normalSpacing, E.padding smallSpacing, E.width E.fill, Background.color altBgDark, Font.color fg, Font.size normalFontSize, Font.family codeFontFamily ]
        (E.el [] (E.text "Counter IDs: ")
            :: List.map
                (\id ->
                    let
                        ( fgColor, bgColor ) =
                            if isCounterFocused id focused_id then
                                ( bgCode, fg )

                            else
                                ( fg, lighterBgCode )
                    in
                    E.el [ E.pointer, E.padding smallSpacing, Border.rounded 3, Font.color fgColor, Background.color bgColor, EE.onMouseEnter (msg (Just id)), EE.onMouseLeave (msg Nothing) ] (E.text (String.fromInt id))
                )
                block.counter_ids
        )


viewCodeLines : (Maybe Int -> msg) -> List CodeLine -> Maybe Int -> E.Element msg
viewCodeLines msg lines focused_id =
    E.row
        [ E.width E.fill
        , Font.color fg
        , Font.size normalFontSize
        , Font.family
            codeFontFamily
        ]
        [ E.column [ E.paddingXY smallSpacing normalSpacing, E.width E.shrink, E.spacing normalSpacing, Background.color lighterBgCode ] (List.map viewCodeLineNumber lines)
        , E.column [ E.paddingXY smallSpacing normalSpacing, E.scrollbarX, E.width E.fill, E.spacing normalSpacing, Background.color bgCode ] (List.map (\line -> viewCodeLine msg line focused_id) lines)
        ]


viewCodeLineNumber : CodeLine -> E.Element msg
viewCodeLineNumber line =
    E.el
        [ E.paddingXY smallSpacing 0, E.alignRight ]
        (E.text
            (String.fromInt line.lineno)
        )


viewCodeLine : (Maybe Int -> msg) -> CodeLine -> Maybe Int -> E.Element msg
viewCodeLine msg line focused_id =
    E.row
        [ E.width E.fill ]
        (List.map
            (\span -> viewCodeSpan msg span focused_id)
            line.spans
        )


viewCodeSpan : (Maybe Int -> msg) -> CodeSpan -> Maybe Int -> E.Element msg
viewCodeSpan msg span focused_id =
    case span.kind of
        Untracked ->
            E.el
                [ Font.color (codeSpanKindColor span.kind) ]
                (E.text span.text)

        NotHit id ->
            viewTrackedCodeSpan msg span.kind id span.text focused_id

        Hit id ->
            viewTrackedCodeSpan msg span.kind id span.text focused_id


viewTrackedCodeSpan : (Maybe Int -> msg) -> CodeSpanKind -> Int -> String -> Maybe Int -> E.Element msg
viewTrackedCodeSpan msg kind id text focused_id =
    let
        ( fgColor, bgColor ) =
            let
                color =
                    codeSpanKindColor kind
            in
            if isCodeSpanKindFocused kind focused_id then
                ( fg, color )

            else
                ( color, makeTransparent color 0.2 )
    in
    E.el
        [ E.pointer
        , Font.color fgColor
        , Background.color bgColor
        , EE.onMouseEnter (msg (Just id))
        , EE.onMouseLeave (msg Nothing)
        ]
        (E.text text)


codeSpanKindColor : CodeSpanKind -> E.Color
codeSpanKindColor kind =
    case kind of
        Untracked ->
            fg

        NotHit _ ->
            red

        Hit _ ->
            green


isCodeSpanKindFocused : CodeSpanKind -> Maybe Int -> Bool
isCodeSpanKindFocused kind focused_id =
    case kind of
        Untracked ->
            False

        NotHit id ->
            isCounterFocused id focused_id

        Hit id ->
            isCounterFocused id focused_id


isCounterFocused : Int -> Maybe Int -> Bool
isCounterFocused id focused_id =
    case focused_id of
        Just focused_id2 ->
            focused_id2 == id

        _ ->
            False


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
    D.map4 CodeBlock
        (D.field "title" D.string)
        (D.field "file" D.string)
        (D.field "lines" (D.list decodeCodeLine))
        (D.field "counter_ids" (D.list D.int))
