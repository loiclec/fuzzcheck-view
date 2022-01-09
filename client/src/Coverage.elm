module Coverage exposing (..)

import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Json.Decode as D
import Layout exposing (Layout)
import Style exposing (..)


type alias FunctionName =
    { name : String
    , demangled_name : String
    }


type alias FunctionCoverage =
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
    | Tracked Int CoverageStatus
    | Inferred CoverageStatus


type CoverageStatus
    = Hit
    | NotHit
    | Best
    | Unique
    | Unknown


type Msg
    = UnselectCounter
    | SelectCounter Int


type alias CodeBlockData a =
    { a | block : FunctionCoverage, layout : Layout, focused_id : Maybe Int }


emptyCodeBlock : FunctionCoverage
emptyCodeBlock =
    FunctionCoverage "empty" "" [] []


viewCodeBlock : CodeBlockData a -> E.Element Msg
viewCodeBlock model =
    E.column [ E.width E.fill ]
        [ E.paragraph
            [ E.width E.fill, Background.color fg, E.padding normalSpacing, Font.color bgCode, Font.size smallFontSize, Font.family codeFontFamily ]
            [ E.text (model.block.title ++ " in " ++ model.block.file) ]
        , viewCodeLines model
        , viewCounterIds model
        ]


viewCounterIds : CodeBlockData a -> E.Element Msg
viewCounterIds model =
    E.wrappedRow
        [ E.width E.fill
        , E.spacing normalSpacing
        , E.padding smallSpacing
        , Background.color altBgDark
        , Font.color fg
        , Font.size smallFontSize
        , Font.family codeFontFamily
        ]
        (E.el [] (E.text "Counter IDs: ")
            :: List.map
                (\id ->
                    let
                        ( fgColor, bgColor ) =
                            if isCounterFocused id model.focused_id then
                                ( bgCode, fg )

                            else
                                ( fg, lighterBgCode )
                    in
                    E.el [ E.pointer, E.padding smallSpacing, Border.rounded 3, Font.color fgColor, Background.color bgColor, EE.onMouseEnter (SelectCounter id), EE.onMouseLeave UnselectCounter ] (E.text (String.fromInt id))
                )
                model.block.counter_ids
        )


viewCodeLines : CodeBlockData a -> E.Element Msg
viewCodeLines { block, layout, focused_id } =
    E.row
        [ E.width E.fill
        , Font.color fg
        , Font.size normalFontSize
        , Font.family
            codeFontFamily
        ]
        [ E.column [ E.alignTop, E.paddingXY smallSpacing normalSpacing, E.spacing normalSpacing, Background.color lighterBgCode ] (List.map viewCodeLineNumber block.lines)
        , E.column [ E.scrollbarX, E.alignTop, E.paddingXY smallSpacing normalSpacing, E.width E.fill, E.spacing normalSpacing, Background.color bgCode ] (List.map (\line -> viewCodeLine { block = block, layout = layout, focused_id = focused_id, line = line }) block.lines)
        ]


viewCodeLineNumber : CodeLine -> E.Element msg
viewCodeLineNumber line =
    E.el
        [ E.alignRight ]
        (E.text
            (String.fromInt line.lineno)
        )


viewCodeLine : CodeBlockData { a | line : CodeLine } -> E.Element Msg
viewCodeLine { block, layout, focused_id, line } =
    E.row
        [ E.width E.fill ]
        (E.text " "
            :: List.map
                (\span -> viewCodeSpan { block = block, layout = layout, focused_id = focused_id, span = span })
                line.spans
        )


viewCodeSpan : CodeBlockData { a | span : CodeSpan } -> E.Element Msg
viewCodeSpan { block, layout, focused_id, span } =
    case span.kind of
        Untracked ->
            E.el
                [ Font.color fg ]
                (E.text span.text)

        Tracked id status ->
            viewTrackedCodeSpan { block = block, layout = layout, focused_id = focused_id, id = id, status = status, text = span.text }

        Inferred status ->
            let
                ( fgColor, bgColor ) =
                    let
                        color =
                            coverageStatusColor status
                    in
                    ( color, bgCode )
            in
            E.el [ Font.color fgColor, Background.color bgColor ]
                (E.text span.text)


viewTrackedCodeSpan : CodeBlockData { a | id : Int, status : CoverageStatus, text : String, focused_id : Maybe Int } -> E.Element Msg
viewTrackedCodeSpan model =
    let
        ( fgColor, bgColor ) =
            let
                color =
                    coverageStatusColor model.status
            in
            if isCounterFocused model.id model.focused_id then
                ( bgCode, color )

            else
                ( color, makeTransparent color 0.2 )
    in
    E.el
        [ E.pointer
        , Font.color fgColor
        , Background.color bgColor
        , EE.onMouseEnter (SelectCounter model.id)
        , EE.onMouseLeave UnselectCounter
        ]
        (E.text model.text)


coverageStatusColor : CoverageStatus -> E.Color
coverageStatusColor status =
    case status of
        NotHit ->
            red

        Hit ->
            green

        Best ->
            blue

        Unique ->
            purple

        Unknown ->
            fg



-- isCodeSpanKindFocused : CodeSpanKind -> Maybe Int -> Bool
-- isCodeSpanKindFocused kind focused_id =
--     case kind of
--         Untracked ->
--             False
--         NotHit id ->
--             isCounterFocused id focused_id
--         Hit id ->
--             isCounterFocused id focused_id


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
        , D.field "Tracked" (D.map2 Tracked (D.field "id" D.int) (D.field "status" decodeCoverageStatus))
        , D.field "Inferred" (D.map Inferred (D.field "status" decodeCoverageStatus))
        ]


decodeCoverageStatus : D.Decoder CoverageStatus
decodeCoverageStatus =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "NotHit" ->
                        D.succeed NotHit

                    "Hit" ->
                        D.succeed Hit

                    "Best" ->
                        D.succeed Best

                    "Unique" ->
                        D.succeed Unique

                    "Unknown" ->
                        D.succeed Unknown

                    _ ->
                        D.fail <| "failed to decode CoverageStatus"
            )


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


{-| Decodes a FunctionName
-}
decodeFunctionName : D.Decoder FunctionName
decodeFunctionName =
    D.map2 FunctionName (D.field "name" D.string) (D.field "demangled_name" D.string)


{-| Decodes a CodeLine
-}
decodeCodeLine : D.Decoder CodeLine
decodeCodeLine =
    D.map2 CodeLine (D.field "lineno" D.int) (D.field "spans" (D.list decodeCodeSpan))


{-| Decodes a FunctionCoverage
-}
decodeFunctionCoverage : D.Decoder FunctionCoverage
decodeFunctionCoverage =
    D.map4 FunctionCoverage
        (D.map .demangled_name (D.field "name" decodeFunctionName))
        (D.field "file" D.string)
        (D.field "lines" (D.list decodeCodeLine))
        (D.field "counter_ids" (D.list D.int))
