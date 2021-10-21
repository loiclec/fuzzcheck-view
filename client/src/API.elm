module API exposing (..)

import Array exposing (Array)
import Coverage exposing (..)
import Http
import Json.Decode as D
import MainModel exposing (CoverageKindFilter, FunctionFilter, InputFilter, InputInfo, Model, getSelectedPoolIdx)
import Set
import Url.Builder as UrlB


getCoverageUrl :
    { a
        | cached_selected_function : Maybe FunctionName
        , input_filter : InputFilter
        , all_inputs : Array { b | pool_idx : Int }
        , selected_input : Maybe Int
    }
    -> Maybe String
getCoverageUrl model =
    getInputFilterString model
        |> Maybe.andThen
            (\input_filter_string ->
                Maybe.map
                    (\function -> UrlB.relative [ "coverage" ] [ UrlB.string "input_filter" input_filter_string, UrlB.string "function" function.name ])
                    model.cached_selected_function
            )


getCoverageCmd : (Result Http.Error FunctionCoverage -> msg) -> { a | cached_selected_function : Maybe FunctionName, input_filter : InputFilter, all_inputs : Array { b | pool_idx : Int }, selected_input : Maybe Int } -> Cmd msg
getCoverageCmd getmsg model =
    let
        optreq =
            getCoverageUrl model
    in
    case optreq of
        Just req ->
            Http.get
                { url = req
                , expect = Http.expectJson getmsg decodeFunctionCoverage
                }

        Nothing ->
            Cmd.none


getListOfFunctionsUrl : Model -> Maybe String
getListOfFunctionsUrl model =
    Maybe.map
        (\input_filter_string ->
            UrlB.relative [ "functions" ]
                (UrlB.string
                    "input_filter"
                    input_filter_string
                    :: (if model.function_filter.exclude_100 then
                            [ UrlB.string "function_filter" "Exclude100PercentCoverage" ]

                        else
                            []
                       )
                    ++ (if model.function_filter.exclude_0 then
                            [ UrlB.string "function_filter" "Exclude0PercentCoverage" ]

                        else
                            []
                       )
                    ++ [ UrlB.string "coverage_kind_filter" (getCoverageKindFilterString model.coverage_kind_filter) ]
                )
        )
        (getInputFilterString model)


getFilesAndFunctionsCmd : (Result Http.Error (Array ( String, Array FunctionName )) -> msg) -> Model -> Cmd msg
getFilesAndFunctionsCmd getmsg model =
    case getListOfFunctionsUrl model of
        Nothing ->
            {- TODO -}
            Cmd.none

        Just url ->
            Http.get
                { url = url
                , expect = Http.expectJson getmsg (D.array (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 (D.array decodeFunctionName))))
                }


getInputCmd : (Result Http.Error String -> msg) -> String -> Cmd msg
getInputCmd getmsg name =
    Http.get
        { url = getInput name
        , expect = Http.expectJson getmsg D.string
        }


getInput : String -> String
getInput name =
    UrlB.relative [ "input" ] [ UrlB.string "hash" name ]


getBestInputForCounter : Int -> String
getBestInputForCounter id =
    UrlB.relative [ "best_input" ] [ UrlB.int "counter" id ]


getListOfInputs : String
getListOfInputs =
    "inputs"


getListOfInputsCmd : (Result Http.Error (Array InputInfo) -> msg) -> Cmd msg
getListOfInputsCmd getmsg =
    Http.get
        { url = getListOfInputs
        , expect =
            Http.expectJson
                getmsg
                (D.array
                    MainModel.decodeInputInfo
                )
        }


getInputFilterString : { a | input_filter : InputFilter, all_inputs : Array { b | pool_idx : Int }, selected_input : Maybe Int } -> Maybe String
getInputFilterString model =
    case model.input_filter of
        MainModel.AllInputs ->
            Just "all"

        MainModel.OnlySelectedInput ->
            Maybe.map String.fromInt (getSelectedPoolIdx model)


getCoverageKindFilterString : CoverageKindFilter -> String
getCoverageKindFilterString filter =
    case filter of
        MainModel.AllCoverageKind ->
            "All"

        MainModel.LeastComplexCoverageKind ->
            "LeastComplex"
