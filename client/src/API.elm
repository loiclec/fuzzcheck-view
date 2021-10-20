module API exposing (..)

import Array exposing (Array)
import Coverage exposing (..)
import Http
import Json.Decode as D
import MainModel exposing (CoverageKindFilter, FunctionFilter, InputFilter, InputInfo, Model)
import Set
import Url.Builder as UrlB


getCoverageUrl :
    { a
        | all_files : Array ( String, Array FunctionName )
        , selected_file : Int
        , selected_function : Int
        , input_filter : InputFilter
    }
    -> Maybe String
getCoverageUrl model =
    Array.get model.selected_file
        model.all_files
        |> Maybe.map Tuple.second
        |> Maybe.andThen
            (\functions ->
                Array.get
                    model.selected_function
                    functions
            )
        |> Maybe.map (\function_name -> UrlB.relative [ "coverage" ] [ UrlB.string "input_filter" (getInputFilterString model.input_filter), UrlB.string "function" function_name.name ])


getCoverageCmd : (Result Http.Error FunctionCoverage -> msg) -> { a | all_files : Array ( String, Array FunctionName ), selected_file : Int, selected_function : Int, input_filter : InputFilter } -> Cmd msg
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


getListOfFunctionsUrl : Model -> String
getListOfFunctionsUrl model =
    UrlB.relative [ "functions" ]
        (UrlB.string "input_filter" (getInputFilterString model.input_filter)
            :: List.map
                (\filter ->
                    UrlB.string "function_filter" (getFunctionFilterString filter)
                )
                (Set.toList model.function_filter)
            ++ [ UrlB.string "coverage_kind_filter" (getCoverageKindFilterString model.coverage_kind_filter) ]
        )


getFilesAndFunctionsCmd : (Result Http.Error (Array ( String, Array FunctionName )) -> msg) -> Model -> Cmd msg
getFilesAndFunctionsCmd getmsg model =
    Http.get
        { url = getListOfFunctionsUrl model
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


getInputFilterString : InputFilter -> String
getInputFilterString choice =
    case choice of
        MainModel.AllInputs ->
            "all"

        MainModel.OnlyInput x ->
            String.fromInt x


getFunctionFilterString : FunctionFilter -> String
getFunctionFilterString choice =
    case choice of
        MainModel.Exclude0PercentCoverageFunctions ->
            "Exclude0PercentCoverage"

        MainModel.Exclude100PercentCoverageFunctions ->
            "Exclude100PercentCoverage"


getCoverageKindFilterString : CoverageKindFilter -> String
getCoverageKindFilterString filter =
    case filter of
        MainModel.AllCoverageKind ->
            "All"

        MainModel.LeastComplexCoverageKind ->
            "LeastComplex"
