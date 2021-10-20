module API exposing (..)

import Array exposing (Array)
import Coverage exposing (..)
import Http
import Json.Decode as D
import MainModel exposing (InputFilter, InputInfo, Model)
import Url.Builder as UrlB


getCoverageUrl : { a | all_functions : Array FunctionName, selected_function : Maybe Int, input_filter : InputFilter } -> Maybe String
getCoverageUrl model =
    model.selected_function
        |> Maybe.andThen
            (\selected_function ->
                Array.get selected_function model.all_functions
            )
        |> Maybe.map (\function_name -> UrlB.relative [ "coverage" ] [ UrlB.string "filter" (getCoverageFilterString model.input_filter), UrlB.string "function" function_name.name ])


getCoverageCmd : (Result Http.Error FunctionCoverage -> msg) -> { a | all_functions : Array FunctionName, selected_function : Maybe Int, input_filter : InputFilter } -> Cmd msg
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
    model.selected_file
        |> Maybe.andThen
            (\selected_file ->
                Array.get selected_file model.all_files
            )
        |> Maybe.map (\filename -> UrlB.relative [ "functions" ] [ UrlB.string "file" filename ])


getListOfFunctionsCmd : (Result Http.Error (Array FunctionName) -> msg) -> Model -> Cmd msg
getListOfFunctionsCmd getmsg model =
    let
        optreq =
            getListOfFunctionsUrl model
    in
    case optreq of
        Just req ->
            Http.get
                { url = req
                , expect = Http.expectJson getmsg (D.array decodeFunctionName)
                }

        Nothing ->
            Cmd.none


getInputCmd : (Result Http.Error String -> msg) -> String -> Cmd msg
getInputCmd getmsg name =
    Http.get
        { url = getInput name
        , expect = Http.expectJson getmsg D.string
        }


getInput : String -> String
getInput name =
    UrlB.relative [ "input" ] [ UrlB.string "hash" name ]


getListOfFiles : String
getListOfFiles =
    "files"


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


getCoverageFilterString : InputFilter -> String
getCoverageFilterString choice =
    case choice of
        MainModel.All ->
            "all"

        MainModel.Input x ->
            String.fromInt x
