module MainModel exposing (..)

import Array exposing (Array)
import Coverage exposing (FunctionCoverage, FunctionName)
import Dict exposing (Dict)
import Json.Decode as D
import Layout
import ListSelect
import Set exposing (Set)



-- MODEL


type alias Model =
    { layout : Layout.Layout
    , all_files : Array ( String, Array FunctionName )
    , selected_file : Int
    , selected_function : Int
    , block : Maybe FunctionCoverage
    , counter_id : Maybe Int
    , best_input : Maybe String
    , input_filter : InputFilter
    , function_filter : Set FunctionFilter
    , coverage_kind_filter : CoverageKindFilter
    , all_inputs : Array InputInfo
    , selected_input : Int
    , previewed_input : Maybe ( String, String )
    }


emptyModel : Model
emptyModel =
    { layout = Layout.initialLayout
    , all_files = Array.empty
    , selected_file = 0
    , selected_function = 0
    , block = Nothing
    , counter_id = Nothing
    , best_input = Nothing
    , input_filter = AllInputs
    , function_filter = Set.empty
    , coverage_kind_filter = AllCoverageKind
    , all_inputs = Array.empty
    , selected_input = 0
    , previewed_input = Nothing
    }


type InputFilter
    = AllInputs
    | OnlyInput Int


type FunctionFilter
    = Exclude0PercentCoverageFunctions
    | Exclude100PercentCoverageFunctions


type CoverageKindFilter
    = AllCoverageKind
    | LeastComplexCoverageKind


type alias InputInfo =
    { pool_idx : Int
    , hash : String
    }


decodeInputInfo : D.Decoder InputInfo
decodeInputInfo =
    D.map2 InputInfo (D.field "pool_idx" D.int) (D.field "hash" D.string)


fileSelectModel : { a | all_files : Array ( String, Array FunctionName ), selected_file : Int } -> ListSelect.Model
fileSelectModel model =
    ListSelect.Model (Array.map Tuple.first model.all_files) model.selected_file


functionSelectModel : { a | all_files : Array ( String, Array FunctionName ), selected_file : Int, selected_function : Int } -> ListSelect.Model
functionSelectModel model =
    ListSelect.Model
        (Array.get model.selected_file model.all_files
            |> Maybe.map Tuple.second
            |> Maybe.map
                (Array.map .demangled_name)
            |> Maybe.withDefault Array.empty
        )
        model.selected_function
