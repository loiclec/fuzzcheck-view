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
    , selected_file : Maybe Int
    , cached_selected_file : Maybe String
    , selected_function : Maybe Int
    , cached_selected_function : Maybe FunctionName
    , function_coverage : Maybe FunctionCoverage
    , counter_id : Maybe Int
    , best_input : Maybe String
    , input_filter : InputFilter
    , function_filter : FunctionFilter
    , coverage_kind_filter : CoverageKindFilter
    , all_inputs : Array InputInfo
    , selected_input : Maybe Int
    , previewed_input : Maybe ( String, String )
    }


emptyModel : Model
emptyModel =
    { layout = Layout.initialLayout
    , all_files = Array.empty
    , selected_file = Nothing
    , cached_selected_file = Nothing
    , selected_function = Nothing
    , cached_selected_function = Nothing
    , function_coverage = Nothing
    , counter_id = Nothing
    , best_input = Nothing
    , input_filter = AllInputs
    , function_filter = { exclude_100 = False, exclude_0 = False }
    , coverage_kind_filter = AllCoverageKind
    , all_inputs = Array.empty
    , selected_input = Nothing
    , previewed_input = Nothing
    }


type InputFilter
    = AllInputs
    | OnlySelectedInput


type alias FunctionFilter =
    { exclude_100 : Bool
    , exclude_0 : Bool
    }


type CoverageKindFilter
    = AllCoverageKind
    | LeastComplexCoverageKind


type alias InputInfo =
    { pool_idx : Int
    , hash : String
    }


getSelectedPoolIdx : { a | all_inputs : Array { b | pool_idx : Int }, input_filter : InputFilter, selected_input : Maybe Int } -> Maybe Int
getSelectedPoolIdx model =
    Maybe.andThen (\selected_input -> Maybe.map .pool_idx (Array.get selected_input model.all_inputs)) model.selected_input


decodeInputInfo : D.Decoder InputInfo
decodeInputInfo =
    D.map2 InputInfo (D.field "pool_idx" D.int) (D.field "hash" D.string)


fileSelectModel : { a | all_files : Array ( String, Array FunctionName ), selected_file : Maybe Int } -> ListSelect.Model
fileSelectModel model =
    ListSelect.Model (Array.map Tuple.first model.all_files) model.selected_file


functionSelectModel : { a | all_files : Array ( String, Array FunctionName ), selected_file : Maybe Int, selected_function : Maybe Int } -> ListSelect.Model
functionSelectModel model =
    case model.selected_file of
        Nothing ->
            { all_items = Array.empty, selected_item = Nothing }

        Just selected_file ->
            { all_items =
                Array.get selected_file model.all_files
                    |> Maybe.map Tuple.second
                    |> Maybe.map
                        (Array.map .demangled_name)
                    |> Maybe.withDefault Array.empty
            , selected_item = model.selected_function
            }
