module MainModel exposing (..)

import Array exposing (Array)
import Coverage exposing (FunctionCoverage, FunctionName)
import Json.Decode as D
import Layout
import ListSelect



-- MODEL


type alias Model =
    { layout : Layout.Layout
    , all_functions : Array FunctionName
    , selected_function : Maybe Int
    , block : Maybe FunctionCoverage
    , all_files : Array String
    , selected_file : Maybe Int
    , counter_id : Maybe Int
    , best_input : Maybe String
    , input_filter : InputFilter
    , all_inputs : Array InputInfo
    , selected_input : Maybe Int
    , previewed_input : Maybe ( String, String )
    }


type InputFilter
    = All
    | Input Int


type alias InputInfo =
    { pool_idx : Int
    , hash : String
    }


decodeInputInfo : D.Decoder InputInfo
decodeInputInfo =
    D.map2 InputInfo (D.field "pool_idx" D.int) (D.field "hash" D.string)


emptyModel : Model
emptyModel =
    Model Layout.initialLayout Array.empty Nothing Nothing Array.empty Nothing Nothing Nothing All Array.empty Nothing Nothing


fileSelectModel : { a | all_files : Array String, selected_file : Maybe Int } -> ListSelect.Model
fileSelectModel model =
    ListSelect.Model model.all_files model.selected_file


functionSelectModel : { a | all_functions : Array FunctionName, selected_function : Maybe Int } -> ListSelect.Model
functionSelectModel model =
    ListSelect.Model (Array.map .demangled_name model.all_functions) model.selected_function
