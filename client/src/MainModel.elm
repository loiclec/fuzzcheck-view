module MainModel exposing (..)

import Array exposing (Array)
import Coverage exposing (CodeBlock, FunctionName)
import Layout
import ListSelect



-- MODEL


type alias Model =
    { layout : Layout.Layout
    , all_functions : Array FunctionName
    , selected_function : Maybe Int
    , block : Maybe CodeBlock
    , all_files : Array String
    , selected_file : Maybe Int
    , counter_id : Maybe Int
    , best_input : Maybe ( String, String )
    , coverage_choice : CoverageChoice
    , all_inputs : Array ( Int, String )
    , selected_input : Maybe Int
    }


type CoverageChoice
    = All
    | Input Int


emptyModel : Model
emptyModel =
    Model Layout.initialLayout Array.empty Nothing Nothing Array.empty Nothing Nothing Nothing All Array.empty Nothing


fileSelectModel : { a | all_files : Array String, selected_file : Maybe Int } -> ListSelect.Model
fileSelectModel model =
    ListSelect.Model model.all_files model.selected_file


functionSelectModel : { a | all_functions : Array FunctionName, selected_function : Maybe Int } -> ListSelect.Model
functionSelectModel model =
    ListSelect.Model (Array.map .demangled_name model.all_functions) model.selected_function
