module MainModel exposing (..)

import Array exposing (Array)
import Coverage exposing (CodeBlock)
import Layout
import ListSelect



-- MODEL


type alias Model =
    { layout : Layout.Layout
    , all_blocks : Array String
    , selected_block : Maybe Int
    , block : Maybe CodeBlock
    , all_files : Array String
    , selected_file : Maybe Int
    , counter_id : Maybe Int
    , best_input : Maybe String
    }


emptyModel : Model
emptyModel =
    Model Layout.initialLayout Array.empty Nothing Nothing Array.empty Nothing Nothing Nothing


fileSelectModel : Model -> ListSelect.Model
fileSelectModel model =
    ListSelect.Model model.all_files model.selected_file


functionSelectModel : Model -> ListSelect.Model
functionSelectModel model =
    ListSelect.Model model.all_blocks model.selected_block
