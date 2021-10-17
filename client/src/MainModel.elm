module MainModel exposing (..)

import Array exposing (Array)
import Coverage
import FileSelect
import Set exposing (Set)



-- MODEL


type alias Model =
    { blocks : List Coverage.CodeBlock
    , all_files : Array String
    , selected_file : Maybe Int
    , counter_id : Maybe Int
    }


emptyModel : Model
emptyModel =
    Model [] Array.empty Nothing Nothing
