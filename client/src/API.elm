module API exposing (..)

import Array
import Coverage exposing (..)
import MainModel exposing (Model)
import Set
import Url exposing (Url)
import Url.Builder as UrlB


getCodeBlocksUrl : Model -> Maybe String
getCodeBlocksUrl model =
    Maybe.andThen
        (\i ->
            Maybe.map
                (\filename ->
                    UrlB.relative [ "code_blocks" ]
                        [ UrlB.string "file" filename ]
                )
                (Array.get i model.all_files)
        )
        model.selected_file


getListOfFiles : String
getListOfFiles =
    "files"
