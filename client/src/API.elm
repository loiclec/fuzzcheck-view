module API exposing (..)

import Array exposing (Array)
import Coverage exposing (..)
import Http
import Json.Decode as D
import MainModel exposing (Model)
import Url.Builder as UrlB


getCodeBlockUrl : Model -> Maybe String
getCodeBlockUrl model =
    model.selected_file
        |> Maybe.andThen
            (\selected_file ->
                Array.get selected_file model.all_files
            )
        |> Maybe.andThen
            (\filename ->
                model.selected_block
                    |> Maybe.andThen
                        (\selected_block ->
                            Array.get selected_block model.all_blocks
                        )
                    |> Maybe.map (\block -> ( filename, block ))
            )
        |> Maybe.map (\( filename, function_name ) -> UrlB.relative [ "code_block" ] [ UrlB.string "file" filename, UrlB.string "function" function_name ])


getCodeBlockCmd : (Result Http.Error CodeBlock -> msg) -> Model -> Cmd msg
getCodeBlockCmd getmsg model =
    let
        optreq =
            getCodeBlockUrl model
    in
    case optreq of
        Just req ->
            Http.get
                { url = req
                , expect = Http.expectJson getmsg decodeCodeBlock
                }

        Nothing ->
            Cmd.none


getListOfBlocksUrl : Model -> Maybe String
getListOfBlocksUrl model =
    model.selected_file
        |> Maybe.andThen
            (\selected_file ->
                Array.get selected_file model.all_files
            )
        |> Maybe.map (\filename -> UrlB.relative [ "code_blocks" ] [ UrlB.string "file" filename ])


getListOfBlocksCmd : (Result Http.Error (Array String) -> msg) -> Model -> Cmd msg
getListOfBlocksCmd getmsg model =
    let
        optreq =
            getListOfBlocksUrl model
    in
    case optreq of
        Just req ->
            Http.get
                { url = req
                , expect = Http.expectJson getmsg (D.array D.string)
                }

        Nothing ->
            Cmd.none



-- Maybe.andThen
--     (\i ->
--         Maybe.map
--             (\filename ->
--                 UrlB.relative [ "code_blocks" ]
--                     [ UrlB.string "file" filename, UrlB.string "function" (model.blocks) ]
--             )
--             Maybe.andThen (\filename -> Array.get model.blocks) (Array.get i model.all_files)
--     )
--     model.selected_file


getListOfFiles : String
getListOfFiles =
    "files"


getBestInputForCounter : Int -> String
getBestInputForCounter id =
    UrlB.relative [ "best_input" ] [ UrlB.int "counter" id ]
