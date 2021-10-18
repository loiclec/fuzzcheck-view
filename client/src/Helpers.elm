module Helpers exposing (..)


prevInt : Maybe Int -> Maybe Int
prevInt x =
    Maybe.map
        (\y ->
            if y > 0 then
                y - 1

            else
                0
        )
        x


nextInt : Int -> Maybe Int -> Maybe Int
nextInt max x =
    Maybe.map
        (\y ->
            if y < max - 1 then
                y + 1

            else
                y
        )
        x
