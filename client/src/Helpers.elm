module Helpers exposing (..)


prevOptInt : Maybe Int -> Int
prevOptInt x =
    case x of
        Nothing ->
            0

        Just i ->
            if i > 0 then
                i - 1

            else
                0


nextOptInt : Maybe Int -> Int -> Int
nextOptInt x max =
    case x of
        Nothing ->
            0

        Just i ->
            if i < max - 1 then
                i + 1

            else
                i


prevInt : Int -> Int
prevInt y =
    if y > 0 then
        y - 1

    else
        0


nextInt : Int -> Int -> Int
nextInt max y =
    if y < max - 1 then
        y + 1

    else
        y
