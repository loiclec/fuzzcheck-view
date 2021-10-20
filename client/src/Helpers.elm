module Helpers exposing (..)


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
