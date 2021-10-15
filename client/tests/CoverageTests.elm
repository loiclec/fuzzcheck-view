module CoverageTests exposing (..)

import Coverage exposing (CodeBlock, CodeSpan, CodeSpanKind(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


decode : Test
decode =
    describe "Decoding in the Coverage module"
        [ describe "CodeBlock"
            [ test "Basic decoding" <|
                \_ ->
                    let
                        json =
                            "{ \"title\": \"the_title\", \"content\": [{\n      \"text\": \"3..=6\",\n      \"kind\": \"Untracked\"\n    }] }"
                    in
                    Expect.equal (D.decodeString Coverage.decodeCodeBlock json) (Ok (CodeBlock "the_title" [ CodeSpan "3..=6" Untracked ]))
            ]
        ]
