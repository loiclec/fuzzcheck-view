module CoverageTests exposing (..)

import Coverage exposing (..)
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
                            """
                            {
                            "title": "_RNvCs9lNSP8NNRqe_10playground5hello",
                            "file": "src/lib.rs",
                            "lines": [
                                {
                                    "lineno": 1,
                                    "spans": [
                                        {
                                        "text": "fn hello(xs: &[u8]) -> bool {",
                                        "kind": "Untracked"
                                        }
                                    ]
                                }
                            ]
                            }
                            """
                    in
                    Expect.equal (D.decodeString Coverage.decodeCodeBlock json) (Ok (CodeBlock "_RNvCs9lNSP8NNRqe_10playground5hello" "src/lib.rs" [ CodeLine 1 [ CodeSpan "fn hello(xs: &[u8]) -> bool {" Untracked ] ]))
            ]
        ]
