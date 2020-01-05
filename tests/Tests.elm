module Tests exposing (..)

import Expect exposing (Expectation)
import String.Interpolate exposing (interpolate)
import Test exposing (..)


suite : Test
suite =
    test "interpolation" <|
        \() ->
            Dict.fromList [ ("greet","hello"), ("punct","!!"), ("who", "world") ]
                |> interpolate "{greet} {who} {punct}"
                |> Expect.equal "hello world !!"
