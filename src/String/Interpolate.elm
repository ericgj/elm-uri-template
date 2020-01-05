module String.Interpolate exposing (interpolate)

{-| String.Interpolate provides a convenient method `interpolate` for injecting
values into a string. This can be useful for i18n of apps and construction of
complex strings in views.

@docs interpolate

-}

import Dict exposing (Dict)
import Maybe
-- import Maybe exposing (andThen, withDefault)
import Regex exposing (Regex, Match)
-- import Regex exposing (Match, Regex, fromString, never, replace)
import String exposing (dropLeft, dropRight, toInt)


{-| Inject other strings into a string in the order they appear in a List
interpolate "{greet} {exclaim} {who}" <| 
    Dict.fromList [("greet", "hello"), ("exclaim", "!!"), ("who", "world")]
-}
interpolate : String -> Dict String String -> String
interpolate string args =
    Regex.replace interpolationRegex (applyInterpolation args) string


interpolationRegex : Regex
interpolationRegex =
    Regex.fromString "\\{\\d+\\}" |> Maybe.withDefault Regex.never


applyInterpolation : Dict String String -> Match -> String
applyInterpolation replacements { match } =
    let
        key =
            (dropLeft 1 << dropRight 1) match
    in
    key
        |> (\k -> Dict.get k replacements)
        |> Maybe.withDefault match

