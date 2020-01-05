module Url.Interpolate exposing (interpolate)

{-| Url.Interpolate provides a single function, `interpolate`, which takes
a [URI Template][rfc6570] string and a Dict of variables, and expands
the input string according to the rules in [IETF RFC 6570][rfc6570],
up to Level 3 (Level 4 compliance is not provided or planned).

PLEASE NOTE: not yet formally tested, use at your own risk.

@docs interpolate

[rfc6570]: https://tools.ietf.org/html/rfc6570
-}

import Dict exposing (Dict)
import Maybe
import Regex exposing (Regex, Match)
import Set exposing (Set)
import Url

{-| Example URI template interpolation:

interpolate "http://example.com/{path}{?x,y,empty}" <| 
    Dict.fromList [("path", "hello"), ("x", "1024"), ("y", "768")]

> "http://example.com/hello?x=1024&y=768&empty="
-}
interpolate : String -> Dict String String -> String
interpolate string args =
    Regex.replace interpolationRegex (applyInterpolation args) string


interpolationRegex : Regex
interpolationRegex =
    "\\{([+#.\\/;?&]{0,1})([A-Za-z0-9_,%]+)\\}" 
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


applyInterpolation : Dict String String -> Match -> String
applyInterpolation replacements { match, submatches } =
    submatches
        |> getTemplateParts
        |> Maybe.map (\(operator, vars) -> expand operator vars replacements)
        |> Maybe.withDefault match

getTemplateParts : List (Maybe String) -> Maybe (String, List String)
getTemplateParts submatches =
    case submatches of
        [ Just operator, Just expression ] ->
            Just (operator, String.split "," expression)
        [ Nothing, Just expression ] ->
            Just ("", String.split "," expression)
        _ ->
            Nothing

expand : String -> List String -> Dict String String -> String
expand operator vars replacements =
    case operator of
        "" ->
            expandSimple vars replacements
        "+" ->
            expandReservedString vars replacements
        "#" ->
            expandFragment vars replacements
        "." ->
            expandLabel vars replacements
        "/" ->
            expandPath vars replacements
        ";" ->
            expandPathParam vars replacements
        "?" ->
            expandQuery vars replacements
        "&" -> 
            expandQueryContinuation vars replacements
        _ ->
            ""  -- Never should get here due to regex

expandSimple : List String -> Dict String String -> String
expandSimple =
    expandUnreservedStringSeparatedBy ","

expandReservedString : List String -> Dict String String -> String
expandReservedString =
    expandReservedStringSeparatedBy ","

expandFragment : List String -> Dict String String -> String
expandFragment vars replacements =
     "#" ++ (expandReservedStringSeparatedBy "," vars replacements)

expandLabel : List String -> Dict String String -> String
expandLabel vars replacements =
     "." ++ (expandUnreservedStringSeparatedBy "," vars replacements)

expandPath : List String -> Dict String String -> String
expandPath vars replacements =
     "/" ++ (expandUnreservedStringSeparatedBy "/" vars replacements)


expandPathParam : List String -> Dict String String -> String
expandPathParam vars replacements =
    vars
        |> List.map (\var -> 
            Dict.get var replacements 
                |> Maybe.withDefault ""
                |> (\val -> (var, percentEncodeWithUnreserved val))
            )
        |> (\pairs -> 
            ";" ++ 
                ( pairs
                    |> List.map (\(var,val) -> 
                        if String.isEmpty val then
                            var
                        else
                            var ++ "=" ++ val
                        )
                    |> String.join ";"
                )
            )


expandQuery : List String -> Dict String String -> String
expandQuery =
    expandQueryHelp "?"

expandQueryContinuation : List String -> Dict String String -> String
expandQueryContinuation =
    expandQueryHelp "&"


-- HELPERS

expandQueryHelp : String -> List String -> Dict String String -> String
expandQueryHelp prefix vars replacements =
    vars
        |> List.map (\var -> 
            Dict.get var replacements 
                |> Maybe.withDefault ""
                |> (\val -> (var, percentEncodeWithUnreserved val))
            )
        |> (\pairs -> 
            prefix ++ 
                ( pairs
                    |> List.map (\(var,val) -> var ++ "=" ++ val)
                    |> String.join "&"
                )
            )

expandUnreservedStringSeparatedBy : String -> List String -> Dict String String -> String
expandUnreservedStringSeparatedBy sep vars replacements =
    vars
        |> List.map (\var -> 
            Dict.get var replacements 
                |> Maybe.map percentEncodeWithUnreserved
                |> Maybe.withDefault ""
            )
        |> String.join sep

expandReservedStringSeparatedBy : String -> List String -> Dict String String -> String
expandReservedStringSeparatedBy sep vars replacements =
    vars
        |> List.map (\var -> 
            Dict.get var replacements 
                |> Maybe.map percentEncodeWithReserved
                |> Maybe.withDefault ""
            )
        |> String.join sep

unreservedChars : Set Char
unreservedChars =
    Set.fromList
        [ '-', '.', '_', '~' ]

reservedChars : Set Char
reservedChars =
    Set.fromList
        [ ':', '/', '?', '#', '[', ']', '@', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=' ]

percentEncodeWithUnreserved : String -> String
percentEncodeWithUnreserved =
    percentEncodeExcept unreservedChars

percentEncodeWithReserved : String -> String
percentEncodeWithReserved =
    percentEncodeExcept (Set.union unreservedChars reservedChars)

percentEncodeExcept : Set Char -> String -> String
percentEncodeExcept exceptChars string =
    let
        encodeChar c strs =
            if Set.member c exceptChars then 
                ((String.fromChar c) :: strs) 
            else 
                ((Url.percentEncode (String.fromChar c)) :: strs)
    in
        String.foldr encodeChar [] string
            |> String.join ""


