module Tests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Test exposing (..)
import Url.Interpolate exposing (interpolate)


type alias Cases =
    { data : Dict String String
    , expectations : List ( String, String )
    }


{-| Note: these are taken from the [test suite][test-suite], with a few extras.

[test-suite]: https://github.com/uri-templates/uritemplate-test/blob/master/spec-examples.json

-}
level3 : Cases
level3 =
    { data =
        Dict.fromList
            [ ( "var", "value" )
            , ( "hello", "Hello World!" )
            , ( "empty", "" )
            , ( "path", "/foo/bar" )
            , ( "x", "1024" )
            , ( "x%10", "2048" )
            , ( "y", "768" )
            ]
    , expectations =
        [ ( "map?{x,y}", "map?1024,768" )
        , ( "{x,hello,y}", "1024,Hello%20World%21,768" )
        , ( "{+x,hello,y}", "1024,Hello%20World!,768" )
        , ( "{+path,x}/here", "/foo/bar,1024/here" )
        , ( "{#x,hello,y}", "#1024,Hello%20World!,768" )
        , ( "{#path,x}/here", "#/foo/bar,1024/here" )
        , ( "X{.var}", "X.value" )
        , ( "X{.x,y}", "X.1024.768" )
        , ( "{/var}", "/value" )
        , ( "{/var,x}/here", "/value/1024/here" )
        , ( "{;x,y}", ";x=1024;y=768" )
        , ( "{;x,y,empty}", ";x=1024;y=768;empty" )
        , ( "{?x,y}", "?x=1024&y=768" )
        , ( "{?x,y,empty}", "?x=1024&y=768&empty=" )
        , ( "?fixed=yes{&x}", "?fixed=yes&x=1024" )
        , ( "{&x,y,empty}", "&x=1024&y=768&empty=" )
        , ( "{&x,y,undefn}", "&x=1024&y=768&undefn=" )
        , ( "{;x%10,y}", ";x%10=2048;y=768" )
        , ( "{?x%10,y}", "?x%10=2048&y=768" )
        , ( "{&x%10,y}", "&x%10=2048&y=768" )
        ]
    }


suite : Test
suite =
    describe "Level 3" <|
        List.map
            (\( template, exp ) ->
                test template <|
                    \() ->
                        level3.data
                            |> interpolate template
                            |> Expect.equal exp
            )
            level3.expectations
