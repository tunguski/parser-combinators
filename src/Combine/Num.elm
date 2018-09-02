module Combine.Num exposing (sign, digit, int, float)

{-| This module contains Parsers specific to parsing numbers.


# Parsers

@docs sign, digit, int, float

-}

import Char
import Combine exposing (..)
import Combine.Char
import Debug exposing (..)
import String


toInt : String -> Int
toInt =
    String.toInt
        >> (\mi ->
                case mi of
                    Just i ->
                        i

                    _ ->
                        0
           )


toFloat : String -> Float
toFloat =
    String.toFloat
        >> (\mf ->
                case mf of
                    Just f ->
                        f

                    _ ->
                        0.0
           )


{-| Parse a numeric sign, returning `1` for positive numbers and `-1`
for negative numbers.
-}
sign : Parser s Int
sign =
    optional 1
        (choice
            [ map (\_ -> 1) (string "+")
            , map (\_ -> -1) (string "-")
            ]
        )


{-| Parse a digit.
-}
digit : Parser s Int
digit =
    let
        toDigit c =
            Char.toCode c - Char.toCode '0'
    in
    map toDigit Combine.Char.digit
        |> mapError (always [ "expected a digit" ])


{-| Flip the order of the first two arguments to a function.
-}
flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


joinBoth : Parser s (a -> b) -> Parser s a -> Parser s b
joinBoth =
    \b a -> andMap a b


{-| Parse an integer.
-}
int : Parser s Int
int =
    joinBoth
        (map (*) sign)
        (map toInt (regex "(0|[1-9][0-9]*)"))
        |> mapError (always [ "expected an integer" ])


{-| Parse a float.
-}
float : Parser s Float
float =
    joinBoth
        (map ((*) << Basics.toFloat) sign)
        (map toFloat (regex "(0|[1-9][0-9]*)(\\.[0-9]+)"))
        |> mapError (always [ "expected a float" ])
