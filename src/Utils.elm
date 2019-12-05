module Utils exposing (..)


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.map2 Tuple.pair xs ys


clamp : Float -> Float -> Float -> Float
clamp lowerBound upperBound num =
    if lowerBound < num then
        if num < upperBound then
            num

        else
            upperBound

    else
        lowerBound


mod2pi : Float -> Float
mod2pi float =
    if float > 2 * pi then
        float - (2 * pi)

    else if float < 0 then
        float + (2 * pi)

    else
        float
