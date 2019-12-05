module Projection exposing (..)


type alias Projection =
    ( Float, Float ) -> ( Float, Float )


visibleStars : ( Float, Float ) -> ( Float, Float ) -> Bool
visibleStars ( modelRa, modelDec ) ( starRa, starDec ) =
    --    TODO
    True


stereographic : ( Float, Float ) -> ( Float, Float )
stereographic ( dec, ra ) =
    let
        x =
            cos dec * sin ra

        y =
            sin dec

        z_ =
            1 + cos ra * cos dec
    in
    ( x / z_, y / z_ )


stereographicFrom : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
stereographicFrom ( decView, raView ) ( decStar, raStar ) =
    stereographic ( raStar - raView, decStar - decView )
