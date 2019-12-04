module Projection exposing (..)

import Stars exposing (Star)


stereographic : Star -> ( Float, Float )
stereographic star =
    let
        x =
            cos star.dec * sin star.ra

        y =
            sin star.dec

        z_ =
            1 + cos star.ra * cos star.dec
    in
    ( x / z_, y / z_ )
