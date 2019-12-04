module Stars exposing (Star, getList, getString)

import Csv exposing (Csv)
import RawData exposing (..)


type alias Star =
    { id : Int
    , bayer : Maybe String
    , proper : Maybe String
    , ra : Float
    , dec : Float
    , mag : Float
    }


csv : Csv
csv =
    Csv.parse nakedData


nonEmpty : String -> Maybe String
nonEmpty str =
    case str of
        "" ->
            Nothing

        _ ->
            Just str


starDecoder : List String -> Maybe Star
starDecoder record =
    case record of
        _ :: id :: hip :: hd :: hr :: gl :: bf :: proper :: ra :: dec :: dist :: pmra :: pmdec :: rv :: mag :: absmag :: spect :: ci :: x :: y :: z :: vx :: vy :: vz :: rarad :: decrad :: pmrarad :: pmdecrad :: bayer :: flam :: con :: comp :: comp_primary :: base :: lum :: var :: var_min :: var_max ->
            Star
                (Maybe.withDefault -1 (String.toInt id))
                (nonEmpty bf)
                (nonEmpty proper)
                (Maybe.withDefault -1 (String.toFloat rarad))
                (Maybe.withDefault -1 (String.toFloat decrad))
                (Maybe.withDefault -1 (String.toFloat mag))
                |> Just

        _ ->
            Nothing


maybeToString : Maybe String -> String
maybeToString maybe =
    case maybe of
        Nothing ->
            "---"

        Just str ->
            str


toString : List Star -> String
toString stars =
    stars
        |> List.map
            (\s ->
                String.concat
                    [ "{"
                    , maybeToString s.proper
                    , ", "
                    , String.fromFloat s.ra
                    , ", "
                    , String.fromFloat s.dec
                    , "}"
                    ]
            )
        |> String.join "\n"


getList : List Star
getList =
    csv.records
        |> List.map starDecoder
        |> List.filterMap identity


getString : String
getString =
    toString getList
