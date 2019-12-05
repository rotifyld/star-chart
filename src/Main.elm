module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, text)
import Html.Events exposing (onClick)
import Projection
import Stars exposing (..)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width)
import Utils exposing (mod2pi)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { text : String
    , stars : List Star
    , ra : Float
    , dec : Float
    }


init : Model
init =
    { text = Stars.getString
    , stars = Stars.getList
    , ra = 0
    , dec = 0
    }



-- UPDATE


type Msg
    = RAPlus
    | RAMinus
    | DecPlus
    | DecMinus


update : Msg -> Model -> Model
update msg model =
    case msg of
        RAPlus ->
            { model | ra = mod2pi (model.ra + 0.2) }

        RAMinus ->
            { model | ra = mod2pi (model.ra - 0.2) }

        DecPlus ->
            { model | dec = min (pi / 2) (model.dec + 0.2) }

        DecMinus ->
            { model | dec = max (pi / -2) (model.dec - 0.2) }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RAPlus ] [ text "RA+" ]
        , button [ onClick RAMinus ] [ text "RA-" ]
        , button [ onClick DecPlus ] [ text "Dec+" ]
        , button [ onClick DecMinus ] [ text "Dec-" ]
        , text (String.fromFloat model.ra)
        , text (String.fromFloat model.dec)
        , svg
            [ viewBox "0 0 10000 10000", fill "black" ]
            (rect [ width "100%", height "100%", fill "black" ] []
                :: circle [ cx "5000", cy "5000", r "4000", stroke "white", strokeWidth "2", fill "none" ] []
                :: (model.stars
                        |> List.filter (\star -> Projection.visibleStars ( model.ra, model.dec ) star.radialPos)
                        |> List.map (Stars.projected <| Projection.stereographicFrom <| ( model.ra, model.dec ))
                        |> List.map
                            (\( star, ( x, y ) ) ->
                                circle
                                    [ cx (String.fromFloat (5000 + x * 4000))
                                    , cy (String.fromFloat (5000 - y * 4000))

                                    {- 6 mag: r = 1; -1 mag: r = 20 -}
                                    , r (String.fromFloat (18 - (19 / 7) * star.mag))
                                    , fill "white"
                                    ]
                                    []
                            )
                   )
            )
        ]
