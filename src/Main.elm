module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, canvas, div, input, pre, text)
import Projection
import Stars exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { text : String
    , stars : List Star
    }


init : Model
init =
    { text = Stars.getString
    , stars = Stars.getList
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            model



-- VIEW


canvasSize =
    ( 500, 500 )


view : Model -> Html Msg
view model =
    svg
        [ viewBox "0 0 10000 10000", fill "black" ]
        (rect [ width "500%", height "100%", fill "black" ] []
            :: (model.stars
                    |> List.filter (\star -> star.ra < (pi / 2) || star.ra > (3 * pi / 2) {- && star.dec < 0 -})
                    |> List.map (\star -> ( Projection.stereographic star, star.mag ))
                    |> List.map
                        (\( ( x, y ), mag ) ->
                            circle
                                [ cx (String.fromFloat (5000 + x * 4000))
                                , cy (String.fromFloat (5000 - y * 4000))

                                {- 6 mag: r = 1; -1 mag: r = 20 -}
                                , r (String.fromFloat (18 - (19 / 7) * mag))
                                , fill "white"
                                ]
                                []
                        )
               )
        )
