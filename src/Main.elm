module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, canvas, div, input, pre, text)
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
        [ viewBox "0 0 500 500", fill "black" ]
        (rect [ width "500%", height "100%", fill "black" ] []
            :: List.map (\s -> circle [ cx (String.fromFloat (s.ra * 10)), cy (String.fromFloat (s.dec * 3)), r "1", fill "white" ] []) model.stars
        )
