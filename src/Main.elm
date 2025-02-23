module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { message : String
    }


init : Model
init =
    { message = "Countdown!" }


type alias Msg =
    ()


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view model =
    layout [] <| el [] <| text model.message
