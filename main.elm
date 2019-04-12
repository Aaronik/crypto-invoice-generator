module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Json.Encode as Encode



-- HELPERS
-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


type Msg
    = None



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ] [ text "Hello, world" ]
