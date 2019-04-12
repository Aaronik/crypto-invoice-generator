module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, placeholder)
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


searchBar : Html Msg
searchBar =
    nav [ class "level" ]
        [ div [ class "level-item" ]
            [ div [ class "field has-addons" ]
                [ p [ class "control" ]
                    [ input [ class "input", placeholder "Type stuff here" ] []
                    ]
                , p [ class "control" ]
                    [ button [ class "button" ] [ text "Do Stuff" ]
                    ]
                ]
            ]
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item" ] [ text "Invoice Generator" ]
            ]
        ]


primaryInput : String -> Html Msg
primaryInput pholder =
    div [ class "field" ]
        [ div [ class "control has-icons-left" ]
            [ input [ class "input is-primary is-large", placeholder pholder, autofocus True ] []
            , span [ class "icon is-large is-left" ]
                [ i [ class "fas fa-envelope fa-xl" ] []
                ]
            ]
        ]


signinPage : Html Msg
signinPage =
    section [ class "hero is-success is-fullheight" ]
        [ navbar
        , div [ class "hero-body" ]
            [ div [ class "container" ]
                [ primaryInput "Username"
                , primaryInput "Password"
                , div [ class "field is-grouped level" ]
                    [ p [ class "control level-item" ]
                        [ button [ class "button is-primary is-large is-inverted" ] [ text "Sign In" ]
                        ]
                    , p [ class "control level-item" ]
                        [ button [ class "button is-primary is-large is-inverted" ] [ text "Sign Up" ]
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    signinPage
