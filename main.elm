module Main exposing (main)

import Browser
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, classList, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Json.Encode as Encode
import Url



-- HELPERS


usernamePasswordJsonEncoder : String -> String -> Encode.Value
usernamePasswordJsonEncoder username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]


signin : String -> String -> Cmd Msg
signin username password =
    Http.post
        { url = "/signin"
        , body = Http.jsonBody (usernamePasswordJsonEncoder username password)
        , expect = Http.expectString SignInResult
        }


signup : String -> String -> Cmd Msg
signup username password =
    Http.post
        { url = "/signup"
        , body = Http.jsonBody (usernamePasswordJsonEncoder username password)
        , expect = Http.expectString SignInResult
        }



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { isSigningIn : Bool
    , isSigningUp : Bool
    , username : String
    , password : String
    }


type Msg
    = SignIn
    | SignUp
    | SignInResult (Result Http.Error String)
    | UpdateUsername String
    | UpdatePassword String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { isSigningIn = False
      , isSigningUp = False
      , username = ""
      , password = ""
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        SignIn ->
            ( { model | isSigningIn = True }, signin model.username model.password )

        SignUp ->
            ( { model | isSigningUp = True }, signup model.username model.password )

        SignInResult result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
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


primaryInput : String -> (String -> Msg) -> Html Msg
primaryInput pholder msg =
    div [ class "field" ]
        [ div [ class "control has-icons-left" ]
            [ input [ class "input is-primary is-large", placeholder pholder, autofocus True, onInput msg ] []
            , span [ class "icon is-large is-left" ]
                [ i [ class "fas fa-envelope fa-xl" ] []
                ]
            ]
        ]


signinPage : Model -> Browser.Document Msg
signinPage model =
    { title = "Invoice Generator | Sign In"
    , body =
        [ section [ class "hero is-success is-fullheight" ]
            [ navbar
            , div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ primaryInput "Username" UpdateUsername
                    , primaryInput "Password" UpdatePassword
                    , div [ class "field is-grouped level" ]
                        [ p [ class "control level-item" ]
                            [ button
                                [ class "button is-primary is-large"
                                , classList [ ( "is-loading", model.isSigningIn ), ( "is-inverted", not model.isSigningIn ) ]
                                , onClick SignIn
                                ]
                                [ text "Sign In" ]
                            ]
                        , p [ class "control level-item" ]
                            [ button
                                [ class "button is-primary is-large"
                                , classList [ ( "is-loading", model.isSigningUp ), ( "is-inverted", not model.isSigningUp ) ]
                                , onClick SignUp
                                ]
                                [ text "Sign Up" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


view : Model -> Browser.Document Msg
view model =
    signinPage model
