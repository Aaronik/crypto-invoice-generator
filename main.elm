module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, classList, href, placeholder)
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


signout : Url.Url -> Cmd Msg
signout url =
    Nav.load (Url.toString url)



-- MAIN


main : Program Flags Model Msg
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


pages =
    { signin = "/signin"
    , invoices = "/invoices"
    , signout = "/signout"
    , home = "/"
    }


type alias Model =
    { key : Nav.Key
    , isSigningIn : Bool
    , isSigningUp : Bool
    , isSignedIn : Bool
    , username : String
    , password : String
    , page : String
    }


type Msg
    = SignIn
    | SignUp
    | SignInResult (Result Http.Error String)
    | UpdateUsername String
    | UpdatePassword String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


type alias Flags =
    { isSignedIn : Bool
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , isSigningIn = False
      , isSigningUp = False
      , isSignedIn = flags.isSignedIn
      , username = ""
      , password = ""
      , page = "/"
      }
    , if flags.isSignedIn then
        if url.path == pages.signin then
            Nav.pushUrl key pages.home

        else
            Nav.pushUrl key url.path

      else
        Nav.pushUrl key pages.signin
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
                    ( { model | isSigningIn = False, isSigningUp = False }, Cmd.none )

                Ok _ ->
                    ( { model | isSigningIn = False, isSigningUp = False, isSignedIn = True }, Nav.pushUrl model.key pages.invoices )

        UrlChanged url ->
            ( { model | page = url.path }, Cmd.none )

        -- TODO Working on signout button
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.path of
                        "/signout" ->
                            ( model, signout url )

                        _ ->
                            ( model, Nav.load url.path )

                Browser.External url ->
                    ( model, Nav.load url )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


title : Html Msg
title =
    div [ class "container" ]
        [ div [ class "hero has-text-centered level" ]
            [ h1 [ class "" ] [ text "Invoice Generator" ]
            ]
        ]


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


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "/" ] [ text "Invoice Generator" ]
            ]
        , div [ class "navbar-end" ]
            [ a [ class "navbar-item", href pages.signout ]
                [ text
                    (if model.isSignedIn then
                        "Sign Out"

                     else
                        ""
                    )
                ]
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


homePage : Model -> Browser.Document Msg
homePage model =
    { title = "Invoice Generator | Home"
    , body =
        [ section [ class "hero is-success is-fullheight" ]
            [ navbar model
            , title
            ]
        ]
    }


signinPage : Model -> Browser.Document Msg
signinPage model =
    { title = "Invoice Generator | Sign In"
    , body =
        [ section [ class "hero is-success is-fullheight" ]
            [ navbar model
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


invoicesPage : Model -> Browser.Document Msg
invoicesPage model =
    { title = "Invoice Generator | Invoices"
    , body =
        [ navbar model
        , div [] [ text "invoices" ]
        ]
    }


invoicePage : Model -> Browser.Document Msg
invoicePage model =
    { title = "Invoice Generator | Invoice"
    , body =
        [ navbar model
        , div [] [ text "invoice" ]
        ]
    }


view : Model -> Browser.Document Msg
view model =
    case model.page of
        "/" ->
            homePage model

        "/signin" ->
            signinPage model

        "/invoices" ->
            invoicesPage model

        _ ->
            invoicePage model
