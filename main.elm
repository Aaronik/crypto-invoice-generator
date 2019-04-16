module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, classList, href, placeholder, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)



-- HELPERS


usernamePasswordJsonEncoder : String -> String -> Encode.Value
usernamePasswordJsonEncoder username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]


type alias Invoice =
    { id : String
    , username : String
    , date : String
    , total : Float
    , paid : Float
    , to : String
    , from : String
    , address : String
    , description : String
    }


invoiceJsonDecoder : Decode.Decoder Invoice
invoiceJsonDecoder =
    Decode.succeed Invoice
        |> required "id" Decode.string
        |> required "username" Decode.string
        |> required "date" Decode.string
        |> required "total" Decode.float
        |> required "paid" Decode.float
        |> required "to" Decode.string
        |> required "from" Decode.string
        |> required "address" Decode.string
        |> required "description" Decode.string


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


createInvoice : Cmd Msg
createInvoice =
    Http.post
        { url = "/create"
        , body = Http.jsonBody (Encode.object [])
        , expect = Http.expectJson CreateInvoiceResult invoiceJsonDecoder
        }


fetchInvoices : Cmd Msg
fetchInvoices =
    Http.get
        { url = "/invoices"
        , expect = Http.expectJson FetchInvoicesResult (Decode.list invoiceJsonDecoder)
        }



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


type Route
    = HomeRoute
    | SignInRoute
    | InvoicesRoute
    | InvoiceRoute String
    | SignOutRoute
    | NotFoundRoute


type alias Model =
    { key : Nav.Key
    , isSigningIn : Bool
    , isSigningUp : Bool
    , isSignedIn : Bool
    , username : String
    , password : String
    , route : Route
    , invoices : List Invoice
    }


type Msg
    = SignIn
    | SignUp
    | SignInResult (Result Http.Error String)
    | UpdateUsername String
    | UpdatePassword String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | CreateInvoice
    | CreateInvoiceResult (Result Http.Error Invoice)
    | FetchInvoicesResult (Result Http.Error (List Invoice))


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
      , route = HomeRoute
      , invoices = []
      }
    , if flags.isSignedIn then
        if url.path == "/signin" then
            Nav.pushUrl key "/"

        else
            Nav.pushUrl key url.path

      else
        Nav.pushUrl key "/signin"
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
                    ( { model | isSigningIn = False, isSigningUp = False, isSignedIn = True }, Nav.pushUrl model.key "/invoices" )

        CreateInvoice ->
            ( model, createInvoice )

        CreateInvoiceResult result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok invoice ->
                    ( { model | invoices = invoice :: model.invoices }, Cmd.none )

        FetchInvoicesResult result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok invoices ->
                    ( { model | invoices = invoices }, Cmd.none )

        UrlChanged url ->
            let
                route =
                    toRoute (Url.toString url)
            in
            case route of
                InvoicesRoute ->
                    let
                        cmd =
                            if model.isSignedIn then
                                fetchInvoices

                            else
                                Nav.pushUrl model.key "/signin"
                    in
                    ( { model | route = route }, cmd )

                _ ->
                    ( { model | route = route }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.path of
                        "/signout" ->
                            ( model, signout url )

                        _ ->
                            ( model, Nav.pushUrl model.key url.path )

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
            [ a [ class "navbar-item", href "/signout" ]
                [ text
                    (if model.isSignedIn then
                        "Sign Out"

                     else
                        ""
                    )
                ]
            ]
        ]


primaryInput : String -> String -> String -> (String -> Msg) -> Html Msg
primaryInput pholder icon attType msg =
    div [ class "field" ]
        [ div [ class "control has-icons-left" ]
            [ input [ class "input is-primary is-large", placeholder pholder, autofocus True, onInput msg, type_ attType ] []
            , span [ class "icon is-large is-left" ]
                [ i [ class ("fas fa-xl fa-" ++ icon) ] []
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
            , a [ class "is-link", href "/invoices" ] [ text "invoices" ]
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
                    [ primaryInput "Username" "user" "" UpdateUsername
                    , primaryInput "Password" "lock" "password" UpdatePassword
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


invoicesToUl : List Invoice -> Html Msg
invoicesToUl invoices =
    ul []
        (List.map
            (\i ->
                li []
                    [ a [ href ("/invoices/" ++ i.id) ]
                        [ h3 [] [ text i.date ]
                        ]
                    , span [] [ text i.from ]
                    ]
            )
            invoices
        )


invoicesPage : Model -> Browser.Document Msg
invoicesPage model =
    { title = "Invoice Generator | Invoices"
    , body =
        [ navbar model
        , h1 [] [ text "invoices" ]
        , invoicesToUl model.invoices
        , button [ class "button is-success", onClick CreateInvoice ]
            [ span [ class "icon" ]
                [ i [ class "fas fa-plus" ] []
                ]
            , span [] [ text "Create Invoice!" ]
            ]
        ]
    }


invoicePage : Model -> String -> Browser.Document Msg
invoicePage model id =
    { title = "Invoice Generator | Invoice"
    , body =
        [ navbar model
        , div [] [ text ("invoice " ++ id) ]
        ]
    }


notFound : Model -> Browser.Document Msg
notFound model =
    { title = "Invoice Generator | 404"
    , body =
        [ navbar model
        , div [] [ text "Not Found!" ]
        ]
    }


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map HomeRoute top
        , map SignInRoute (s "signin")
        , map InvoicesRoute (s "invoices")
        , map InvoiceRoute (s "invoices" </> string)
        , map SignOutRoute (s "signout")
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFoundRoute

        Just url ->
            Maybe.withDefault NotFoundRoute (parse routeParser url)


view : Model -> Browser.Document Msg
view model =
    -- TODO for a cleaner, elmier way of routing here, use types:
    -- https://elmprogramming.com/routing.html
    case model.route of
        HomeRoute ->
            homePage model

        SignInRoute ->
            signinPage model

        SignOutRoute ->
            homePage model

        InvoicesRoute ->
            invoicesPage model

        InvoiceRoute id ->
            invoicePage model id

        NotFoundRoute ->
            notFound model
