module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
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


invoiceJsonEncoder : Invoice -> Encode.Value
invoiceJsonEncoder invoice =
    Encode.object
        [ ( "id", Encode.string invoice.id )
        , ( "username", Encode.string invoice.username )
        , ( "date", Encode.string invoice.date )
        , ( "total", Encode.float invoice.total )
        , ( "paid", Encode.float invoice.paid )
        , ( "to", Encode.string invoice.to )
        , ( "from", Encode.string invoice.from )
        , ( "address", Encode.string invoice.address )
        , ( "description", Encode.string invoice.description )
        ]


defaultInvoice : Invoice
defaultInvoice =
    { id = "loading..."
    , username = "loading..."
    , date = "loading..."
    , total = 0
    , paid = 0
    , to = "loading..."
    , from = "loading..."
    , address = "loading..."
    , description = "loading..."
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


updateInvoice : Invoice -> Cmd Msg
updateInvoice invoice =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/update"
        , body = Http.jsonBody (invoiceJsonEncoder invoice)
        , expect = Http.expectJson UpdateInvoiceResult invoiceJsonDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getInvoice : Model -> String -> Invoice
getInvoice model id =
    let
        invoice =
            List.filter (\i -> i.id == id) model.invoices |> List.head
    in
    Maybe.withDefault defaultInvoice invoice



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
    , isUpdatingInvoice : Bool
    , isSignedIn : Bool
    , username : String
    , password : String
    , route : Route
    , invoices : List Invoice
    }


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


type Msg
    = SignIn
    | SignUp
    | SignInResult (Result Http.Error String)
    | UpdateUsername String
    | UpdatePassword String
    | UpdateInvoiceTotal String Float
    | UpdateInvoiceTo String String
    | UpdateInvoiceFrom String String
    | UpdateInvoiceDescritpion String String
    | UpdateInvoice Invoice
    | UpdateInvoiceResult (Result Http.Error Invoice)
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
      , isUpdatingInvoice = False
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
                    ( { model | invoices = invoice :: model.invoices }, Nav.pushUrl model.key ("/invoices/" ++ invoice.id) )

        FetchInvoicesResult result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok invoices ->
                    ( { model | invoices = invoices }, Cmd.none )

        UpdateInvoiceTo id to ->
            let
                invoice =
                    getInvoice model id

                invoices =
                    List.filter (\i -> i.id /= id) model.invoices
            in
            ( { model | invoices = { invoice | to = to } :: invoices }, Cmd.none )

        UpdateInvoiceFrom id from ->
            let
                invoice =
                    getInvoice model id

                invoices =
                    List.filter (\i -> i.id /= id) model.invoices
            in
            ( { model | invoices = { invoice | from = from } :: invoices }, Cmd.none )

        UpdateInvoiceTotal id total ->
            let
                invoice =
                    getInvoice model id

                invoices =
                    List.filter (\i -> i.id /= id) model.invoices
            in
            ( { model | invoices = { invoice | total = total } :: invoices }, Cmd.none )

        UpdateInvoiceDescritpion id description ->
            let
                invoice =
                    getInvoice model id

                invoices =
                    List.filter (\i -> i.id /= id) model.invoices
            in
            ( { model | invoices = { invoice | description = description } :: invoices }, Cmd.none )

        UpdateInvoice invoice ->
            ( { model | isUpdatingInvoice = True }, updateInvoice invoice )

        UpdateInvoiceResult result ->
            case result of
                Ok _ ->
                    ( { model | isUpdatingInvoice = False }, Nav.pushUrl model.key "/invoices" )

                Err _ ->
                    ( model, Cmd.none )

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

                InvoiceRoute _ ->
                    ( { model | route = route }, fetchInvoices )

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


navbar : Model -> Html Msg
navbar model =
    if model.isSignedIn then
        nav [ class "navbar is-primary" ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "/" ] [ text "Invoice Generator" ]
                ]
            , div [ class "navbar-menu" ]
                [ div [ class "navbar-start" ]
                    [ a [ class "navbar-item", href "/invoices" ] [ text "Invoices" ]
                    , a [ class "navbar-item", href "/wallets" ] [ text "Wallets" ]
                    ]
                ]
            , div [ class "navbar-end" ]
                [ a [ class "navbar-item", href "/signout" ] [ text "Sign Out" ]
                ]
            ]

    else
        nav [ class "navbar is-primary" ]
            [ div [ class "navbar-end" ]
                [ a [ class "navbar-item", href "/signin" ] [ text "Sign In" ]
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
        [ navbar model
        , section [ class "hero is-success is-fullheight" ]
            [ div [ class "container has-text-centered" ]
                [ h1 [ class "title" ] [ text "Invoice Generator" ]
                ]
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
    if List.isEmpty invoices then
        div [ class "section" ]
            [ div [ class "container" ]
                [ article [ class "message is-info" ]
                    [ div [ class "message-header" ]
                        [ p [] [ text "No Invoices!" ]
                        ]
                    , div [ class "message-body" ] [ text "Click the button below to create one :)" ]
                    ]
                ]
            ]

    else
        div [ class "section" ]
            [ div [ class "container" ]
                [ aside [ class "menu" ]
                    [ p [ class "menu-label" ] [ text "Your Invoices" ]
                    , ul [ class "menu-list" ]
                        (List.map
                            (\i ->
                                li []
                                    [ a [ href ("/invoices/" ++ i.id) ]
                                        [ span [] [ text (i.date ++ " to: " ++ i.to) ]
                                        ]
                                    ]
                            )
                            invoices
                        )
                    ]
                ]
            ]


invoicesPage : Model -> Browser.Document Msg
invoicesPage model =
    { title = "Invoice Generator | Invoices"
    , body =
        [ navbar model
        , invoicesToUl model.invoices
        , div [ class "section" ]
            [ div [ class "container" ]
                [ button [ class "button is-info", onClick CreateInvoice ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-plus" ] []
                        ]
                    , span [] [ text "Create Invoice!" ]
                    ]
                ]
            ]
        ]
    }


onUpdateTotal : String -> String -> Msg
onUpdateTotal id string =
    UpdateInvoiceTotal id (Maybe.withDefault 0 (String.toFloat string))


invoicePage : Model -> String -> Browser.Document Msg
invoicePage model id =
    let
        invoice =
            getInvoice model id
    in
    { title = "Invoice Generator | Invoice"
    , body =
        [ navbar model
        , section [ class "section" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text invoice.date ]
                , h3 [ class "subtitle" ] [ text invoice.id ]
                , div [ class "level" ]
                    [ div [ class "level-left" ]
                        [ span [ class "level-item" ] [ text "to:" ]
                        , textarea [ class "level-item textarea", value invoice.to, placeholder "For whom is this invoice destined?", onInput (UpdateInvoiceTo id) ] []
                        ]
                    , div [ class "level-right" ]
                        [ span [ class "level-item" ] [ text "from:" ]
                        , textarea [ class "level-item textarea", value invoice.from, placeholder "Who gets the money money?", onInput (UpdateInvoiceFrom id) ] []
                        ]
                    ]
                , div [ class "level" ]
                    [ span [ class "level-item" ] [ text "description:" ]
                    , textarea [ class "level-item textarea", value invoice.description, placeholder "Describe the work done, etc.", onInput (UpdateInvoiceDescritpion id) ] []
                    ]
                , div [ class "level" ]
                    [ span [ class "level-item" ] [ text "wallet address:" ]
                    , span [ class "level-item" ] [ text invoice.address ]
                    ]
                , div [ class "level" ]
                    [ div [ class "level-left" ]
                        [ span [ class "level-item" ] [ text "total (in wei):" ]
                        , input [ class "level-item input", String.fromFloat invoice.total |> value, placeholder "How many money money?", onInput (onUpdateTotal id) ] []
                        ]
                    , div [ class "level-right" ]
                        [ span [ class "level-item" ] [ text "paid:" ]
                        , code [ class "level-item" ] [ String.fromFloat invoice.paid |> text ]
                        ]
                    ]
                , button [ class "button is-info", classList [ ( "is-loading", model.isUpdatingInvoice ) ], onClick (UpdateInvoice invoice) ] [ text "Update Invoice!" ]
                ]
            ]
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
    -- TODO Need a wallets page
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
