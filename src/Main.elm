-- Main.elm
port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Route exposing (Route(..))
import Url exposing (Url)
import Browser.Navigation as Nav


-- PORTS
port saveToken : String -> Cmd msg
port loadToken : () -> Cmd msg
port receiveToken : (String -> msg) -> Sub msg
port clearToken : () -> Cmd msg


-- MODEL
type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , token : Maybe String
    }


-- MESSAGES
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | NavigateTo Route
    | GotToken String


-- INIT
init : Nav.Key -> Url -> ( Model, Cmd Msg )
init key url =
    let
        route = Route.parse url
    in
    ( { key = key, route = route, token = Nothing }
    , loadToken ()
    )


-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.parse url }, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        NavigateTo newRoute ->
            ( model, Nav.pushUrl model.key (Route.toUrl newRoute) )

        GotToken str ->
            let
                maybeToken =
                    if String.isEmpty str then
                        Nothing
                    else
                        Just str
            in
            ( { model | token = maybeToken }, Cmd.none )


-- VIEW
view : Model -> Browser.Document Msg
view model =
    { title = "ToS App"
    , body =
        case model.route of
            Home -> [ div [] [ text "Welcome to the TOS Dashboard" ] ]
            Login -> [ div [] [ text "Login Form" ] ]
            Users -> [ div [] [ text "User List" ] ]
            Acceptances -> [ div [] [ text "TOS Acceptances" ] ]
            NotFound -> [ div [] [ text "404 Not Found" ] ]
    }


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveToken GotToken


-- MAIN
main : Program () Model Msg
main =
    Browser.application
        { init = \flags url key -> init key url
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
