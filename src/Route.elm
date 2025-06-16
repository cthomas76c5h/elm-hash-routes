module Route exposing (Route(..), parse, toUrl)

import Url.Parser exposing (Parser, (</>), map, oneOf, s, top)
import Url exposing (Url)

type Route
    = Home
    | Users
    | Login
    | NotFound
    | Acceptances

parse : Url -> Route
parse url =
    case url.fragment of
        Just frag ->
            -- remove leading slash in "#/users" => "users"
            case String.split "/" frag of
                [ "", "login" ] -> Login
                [ "", "users" ] -> Users
                [ "", "acceptances" ] -> Acceptances
                [ "" ] -> Home
                _ -> NotFound

        Nothing ->
            Home

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Users (s "users")
        , map Login (s "login")
        ]

toUrl : Route -> String
toUrl route =
    case route of
        Home -> "#/"
        Users -> "#/users"
        Login -> "#/login"
        NotFound -> "#/404"
        Acceptances -> "#/acceptances"
