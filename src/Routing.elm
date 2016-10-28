module Routing exposing (..)

import Hop.Types exposing (Config)
import UrlParser exposing ((</>), oneOf, int, s)


type Route
    = HomeRoute
    | InputRoutes Int
    | NotFoundRoute


matchers : List (UrlParser.Parser (Route -> a) a)
matchers =
    [ UrlParser.format HomeRoute (s "")
    , UrlParser.format InputRoutes (s "inputs" </> int)
    ]


routes : UrlParser.Parser (Route -> a) a
routes =
    oneOf matchers


config : Config
config =
    { basePath = "/app"
    , hash = False
    }


reverse : Route -> String
reverse route =
    case route of
        HomeRoute ->
            ""

        InputRoutes id ->
            "inputs" ++ (toString id)

        NotFoundRoute ->
            ""
