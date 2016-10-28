module Main exposing (..)

{- vendor -}

import Navigation
import Hop
import Hop.Types exposing (Address)
import UrlParser


{- src -}

import Models exposing (AppModel, newAppModel)
import Routing exposing (..)
import View exposing (view)
import Messages exposing (Msg)
import Update exposing (update)


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = (always Sub.none)
        }


urlUpdate : ( Route, Address ) -> AppModel -> ( AppModel, Cmd Msg )
urlUpdate ( route, address ) model =
    let
        _ =
            Debug.log "urlUpdate address" address
    in
        ( { model | route = route, address = address }, Cmd.none )


urlParser : Navigation.Parser ( Route, Address )
urlParser =
    let
        parse path =
            path
                |> UrlParser.parse identity Routing.routes
                |> Result.withDefault NotFoundRoute

        matcher =
            Hop.makeResolver Routing.config parse
    in
        Navigation.makeParser (.href >> matcher)


init : ( Route, Address ) -> ( AppModel, Cmd Msg )
init ( route, address ) =
    ( newAppModel route address, Cmd.none )
