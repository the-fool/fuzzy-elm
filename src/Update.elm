module Update exposing (..)

import Debug
import Navigation
import Models exposing (AppModel)
import Messages exposing (Msg(..))
import Routing exposing (Route(..))
import Hop exposing (outputFromPath)


navigationCmd : String -> Cmd a
navigationCmd path =
    path
        |> outputFromPath Routing.config
        |> Navigation.newUrl


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update message model =
    case Debug.log "msg" message of
        SelectInput id ->
            ( { model | selectedInput = Maybe.Just id }, Cmd.none )

        GoHome ->
            let
                path =
                    Routing.reverse HomeRoute
            in
                ( model, navigationCmd path )

        GoInputs ->
            let
                path =
                    Routing.reverse (InputRoutes 54)
            in
                ( model, navigationCmd path )
