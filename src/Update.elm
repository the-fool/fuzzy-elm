module Update exposing (..)

import Debug
import Models exposing (Model, newNeuron)


type Msg
    = NoOp
    | AddNeuron


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "msg" message of
        NoOp ->
            model ! []

        AddNeuron ->
            model ! []
