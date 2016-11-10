module Update exposing (..)

import Debug
import Models exposing (Model, Layer, Network, newNeuron)
import Monocle.Lens exposing (..)


type alias Column =
    Int


type Msg
    = NoOp
    | AddNeuron Column
    | AddLayer
    | RemoveLayer


modelHiddenLayerLens : Lens Model (List Layer)
modelHiddenLayerLens =
    let
        networkHiddenLayerLens : Lens Network (List Layer)
        networkHiddenLayerLens =
            let
                get a =
                    a.hidden

                set hd a =
                    { a | hidden = hd }
            in
                Lens get set

        modelNetworkLens : Lens Model Network
        modelNetworkLens =
            let
                get a =
                    a.network

                set net a =
                    { a | network = net }
            in
                Lens get set
    in
        modelNetworkLens `compose` networkHiddenLayerLens


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "msg" message of
        NoOp ->
            model ! []

        AddNeuron column ->
            model ! []

        AddLayer ->
            let
                newHidden =
                    if List.length model.network.hidden > 5 then
                        modelHiddenLayerLens.get model
                    else
                        (modelHiddenLayerLens.get model) ++ [ [ newNeuron 3 ] ]
            in
                modelHiddenLayerLens.set newHidden model ! []

        RemoveLayer ->
            let
                hidden =
                    modelHiddenLayerLens.get model

                newHidden =
                    if List.length hidden == 0 then
                        hidden
                    else
                        List.take (List.length hidden - 1) hidden
            in
                modelHiddenLayerLens.set newHidden model ! []
