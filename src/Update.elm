module Update exposing (..)

import Debug
import Models exposing (Model, Point)
import Network exposing (..)


type alias Column =
    Int


type Msg
    = NoOp
    | AddNeuron Column
    | RemoveNeuron Column
    | AddLayer
    | RemoveLayer
    | WindowResize ( Int, Int )
    | SelectInput (List Point)


alterLayer : (Int -> Bool) -> (Int -> Int) -> Int -> Network -> Network
alterLayer predicate action layerIndex oldNetwork =
    let
        oldDims =
            List.map List.length oldNetwork.layers

        newDims =
            List.indexedMap
                (\i neuronCount ->
                    if i == layerIndex && predicate neuronCount then
                        action neuronCount
                    else
                        neuronCount
                )
                oldDims
    in
        networkFactory newDims


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "msg" message of
        NoOp ->
            model ! []

        WindowResize ( width, height ) ->
            { model | window = ( width, height ) } ! []

        SelectInput points ->
            { model | inputs = points } ! []

        AddNeuron column ->
            let
                predicate =
                    (>) 8

                action =
                    (+) 1
            in
                { model | network = alterLayer predicate action column model.network } ! []

        RemoveNeuron column ->
            let
                predicate =
                    (/=) 1

                action =
                    (+) -1
            in
                { model | network = alterLayer predicate action column model.network } ! []

        AddLayer ->
            let
                oldDims =
                    List.map List.length model.network.layers

                newDims =
                    if List.length oldDims > 5 then
                        oldDims
                    else
                        oldDims ++ [ 1 ]
            in
                { model | network = networkFactory newDims } ! []

        RemoveLayer ->
            let
                oldDims =
                    List.map List.length model.network.layers

                newDims =
                    if List.length oldDims == 1 then
                        oldDims
                    else
                        List.take (List.length oldDims - 1) oldDims
            in
                { model | network = networkFactory newDims } ! []
