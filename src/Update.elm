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
        oldShape =
            Network.getShape oldNetwork

        newShape =
            List.indexedMap
                (\i neuronCount ->
                    if i == layerIndex && predicate neuronCount then
                        action neuronCount
                    else
                        neuronCount
                )
                oldShape
    in
        networkFactory newShape


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
                oldShape =
                    Network.getShape model.network

                newShape =
                    if List.length oldShape > 5 then
                        oldShape
                    else
                        oldShape ++ [ 1 ]
            in
                { model | network = networkFactory newShape } ! []

        RemoveLayer ->
            let
                oldShape =
                    Network.getShape model.network

                newShape =
                    if List.length oldShape == 1 then
                        oldShape
                    else
                        List.take (List.length oldShape - 1) oldShape
            in
                { model | network = networkFactory newShape } ! []
