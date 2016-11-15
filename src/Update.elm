port module Update exposing (..)

import Debug
import Models exposing (Model, Point)
import Network exposing (..)
import Time exposing (Time)
import Random.Pcg as Random

type alias Column =
    Int


type Msg
    = NoOp
    | AddNeuron Column
    | RemoveNeuron Column
    | AddLayer
    | RemoveLayer
    | Begin
    | Pause
    | Reset
    | Learn Time
    | WindowResize ( Int, Int )
    | SelectInput ((List Point), Random.Seed)


port drawCanvas : List ( String, ( Float, Float ), List Int ) -> Cmd msg


alterLayerCount : (Int -> Bool) -> (List Int -> List Int) -> Network -> Network
alterLayerCount predicate action oldNetwork =
    let
        oldShape =
            Network.getShape oldNetwork

        newShape =
            if List.length oldShape |> predicate then
                action oldShape
            else
                oldShape
    in
        { oldNetwork | layers = layersFactory newShape }


alterNeuronCount : (Int -> Bool) -> (Int -> Int) -> Int -> Network -> Network
alterNeuronCount predicate action layerIndex oldNetwork =
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
        { oldNetwork | layers = layersFactory newShape }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "msg" message of
        NoOp ->
            model ! []

        WindowResize ( width, height ) ->
            { model | window = ( width, height ) } ! []

        SelectInput (points, seed) ->
            { model | inputs = points, randomSeed = seed } ! []

        Begin ->
            { model | state = 1 } ! []

        Pause ->
            { model | state = 0 } ! []

        Reset ->
            { model | nTicks = 0, state = 0 } ! []

        Learn time ->
            ( { model | nTicks = model.nTicks + 1 }, drawCanvas [ ( "foo", ( 300, 300 ), [ 1, 2, 3 ] ) ] )

        AddNeuron column ->
            let
                predicate =
                    (>) 8

                action =
                    (+) 1
            in
                { model | network = alterNeuronCount predicate action column model.network } ! []

        RemoveNeuron column ->
            let
                predicate =
                    (/=) 1

                action =
                    (+) -1
            in
                { model | network = alterNeuronCount predicate action column model.network } ! []

        AddLayer ->
            let
                predicate =
                    (>=) 5

                action =
                    flip (++) <| [ 1 ]
            in
                { model | network = alterLayerCount predicate action model.network } ! []

        RemoveLayer ->
            let
                predicate =
                    (<) 1

                action =
                    List.length model.network.layers
                        |> (+) -2
                        -- Remove 2, to account for the fact that the output neuron is appended in the factory
                        |>
                            List.take
            in
                { model | network = alterLayerCount predicate action model.network } ! []
