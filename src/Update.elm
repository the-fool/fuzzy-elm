port module Update exposing (..)

import Debug
import Models exposing (Model)
import Datasets exposing (Point)
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
    | SetInput (List Point)


port drawCanvas : List ( String, ( Float, Float ), List Int ) -> Cmd msg


alterLayerCount : (Int -> Bool) -> (List Int -> List Int) -> Model -> Network
alterLayerCount predicate action model =
    let
        oldNetwork =
            model.network

        oldShape =
            Network.getShape oldNetwork

        newShape =
            if List.length oldShape |> predicate then
                action oldShape
            else
                oldShape
    in
        { oldNetwork | layers = layersFactory model.randomSeed newShape }


alterNeuronCount : (Int -> Bool) -> (Int -> Int) -> Int -> Model -> Network
alterNeuronCount predicate action layerIndex model =
    let
        oldNetwork =
            model.network

        oldShape =
            Network.getShape model.network

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
        { oldNetwork | layers = layersFactory model.randomSeed newShape }


nextSeed : Random.Seed -> Random.Seed
nextSeed seed =
    Random.step Random.bool seed |> snd


swapSeed : Model -> Model
swapSeed model =
    { model | randomSeed = nextSeed model.randomSeed }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "msg" message of
        NoOp ->
            model ! []

        WindowResize ( width, height ) ->
            { model | window = ( width, height ) } ! []

        SetInput points ->
            swapSeed { model | inputs = points } ! []

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
                swapSeed { model | network = alterNeuronCount predicate action column model } ! []

        RemoveNeuron column ->
            let
                predicate =
                    (/=) 1

                action =
                    (+) -1
            in
                swapSeed { model | network = alterNeuronCount predicate action column model } ! []

        AddLayer ->
            let
                predicate =
                    (>=) 5

                action =
                    flip (++) <| [ 1 ]
            in
                swapSeed { model | network = alterLayerCount predicate action model } ! []

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
                swapSeed { model | network = alterLayerCount predicate action model } ! []
