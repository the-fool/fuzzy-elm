port module Update exposing (..)

import Debug
import Constants
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


port canvasMessage : { jumboDims : ( Int, Int ), payload : List Layer } -> Cmd msg


drawCanvas : Network -> Cmd a
drawCanvas network =
    canvasMessage { jumboDims = ( Constants.jumboCanvasSize, Constants.jumboCanvasSize ), payload = network.layers }


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
        Network.networkFactory model.randomSeed Network.Tanh [ Network.X, Network.Y ] newShape


alterNeuronCount : (Int -> Bool) -> (Int -> Int) -> Int -> Model -> Network
alterNeuronCount predicate action layerIndex model =
    let
        oldNetwork =
            model.network

        oldShape =
            Network.getShape oldNetwork

        newShape =
            List.indexedMap
                (\i neuronCount ->
                    if i == (layerIndex - 1) && predicate neuronCount then
                        action neuronCount
                    else
                        neuronCount
                )
                oldShape
    in
        Network.networkFactory model.randomSeed Network.Tanh [ Network.X, Network.Y ] newShape


nextSeed : Random.Seed -> Random.Seed
nextSeed seed =
    Random.step Random.bool seed |> Tuple.second


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
            ( { model | nTicks = model.nTicks + 1, network = Network.batchPredict model.network }, drawCanvas model.network )

        AddNeuron column ->
            let
                predicate =
                    (>) 8

                action =
                    (+) 1
            in
                ( swapSeed { model | network = alterNeuronCount predicate action column model }, drawCanvas model.network )

        RemoveNeuron column ->
            let
                predicate =
                    (/=) 1

                action =
                    (+) -1
            in
                ( swapSeed { model | network = alterNeuronCount predicate action column model }, drawCanvas model.network )

        AddLayer ->
            let
                predicate =
                    (>=) 5

                action =
                    flip (++) <| [ 1 ]
            in
                ( swapSeed { model | network = alterLayerCount predicate action model }, drawCanvas model.network )

        RemoveLayer ->
            let
                predicate =
                    (<) 0

                action =
                    List.reverse << List.drop 1 << List.reverse
            in
                ( swapSeed { model | network = alterLayerCount predicate action model }, drawCanvas model.network )
