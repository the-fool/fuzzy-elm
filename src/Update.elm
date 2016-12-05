module Update exposing (..)

import Array exposing (Array)
import Models exposing (Model, NetworkState(..))
import Core
import Datasets exposing (Point)
import Network exposing (..)
import Time exposing (Time)
import Random.Pcg as Random
import Canvas
import Task
import Buffer exposing (Buffer)


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
    | ToggleEntry Network.EntryNeuronType
    | WindowResize ( Int, Int )
    | SetInput (Array Point)
    | PaintCanvases Buffer


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
        Network.changeShape model.randomSeed oldNetwork newShape


processNetwork : Model -> Network
processNetwork model =
    let
        shuffledArray =
            Random.step (Core.shuffle model.inputs) model.randomSeed |> Tuple.first
    in
        Network.batchLearn model.network shuffledArray
            |> Network.batchPredict


resetCounter : Model -> Model
resetCounter model =
    { model | nTicks = 0 }


onNetworkChange : Model -> ( Model, Cmd Msg )
onNetworkChange model =
    model
        |> swapSeed
        |> resetCounter
        |> \m -> ( m, Task.perform PaintCanvases (Canvas.generateCanvasPayload model.network) )


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
                    if i == (layerIndex) && predicate neuronCount then
                        action neuronCount
                    else
                        neuronCount
                )
                oldShape
    in
        Network.changeShape model.randomSeed oldNetwork newShape


swapSeed : Model -> Model
swapSeed model =
    { model | randomSeed = Core.nextSeed model.randomSeed }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            model ! []

        PaintCanvases payload ->
            model ! [ Canvas.paintCanvas payload ]

        WindowResize ( width, height ) ->
            ( { model | window = ( width, height ) }, Cmd.batch [ Canvas.paintEntry model.network, Task.perform PaintCanvases (Canvas.generateCanvasPayload model.network) ] )

        SetInput points ->
            (resetCounter <| swapSeed { model | inputs = points }) ! []

        Begin ->
            { model | state = Going } ! []

        Pause ->
            { model | state = Paused } ! []

        Reset ->
            { model | state = Paused, nTicks = 0 } ! []

        Learn time ->
            ( swapSeed
                { model
                    | network = processNetwork model
                    , nTicks =
                        {- There is some sort of race condition with switching out animation frame subs,
                           which results in the ticks being incremented once after the state should be paused,
                           as if this update routine is still in the queso, this extra check fixes the problem
                        -}
                        if model.state == Going then
                            model.nTicks + 1
                        else
                            model.nTicks
                }
            , Task.perform PaintCanvases (Canvas.generateCanvasPayload model.network)
            )

        AddNeuron column ->
            let
                predicate =
                    (>) 7

                action =
                    (+) 1
            in
                { model | network = alterNeuronCount predicate action column model } |> onNetworkChange

        RemoveNeuron column ->
            let
                predicate =
                    (/=) 1

                action =
                    (+) -1
            in
                { model | network = alterNeuronCount predicate action column model } |> onNetworkChange

        AddLayer ->
            let
                predicate =
                    (>=) 5

                action =
                    flip (++) <| [ 1 ]
            in
                { model | network = alterLayerCount predicate action model } |> onNetworkChange

        RemoveLayer ->
            let
                predicate =
                    (<) 0

                action =
                    List.reverse << List.drop 1 << List.reverse
            in
                { model | network = alterLayerCount predicate action model } |> onNetworkChange

        ToggleEntry kind ->
            { model
                | network =
                    Network.networkFactory model.randomSeed model.network.activation (Network.toggleEntryNeuron model.network kind) (Network.getShape model.network)
            }
                |> onNetworkChange
