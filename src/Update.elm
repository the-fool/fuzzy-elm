port module Update exposing (..)

import Array exposing (Array)
import Models exposing (Model, NetworkState(..))
import Core
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
    | ToggleEntry Network.EntryNeuronType
    | WindowResize ( Int, Int )
    | SetInput (Array Point)


port canvasMessage : { payload : List { id : String, outputs : Array.Array Float } } -> Cmd msg


drawCanvas : Network -> Cmd a
drawCanvas network =
    let
        toRecord n =
            { id = n.id, outputs = n.outputs }

        hidden =
            network.layers
                |> List.concatMap (List.map toRecord)

        output =
            { id = network.outputNeuron.id, outputs = network.outputNeuron.outputs }
    in
        canvasMessage { payload = output :: hidden }


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


doNetwork : Model -> Network
doNetwork model =
    let
        shuffledArray =
            Random.step (Core.shuffle model.inputs) model.randomSeed |> Tuple.first
    in
        Network.batchLearn model.network shuffledArray
            |> Network.batchPredict


resetCounter : Model -> Model
resetCounter model =
    { model | nTicks = 0 }


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
    case Debug.log "msg" message of
        NoOp ->
            model ! []

        WindowResize ( width, height ) ->
            { model | window = ( width, height ) } ! []

        SetInput points ->
            swapSeed { model | inputs = points } ! []

        Begin ->
            { model | state = Going } ! []

        Pause ->
            { model | state = Paused } ! []

        Reset ->
            { model | state = Paused, nTicks = 0 } ! []

        Learn time ->
            ( swapSeed
                { model
                    | network = doNetwork model
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
            , drawCanvas model.network
            )

        AddNeuron column ->
            let
                predicate =
                    (>) 8

                action =
                    (+) 1
            in
                ( resetCounter <| swapSeed { model | network = alterNeuronCount predicate action column model }, drawCanvas model.network )

        RemoveNeuron column ->
            let
                predicate =
                    (/=) 1

                action =
                    (+) -1
            in
                ( resetCounter <| swapSeed { model | network = alterNeuronCount predicate action column model }, drawCanvas model.network )

        AddLayer ->
            let
                predicate =
                    (>=) 5

                action =
                    flip (++) <| [ 1 ]
            in
                ( resetCounter <| swapSeed { model | network = alterLayerCount predicate action model }, drawCanvas model.network )

        RemoveLayer ->
            let
                predicate =
                    (<) 0

                action =
                    List.reverse << List.drop 1 << List.reverse
            in
                ( resetCounter <| swapSeed { model | network = alterLayerCount predicate action model }, drawCanvas model.network )

        ToggleEntry kind ->
            ( resetCounter <|
                swapSeed
                    { model
                        | network =
                            Network.networkFactory model.randomSeed model.network.activation (Network.toggleEntryNeuron model.network kind) (Network.getShape model.network)
                    }
            , drawCanvas model.network
            )
