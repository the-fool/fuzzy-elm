module Update exposing (..)

import Models exposing (Model, NetworkState(..), DataMode(..), Brush(..))
import Core
import Datasets exposing (Point, Dataset)
import Network exposing (..)
import Time exposing (Time)
import Random.Pcg as Random
import Canvas
import Task
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode exposing ((|:))
import Buffer exposing (Buffer)
import Array


type alias Column =
    Int


type alias Position =
    { x : Int, y : Int }


type BackAction
    = Step
    | All


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
    | SetLearningRate Float
    | WindowResize ( Int, Int )
    | SetDataMode DataMode
    | SetBrush Brush
    | PaintCanvases Buffer
    | ShowHoverCard Float Position
    | AddPoint Datasets.Coord
    | HideHoverCard
    | CustomDataBack BackAction


mouseEventDecoder : ({ x : Int, y : Int } -> Msg) -> Decoder Msg
mouseEventDecoder cb =
    Decode.succeed (cb)
        |: (Decode.succeed Position
                |: (Decode.field "layerX" Decode.int)
                |: (Decode.field "layerY" Decode.int)
           )


hoverCardPositioner : Float -> Decoder Msg
hoverCardPositioner weight =
    mouseEventDecoder (ShowHoverCard weight)


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
        |> \m -> ( { m | network = processNetwork m }, Task.perform PaintCanvases (Canvas.generateCanvasPayload model.network) )


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

        CustomDataBack action ->
            let
                alteration =
                    case action of
                        Step ->
                            \points ->
                                points
                                    |> Array.length
                                    |> (+) -1
                                    |> \l -> Array.slice 0 l points

                        All ->
                            always (Array.fromList [])

                newCustomData =
                    alteration model.customData
            in
                { model | inputs = newCustomData, customData = newCustomData } ! []

        AddPoint ( x, y ) ->
            let
                label =
                    case model.brush of
                        Positive ->
                            1

                        Negative ->
                            -1

                newInputs =
                    Array.push { coord = ( x, y ), label = label } model.customData
            in
                if model.dataMode == Custom then
                    { model | customData = newInputs, inputs = newInputs } ! []
                else
                    model ! []

        HideHoverCard ->
            { model | hoverCard = { x = 0, y = 0, visible = False, weight = 0 } } ! []

        ShowHoverCard weight pos ->
            let
                newHoverCard =
                    { x = pos.x, y = pos.y, visible = True, weight = weight }
            in
                { model | hoverCard = newHoverCard } ! []

        WindowResize ( width, height ) ->
            ( { model | window = ( width, height ) }, Cmd.batch [ Canvas.paintEntry model.network, Task.perform PaintCanvases (Canvas.generateCanvasPayload model.network) ] )

        SetBrush kind ->
            { model | brush = kind } ! []

        SetDataMode mode ->
            let
                newPoints =
                    case mode of
                        Custom ->
                            model.customData

                        Stock kind ->
                            Datasets.getData kind model.randomSeed
            in
                { model | dataMode = mode, inputs = newPoints, network = (Network.shuffleNetwork model.randomSeed model.network), best = 0 } |> onNetworkChange

        Begin ->
            { model | state = Going } ! []

        Pause ->
            { model | state = Paused } ! []

        Reset ->
            { model | state = Paused, nTicks = 0, network = (Network.shuffleNetwork model.randomSeed model.network) } |> onNetworkChange

        Learn time ->
            let
                bestSetter model =
                    if ((model.network.error < 0.01) && ((model.nTicks < model.best) || model.best == 0)) then
                        model.nTicks
                    else
                        model.best
            in
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
                        , best = bestSetter model
                    }
                , Task.perform PaintCanvases (Canvas.generateCanvasPayload model.network)
                )

        SetLearningRate alpha ->
            let
                newNetwork network =
                    { network | learningRate = alpha }
            in
                { model | network = newNetwork model.network } ! []

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
            { model | network = Network.toggleEntryNeuron model.network kind |> Network.shuffleNetwork model.randomSeed }
                |> onNetworkChange
