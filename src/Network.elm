module Network exposing (..)

import String
import Debug
import Random.Pcg as Random
import Datasets
import Array exposing (Array)
import Constants
import List.Extra exposing ((!!))


type alias Network =
    { layers : List Layer
    , activation : Activation
    , entryNeurons : List EntryNeuron
    , outputNeuron : Neuron
    }



-- Entry neurons have a Neuron type within, for the purpose of playing nicely with scans and folds


type alias EntryNeuron =
    { name : String
    , func : ( Float, Float ) -> Float
    , active : Bool
    , neuron : Neuron
    }


type EntryNeuronType
    = X
    | Y


type alias Layer =
    List Neuron


type alias Neuron =
    { id : String
    , weights : List Float
    , outputs : Array Float
    }


type Activation
    = Sigmoid
    | Tanh


type alias Prediction =
    List (List Float)


type alias AggregatedPredictions =
    List (List (List Float))


entryFunction : EntryNeuronType -> (( a, a ) -> a)
entryFunction nt =
    case nt of
        X ->
            Tuple.first

        Y ->
            Tuple.second


entryName : EntryNeuronType -> String
entryName nt =
    case nt of
        X ->
            "X"

        Y ->
            "Y"


setAllEntryNeurons : List EntryNeuronType -> List EntryNeuron
setAllEntryNeurons types =
    let
        active nt =
            List.member nt types
    in
        [ X, Y ]
            |> List.map
                (\t ->
                    { func = entryFunction t
                    , active = active t
                    , name = entryName t
                    , neuron =
                        { outputs = Array.map (entryFunction t) (Array.fromList Constants.brutePoints)
                        , id = entryName t
                        , weights = [ 0.0 ]
                        }
                    }
                )


feedForward : Network -> ( Float, Float ) -> List (List Float)
feedForward network coord =
    let
        inputVector =
            getInputVector network coord

        activation =
            activationFunction network.activation

        doNeuron incomingVector neuron =
            dot (1 :: incomingVector) neuron.weights
                |> activation
    in
        network.layers
            ++ [ [ network.outputNeuron ] ]
            |> List.scanl (\layer incomingVector -> List.map (doNeuron incomingVector) layer) inputVector
            |> List.drop 1


adjustWeights : Neuron -> List Float -> List Float -> Neuron
adjustWeights neuron deltas inputs =
    if (List.length neuron.weights) /= (List.length deltas) then
        Debug.crash "Number of deltas must equal weights!"
    else
        { neuron | weights = List.map ((-) (dot deltas (1 :: inputs))) neuron.weights }


learn : Network -> Datasets.Point -> Network
learn network { coord, label } =
    let
        outputs =
            feedForward network coord

        outputNeuron =
            network.outputNeuron

        der =
            activationDerivative network.activation

        outputDeltas =
            case List.Extra.last outputs of
                Just out ->
                    out
                        -- Gradient * Error
                        |>
                            List.map (\o -> (der o) * (o - (toFloat label)))

                Nothing ->
                    Debug.crash "feedForward returned empty list!"

        finalHiddenOutputs =
            case outputs !! (List.length network.layers - 1) of
                Just val ->
                    val

                Nothing ->
                    Debug.crash "feedForward returned empty list!"

        hiddenDeltas =
            []

        adjustedOutputNeuron =
            { outputNeuron
                | weights =
                    List.map ((-) (dot outputDeltas (1 :: finalHiddenOutputs))) network.outputNeuron.weights
            }
    in
        network


getInputVector : Network -> ( Float, Float ) -> List Float
getInputVector network ( x, y ) =
    let
        activeEntryNeurons =
            List.filter .active network.entryNeurons
    in
        1 :: List.map (\neuron -> neuron.func ( x, y )) activeEntryNeurons


extractOutputNeuron : List Layer -> Neuron
extractOutputNeuron layers =
    case List.Extra.last layers of
        Just finalLayer ->
            case List.head finalLayer of
                Just outputNeuron ->
                    outputNeuron

                Nothing ->
                    Debug.crash "no output"

        Nothing ->
            Debug.crash "no layers"



{-
   For memory efficieny, batchPredict uses slab-allocation of Arrays for neuron outputs.
   Rather than producing new lists through maps, the algorithm runs setters and getters on a pre-existing array.
   The reduction in garbage collection cause a 2x speed-up!
-}
--batchPredict : Network -> Network


batchPredict : Network -> Network
batchPredict network =
    let
        activation =
            activationFunction network.activation

        activeEntryNeurons =
            List.filter .active network.entryNeurons |> List.map .neuron

        getPreviousVector layer index =
            layer
                |> List.map
                    (\neuron ->
                        case Array.get (index) neuron.outputs of
                            Just v ->
                                v

                            Nothing ->
                                Debug.crash "Previous getter"
                    )

        forwardProp index layers =
            layers
                |> List.scanl
                    (\layer previousLayer ->
                        (List.map << doNeuron <| ( index, getPreviousVector previousLayer index )) layer
                    )
                    activeEntryNeurons
                |> List.drop 1

        doNeuron ( index, incomingVector ) neuron =
            dot (1 :: incomingVector) neuron.weights
                |> activation
                |> \val ->
                    { neuron | outputs = Array.set index val neuron.outputs }

        doLayers =
            List.foldl forwardProp (network.layers ++ [ [ network.outputNeuron ] ]) (List.range 0 (List.length Constants.brutePoints - 1))
                |> \allLayers ->
                    { hidden = List.take (List.length network.layers) allLayers
                    , output = extractOutputNeuron allLayers
                    }
    in
        { network | layers = doLayers.hidden, outputNeuron = doLayers.output }


weightsFactory : Random.Seed -> Int -> ( List Float, Random.Seed )
weightsFactory seed numWeights =
    let
        gen =
            Random.float -1 1 |> Random.list numWeights
    in
        Random.step gen seed



{-
   A grid of neurons go in, a grid of neurons come out
   This is used to transform a simple neuron (e.g. an array of weights),
   into a record with a string id and something else (e.g. {id: String, weights: List Float})
   It is also used to transform the predictions grid into a record with an id field
-}


gridPrism : (String -> a -> b) -> List (List a) -> List (List b)
gridPrism recordFactory networkGrid =
    let
        idMaker ( layerIdx, rowIdx ) =
            List.map toString [ layerIdx, rowIdx ] |> String.join "-"
    in
        List.indexedMap
            (\layerIdx ->
                List.indexedMap
                    (\rowIdx weights -> recordFactory (idMaker ( layerIdx, rowIdx )) weights)
            )
            networkGrid


layersFactory : Random.Seed -> Int -> List Int -> List Layer
layersFactory seeder numEntry layerDims =
    let
        --Add the output node
        layers =
            List.map2 (,) (numEntry :: layerDims) layerDims

        weightsGrid =
            layers
                |> List.map
                    (\( numWeights, numNeurons ) ->
                        List.scanl
                            (\_ prev_ ->
                                (weightsFactory (Tuple.second prev_) (numWeights + 1))
                            )
                            ( [], seeder )
                            (List.range 1 numNeurons)
                            -- remove the initial seed entry
                            |>
                                List.drop 1
                            -- trim off the random seeds, leave just the lists of neurons
                            |>
                                List.map Tuple.first
                    )

        neuronFactory id weights =
            { id = id, weights = weights, outputs = (0 |> Array.repeat (Constants.density ^ 2)) }
    in
        gridPrism neuronFactory weightsGrid


networkFactory : Random.Seed -> Activation -> List EntryNeuronType -> List Int -> Network
networkFactory seed activation entryNeurons layerDims =
    let
        layers =
            layersFactory seed (List.length entryNeurons) layerDims

        outputNeuron =
            case List.head (List.reverse layerDims) of
                Just numFinalHiddenNeurons ->
                    { id = "output"
                    , weights = weightsFactory seed (numFinalHiddenNeurons + 1) |> Tuple.first
                    , outputs = (0 |> Array.repeat (Constants.density ^ 2))
                    }

                Nothing ->
                    Debug.crash "layerDims was empty!"

        entryNeuronConfig =
            setAllEntryNeurons entryNeurons
    in
        batchPredict
            { layers = layers
            , outputNeuron = outputNeuron
            , activation = activation
            , entryNeurons = entryNeuronConfig
            }


getShape : Network -> List Int
getShape network =
    List.map List.length network.layers


dot : List Float -> List Float -> Float
dot xs =
    List.sum << List.map2 (*) xs


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + e ^ -x)


tanh : Float -> Float
tanh x =
    let
        e2x =
            e ^ (2 * x)
    in
        (e2x - 1) / (e2x + 1)


activationFunction : Activation -> (Float -> Float)
activationFunction f =
    case f of
        Sigmoid ->
            sigmoid

        Tanh ->
            tanh


activationDerivative : Activation -> (Float -> Float)
activationDerivative f =
    case f of
        Sigmoid ->
            \x -> (sigmoid x) * (1 - sigmoid x)

        Tanh ->
            \x -> 1 - (tanh x) ^ 2
