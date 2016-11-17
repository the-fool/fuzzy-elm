module Network exposing (..)

import String
import Debug
import Random.Pcg as Random
import Array
import Constants

type alias Network =
    { layers : List Layer
    , activation : Activation
    , entryNeurons : List ( EntryNeuron, Bool )
    }


type alias EntryNeuron =
    { name : String
    , func : ( Float, Float ) -> Float
    }


type EntryNeuronType
    = X
    | Y


type alias Layer =
    List Neuron


type alias Neuron =
    { id : String
    , weights : List Float
    , outputs : Array.Array Float
    }


type Activation
    = Sigmoid
    | Tanh

type alias Prediction =
    List (List Float)


type alias AggregatedPredictions =
    List (List (List Float))

getEntryNeuron : EntryNeuronType -> EntryNeuron
getEntryNeuron nt =
    case nt of
        X ->
            { name = "X", func = fst }

        Y ->
            { name = "Y", func = snd }


setAllEntryNeurons : List EntryNeuronType -> List ( EntryNeuron, Bool )
setAllEntryNeurons types =
    let
        active nt =
            List.member nt types
    in
        List.map
            (\t ->
                ( getEntryNeuron t, active t )
            )
            [ X, Y ]


batchPredict : Network -> Network
batchPredict = identity


processInput : Network -> ( Float, Float ) -> List Float
processInput network ( x, y ) =
    network.entryNeurons
        |> List.map (fst >> .func >> ((|>) ( x, y )))


{--
Returns a matrix representing the output of every node in the network.
That is why the list operation is a scan
--}


forwardProp : Network -> Int -> ( Float, Float ) -> List (List Float)
forwardProp network index ( x, y ) =
    let
        activation =
            activationFunction network.activation

        nonEntryLayers =
            case List.tail network.layers of
                Just layers ->
                    layers

                Nothing ->
                    Debug.crash "No layers found in network!"

        firstInputs =
            processInput network ( x, y )

        doNeuron incoming neuron =
            dot (1 :: incoming) neuron.weights
                |> activation

        doLayer layer incoming =
            List.map (doNeuron incoming) layer
    in
        List.scanl doLayer firstInputs nonEntryLayers


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


layersFactory : Random.Seed -> List Int -> List Layer
layersFactory seeder layerDims =
    let
        --Add the output node
        layers =
            List.drop 1 <| (layerDims ++ [ 1 ])

        entryLayer =
            case List.head layerDims of
                Just n ->
                    -- Dummy values.  Entry neurons do not have weights.
                    -- there just has to be a matrix of numbers in the entry layer
                    -- for the sake of bootstrapping the recursive construction of hidden layers
                    List.repeat n [ 0.0 ]

                Nothing ->
                    Debug.crash "No entry layer"

        weightsGrid =
            List.scanl
                (\cur prev ->
                    List.scanl (\_ prev' -> (weightsFactory (snd prev') (List.length prev + 1))) ( [], seeder ) [0..(cur - 1)]
                        |> List.drop 1
                        |> List.map fst
                )
                entryLayer
                layers

        neuronFactory id weights =
            { id = id, weights = weights, outputs = (0 |> Array.repeat (Constants.density ^ 2)) }
    in
        gridPrism neuronFactory weightsGrid


networkFactory : Random.Seed -> Activation -> List EntryNeuronType -> List Int -> Network
networkFactory seed activation entryNeurons layerDims =
    let
        layers =
            layersFactory seed layerDims
    in
        if List.head layerDims == Just (List.length entryNeurons) then
            { layers = layers, activation = activation, entryNeurons = (setAllEntryNeurons entryNeurons) }
        else
            Debug.crash "Entry neuron function list is not the same length as the layer dimension!"



getShape : Network -> List Int
getShape network =
    let
        nonOutput =
            List.take (List.length network.layers - 1) network.layers
    in
        List.map List.length nonOutput


dot : List Float -> List Float -> Float
dot xs =
    List.sum << List.map2 (*) xs


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + e ^ -x)


activationFunction : Activation -> (Float -> Float)
activationFunction f =
    case f of
        Sigmoid ->
            sigmoid

        Tanh ->
            sigmoid



-- Batch Predictions --


brutePredictions : Network -> List Prediction
brutePredictions network =
    -- The entry layer in this list are bogus
    List.indexedMap (forwardProp network) Constants.brutePoints



{--
Ok, this is kind of crazy.  For simplicity, we collect X number of predictions in a list of
X length, where each element is a jagged 2d array representing the whole network of neurons' separate predictions.
This list of X separate network-predictions for each data point needs to be folded down to an array
of the same shape as the network, except insted of neurons for the basic element,
there is the X-length list of predictions
--}


aggregatePredictions : List Prediction -> AggregatedPredictions
aggregatePredictions allPoints =
    let
        shape =
            -- each element in points has same shape
            List.take 1 allPoints
                |> -- replace all the number elements with empty lists to 'seed' the fold
                   List.concatMap (List.map (List.map (always [])))
    in
        List.foldl (List.map2 (List.map2 (::))) shape allPoints


getPredictionGrid : Network -> AggregatedPredictions
getPredictionGrid =
    brutePredictions >> aggregatePredictions
