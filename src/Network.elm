module Network exposing (..)

import Debug


type alias Network =
    { layers : List Layer
    , activation : String
    , entryNeurons : List String
    }


type alias Layer =
    List Neuron


type alias Neuron =
    List Float



{--Returns a matrix representing the output of every node in the network --}


forwardProp : ( Float, Float ) -> Network -> List (List Float)
forwardProp ( x, y ) network =
    let
        activation =
            activations network.activation

        nonEntryLayers =
            case List.tail network.layers of
                Just layers ->
                    layers

                Nothing ->
                    Debug.crash "No layers found in network!"

        firstInputs =
            processInput network ( x, y )

        doNeuron incoming weights =
            dot (1 :: incoming) weights
                |> activation

        doLayer layer incoming =
            List.map (doNeuron incoming) layer
    in
        List.scanl doLayer firstInputs nonEntryLayers


layersFactory : List Int -> List Layer
layersFactory layerDims =
    let
        --Add the output node
        layers =
            List.drop 1 <| (layerDims ++ [ 1 ])

        randomWeights len =
            List.repeat len 1

        entryLayer =
            case List.head layerDims of
                Just n ->
                    -- Dummy values.  Entry neurons do not have weights.
                    -- there just has to be a matrix of numbers in the entry layer
                    -- for the sake of bootstrapping the recursive construction of hidden layers
                    List.repeat n [ 0.0 ]

                Nothing ->
                    Debug.crash "No entry layer"
    in
        List.scanl
            (\cur prev ->
                List.length prev
                    |> randomWeights
                    |> (::) 1
                    |> List.repeat cur
            )
            entryLayer
            layers


networkFactory : String -> List String -> List Int -> Network
networkFactory activation entryNeurons layerDims =
    let
        layers =
            layersFactory layerDims
    in
        { layers = layers, activation = activation, entryNeurons = entryNeurons }


processInput : Network -> ( Float, Float ) -> List Float
processInput network ( x, y ) =
    network.entryNeurons
        |> List.map (entryNeuronFunctions >> ((|>) ( x, y )))


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



{--In order to serialize to localStorage, activation function must be stored as a string --}


activations : String -> Float -> Float
activations k =
    case k of
        "sigmoid" ->
            sigmoid

        _ ->
            Debug.crash ("Illegal string key " ++ k ++ " for activation function")


entryNeuronFunctions : String -> ( Float, Float ) -> Float
entryNeuronFunctions k =
    case k of
        "x" ->
            \( x, y ) -> x

        "y" ->
            \( x, y ) -> y

        _ ->
            Debug.crash "Invalid key for input function!"
