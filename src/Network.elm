module Network exposing (..)

import Debug


{--In order to serialize to localStorage, activation function must be stored as a string --}


activations : String -> Float -> Float
activations k =
    case k of
        "sigmoid" ->
            sigmoid

        _ ->
            sigmoid


entryNeuronFunctions : String -> ( Float, Float ) -> Float
entryNeuronFunctions k =
    case k of
        "x" ->
            \( x, y ) -> x

        "y" ->
            \( x, y ) -> y

        _ ->
            Debug.crash "Invalid key for input function!"


type alias Network =
    { layers : List Layer
    , activation : String
    , entryNeurons : List String
    }


type alias Layer =
    List Neuron


type alias Neuron =
    List Float


getShape : Network -> List Int
getShape network =
    let
        nonOutput =
            List.take (List.length network.layers - 1) network.layers
    in
        List.map List.length nonOutput


processInput : Network -> ( Float, Float ) -> List Float
processInput network ( x, y ) =
    network.entryNeurons
        |> List.map (entryNeuronFunctions >> ((|>) ( x, y )))


dot : List Float -> List Float -> Float
dot xs =
    List.sum << List.map2 (*) xs



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

        doNeuron : List Float -> List Float -> Float
        doNeuron incoming weights =
            dot (1 :: incoming) weights
                |> activation

        doLayer : List (List Float) -> List Float -> List Float
        doLayer layer incoming =
            List.map (doNeuron incoming) layer
    in
        List.scanl doLayer firstInputs nonEntryLayers


networkFactory : List Int -> Network
networkFactory layerDims =
    let
        {--Add the output node--}
        layers =
            List.drop 1 <| (layerDims ++ [ 1 ])

        randomWeights len =
            List.repeat len 1

        entryLayer =
            case List.head layerDims of
                Just n ->
                    List.repeat n [ 1.0, 0.0 ]

                Nothing ->
                    Debug.crash "No entry layer"

        allLayers =
            List.scanl
                (\cur prev ->
                    List.length prev
                        |> randomWeights
                        |> (::) 1
                        |> List.repeat cur
                )
                entryLayer
                layers
    in
        { layers = allLayers, activation = "sigmoid", entryNeurons = [ "x", "y" ] }


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + e ^ -x)
