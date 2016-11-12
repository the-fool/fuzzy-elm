module Network exposing (..)

import Array exposing (..)


{--In order to serialize to localStorage, activation function must be stored as a string --}


activations : String -> Float -> Float
activations k =
    case k of
        "sigmoid" ->
            sigmoid

        _ ->
            sigmoid


entryNeuronTypes : String -> ( Float, Float ) -> Float
entryNeuronTypes k =
    case k of
        "x" ->
            \( x, y ) -> x

        "y" ->
            \( x, y ) -> y

        _ ->
            \( x, y ) -> x


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



{--Returns a matrix representing the output of every node in the network --}


forwardProp : ( Float, Float ) -> Network -> List (List Float)
forwardProp input network =
    [ [ 0.3, 0.6 ], [ 0.2, 0.8, 0.9 ] ]


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + e ^ -x)


networkFactory : List Int -> Network
networkFactory layerDims =
    let
        layers =
            fromList (layerDims ++ [ 1 ])

        randomWeights len =
            List.repeat len 0.7

        entryLayer =
            case List.head layerDims of
                Just n ->
                    List.repeat n [ 1.0, 0.0 ]

                Nothing ->
                    []

        hiddenLayers =
            List.map
                (\x ->
                    case get (x - 1) layers of
                        Just numPrev ->
                            case get x layers of
                                Just numHere ->
                                    List.repeat numHere (randomWeights numPrev)

                                Nothing ->
                                    []

                        Nothing ->
                            []
                )
                [1..(length layers - 1)]

        allLayers =
            [ entryLayer ] ++ hiddenLayers
    in
        { layers = allLayers, activation = "sigmoid", entryNeurons = [ "x", "y" ] }
