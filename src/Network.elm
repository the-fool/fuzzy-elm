module Network exposing (..)

import Debug
import Set exposing (Set)


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


type alias EntryNeuronConfig =
    Set String


type alias Layer =
    List Neuron


type alias Neuron =
    List Float



{--
Returns a matrix representing the output of every node in the network.
That is why the list operation is a scan
--}


forwardProp : Network -> ( Float, Float ) -> List (List Float)
forwardProp network ( x, y ) =
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


networkFactory : Activation -> List EntryNeuronType -> List Int -> Network
networkFactory activation entryNeurons layerDims =
    let
        layers =
            layersFactory layerDims
    in
        if List.head layerDims == Just (List.length entryNeurons) then
            { layers = layers, activation = activation, entryNeurons = (setAllEntryNeurons entryNeurons) }
        else
            Debug.crash "Entry neuron function list is not the same length as the layer dimension!"


processInput : Network -> ( Float, Float ) -> List Float
processInput network ( x, y ) =
    network.entryNeurons
        |> List.map (fst >> .func >> ((|>) ( x, y )))


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


type Activation
    = Sigmoid
    | Tanh


activationFunction : Activation -> (Float -> Float)
activationFunction f =
    case f of
        Sigmoid ->
            sigmoid

        Tanh ->
            sigmoid
