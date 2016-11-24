module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, float, tuple, string)
import String
import Network exposing (..)
import Models
import Array


controlNetwork : Network
controlNetwork =
    { activation = Network.Linear
    , outputNeuron = { weights = [ 1, 1, 1 ], id = "output", outputs = Array.fromList [] }
    , entryNeurons = [ { name = "X", func = Tuple.first, active = True, neuron = { weights = [], id = "X", outputs = Array.fromList [] } } ]
    , layers = [ [], [] ]
    }


sampleNetwork : Network
sampleNetwork =
    let
        stockNetwork =
            networkFactory Models.seed0 Sigmoid [ X, Y ] [ 2 ]

        setWeights neuron newWeights =
            { neuron | weights = newWeights }

        setOutputNeuron neuron =
            { neuron | weights = [ 0.3, -1.2, 1.1 ] }

        setLayers layers =
            List.map2 (\layer weights -> List.map2 (setWeights) layer weights) layers [ [ [ 0.8, 0.5, 0.4 ], [ -0.1, 0.9, 1.0 ] ] ]
    in
        { stockNetwork
            | layers = setLayers stockNetwork.layers
            , outputNeuron = setOutputNeuron stockNetwork.outputNeuron
        }


constantsNetwork : Float -> Network
constantsNetwork x =
    let
        updateOutputNeuron outputNeuron =
            { outputNeuron | weights = List.repeat 3 x }
    in
        networkFactory Models.seed0 Linear [ X, Y ] [ 2, 2 ]
            |> (\net -> { net | layers = List.map (List.map (\neuron -> { neuron | weights = List.map (always x) neuron.weights })) net.layers })
            |> (\net -> { net | outputNeuron = updateOutputNeuron net.outputNeuron })


randomNetwork =
    networkFactory Models.seed0 Linear [ X, Y ] [ 2, 2 ]


ones : Network.Network
ones =
    constantsNetwork 1


zeroes : Network
zeroes =
    constantsNetwork 0


all : Test
all =
    describe "Network functions"
        [ describe "Get input vector"
            [ test "It returns the entry functions applied to (0,0)" <|
                \() ->
                    Expect.equal (getInputVector ones ( 0, 0 )) [ 0, 0 ]
            , test "It returns the entry function applied to (1,1)" <|
                \() ->
                    Expect.equal (getInputVector ones ( 1, 1 )) [ 1, 1 ]
            ]
        , describe "Feed forward"
            [ test "Feed zeros to ones" <|
                \() ->
                    Expect.equal (feedForward ones ( 0, 0 )) [ [ 1, 1 ], [ 3, 3 ], [ 7 ] ]
            , test "Feed zeros to zeroes" <|
                \() ->
                    Expect.equal (feedForward zeroes ( 0, 0 )) [ [ 0, 0 ], [ 0, 0 ], [ 0 ] ]
            , test "Feed ones to ones" <|
                \() ->
                    Expect.equal (feedForward ones ( 1, 1 )) [ [ 3, 3 ], [ 7, 7 ], [ 15 ] ]
            ]
        , describe "Get hidden deltas"
            [ fuzzWith { runs = 100 } float "A zero delta for the output always makes for zero deltas in hidden" <|
                \i ->
                    (gradients (always i) ( [ [ i, i, i ] ], [ 0 ] ) randomNetwork.layers (feedForward randomNetwork ( i, i )))
                        |> Expect.equal [ [ 0, 0 ], [ 0 ] ]
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
