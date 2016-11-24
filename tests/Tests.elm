module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
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


constantsNetwork : Float -> Network
constantsNetwork x =
    let
        updateOutputNeuron outputNeuron =
            { outputNeuron | weights = List.repeat 3 x }
    in
        networkFactory Models.seed0 Linear [ X, Y ] [ 2, 2 ]
            |> (\net -> { net | layers = List.map (List.map (\neuron -> { neuron | weights = List.map (always x) neuron.weights })) net.layers })
            |> (\net -> { net | outputNeuron = updateOutputNeuron net.outputNeuron })


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
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
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
