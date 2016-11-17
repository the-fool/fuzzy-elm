module Datasets exposing (..)

import Random.Pcg as Random
import List.Extra exposing (lift2)
import Constants
import Network


type alias Coord =
    ( Float, Float )


type alias Point =
    ( Float, Float, Int )


type alias Prediction =
    List (List Float)


type alias AggregatedPredictions =
    List (List (List Float))


xorData : Random.Seed -> List Point
xorData seeder =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                ( x, y, 1 )
            else
                ( x, y, -1 )

        padding =
            Constants.dataRange / 20

        pad i =
            if i > 0 then
                i + padding
            else
                i - padding

        data =
            randomCoords seeder ( -(Constants.dataRange - 1), (Constants.dataRange - 1) ) 200 |> fst
    in
        data
            |> List.map (\( x, y ) -> ( pad x, pad y ))
            |> List.map label


randomCoords : Random.Seed -> Coord -> Int -> ( List Coord, Random.Seed )
randomCoords seed ( min, max ) len =
    let
        gen =
            Random.pair (Random.float min max) (Random.float min max)
                |> Random.list len
    in
        Random.step gen seed


scale : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
scale domain range x =
    let
        ( min, max ) =
            domain

        ( a, b ) =
            range
    in
        (b - a) * (x - min) / (max - min) + a



{--
Produce list of network outputs (in the form of matrices) for each input point.
There will be density ^ 2 points
--}


brutePredictions : Network.Network -> List Prediction
brutePredictions network =
    let
        scaleFun =
            scale ( 0, toFloat Constants.density ) ( -Constants.dataRange, Constants.dataRange )

        scaledInputs =
            List.map (toFloat >> scaleFun) [0..(Constants.density - 1)]

        points =
            lift2 (\y x -> ( x, y )) scaledInputs scaledInputs
    in
        -- The entry layer in this list are bogus
        List.map (Network.forwardProp network) points



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


getPredictionGrid : Network.Network -> AggregatedPredictions
getPredictionGrid =
    brutePredictions >> aggregatePredictions
