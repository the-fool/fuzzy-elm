module Datasets exposing (..)

import Random.Pcg as Random
import CanvasViz exposing (density)
import List.Extra exposing (lift2)
import Network


type alias Point =
    ( Float, Float, Int )


dataRange : Float
dataRange =
    5



xorData : Random.Seed -> (List Point, Random.Seed)
xorData seeder =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                ( x, y, 1 )
            else
                ( x, y, -1 )

        padding =
            0.04

        pad i =
            if i > 0 then
                i + padding
            else
                i - padding
        (data, seed) =
            randomPairs seeder ( -(dataRange - 1), (dataRange + 1) ) 200
    in
        (data
            |> List.map (\( x, y ) -> ( pad x, pad y ))
            |> List.map label
          , seed)


randomPairs : Random.Seed -> ( Float, Float ) -> Int -> (List ( Float, Float ), Random.Seed)
randomPairs seed ( min, max ) len =
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


brutePredictions : Network.Network -> List (List (List Float))
brutePredictions network =
    let
        scaleFun =
            scale ( 0, density ) ( -dataRange, dataRange )

        scaledInputs =
            List.map scaleFun [0..density]

        points =
            lift2 (\y x -> ( x, y )) scaledInputs scaledInputs
    in
        -- Drop 1 to ignore the original input element
        List.map ((Network.forwardProp network) >> (List.drop 1)) points



{--
Ok, this is kind of crazy.  For simplicity, we collect X number of predictions in a list of
X length, where each element is a jagged 2d array representing the whole network of neurons' separate predictions.
This list of X separate network-predictions for each data point needs to be folded down to an array
of the same shape as the network, except insted of neurons for the basic element,
there is the X-length list of predictions
--}


aggregatePredictions : List (List (List Float)) -> List (List (List Float))
aggregatePredictions allPoints =
    let
        shape =
            -- each element in points has same shape
            List.take 1 allPoints
                |> -- replace all the number elements with empty lists to 'seed' the fold
                   List.concatMap (List.map (List.map (always [])))
    in
        List.foldr (List.map2 (List.map2 (::))) shape allPoints
