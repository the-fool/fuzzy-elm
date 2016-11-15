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


selectXor : Float -> List Point
selectXor val =
    let
        {--TODO: figure out a more elegant way to keep introducing new seeds --}
        seeder =
            toFloat Random.maxInt / val |> truncate
    in
        xorData seeder


xorData : Int -> List Point
xorData seeder =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                ( x, y, 1 )
            else
                ( x, y, -1 )

        padding =
            0.04 * dataRange

        pad i =
            if i > 0 then
                i + padding
            else
                i - padding
    in
        randomPairs seeder ( -(dataRange - 1), (dataRange + 1) ) 200
            |> List.map (\( x, y ) -> ( pad x, pad y ))
            |> List.map label


randomPairs : Int -> ( Float, Float ) -> Int -> List ( Float, Float )
randomPairs seeder ( min, max ) len =
    let
        seed =
            Random.initialSeed seeder |> Random.step Random.independentSeed |> snd

        gen =
            Random.pair (Random.float min max) (Random.float min max)
                |> Random.list len
    in
        Random.step gen seed |> fst


scale : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
scale domain range x =
    let
        ( min, max ) =
            domain

        ( a, b ) =
            range
    in
        (b - a) * (x - min) / (max - min) + a



--    Produce list of network outputs (in the form of matrices) for each input point.
--    There will be density ^ 2 points


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
