module Datasets exposing (..)

import Random.Pcg as Random
import List.Extra
import Core


type alias Coord =
    ( Float, Float )


type alias Point =
    { coord : Coord, label : Int }


normal : Float -> Float -> Float -> Float
normal mu sigma x =
    let
        factor =
            1 / (sigma * sqrt (2 * pi))

        exponent =
            -((x - mu) ^ 2 / (2 * sigma ^ 2))
    in
        factor * e ^ exponent


gauss : Float -> Float -> ( Float, Float ) -> ( Float, Float )
gauss mean variance ( a, b ) =
    let
        {- ( a, b ) =
           Random.step (Random.pair (Random.float 0 1) (Random.float 0 1)) seed |> Tuple.first
        -}
        ( x1, x2 ) =
            ( 2 * a - 1, 2 * b - 1 )

        w =
            x1 ^ 2 + x2 ^ 2

        w1 =
            if w >= 1 then
                0.999
            else
                w

        factor =
            sqrt (-2 * (logBase e w1) / w1)

        shift x =
            mean + sqrt variance * x
    in
        ( shift <| factor * x1, shift <| factor * x2 )


gaussData : Random.Seed -> List Point
gaussData seeder =
    let
        midway =
            (Core.dataRange - 1) / 2

        deviance =
            randomCoords seeder ( 0, 1 ) Core.numInputs
                |> Tuple.first
                |> List.map (gauss 0 (midway / 3))
                |> List.Extra.splitAt (Core.numInputs // 2)

        positive =
            Tuple.first deviance
                |> List.map (\( a, b ) -> ( midway + a, midway + b ))

        negative =
            Tuple.second deviance
                |> List.map (\( a, b ) -> ( -midway + a, -midway - b ))

        toPoint label coord =
            { coord = coord, label = label }
    in
        (positive |> List.map (toPoint 1)) ++ (negative |> List.map (toPoint -1))


xorData : Random.Seed -> List Point
xorData seeder =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                { coord = ( x, y ), label = 1 }
            else
                { coord = ( x, y ), label = -1 }

        padding =
            Core.dataRange / 20

        pad i =
            if i > 0 then
                i + padding
            else
                i - padding

        data =
            randomCoords seeder ( -(Core.dataRange - 1), (Core.dataRange - 1) ) Core.numInputs |> Tuple.first
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
