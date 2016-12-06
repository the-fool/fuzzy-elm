module Datasets exposing (..)

import Random.Pcg as Random
import List.Extra
import Array exposing (Array)
import Core


type Dataset
    = XOR
    | Gauss
    | Circle


type alias Coord =
    ( Float, Float )


type alias Point =
    { coord : Coord, label : Int }


getData : Dataset -> Random.Seed -> Array Point
getData kind =
    case kind of
        XOR ->
            xorData

        Gauss ->
            gaussData

        Circle ->
            circleData


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


gaussData : Random.Seed -> Array Point
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
    in
        (positive |> List.map (toPoint 1))
            ++ (negative |> List.map (toPoint -1))
            |> Array.fromList


toPoint : Int -> Coord -> Point
toPoint label coord =
    { coord = coord, label = label }


circleData : Random.Seed -> Array Point
circleData seeder =
    let
        radius =
            Core.dataRange - 1

        angles =
            randomList seeder ( 0, 2 * pi ) Core.numInputs

        radii seed a b =
            randomList seed ( a, b ) (Core.numInputs // 2) |> Tuple.first

        innerCircle =
            List.map2 (pointFactory 1)
                (radii seeder 0 (radius * 0.5))
                (List.take (Core.numInputs // 2) (angles |> Tuple.first))

        outerCircle =
            List.map2 (pointFactory -1)
                (radii (angles |> Tuple.second) (radius * 0.7) radius)
                (List.drop (Core.numInputs // 2) (angles |> Tuple.first))

        pointFactory label r angle =
            toPoint label ( sin angle |> (*) r, cos angle |> (*) r )
    in
        innerCircle ++ outerCircle |> Array.fromList


xorData : Random.Seed -> Array Point
xorData seeder =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                { coord = ( x, y ), label = -1 }
            else
                { coord = ( x, y ), label = 1 }

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
            |> Array.fromList


randomList : Random.Seed -> ( Float, Float ) -> Int -> ( List Float, Random.Seed )
randomList seed ( min, max ) len =
    let
        gen =
            Random.float min max |> Random.list len
    in
        Random.step gen seed


randomCoords : Random.Seed -> Coord -> Int -> ( List Coord, Random.Seed )
randomCoords seed ( min, max ) len =
    let
        gen =
            Random.pair (Random.float min max) (Random.float min max)
                |> Random.list len
    in
        Random.step gen seed
