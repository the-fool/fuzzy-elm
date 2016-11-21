module Datasets exposing (..)

import Random.Pcg as Random
import Constants
import Network


type alias Coord =
    ( Float, Float )


type alias Point =
    ( Float, Float, Int )


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
            randomCoords seeder ( -(Constants.dataRange - 1), (Constants.dataRange - 1) ) 200 |> Tuple.first
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
