module Datasets exposing (..)

import Random.Pcg as Random
import Models exposing (Point)
import Update exposing (Msg(..))


selectXor : Float -> Msg
selectXor val =
    let
        seeder =
            toFloat Random.maxInt / val |> truncate
    in
        SelectInput <| xorData seeder ( -5, 5 )


xorData : Int -> ( Float, Float ) -> List Point
xorData seeder ( min, max ) =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                ( x, y, 1 )
            else
                ( x, y, -1 )

        padding =
            0.03 * (max - min)

        pad i =
            if i > 0 then
                i + padding
            else
                i - padding
    in
        randomPairs seeder ( min, max ) 100
            |> List.map (\( x, y ) -> ( pad x, pad y ))
            |> List.map label


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> snd


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
