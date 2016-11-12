module Datasets exposing (..)

import Random.Pcg as Random
import Models exposing (Point)
import Update exposing (Msg(..))


selectXor : Float -> Msg
selectXor val =
    let
        {--TODO: figure out a more elegant way to keep introducing new seeds --}
        seeder =
            toFloat Random.maxInt / val |> truncate
    in
        SelectInput <| xorData seeder ( -4, 4 )


xorData : Int -> ( Float, Float ) -> List Point
xorData seeder ( min, max ) =
    let
        label ( x, y ) =
            if (x * y) >= 0 then
                ( x, y, 1 )
            else
                ( x, y, -1 )

        padding =
            0.04 * (max - min)

        pad i =
            if i > 0 then
                i + padding
            else
                i - padding
    in
        randomPairs seeder ( min, max ) 200
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
