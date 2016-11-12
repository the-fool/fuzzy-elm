module Datasets exposing (..)

import Random.Pcg as Random
import Models exposing (Point)
import Update exposing (Msg(..))


selectXor : Msg
selectXor =
    SelectInput <| xorData ( -5, 5 )


xorData : ( Float, Float ) -> List Point
xorData ( min, max ) =
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
        randomPairs ( min, max ) 100
            |> List.map (\( x, y ) -> ( pad x, pad y ))
            |> List.map label


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> snd


randomPairs : ( Float, Float ) -> Int -> List ( Float, Float )
randomPairs ( min, max ) len =
    let
        gen =
            Random.pair (Random.float min max) (Random.float min max)
                |> Random.list len
    in
        Random.step gen seed0 |> fst
