module Core exposing (..)

import List.Extra exposing (lift2)
import Random.Pcg as Random exposing (Generator)
import Array exposing (Array)


colors : { positive : String, negative : String }
colors =
    { negative = "#4EA701"
    , positive = "#2874ae"
    }


dataRange : Float
dataRange =
    5


learningRates : List Float
learningRates =
    [ 0.001, 0.005, 0.01, 0.05, 0.1, 0.5 ]


density : Int
density =
    30


numInputs : Int
numInputs =
    200


scale : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
scale ( min, max ) ( a, b ) x =
    (b - a) * (x - min) / (max - min) + a


brutePoints : List ( Float, Float )
brutePoints =
    let
        scaleFun =
            scale ( 0, toFloat density ) ( -dataRange, dataRange )

        scaledInputs =
            List.map (toFloat >> scaleFun) (List.range 0 (density - 1))
    in
        lift2 (\y x -> ( x, y )) scaledInputs scaledInputs


indexedBrutePoints : List ( Int, ( Float, Float ) )
indexedBrutePoints =
    List.Extra.zip (List.range 0 (List.length brutePoints)) brutePoints


nextSeed : Random.Seed -> Random.Seed
nextSeed seed =
    Random.step Random.bool seed |> Tuple.second



--- Randomizer


choose : Array a -> Generator ( Maybe a, Array a )
choose arr =
    if Array.isEmpty arr then
        Random.constant ( Nothing, arr )
    else
        let
            lastIndex =
                Array.length arr - 1

            front i =
                Array.slice 0 i arr

            back i =
                if
                    i == lastIndex
                    -- workaround for #1
                then
                    Array.empty
                else
                    Array.slice (i + 1) (lastIndex + 1) arr

            gen =
                Random.int 0 lastIndex
        in
            Random.map
                (\index ->
                    ( Array.get index arr, Array.append (front index) (back index) )
                )
                gen


shuffle : Array a -> Generator (Array a)
shuffle arr =
    if Array.isEmpty arr then
        Random.constant arr
    else
        let
            helper : ( List a, Array a ) -> Generator ( List a, Array a )
            helper ( done, remaining ) =
                choose remaining
                    |> Random.andThen
                        (\( m_val, shorter ) ->
                            case m_val of
                                Nothing ->
                                    Random.constant ( done, shorter )

                                Just val ->
                                    helper ( val :: done, shorter )
                        )
        in
            Random.map (Tuple.first >> Array.fromList) (helper ( [], arr ))
