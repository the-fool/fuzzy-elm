module Network exposing (..)

import Array exposing (..)


type alias Network =
    { layers : List Layer }


type alias Layer =
    List Neuron


type alias Neuron =
    List Float


networkFactory : List Int -> Network
networkFactory layerDims =
    let
        layers =
            fromList layerDims

        randomWeights len =
            List.repeat len 0.7

        entryLayer =
            case List.head layerDims of
                Just n ->
                    List.repeat n [ 1.0, 0.0 ]

                Nothing ->
                    []

        hiddenLayers =
            List.map
                (\x ->
                    case get (x - 1) layers of
                        Just numPrev ->
                            case get x layers of
                                Just numHere ->
                                    List.repeat numHere (randomWeights numPrev)

                                Nothing ->
                                    []

                        Nothing ->
                            []
                )
                [1..(length layers - 1)]

        allLayers =
            [ entryLayer ] ++ hiddenLayers
    in
        { layers = allLayers }
