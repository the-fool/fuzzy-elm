module CanvasViz exposing (..)

import Datasets
import Network
import List.Extra


type alias RgbaGrid =
    List (List (List Int))


type alias NeuronDatum =
    { id : String, data : List Int }


makeCanvasMessage : Datasets.AggregatedPredictions -> String -> { output : List Int, neurons : List NeuronDatum }
makeCanvasMessage predictions jumboID =
    let
        neurons =
            predictions |> toRgba |> toCanvasData

        output =
            case List.Extra.find (.id >> (==) jumboID) neurons of
                Just item ->
                    item.data

                Nothing ->
                    Debug.crash "Unfound ID for jumbo neuron!"
    in
        { output = output, neurons = neurons }


toCanvasData : RgbaGrid -> List NeuronDatum
toCanvasData grid =
    let
        idDataRecorder id data =
            { id = id, data = data }
    in
        Network.gridPrism idDataRecorder grid |> List.concat


toRgba : Datasets.AggregatedPredictions -> RgbaGrid
toRgba =
    let
        opacity =
            120

        toRgba point =
            if point < 0 then
                [ 245, 147, 34 ] ++ [ opacity ]
            else
                [ 8, 119, 189 ] ++ [ opacity ]
    in
        List.map (List.map (List.concatMap toRgba))
