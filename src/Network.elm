module Network exposing (..)


type alias Network =
    { hidden : List Layer
    , entry : Layer
    }


type alias Layer =
    List Neuron


type alias Neuron =
    List Float


initNetwork : List Int -> Network
initNetwork layerDims =
    { hidden = List.map (\len -> List.repeat len newNeuron) layerDims
    , entry = [ newNeuron, newNeuron ]
    }


newNeuron : Neuron
newNeuron =
    [ 0.1, 0.3 ]
