module Models exposing (..)


type alias Model =
    { network : Network
    }


type alias Network =
    { hidden : List Layer
    , entry : Layer
    }


type alias Layer =
    List Neuron


type alias Neuron =
    { id : Int
    }


emptyModel : Model
emptyModel =
    { network = initNetwork
    }


initNetwork : Network
initNetwork =
    { hidden = List.repeat 5 (List.map newNeuron [1..4])
    , entry = [ newNeuron 0, newNeuron 1 ]
    }


newNeuron : Int -> Neuron
newNeuron id =
    { id = id
    }
