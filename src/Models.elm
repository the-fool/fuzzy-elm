module Models exposing (..)

import Random.Pcg as Random


colors : { positive : String, negative : String }
colors =
    { negative = "rgb(245, 147, 34)"
    , positive = "rgb(8, 119, 189)"
    }


type alias Model =
    { network : Network
    , window : ( Int, Int )
    , inputs : List Point
    }


type alias Point =
    ( Float, Float, Int )


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
    , window = ( 1, 1 )
    , inputs = [ ( 0, 0, 0 ) ]
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
