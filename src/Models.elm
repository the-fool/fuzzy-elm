module Models exposing (..)

import Network exposing (..)
import Datasets
import Random.Pcg as Random


-- TODO: Write a decode/encode to move union types to localStorage


type alias NetworkState =
    Int


type alias Model =
    { network : Network.Network
    , window : ( Int, Int )
    , inputs : List Datasets.Point
    , state : NetworkState
    , nTicks : Int
    , brutePredictions : List Network.Prediction
    , randomSeed : Random.Seed
    }


initialNetwork : Network
initialNetwork =
    Network.networkFactory seed0 Sigmoid [ X, Y ] [ 2, 2 ]


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> Tuple.second


initialModel : Model
initialModel =
    { network = initialNetwork
    , window = ( 1, 1 )
    , inputs = Datasets.xorData seed0
    , state = 0
    , nTicks = 0
    , brutePredictions = Network.brutePredictions initialNetwork
    , randomSeed = seed0
    }
