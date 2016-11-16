module Models exposing (..)

import Network exposing (..)
import Datasets
import Random.Pcg as Random


colors : { positive : String, negative : String }
colors =
    { negative = "rgb(245, 147, 34)"
    , positive = "rgb(8, 119, 189)"
    }



-- TODO: Write a decode/encode to move union types to localStorage


type alias NetworkState =
    Int


type alias Predictions =
    List (List (List Float))


type alias Model =
    { network : Network
    , window : ( Int, Int )
    , inputs : List Point
    , state : NetworkState
    , nTicks : Int
    , brutePredictions : Predictions
    , randomSeed : Random.Seed
    }


type alias Point =
    ( Float, Float, Int )


initialNetwork : Network
initialNetwork =
    Network.networkFactory Sigmoid [ X, Y ] [ 2, 2 ]


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> snd


initialInputsAndSeed : ( List Point, Random.Seed )
initialInputsAndSeed =
    Datasets.xorData seed0


initialModel : Model
initialModel =
    { network = initialNetwork
    , window = ( 1, 1 )
    , inputs = fst initialInputsAndSeed
    , state = 0
    , nTicks = 0
    , brutePredictions = Datasets.brutePredictions initialNetwork
    , randomSeed = snd initialInputsAndSeed
    }
