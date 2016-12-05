module Models exposing (..)

import Network exposing (..)
import Datasets
import Random.Pcg as Random
import Array exposing (Array)


-- TODO: Write a decode/encode to move union types to localStorage


type NetworkState
    = Going
    | Paused


type alias Model =
    { network : Network.Network
    , window : ( Int, Int )
    , inputs : Array Datasets.Point
    , state : NetworkState
    , nTicks : Int
    , randomSeed : Random.Seed
    , hoverCard : HoverCard
    }


type alias HoverCard =
    { x : Int
    , y : Int
    , weight : Float
    , visible : Bool
    }


initialNetwork : Network
initialNetwork =
    Network.networkFactory seed0 Tanh [ X, Y ] [ 3, 2 ]


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> Tuple.second


initialModel : Model
initialModel =
    { network = initialNetwork
    , window = ( 1, 1 )
    , inputs = Datasets.xorData seed0
    , state = Paused
    , nTicks = 0
    , randomSeed = seed0
    , hoverCard = { x = 0, y = 0, weight = 0.0, visible = False }
    }
