module Models exposing (..)

import Network exposing (..)
import Datasets
import Random.Pcg as Random
import Array exposing (Array)
import Datasets exposing (Point, Dataset)


-- TODO: Write a decode/encode to move union types to localStorage


type NetworkState
    = Going
    | Paused


type Brush
    = Positive
    | Negative


type DataMode
    = Custom
    | Stock Dataset


type alias Model =
    { network : Network.Network
    , window : ( Int, Int )
    , inputs : Array Datasets.Point
    , state : NetworkState
    , nTicks : Int
    , randomSeed : Random.Seed
    , hoverCard : HoverCard
    , best : Int
    , customData : Array Point
    , dataMode : DataMode
    , brush : Brush
    }


type alias HoverCard =
    { x : Int
    , y : Int
    , weight : Float
    , visible : Bool
    }


initialNetwork : Network
initialNetwork =
    Network.networkFactory seed0 Tanh [ X, Y ] [ 3, 2 ] 0.01


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
    , best = 0
    , customData = Array.fromList []
    , dataMode = Stock Datasets.XOR
    , brush = Positive
    }
