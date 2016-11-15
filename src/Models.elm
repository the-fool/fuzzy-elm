module Models exposing (..)

import Network exposing (..)
import Datasets


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
    }


type alias Point =
    ( Float, Float, Int )


initialNetwork : Network
initialNetwork =
    Network.networkFactory "sigmoid" [ "x", "y" ] [ 2, 2 ]


initialModel : Model
initialModel =
    { network = initialNetwork
    , window = ( 1, 1 )
    , inputs = Datasets.xorData 999
    , state = 0
    , nTicks = 0
    , brutePredictions = Datasets.brutePredictions initialNetwork
    }
