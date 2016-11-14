module Models exposing (..)

import Network exposing (..)


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
    , heatViz : Predictions
    }


type alias Point =
    ( Float, Float, Int )


emptyModel : Model
emptyModel =
    { network = networkFactory "sigmoid" [ "x", "y" ] [ 2, 3, 4, 3, 1 ]
    , window = ( 1, 1 )
    , inputs = [ ( 0, 0, 0 ) ]
    , state = 0
    , nTicks = 0
    , heatViz = [ [ [ 0.0 ] ] ]
    }
