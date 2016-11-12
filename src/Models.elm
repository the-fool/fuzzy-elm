module Models exposing (..)

import Network exposing (..)


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


emptyModel : Model
emptyModel =
    { network = networkFactory [ 2, 3, 4, 3, 1 ]
    , window = ( 1, 1 )
    , inputs = [ ( 0, 0, 0 ) ]
    }
