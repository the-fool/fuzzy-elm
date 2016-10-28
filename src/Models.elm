module Models exposing (..)

import Hop.Types exposing (Address)
import Routing exposing (Route)


type alias AppModel =
    { route : Route
    , address : Address
    , selectedInput : Maybe Int
    }


newAppModel : Route -> Address -> AppModel
newAppModel route address =
    { selectedInput = Maybe.Nothing
    , route = route
    , address = address
    }
