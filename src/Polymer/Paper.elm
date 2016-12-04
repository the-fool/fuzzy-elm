module Polymer.Paper exposing (button)

import Html exposing (Html, Attribute, node)


button : List (Attribute a) -> List (Html a) -> Html a
button =
    node "paper-button"
