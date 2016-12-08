module Polymer.Paper exposing (..)

import Html exposing (Html, Attribute, node)


button : List (Attribute a) -> List (Html a) -> Html a
button =
    node "paper-button"


listbox : List (Attribute a) -> List (Html a) -> Html a
listbox =
    node "paper-listbox"


item : List (Attribute a) -> List (Html a) -> Html a
item =
    node "paper-item"


dropdown : List (Attribute a) -> List (Html a) -> Html a
dropdown =
    node "paper-dropdown-menu"
