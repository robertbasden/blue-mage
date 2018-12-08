module Page.ViewImage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view imageId =
    div [] [ h1 [] [ text ("View Image " ++ imageId) ] ]
