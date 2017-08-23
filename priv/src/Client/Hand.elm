module Client.Hand exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Tile exposing (Tile)


type alias Hand =
    { tiles : List Tile
    }


view : Hand -> Html a
view model =
    div [ class "hand" ]
        [ div [ class "tiles open" ] <|
            List.map Tile.view model.tiles
        ]
