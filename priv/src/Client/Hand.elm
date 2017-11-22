module Client.Hand exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Tile exposing (Tile)


type alias Hand =
    { tiles : List Tile
    }


view : Hand -> Bool -> Html a
view model open =
    div [ class "hand" ]
        [ div
            [ class "tiles" ]
          <|
            case open of
                True ->
                    List.map Tile.view model.tiles
                False ->
                    List.map Tile.viewHidden model.tiles
        ]
