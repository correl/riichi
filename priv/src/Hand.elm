module Hand exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Maybe.Extra
import Tile exposing (Tile)


type alias JSON =
    { tiles : List String
    }


type alias Model =
    { tiles : List Tile
    }


fromJSON : JSON -> Model
fromJSON j =
    { tiles =
        j.tiles
            |> List.map Tile.fromString
            |> Maybe.Extra.values
    }


view : Model -> Html a
view model =
    div [ class "hand" ]
        [ div [ class "tiles open" ] <|
            List.map Tile.view model.tiles
        ]
