module Client.Game exposing (..)

import Client.Player exposing (Player)
import Html exposing (..)


type alias Game =
    { players : List Player }


view : Game -> Html msg
view game =
    div [] <|
        List.map
            Client.Player.view
            game.players
