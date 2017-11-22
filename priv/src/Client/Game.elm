module Client.Game exposing (..)

import Client.Player exposing (Player)
import Html exposing (..)


type alias Game =
    { players : List Player
    , wall : Int
    }


view : Game -> Html msg
view game =
    div []
        [ div [] [ text (toString game.wall) ]
        , div [] <|
            List.map
                Client.Player.view
                game.players
        ]
