module Client.Player exposing (..)

import Client.Hand exposing (Hand)
import Tile exposing (Tile)
import Html exposing (..)

type Wind = East | South | West | North

type alias Player =
    { isMe : Bool
    , name : String
    , seat : Wind
    , hand : Hand
    , discards : List Tile
    }


view : Player -> Html msg
view player =
    fieldset []
        [ legend [] [ text ("Player: " ++ player.name ++ "(" ++ (toString player.seat) ++ ")") ]
        , Client.Hand.view player.hand player.isMe
        , viewDiscards player.discards
        ]

viewDiscards : List Tile -> Html msg
viewDiscards tiles =
    div [] <| List.map Tile.view tiles
