module Client.Player exposing (..)

import Client.Hand exposing (Hand)
import Html exposing (..)

type Wind = East | South | West | North

type alias Player =
    { isMe : Bool
    , name : String
    , seat : Wind
    , hand : Hand
    }


view : Player -> Html msg
view player =
    fieldset []
        [ legend [] [ text ("Player: " ++ player.name ++ "(" ++ (toString player.seat) ++ ")") ]
        , Client.Hand.view player.hand player.isMe
        ]
