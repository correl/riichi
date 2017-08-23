module Client.Player exposing (..)

import Client.Hand exposing (Hand)
import Html exposing (..)


type alias Player =
    { name : String
    , hand : Hand
    }


view : Player -> Html msg
view player =
    fieldset []
        [ legend [] [ text ("Player: " ++ player.name) ]
        , Client.Hand.view player.hand
        ]
