module Client.Decode exposing (..)

import Client.Hand exposing (Hand)
import Client.Game exposing (Game)
import Client.Player exposing (Player)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (fromResult)
import Tile exposing (Tile)


tile : Decoder Tile
tile =
    let
        combine a b =
            a ++ " " ++ b

        stringOrInt =
            oneOf
                [ string
                , int |> map toString
                ]
    in
        map2 combine
            (field "value" stringOrInt)
            (field "suit" string)
            |> andThen
                (Tile.fromString
                    >> (Result.fromMaybe "invalid tile")
                    >> fromResult
                )


hand : Decoder Hand
hand =
    map Hand
        (field "tiles" (list tile))


player : Decoder Player
player =
    map2 Player
        (field "name" string)
        (field "hand" hand)


game : Decoder Game
game =
    map Game
        (field "players" (list player))
