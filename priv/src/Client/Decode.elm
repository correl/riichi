module Client.Decode exposing (..)

import Client.Hand exposing (Hand)
import Client.Game exposing (Game)
import Client.Player exposing (Player, Wind(..))
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


wind : Decoder Wind
wind =
    let
        toWind s = case s of
                       "east" -> East
                       "west" -> West
                       "south" -> South
                       _ -> North
    in
        string |> map toWind
            
player : Decoder Player
player =
    map5 Player
        (field "name" <| map (\s -> s == "Websocket") string)
        (field "name" string)
        (field "seat" wind)
        (field "hand" hand)
        (field "discards" (list tile))


game : Decoder Game
game =
    map Game
        (field "players" (list player))
