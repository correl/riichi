module Tile exposing (..)

import Dict
import Html exposing (Html, span, text)
import Html.CssHelpers
import List.Extra
import String
import Stylesheets as S


{ id, class, classList } =
    Html.CssHelpers.withNamespace "riichi"


type Wind
    = East
    | South
    | West
    | North


type Dragon
    = Green
    | Red
    | White


type Suit
    = Pin
    | Sou
    | Man


type Tile
    = Wind Wind
    | Dragon Dragon
    | Suited Suit Int


type CssClass
    = Tile Tile


fromString : String -> Maybe Tile
fromString s =
    let
        parts =
            String.toLower s
                |> String.split " "
    in
        case parts of
            [ value, suit ] ->
                make suit value

            _ ->
                Nothing


make : String -> String -> Maybe Tile
make suit value =
    let
        winds =
            Dict.fromList
                [ ( "east", East )
                , ( "south", South )
                , ( "west", West )
                , ( "north", North )
                ]

        dragons =
            Dict.fromList
                [ ( "green", Green )
                , ( "red", Red )
                , ( "white", White )
                ]

        suits =
            Dict.fromList
                [ ( "pin", Pin )
                , ( "sou", Sou )
                , ( "man", Man )
                ]
    in
        case suit of
            "wind" ->
                Dict.get value winds
                    |> Maybe.map Wind

            "dragon" ->
                Dict.get value dragons
                    |> Maybe.map Dragon

            _ ->
                let
                    s =
                        Dict.get suit suits

                    v =
                        String.toInt value
                            |> Result.toMaybe
                            |> Maybe.andThen
                                (\x ->
                                    if (x >= 1) && (x <= 9) then
                                        Just x
                                    else
                                        Nothing
                                )
                in
                    Maybe.map2 Suited s v


tiles : List Tile
tiles =
    List.concat
        [ List.map Wind [ East, South, West, North ]
        , List.map Dragon [ Red, Green, White ]
        , List.Extra.lift2 Suited
            [ Man, Sou, Pin ]
            (List.range 1 9)
        ]


cssName : Tile -> String
cssName tile =
    case tile of
        Dragon Red ->
            "Chun"

        Dragon Green ->
            "Hatsu"

        Dragon White ->
            "Haku"

        Wind East ->
            "Ton"

        Wind South ->
            "Nan"

        Wind West ->
            "Shaa"

        Wind North ->
            "Pei"

        Suited Pin v ->
            String.concat [ "Pin", (toString v) ]

        Suited Sou v ->
            String.concat [ "Sou", (toString v) ]

        Suited Man v ->
            String.concat [ "Man", (toString v) ]


view : Tile -> Html a
view tile =
    span [ class [ (S.Tile (cssName tile)) ] ] [ span [] [] ]
