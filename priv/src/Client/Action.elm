module Client.Action exposing (..)

import Html exposing (Html)
import Html.CssHelpers
import Tile exposing (Tile)
import Stylesheets as S


{ id, class, classList } =
    Html.CssHelpers.withNamespace "riichi"


type Action
    = Discard Tile
    | DoNothing


viewChoice : List Action -> Html msg
viewChoice actions =
    Html.div []
        [ Html.h1 [] [ Html.text "Choose an action:" ]
        , Html.ol [] (List.map view actions)
        ]


view : Action -> Html msg
view action =
    case action of
        Discard tile ->
            Html.li []
                [ Html.text "Discard"
                , Tile.view tile
                ]

        _ ->
            Html.li [] [ Html.text (toString action) ]
