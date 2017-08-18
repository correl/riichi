module Riichi exposing (..)

import Hand
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Stylesheets as S


{ id, class, classList } =
    Html.CssHelpers.withNamespace "riichi"


type alias Model =
    { tileset : S.Tileset
    , hand : Hand.Model
    }


type Msg
    = SetTileset S.Tileset


init : ( Model, Cmd Msg )
init =
    ( { tileset = S.White
      , hand =
            Hand.fromJSON
                { tiles =
                    [ "4 pin"
                    , "5 pin"
                    , "6 pin"
                    , "4 sou"
                    , "5 sou"
                    , "6 sou"
                    , "4 man"
                    , "5 man"
                    , "6 man"
                    , "red dragon"
                    ]
                }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTileset tileset ->
            ( { model | tileset = tileset }
            , Cmd.none
            )


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    div [ class [ S.Tileset model.tileset ] ]
        [ h1 [] [ text "Riichi Mahjong" ]
        , fieldset []
            [ legend [] [ text "Tile Set" ]
            , radio "tileset" "White" (SetTileset S.White) (model.tileset == S.White)
            , radio "tileset" "Black" (SetTileset S.Black) (model.tileset == S.Black)
            ]
        , Hand.view model.hand
        ]


radio : String -> String -> Msg -> Bool -> Html Msg
radio name_ value msg checked_ =
    label []
        [ input
            [ type_ "radio"
            , name name_
            , onClick msg
            , checked checked_
            ]
            []
        , text value
        ]
