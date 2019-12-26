module Riichi exposing (..)

import Client
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Stylesheets as S


{ id, class, classList } =
    Html.CssHelpers.withNamespace "riichi"


type alias Model =
    { tileset : S.Tileset
    , client : Client.Model
    }

type alias Flags =
    { websocket : String }


type Msg
    = SetTileset S.Tileset
    | ClientMsg Client.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { tileset = S.White
      , client = Client.init flags.websocket
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

        ClientMsg m ->
            let
                ( client, effects ) =
                    Client.update m model.client
            in
                ( { model | client = client }
                , Cmd.map ClientMsg effects
                )


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map ClientMsg <| Client.subscriptions model.client ]


view : Model -> Html Msg
view model =
    div [ class [ S.Tileset model.tileset ] ]
        [ h1 [] [ text "Riichi Mahjong" ]
        , fieldset []
            [ legend [] [ text "Tile Set" ]
            , radio "tileset" "White" (SetTileset S.White) (model.tileset == S.White)
            , radio "tileset" "Black" (SetTileset S.Black) (model.tileset == S.Black)
            ]
        , Client.view model.client
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
