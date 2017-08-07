module Riichi exposing (..)

import Hand
import Html exposing (..)
import Html.CssHelpers
import Stylesheets as S


{ id, class, classList } =
    Html.CssHelpers.withNamespace "riichi"


type alias Model =
    { hand : Hand.Model }


type Msg
    = Noop


init : ( Model, Cmd Msg )
init =
    ( { hand =
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
    ( model, Cmd.none )


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
    div [ class [ S.Tileset S.White ] ]
        [ h1 [] [ text "Riichi Mahjong" ]
        , Hand.view model.hand
        ]
