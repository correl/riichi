module Client exposing (..)

import Debug
import Client.Decode
import Client.Game exposing (Game)
import Html exposing (..)
import Json.Decode exposing (decodeString)
import String
import WebSocket


type alias Model =
    { game : Maybe Game
    , log : List String
    }


type Msg
    = Receive String
    | Send String
    | Log String
    | NewState Game


init : Model
init =
    { game = Nothing
    , log = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive m ->
            socketMsg m
                |> Maybe.map (\msg -> update msg model)
                |> Maybe.withDefault ( model, Cmd.none )

        Log m ->
            ( { model | log = m :: model.log }
            , Cmd.none
            )

        NewState game ->
            ( { model | game = Just (Debug.log "game" game) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


socketMsg : String -> Maybe Msg
socketMsg message =
    let
        splitMsg =
            case String.split ":" (Debug.log "message" message) of
                msgType :: rest ->
                    Just ( msgType, String.join ":" rest )

                _ ->
                    Nothing

        toMsg ( msgType, rest ) =
            case msgType of
                "log" ->
                    Just (Log rest)

                "new_state" ->
                    decodeString Client.Decode.game rest
                        |> Result.toMaybe
                        |> Maybe.map NewState

                _ ->
                    Nothing
    in
        splitMsg
            |> Maybe.andThen toMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://localhost:8080/websocket" Receive


view : Model -> Html msg
view model =
    div []
        [ Maybe.map Client.Game.view model.game
            |> Maybe.withDefault (div [] [])
        , pre [] [ text <| String.join "\n" model.log ]
        ]
