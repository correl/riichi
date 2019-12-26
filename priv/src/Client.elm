module Client exposing (..)

import Debug
import Client.Decode
import Client.Game exposing (Game)
import Client.Action exposing (Action)
import Html exposing (..)
import Json.Decode exposing (decodeString)
import String
import WebSocket


type alias Model =
    { url : String
    , game : Maybe Game
    , choice : Maybe (List Action)
    , log : List String
    }


type Msg
    = Receive String
    | Send String
    | Log String
    | Choice (List Action)
    | NewState Game


init : String -> Model
init url =
    { url = url
    , game = Nothing
    , choice = Nothing
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

        Choice choice ->
            ( { model | choice = Just (Debug.log "choice" choice) }
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

                "choose" ->
                    decodeString Client.Decode.choice rest
                        |> Result.toMaybe
                        |> Maybe.map Choice

                _ ->
                    Nothing
    in
        splitMsg
            |> Maybe.andThen toMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.url Receive


view : Model -> Html msg
view model =
    div []
        [ Maybe.map Client.Game.view model.game
            |> Maybe.withDefault (div [] [])
        , Maybe.map Client.Action.viewChoice model.choice
            |> Maybe.withDefault (div [] [])
        , pre [] [ text <| String.join "\n" model.log ]
        ]
