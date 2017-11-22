-module(game_tree).

-include("../include/riichi.hrl").
-include("../include/lazy.hrl").

-export([build/1,
         do/2]).

-record(game_tree, {game, actions}).

-type game_tree() :: #game_tree{}.

-define(cond_actions(Expr, Actions), case Expr of
                                         true ->
                                             Actions;
                                         _ ->
                                             []
                                     end).

-spec(build(game()) -> game_tree()).
build(Game) ->
    {game_tree, Game, actions(Game)}.

-spec(actions(game()) -> game_action:game_action()).
actions(#game{phase = start, wall = []}) ->
    %% No tile remain in the live wall at the start of a player's turn
    %% Terminate the game

    %% TODO: Score exhaustive draw
    exhaustive_draw;
actions(#game{phase = start, turn = Turn} = Game) ->
    %% Begin a player's turn by having them draw a tile from the live wall

    Updated = game:draw(Game),
    [{game_action:new(Turn, draw), ?LAZY(build(Updated#game{phase=draw}))}];
actions(#game{phase = draw, turn = Turn} = Game) ->
    %% This is the player's main turn phase

    Player = game:current_player(Game),

    lists:flatten([
                   ?cond_actions(riichi_hand:is_complete(Player#player.hand),
                                 [{game_action:new(Turn, tsumo), Game}]),
                   [{game_action:new(Turn, discard, Tile), ?LAZY(build(Updated#game{phase=discard}))}
                    || {discard, Tile, Updated} <- game:discards(Game)]
                  ]);
actions(#game{phase = discard, turn = Turn} = Game) ->
    %% TODO: Can any of the players steal the discarded tile?
    Updated = Game#game{phase = start, turn = riichi:next(wind, Turn)},
    [{game_action:new(Seat, pass), ?LAZY(build(Updated))}
     || Seat <- [east, south, west, north]];
actions(#game{} = Game) ->
    error_logger:error_report([{?MODULE, invalid_game_state},
                               {game, Game}]),
    {error, invalid_game_state}.

-spec(do(game_tree(), game_action:game_action()) -> game_tree()).
do(Tree, Action) ->
    case proplists:get_value(Action, Tree#game_tree.actions) of
        undefined ->
            error(invalid_game_action);
        Thunk ->
            case Action of
                tsumo -> {win, tsumo, Tree#game_tree.game};
                ron -> {win, ron, Tree#game_tree.game};
                _ -> ?FORCE(Thunk)
            end
    end.
