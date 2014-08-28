%% @author Correl Roush <correl@gmail.com>
%%
%% @doc Riichi Mahjong library.
%%
%% @headerfile "../include/riichi.hrl"

-module(game).

-include("../include/riichi.hrl").

-export([new/0,
         new/1,
         add_player/2,
         current_player/1,
         position/1,
         discards/1,
         draw/1,
         get_player/2,
         update_player/3
]).

%% @doc Creates a new mahjong game, with all 136 tiles shuffled and organized.
-spec new() -> game().
new() ->
    Tiles = riichi:shuffle(riichi:tiles()),
    #game{rinshan=lists:sublist(Tiles, 1, 4),
          dora=lists:sublist(Tiles, 5, 5),
          uradora=lists:sublist(Tiles, 10,5),
          wall=lists:sublist(Tiles, 15, 124)}.

new(Players) ->
    lists:foldl(fun add_player/2, new(), Players).

add_player(_Player, #game{players=Players})
  when length(Players) >= 4 ->
    throw("Game full");
add_player(Name, Game = #game{})
  when is_list(Name) ->
    add_player(#player{name=Name}, Game);
add_player(Player = #player{}, Game = #game{players=Players}) ->
    Seats = ?WINDS,
    Seat = lists:nth(length(Players) + 1, Seats),
    {Tiles, Wall} = lists:split(12, Game#game.wall),
    Hand = #hand{tiles=Tiles},
    Game#game{wall = Wall,
              players=Players ++ [Player#player{seat = Seat, hand = Hand}]}.

current_player(#game{players = Players, turn = Turn}) ->
    lists:nth(position(Turn) + 1, Players).

position(Wind) ->
    case lists:member(Wind, ?WINDS) of
        true ->
            length(lists:takewhile(fun(W) ->
                                           W =/= Wind
                                   end,
                                   ?WINDS));
        _ ->
            error(invalid_wind)
    end.

discards(#game{turn = Turn} = Game) ->
    Player = current_player(Game),
    [{discard, Tile, update_player(Game, Turn, Updated)}
     || {discard, Tile, Updated} <- player:discards(Player)].

draw(#game{turn = Turn} = Game) ->
    [Tile|Wall] = Game#game.wall,
    Player = player:draw(current_player(Game), Tile),
    Updated = update_player(Game, Turn, Player),
    Updated#game{wall=Wall}.

get_player(#game{players = Players}, Seat) ->
    Pos = position(Seat) + 1,
    lists:nth(Pos, Players).

update_player(#game{players = Players} = Game, Seat, Player) ->
    Pos = position(Seat),
    Updated = lists:sublist(Players, Pos)
        ++ [Player]
        ++ lists:nthtail(Pos + 1, Players),
    Game#game{players = Updated}.
   
