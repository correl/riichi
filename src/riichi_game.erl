%% @author Correl Roush <correl@gmail.com>
%%
%% @doc Riichi Mahjong library.

-module(riichi_game).

-include("../include/riichi.hrl").

-export([new/0]).

%% @doc Creates a new mahjong game, with all 136 tiles shuffled and organized.
-spec new() -> game().
new() ->
    Tiles = riichi:shuffle(riichi:tiles()),
    #game{rinshan=lists:sublist(Tiles, 1, 4),
          dora=lists:sublist(Tiles, 5, 5),
          uradora=lists:sublist(Tiles, 10,5),
          wall=lists:sublist(Tiles, 15, 124)}.
