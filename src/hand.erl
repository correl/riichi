-module(hand).

-include("riichi.hrl").

-export([discards/1,
         draw/2]).

discards(#hand{tiles=Tiles} = Hand) ->
    [{discard, Tile, Hand#hand{tiles = Tiles -- [Tile]}} || Tile <- Tiles].

draw(#hand{tiles = Tiles} = Hand, Tile) ->
    Hand#hand{tiles = Tiles ++ [Tile]}.
