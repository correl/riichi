-module(player).

-include("riichi.hrl").

-export([new/0,
         new/1,
         new/2,
         discards/1,
         draw/2]).

new() ->
    new("Computer").

new(Name) ->
    new(Name, player_dummy).

new(Name, Type) ->
    {ok, PID} = Type:start_link(Name),
    #player{name = Name, pid = PID}.

discards(#player{discards = Discards} = Player) ->
    [{discard, Tile, Player#player{hand = Hand, discards = [Tile|Discards]}}
     || {discard, Tile, Hand} <- hand:discards(Player#player.hand)].

draw(#player{hand = Hand} = Player, Tile) ->
    Player#player{hand = hand:draw(Hand, Tile)}.
