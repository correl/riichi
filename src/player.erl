-module(player).

-include("riichi.hrl").

-export([new/0,
         new/1,
         new/2,
         send/2,
         discards/1,
         draw/2]).

new() ->
    new("Computer").

new(Name) ->
    {ok, Pid} = player_dummy:start_link(Name),
    new(Name, Pid).

new(Name, Pid) ->
    #player{name = Name, pid = Pid}.

send(#player{pid = Pid}, Message) ->
    gen_server:cast(Pid, Message).

discards(#player{discards = Discards} = Player) ->
    [{discard, Tile, Player#player{hand = Hand, discards = [Tile|Discards]}}
     || {discard, Tile, Hand} <- hand:discards(Player#player.hand)].

draw(#player{hand = Hand} = Player, Tile) ->
    Player#player{hand = hand:draw(Hand, Tile)}.
