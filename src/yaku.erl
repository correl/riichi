%% @author Correl Roush <correl@gmail.com>
%%
%% @doc Riichi Mahjong library.

-module(yaku).

-include("../include/riichi.hrl").

-export([yakuhai/2,
        tanyao/2,
        pinfu/2,
        chiitoitsu/2,
        kokushi_musou/2]).

%% @doc Counts the pons/kans of value tiles in a player's hand.
%%      Value tiles include all of the dragons, plus the round wind and the player's seat wind.
-spec yakuhai(game(), player()) -> integer().
yakuhai(#game{round=Round}, #player{seat=Seat, hand=#hand{melds=Melds}}) ->
    length(lists:filter(fun(#meld{type=Type, tiles=[T|_]}) ->
                                case {Type, T} of
                                    {pair, _} ->
                                        false;
                                    {chii, _} ->
                                        false;
                                    {_, #tile{suit=wind, value=Round}} ->
                                        true;
                                    {_, #tile{suit=wind, value=Seat}} ->
                                        true;
                                    {_, #tile{suit=dragon}} ->
                                        true;
                                    _ ->
                                        false
                                end
                        end,
                        Melds)).
%% @doc Returns true if the hand consists only of simple tiles.
%%      Terminals, winds and dragons are not allowed.
-spec tanyao(game(), player()) -> boolean().
tanyao(#game{}, #player{hand=Hand}) ->
    not lists:any(fun(T = #tile{}) ->
                          case T#tile.suit of
                              dragon ->
                                  true;
                              wind ->
                                  true;
                              _ ->
                                  lists:member(T#tile.value, [1,9])
                          end
                  end,
                  riichi_hand:tiles(Hand)).

%% @doc Returns true for a no-points hand.
%%      To qualify for pinfu, the hand must be fully concealed, contain no pons/kans,
%%      contain no dragons, round winds or seat winds, and must be won on an open wait.
-spec pinfu(game(), player()) -> boolean().
pinfu(#game{round=Round}, #player{seat=Seat, hand=Hand=#hand{melds=Melds}, drawn={_, Drawn}}) ->
    Closed = lists:all(fun(T) -> T#tile.from =:= draw end, riichi_hand:tiles(Hand)),
    OpenWait = length(riichi_hand:waits(#hand{tiles=riichi_hand:tiles(Hand) -- [Drawn]})) > 1,
    Chiis = length([M || M = #meld{type=chii} <- Melds]) =:= 4,
    #meld{type=pair, tiles=[HeadTile,HeadTile]} = riichi_hand:head(Hand),
    NonValuePair = HeadTile#tile.value =/= Round
        andalso HeadTile#tile.value =/= Seat
        andalso HeadTile#tile.suit =/= dragon,
    Closed and OpenWait and Chiis and NonValuePair.

%% @doc Returns true for a 7-pair hand.
-spec chiitoitsu(game(), player()) -> boolean().
chiitoitsu(#game{}, #player{hand=#hand{tiles=[], melds=Melds}})
  when length(Melds) =:= 7 ->
    Pairs = [S || S <- Melds, S#meld.type =:= pair],
    length(Pairs) =:= 7 andalso sets:size(sets:from_list(Pairs)) =:= 7.

%% @doc Returns true for a 13 Orphans hand.
%%      The hand must contain one each of every terminal and honour tile, plus one
%%      additional tile matching any of the others in the hand.
-spec kokushi_musou(game(), player()) -> boolean().
kokushi_musou(#game{}, #player{hand=#hand{tiles=Tiles, melds=[#meld{type=pair, tiles=[T,T]}]}}) ->
    not lists:any(fun(#tile{value=V}) ->
                          lists:member(V, lists:seq(2,8))
                  end,
                  [T|Tiles])
        andalso sets:size(sets:from_list([T|Tiles])) =:= 13;
kokushi_musou(#game{}, #player{}) ->
    false.
