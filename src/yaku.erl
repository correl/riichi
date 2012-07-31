-module(yaku).

-include("../include/riichi.hrl").

-compile([export_all]).


yakuhai(#hand{melds=Melds}) ->
    length(lists:filter(fun(#meld{type=Type, tiles=[T|_]}) ->
                                case {Type, T} of
                                    {pair, _} ->
                                        false;
                                    {chii, _} ->
                                        false;
                                    {_, #tile{suit=wind}} ->
                                        % TODO: Round/Seat Winds
                                        false;
                                    {_, #tile{suit=dragon}} ->
                                        true
                                end
                        end,
                        Melds)).

tanyao(#hand{melds=Melds}) ->
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
                  lists:flatten([Tiles || #meld{tiles=Tiles} <- Melds])).

pinfu(#hand{melds=Melds}) ->
    % TODO: Verify closed, open wait, pair not round/seat wind
    length([M || M = #meld{type=T} <- Melds, T =:= chii]) =:= 4.

% 7 Pairs
chiitoitsu(#hand{tiles=[], melds=Melds})
  when length(Melds) =:= 7 ->
    Pairs = [S || S <- Melds, S#meld.type =:= pair],
    length(Pairs) =:= 7 andalso sets:size(sets:from_list(Pairs)) =:= 7.

% 13 Orphans
kokushi_musou(#hand{tiles=Tiles, melds=[#meld{type=pair, tiles=[T,T]}]}) ->
    not lists:any(fun(#tile{value=V}) ->
                          lists:member(V, lists:seq(2,8))
                  end,
                  [T|Tiles])
        andalso sets:size(sets:from_list([T|Tiles])) =:= 13;
kokushi_musou(#hand{}) ->
    false.
