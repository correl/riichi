-module(yaku).

-include("../include/riichi.hrl").

-compile([export_all]).


yakuhai(#hand{sets=Sets}) ->
    length(lists:filter(fun(T = #tile{}) ->
                                case T#tile.suit of
                                    wind ->
                                        % TODO: Round/Seat Winds
                                        false;
                                    dragon ->
                                        true;
                                    _ ->
                                        false
                                end
                        end,
                        Sets)).

tanyao(#hand{sets=Sets}) ->
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
                  Sets).

pinfu(#hand{sets=Sets}) ->
    % TODO: Verify closed, open wait, pair not round/seat wind
    lists:all(fun(S) ->
                      case S of
                          #seq{} ->
                              true;
                          _ ->
                              false
                      end
              end,
              [S || S <- Sets]).

% 7 Pairs
chiitoitsu(#hand{tiles=[], sets=Sets})
  when length(Sets) =:= 7 ->
    Pairs = [S || S <- Sets, S#set.count =:= 2],
    length(Pairs) =:= 7 andalso sets:size(sets:from_list(Pairs)) =:= 7.

% 13 Orphans
kokushi_musou(#hand{tiles=Tiles, sets=Sets})
  when length(Tiles) =:= 13
       andalso length(Sets) =:= 0 ->
    not lists:any(fun(#tile{value=V}) ->
                          lists:member(V, lists:seq(2,8))
                  end,
                  Tiles)
        andalso sets:size(sets:from_list(Tiles)) =:= 13.
