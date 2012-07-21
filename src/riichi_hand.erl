-module(riichi_hand).

-include("../include/riichi.hrl").

-compile([export_all]).

find(Tiles) ->
    find(lists:sort(Tiles), #hand{}, []).

find([], Hand, Possible) ->
    [Hand|Possible];

find(Tiles, Hand = #hand{tiles=HT, sets=HS}, Possible) ->
    case Tiles of
        [T, T, T, T|Rest] ->
            find(Rest, Hand#hand{sets=[#set{count=4, tile=T, open=false}|HS]}, Possible);
        _ -> []
    end ++
    case Tiles of
        [T, T, T|Rest] ->
            find(Rest, Hand#hand{sets=[#set{count=3, tile=T, open=false}|HS]}, Possible);
        _ ->
            []
    end ++
    case Tiles of
        [T, T|Rest] ->
            find(Rest, Hand#hand{sets=[#set{count=2, tile=T, open=false}|HS]}, Possible);
        _ ->
            []
    end ++
    case lists:sort(sets:to_list(sets:from_list(Tiles))) of
        [T1 = #tile{value=V1, suit=S}, T2 = #tile{value=V2, suit=S}, T3 = #tile{value=V3, suit=S}|_] when
              is_integer(V1) andalso
              [V1, V2, V3] =:= [V1, V1 + 1, V1 + 2] ->
            find(Tiles -- [T1, T2, T3], Hand#hand{sets=[#seq{tiles=[T1, T2, T3], open=false}|HS]}, Possible);
        _ ->
            []
    end ++
    case Tiles of
        [T|Rest] ->
            find(Rest, Hand#hand{tiles=[T|HT]}, Possible)
    end.

is_complete(#hand{tiles=[], sets=Sets}) ->
    Pairs = [S || S <- Sets, S#set.count =:= 2],
    case length(Pairs) of
        1 ->
            % Four mentsu + 1 pair = 5 sets
            length(Sets) =:= 5;
        7 ->
            % Must be seven *unique* pairs
            sets:size(sets:from_list(Pairs)) =:= 7;
        _ ->
            false
    end;
is_complete(#hand{}=Hand) ->
    kokushi_musou(Hand).

% 13 Orphans
kokushi_musou(#hand{tiles=Tiles, sets=Sets}) when
      length(Tiles) =:= 13
      andalso length(Sets) =:= 0 ->
    not lists:any(fun(#tile{value=V}) ->
                          lists:member(V, lists:seq(2,8))
                  end,
                  Tiles)
        andalso sets:size(sets:from_list(Tiles)) =:= 13.
