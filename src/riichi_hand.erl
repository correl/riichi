-module(riichi_hand).

-include("../include/riichi.hrl").

-compile([export_all]).

find(Tiles) ->
    find(lists:sort(Tiles), #hand{}, []).

find([], Hand, Possible) ->
    [Hand|Possible];

find(Tiles, Hand = #hand{tiles=HT, melds=HM}, Possible) ->
    case Tiles of
        [T, T, T|Rest] ->
            find(Rest, Hand#hand{melds=[#meld{type=pon, tiles=[T, T, T]}|HM]}, Possible);
        _ ->
            []
    end ++
    case Tiles of
        [T, T|Rest] ->
            find(Rest, Hand#hand{melds=[#meld{type=pair, tiles=[T, T]}|HM]}, Possible);
        _ ->
            []
    end ++
    case lists:sort(sets:to_list(sets:from_list(Tiles))) of
        [T1 = #tile{value=V1, suit=S}, T2 = #tile{value=V2, suit=S}, T3 = #tile{value=V3, suit=S}|_] when
              is_integer(V1) andalso
              [V1, V2, V3] =:= [V1, V1 + 1, V1 + 2] ->
            find(Tiles -- [T1, T2, T3], Hand#hand{melds=[#meld{type=chii, tiles=[T1, T2, T3]}|HM]}, Possible);
        _ ->
            []
    end ++
    case Tiles of
        [T|Rest] ->
            find(Rest, Hand#hand{tiles=[T|HT]}, Possible)
    end.

tiles(#hand{tiles=Tiles, melds=Melds}) ->
    lists:flatten([TS || #meld{tiles=TS} <- Melds]) ++ Tiles.

head(#hand{melds=Melds}) ->
    case [M || M = #meld{type=pair} <- Melds] of
        [Pair|_] ->
            Pair;
        [] ->
            none
    end.

is_complete(#hand{tiles=[], melds=Melds}=Hand) ->
    Pairs = [M || M <- Melds, M#meld.type =:= pair],
    case length(Pairs) of
        1 ->
            % Four mentsu + 1 pair = 5 sets
            length(Melds) =:= 5;
        7 ->
            % Must be seven *unique* pairs
            yaku:chiitoitsu(Hand);
        _ ->
            false
    end;
is_complete(#hand{}=Hand) ->
    yaku:kokushi_musou(Hand).
