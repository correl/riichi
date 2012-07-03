-module(riichi_hand).

-include("riichi.hrl").

-compile([export_all]).


find_sets(Tiles) ->
    Unique = sets:to_list(sets:from_list(Tiles)),
    [#set{count=length(lists:filter(fun(X) -> X == T end, Tiles)), tile=T, open=false}
    || T <- Unique].

reorder_seqs(Tiles) ->
    Unique = sets:to_list(sets:from_list(Tiles)),
    lists:sort(Unique) ++ (Tiles -- Unique).

find_seqs(Tiles) ->
    find_seqs(reorder_seqs(Tiles), {[], []}).

find_seqs([], {Seqs, Rest}) ->
    {lists:sort(Seqs), lists:sort(Rest)};

find_seqs([T1 = #tile{suit=Suit}, T2 = #tile{suit=Suit}, T3  = #tile{suit=Suit} | Tiles], {Seqs, Rest})
        when T2#tile.value =:= (T1#tile.value + 1)
             andalso T3#tile.value =:= (T2#tile.value + 1) ->
    find_seqs(reorder_seqs(Tiles), {[#seq{tiles=[T1, T2, T3], open=false} | Seqs], Rest});

find_seqs([T | Tiles], {Seqs, Rest}) ->
    find_seqs(Tiles, {Seqs, [T | Rest]}).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

combinations(0, _) -> [[]];
combinations(_, []) -> [];
combinations(N, [X|XS]) -> [[X|YS] || YS <- combinations(N-1, XS)] ++ combinations(N, XS).

find_hands(Tiles) ->
    find_hands(Tiles, []).
find_hands([], Hands) ->
    Hands;
find_hands([_T | _Remaining] = Tiles, Hands) ->
    [#hand{tiles=Tiles} | Hands].
