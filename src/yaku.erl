%% @author Correl Roush <correl@gmail.com>
%%
%% @doc Riichi Mahjong library.
%%
%% @headerfile "../include/riichi.hrl"

-module(yaku).

-include("../include/riichi.hrl").

-export([yakuhai/2,
         tanyao/2,
         pinfu/2,
         iipeikou/2,
         chanta/2,
         itsuu/2,
         chiitoitsu/2,
         san_shoku_doujun/2,
         san_shoku_douko/2,
         san_kan_tsu/2,
         toi_toi/2,
         san_an_kou/2,
         shou_san_gen/2,
         honrouto/2,
         honitsu/2,
         jun_chan/2,
         ryanpeikou/2,
         chinitsu/2,
         kokushi_musou/2,
         ryuu_iisou/2,
         dai_san_gen/2,
         suu_an_kou/2,
         tsu_iisou/2,
         chinrouto/2,
         shou_suushi/2,
         dai_suushi/2,
         chuuren_pooto/2,
         suu_kan_tsu/2
        ]).

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

%% @doc Counts unique elements in a list
-spec count_unique(list()) -> list({term(), integer()}).
count_unique(L) ->
    Unique = sets:to_list(sets:from_list(L)),
    [{I, length(lists:filter(fun (X) ->
                                     X == I
                             end,
                             L))}
     || I <- Unique].

%% @doc Returns true for a hand containing two identical straights in the same suit
%%      Will NOT return true if there are more than two, as this yaku and ryanpeikou
%%      are mutually exclusive.
-spec iipeikou(game(), player()) -> boolean().
iipeikou(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Chiis = [M || M = #meld{type=chii} <- Melds],
    Counts = [C || {_, C} <- count_unique(Chiis), C == 2],
    length(Counts) == 1.

%% @doc Returns true for a Chanta hand
%%      All melds and the pair must include a terminal or honor tile
-spec chanta(game(), player()) -> boolean().
chanta(#game{}, #player{hand=#hand{tiles=[], melds=Melds}}) ->
    Sets = [[{T#tile.suit, T#tile.value} || T <- Tiles]
            || #meld{tiles=Tiles} <- Melds],
    ChantaTiles = [{T#tile.suit, T#tile.value} || T <- (?T_TERMINALS ++ ?T_HONOURS)],
    lists:all(fun(Tiles) ->
                      (Tiles -- ChantaTiles =/= Tiles)
              end,
              Sets).

%% @doc Returns true for an Itsuu hand
%%      Hand must contain a 1-9 run in one suit
-spec itsuu(game(), player()) -> boolean().
itsuu(#game{}, #player{hand=#hand{tiles=[], melds=Melds}}) ->
    Tiles = lists:flatten([TS || #meld{type=chii, tiles=TS} <- Melds]),
    Runs = [lists:filter(fun(#tile{suit=S}) -> S =:= Suit end, Tiles)
            || Suit <- [man,sou,pin]],
    lists:any(fun(TS) ->
                      sets:from_list([V || #tile{value=V} <- TS]) =:= sets:from_list(lists:seq(1,9))
              end,
              Runs).
%% @doc Returns true if the provided meld is present in the hand
%%      in all three suits
-spec san_shoku(meld(), hand()) -> boolean().
san_shoku(#meld{tiles=Tiles}, #hand{melds=Melds}) ->
    [V1, V2, V3] = [V || #tile{value=V} <- Tiles],
    MeldTiles = [TS || #meld{tiles=TS} <- Melds],
    TSV = fun(#tile{suit=S,value=V}) -> {S,V} end,
    MeldValues = lists:map(fun(L) -> lists:map(TSV, L) end, MeldTiles),
    lists:all(fun(M) -> lists:member(M, MeldValues) end,
              [[{S, V1}, {S, V2}, {S, V3}]
               || S <- [pin, sou, man]]).

%% @doc Returns true for a San shoku doujun hand
%%      Hand must contain the same sequence in all three suits
-spec san_shoku_doujun(game(), player()) -> boolean().
san_shoku_doujun(#game{}, #player{hand=#hand{melds=Melds}=Hand}) ->
    Chiis = [M || #meld{type=chii} = M <- Melds],
    lists:any(fun(M) -> san_shoku(M, Hand) end, Chiis).

%% @doc Returns true for a San shoku douko hand
%%      Hand must contain the same triplet in all three suits
-spec san_shoku_douko(game(), player()) -> boolean().
san_shoku_douko(#game{}, #player{hand=#hand{melds=Melds}=Hand}) ->
    Pons = [M || #meld{type=pon} = M <- Melds],
    lists:any(fun(M) -> san_shoku(M, Hand) end, Pons).

%% @doc Returns true for a San kan tsu hand
%%      Hand must contain three kans
san_kan_tsu(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Kans = [M || M = #meld{type=kan} <- Melds],
    length(Kans) =:= 3.

%% @doc Returns true for a Toi toi hand
%%      Hand must contain all triplets
toi_toi(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Pons = [M || M = #meld{type=T} <- Melds,
                  lists:member(T, [pon,kan])],
    length(Pons) =:= 4.

%% @doc Returns true for a San an kou hand
%%      Hand must contain three concealed triplets
san_an_kou(#game{}, #player{hand=#hand{melds=Melds}}) ->
    ClosedPons = [M || M = #meld{type=T} <- Melds,
                       not riichi:is_open(M),
                       lists:member(T, [pon,kan])],
    length(ClosedPons) =:= 3.

%% @doc Returns true for a Shou san gen hand
shou_san_gen(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Pons = [M || M = #meld{type=Type, tiles=[#tile{value=Value}|_]} <- Melds,
                 lists:member(Type, [pon, kan]),
                 lists:member(Value, [red, green, white])],
    Pairs = [M || M = #meld{type=Type, tiles=[#tile{value=Value}|_]} <- Melds,
                  Type == pair,
                  lists:member(Value, [red, green, white])],
    length(Pons) == 2 andalso length(Pairs) == 1.

%% @doc Returns true for a Honrouto hand
honrouto(#game{}, #player{hand=Hand}) ->
    IsHonour = fun(T) ->
                       lists:member(T, ?T_HONOURS ++ ?T_TERMINALS)
               end,
    lists:all(IsHonour, riichi_hand:tiles(Hand)).
    
%% @doc Returns true for a 7-pair hand.
-spec chiitoitsu(game(), player()) -> boolean().
chiitoitsu(#game{}, #player{hand=#hand{tiles=[], melds=Melds}})
  when length(Melds) =:= 7 ->
    Pairs = [S || S <- Melds, S#meld.type =:= pair],
    length(Pairs) =:= 7 andalso sets:size(sets:from_list(Pairs)) =:= 7.

%% @doc Returns true for a Honitsu hand.
honitsu(#game{}, #player{hand=Hand}) ->
    Tiles = riichi_hand:tiles(Hand),
    Suits = sets:to_list(sets:from_list([Suit || #tile{suit=Suit} <- Tiles,
                                                 lists:member(Suit, [pin, sou, man])])),
    IsHonour = fun(T) ->
                       lists:member(T, ?T_HONOURS)
               end,
    length(Suits) == 1 andalso lists:any(IsHonour, Tiles).

%% @doc Returns true for a Jun chan hand.
jun_chan(#game{}, #player{hand=#hand{melds=Melds}}) ->
    IsTerminal = fun(T) ->
                         lists:member(T, ?T_TERMINALS)
                 end,
    HasTerminal = fun(#meld{tiles=Tiles}) ->
                          lists:any(IsTerminal, Tiles)
                  end,
    lists:all(HasTerminal, Melds).

%% @doc Returns true for a Ryanpeikou hand.
ryanpeikou(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Chiis = [M || M = #meld{type=chii} <- Melds],
    Counts = [C || {_, C} <- count_unique(Chiis), C == 2],
    length(Counts) == 2.

%% @doc Returns true for a Chinitsu hand.
chinitsu(#game{}, #player{hand=Hand}) ->
    Tiles = riichi_hand:tiles(Hand),
    Suits = sets:to_list(sets:from_list([Suit || #tile{suit=Suit} <- Tiles,
                                                 lists:member(Suit, [pin, sou, man])])),
    IsHonour = fun(T) ->
                       lists:member(T, ?T_HONOURS)
               end,
    length(Suits) == 1 andalso not lists:any(IsHonour, Tiles).


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

%% @doc Returns true for an all-green hand.
-spec ryuu_iisou(game(), player()) -> boolean().
ryuu_iisou(#game{}, #player{hand=Hand}) ->
    Greens = sets:from_list([#tile{suit=sou, value=V} || V <- [2,3,4,6,8]] ++ [#tile{suit=dragon, value=green}]),
    Tiles = sets:from_list(riichi_hand:tiles(Hand)),
    sets:is_subset(Tiles, Greens).

%% @doc Returns true for a Big Three Dragons hand
-spec dai_san_gen(game(), player()) -> boolean().
dai_san_gen(#game{}, #player{hand=#hand{melds=Melds}}) ->
    [green, red, white] =:= lists:usort([V || #meld{type=T, tiles=[#tile{suit=dragon, value=V}|_]} <- Melds,
                                              lists:member(T, [pon, kan])]).

suu_an_kou(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Pons = [M || M = #meld{type=T} <- Melds,
                 lists:member(T, [pon, kan]),
                 not riichi:is_open(M)],
    length(Pons) == 4.

tsu_iisou(#game{}, #player{hand=Hand}) ->
    IsHonour = fun(T) ->
                       lists:member(T, ?T_HONOURS)
               end,
    lists:all(IsHonour, riichi_hand:tiles(Hand)).

chinrouto(#game{}, #player{hand=Hand}) ->
    IsTerminal = fun(T) ->
                         lists:member(T, ?T_TERMINALS)
                 end,
    lists:all(IsTerminal, riichi_hand:tiles(Hand)).

shou_suushi(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Pons = [M || M = #meld{type=Type, tiles=[#tile{suit=wind}|_]} <- Melds,
                 lists:member(Type, [pon, kan])],
    Pairs = [M || M = #meld{type=Type, tiles=[#tile{suit=wind}|_]} <- Melds,
                  Type == pair],
    length(Pons) == 2 andalso length(Pairs) == 1.

dai_suushi(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Pons = [M || M = #meld{type=Type, tiles=[#tile{suit=wind}|_]} <- Melds,
                 lists:member(Type, [pon, kan])],
    length(Pons) == 3.

chuuren_pooto(#game{} = Game, #player{hand=#hand{melds=Melds} = Hand} = Player) ->
    Tiles = riichi_hand:tiles(Hand),
    [#tile{suit=Suit}|_] = Tiles,
    chinitsu(Game, Player)
        andalso length([M || M = #meld{type=T, tiles=[#tile{value=1}|_]} <- Melds,
                             lists:member(T, [pon, kan])]) == 1
        andalso length([M || M = #meld{type=T, tiles=[#tile{value=9}|_]} <- Melds,
                             lists:member(T, [pon, kan])]) == 1
        andalso lists:all(fun(T) ->
                                  lists:member(T, Tiles)
                          end,
                          [#tile{value=Value, suit=Suit} || Value <- lists:seq(2,8)]).

suu_kan_tsu(#game{}, #player{hand=#hand{melds=Melds}}) ->
    Kans = [M || M = #meld{type=kan} <- Melds],
    length(Kans) == 4.
