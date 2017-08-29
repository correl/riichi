%% @author Correl Roush <correl@gmail.com>
%%
%% @doc Riichi Mahjong library.
%%
%% @headerfile "../include/riichi.hrl"

-module(riichi).

-include("../include/riichi.hrl").

-export([
         start/0,
         is_valid_tile/1,
         is_open/1,
         dora/1,
         next/2,
         nearest/2,
         score/3,
         score_hand/1,
         score_hand/2,
         score_hand/3,
         shuffle/1,
         tiles/0
]).

start() ->
    application:start(riichi).

-spec is_valid_tile(tile()) -> boolean().
is_valid_tile(#tile{suit=dragon, value=Value}) ->
    lists:member(Value, [white, green, red]);
is_valid_tile(#tile{suit=wind, value=Value}) ->
    lists:member(Value, [east, south, west, north]);
is_valid_tile(#tile{suit=Suit, value=Value}) ->
    ValidSuit = lists:member(Suit, [pin, sou, man]),
    ValidValue = is_integer(Value) and (Value > 0) and (Value < 10),
    ValidSuit and ValidValue;
is_valid_tile(_) ->
    false.

-spec is_open(hand() | meld() | tile()) -> boolean().
is_open(#tile{from=draw}) ->
    false;
is_open(#tile{}) ->
    true;
is_open(#meld{tiles=Tiles}) ->
    lists:any(fun is_open/1, Tiles);
is_open(#hand{tiles=Tiles, melds=Melds}) ->
    lists:any(fun is_open/1, Tiles)
        orelse lists:any(fun is_open/1, Melds).

-spec dora(tile()) -> tile().
dora(#tile{suit = Suit, value = Value} = Indicator) ->
    case is_valid_tile(Indicator) of
        true ->
            Next = next(Suit, Value),
            Indicator#tile{value = Next};
        _ ->
            throw({error, invalid_tile})
    end.

-spec next(suit(), term()) -> term().
next(dragon, Value) ->
    case Value of
        white -> green;
        green -> red;
        red -> white
    end;
next(wind, Value) ->
    case Value of
        east -> south;
        south -> west;
        west -> north;
        north -> east
    end;
next(_Suit, Value) when is_integer(Value) ->
    case Value < 9 of
        true -> Value + 1;
        _ -> 1
    end.

-spec nearest(integer(), integer()) -> integer().
nearest(Num, To) when Num rem To == 0 ->
    Num;
nearest(Num, To) ->
    Num - (Num rem To) + To.

-spec score(integer(), integer(), boolean()) -> integer().
score(_Fu, Han, Limit) when (Han >= 5) and (Limit == true) ->
    if
        Han < 6 ->
            2000;
        Han < 8 ->
            3000;
        Han < 11 ->
            4000;
        Han < 13 ->
            6000;
        true ->
            8000
    end;
score(Fu, Han, Limit) ->
    Score = nearest(Fu * round(math:pow(2, 2 + Han)), 100),
    if
        Limit and (Score > 2000) ->
            2000;
        true ->
            Score
    end.

-spec score_hand(hand()) -> integer().
score_hand(#hand{}=H) ->
    score_hand(H, 20).

score_hand(#hand{}=H, Fu) ->
    score_hand(H, Fu, true).

-spec score_hand(hand(), integer(), boolean()) -> integer().
score_hand(#hand{}=Hand, BaseFu, Limit) ->
    Fu = [
        BaseFu
    ],
    Yakuman = [
        _DaiSanGen = fun(#hand{}=H) ->
                Sets = find_sets(H#hand.tiles),
                HasRed = lists:member({3, #tile{suit=dragon, value=red}}, Sets),
                HasWhite = lists:member({3, #tile{suit=dragon, value=white}}, Sets),
                HasGreen = lists:member({3, #tile{suit=dragon, value=green}}, Sets),
                case HasRed and HasWhite and HasGreen of
                    true -> 13;
                    _ -> 0
                end
        end,
        _Suu_An_Kou = fun(#hand{}=H) ->
                Sets = find_sets(H#hand.tiles),
                case 4 == length([C || {C, _T} <- Sets, C == 3]) of
                    true -> 13;
                    _ -> 0
                end
        end,
        _Ryuu_Ii_Sou = fun(#hand{}=H) ->
                Set = sets:from_list(H#hand.tiles),
                Greens = sets:from_list(lists:flatten([
                            [#tile{suit=sou, value=V} || V <- [2,3,4,6,8]],
                            [#tile{suit=dragon, value=green}]
                        ])),
                case sets:is_subset(Set, Greens) of
                    true -> 13;
                    _ -> 0
                end
        end,
        _Kokushi_Musou = fun(#hand{}=H) ->
                Sets = find_sets(H#hand.tiles),
                Terminals = not lists:any(fun(#tile{value=V}) -> lists:member(V, lists:seq(2,8)) end, H#hand.tiles),
                Orphans = 12 == length([C || {C, _T} <- Sets, C == 1]),
                case Terminals and Orphans of
                    true -> 13;
                    _ -> 0
                end
        end
    ],
    score(lists:sum(Fu), lists:sum([F(Hand) || F <- Yakuman]), Limit).

-spec find_sets([tile()]) -> [{integer(), tile()}].
find_sets(Tiles) ->
    Unique = sets:to_list(sets:from_list(Tiles)),
    [{length(lists:filter(fun(X) -> X == T end, Tiles)), T}
    || T <- Unique].

-spec shuffle(list()) -> list().
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), I} || I <- List])].

-spec tiles() -> [string()].
tiles() ->
    lists:flatten([lists:duplicate(4, T) || T <- ?TILES]).
