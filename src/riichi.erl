-module(riichi).

-include("riichi.hrl").

-export([
    is_valid_tile/1,
    dora/1,
    nearest/2,
    score/3,
    score_hand/1,
    score_hand/2,
    score_hand/3
]).

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

dora(#tile{suit=dragon, value=Value}=Indicator) ->
    case Value of
        white -> Indicator#tile{value=green};
        green -> Indicator#tile{value=red};
        red   -> Indicator#tile{value=white}
    end;
dora(#tile{suit=wind, value=Value}=Indicator) ->
    case Value of
        east  -> Indicator#tile{value=south};
        south -> Indicator#tile{value=west};
        west  -> Indicator#tile{value=north};
        north -> Indicator#tile{value=east}
    end;
dora(#tile{value=Value}=Indicator) ->
    case is_valid_tile(Indicator) of
        false ->
            throw({error, invalid_tile});
        _     ->
            if
                Value == 9 -> Indicator#tile{value=1};
                true       -> Indicator#tile{value=Value + 1}
            end
    end.

nearest(Num, To) when Num rem To == 0 ->
    Num;
nearest(Num, To) ->
    Num - (Num rem To) + To.


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

score_hand(#hand{}=H) ->
    score_hand(H, 20).

score_hand(#hand{}=H, Fu) ->
    score_hand(H, Fu, true).

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

find_sets(Tiles) ->
    Unique = sets:to_list(sets:from_list(Tiles)),
    [{length(lists:filter(fun(X) -> X == T end, Tiles)), T}
    || T <- Unique].
