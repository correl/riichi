-module(riichi).

-include("riichi.hrl").

-export([
    is_valid_tile/1,
    dora/1,
    nearest/2,
    score/3
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


score(_Fu, Yaku, Limit) when (Yaku >= 5) and (Limit == true) ->
    if
        Yaku < 6 ->
            2000;
        Yaku < 8 ->
            3000;
        Yaku < 11 ->
            4000;
        Yaku < 14 ->
            6000;
        true ->
            8000
    end;
score(Fu, Yaku, Limit) ->
    Score = nearest(Fu * round(math:pow(2, 2 + Yaku)), 100),
    if
        Limit and (Score > 2000) ->
            2000;
        true ->
            Score
    end.
