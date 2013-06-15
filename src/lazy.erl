-module(lazy).

-export([find/2]).

-include("lazy.hrl").


find(Predicate, [H|T]) when is_function(H) ->
    Value = ?FORCE(H),
    case Predicate(Value) of
        true ->
            {ok, Value};
        _ ->
            find(Predicate, T)
    end;
find(_Predicate, []) ->
    undefined.
