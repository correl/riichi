-module(player_websocket).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {socket, game, seat}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    Socket ! <<"oh hello!">>,
    {ok, #state{socket=Socket}}.

handle_call({choose, Actions}, _From, State) ->
    %% [Action|_] = sort_actions(Actions),
    %% {reply, Action, State};
    State#state.socket ! {choose, Actions},
    {noreply, State};

handle_call(get_name, _From, State) ->
    {reply, io_lib:format("~p", [State#state.socket]), State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(Msg, State) ->
    State#state.socket! Msg,
    {noreply, State}.

handle_info(Msg, State) ->
    State#state.socket ! Msg,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sort_actions(Actions) ->
    Order = fun({game_action, A, _, _}, {game_action, B, _, _}) ->
                    Weighted = [ron, tsumo, kan, pon, chi],
                    Weights = lists:zip(Weighted, lists:reverse(lists:seq(1, length(Weighted)))),
                    VA = proplists:get_value(A, Weights, 0),
                    VB = proplists:get_value(B, Weights, 0),
                    case VA == VB of
                        true ->
                            A >= B;
                        _ ->
                            VA >= VB
                    end
            end,
    lists:sort(Order, Actions).
