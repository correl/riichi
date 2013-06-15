-module(player_dummy).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {name, game, seat}).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
    {ok, #state{name=Name}}.

handle_call({choose, Actions}, _From, State) ->
    [Action|_] = sort_actions(Actions),
    {reply, Action, State};

handle_call(get_name, _From, State) ->
    {reply, State#state.name, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({message, _From, Body}, State) ->
    error_logger:info_report({message_received,
                              {self, self()},
                              {state, State},
                              {body, Body}}),
    {noreply, State};

handle_cast({action, Seat, Action}, State) ->
    error_logger:info_report([game_event,
                              {seat, Seat},
                              {action, Action}]),
    {noreply, State};

handle_cast({new_state, Game}, State) ->
    {noreply, State#state{game=Game}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
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
