-module(player_dummy).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {name}).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
    {ok, #state{name=Name}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({message, _From, Body}, State) ->
    error_logger:info_report({message_received,
                              {self, self()},
                              {state, State},
                              {body, Body}}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.