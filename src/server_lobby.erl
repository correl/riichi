-module(server_lobby).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {players=[]}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({add_player, Player}, _From, State) ->
    {reply, ok, State#state{players=[Player|State#state.players]}};

handle_call(get_players, _From, State) ->
    {reply, State#state.players, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({send, {message, From, Body}=Message}, State) ->
    Players = State#state.players,
    [gen_server:cast(Pid, Message) || Pid <- Players],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.