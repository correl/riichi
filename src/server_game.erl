-module(server_game).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([waiting/3,
         playing/2,
         turn/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3]).

-include("../include/riichi.hrl").

-record(state, {players=[]}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, waiting, #state{}}.

waiting({add_player, Player}, _From, State) ->
    error_logger:info_report({adding_player, [{player, Player}]}),
    Players = [Player|State#state.players],
    case length(Players) of
        4 ->
            Game = game:new(Players),
            error_logger:info_report({starting_game, []}),
            gen_fsm:send_event(self(), game_tree:build(Game)),
            {reply, ok, playing, Game};
        _ -> {reply, ok, waiting, State#state{players=Players}}
    end.

playing({game_tree, Game, Branches} = Tree, State) ->
    Actions = proplists:get_keys(Branches),
    [gen_server:cast(Player#player.pid, {new_state, Game}) || Player <- Game#game.players],
    [Choice|_] = lists:flatten(lists:map(fun(Seat) ->
                                                 Player = game:get_player(Game, Seat),
                                                 PlayerActions = [A || A = {game_action, W, _, _} <- Actions,
                                                                       W =:= Seat],
                                                 case PlayerActions of
                                                     [] ->
                                                         [];
                                                     _ ->
                                                         [gen_server:call(Player#player.pid, {choose, PlayerActions})]
                                                 end
                                         end,
                                         [east, south, west, north])),
    error_logger:info_report([resolving_choice, {chosen, Choice}, {valid, Actions}]),
    gen_fsm:send_event(self(), game_tree:do(Tree, Choice)),
    {next_state, playing, State}.

turn(start, Game) ->
    error_logger:info_report({starting_turn, Game#game.turn}),
    {next_state, start, Game}.

handle_event(Event, StateName, State) ->
    io:format("Unexpected ~p during ~p", [Event, StateName]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    io:format("Unexpected ~p during ~p", [Event, StateName]),
    {next_state, StateName, State}.
    
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(Reason, StateName, _State) ->
    error_logger:error_report([terminating,
                               {from_state, StateName},
                               {reason, Reason}]),
    ok.
