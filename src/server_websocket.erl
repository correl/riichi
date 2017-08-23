-module(server_websocket).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-include("../include/riichi.hrl").

-export([init/3, handle/2, terminate/3]).
-export([
         websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3
        ]).

-record(state, {game, player}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

handle(_Req, State) ->
    {ok, Response} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Response, State}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("init websocket"),
    {ok, Game} = server_game:start_link(),
    {ok, Pid} = player_websocket:start_link(self()),
    Player = player:new("Websocket", Pid),
    gen_fsm:send_event(Game, {add_player, Player}),
    [gen_fsm:send_event(Game,{add_player, player:new()})
     || _N <- lists:seq(1, 3)],
    {ok, Req, #state{game=Game,player=Player}}.

websocket_handle({text, Msg}, Req, State) ->
    lager:info("Got Data: ~p", [Msg]),
    {reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };


websocket_handle(_Any, Req, State) ->
    {reply, {text, << "whut?">>}, Req, State, hibernate }.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(Info, Req, State) ->
    lager:info("websocket info: ~p", [Info]),
    case encode(Info) of
        {ok, Msg} ->
            {reply, {text, Msg}, Req, State};
        {error, _Reason} ->
            lager:error("Unhandled message: ~p", [Info]),
            {ok, Req, State, hibernate}
    end.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.

encode({log, Msg}) ->
    {ok, io_lib:format("log: ~s", [Msg])};
encode({new_state, Game}) ->
    {ok, ["new_state: ", jsx:encode(encode_game(Game))]};
encode(_) ->
    {error, invalid_message}.

encode_game(Game) ->
    #{round => Game#game.round,
      turn => Game#game.turn,
      phase => Game#game.phase,
      wall => length(Game#game.wall),
      dora => lists:map(fun encode_tile/1, Game#game.dora),
      players => lists:map(fun encode_player/1, Game#game.players)}.

encode_tile(Tile) ->
    #{suit => Tile#tile.suit,
      value => Tile#tile.value,
      from => Tile#tile.from}.

encode_player(Player) ->
    #{name => list_to_binary(Player#player.name),
      seat => Player#player.seat,
      hand => encode_hand(Player#player.hand),
      discards => lists:map(fun encode_tile/1, Player#player.discards)}.

encode_hand(Hand) ->
    #{tiles => lists:map(fun encode_tile/1, Hand#hand.tiles),
      melds => lists:map(fun encode_meld/1, Hand#hand.melds)}.

encode_meld(Meld) ->
    #{type => Meld#meld.type,
      tiles => lists:map(fun encode_tile/1, Meld#meld.tiles)}.
