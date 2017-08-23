-module(riichi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/", cowboy_static, {priv_file, riichi, "index.html"}},
                         {"/js/[...]", cowboy_static, {priv_dir, riichi, "js"}},
                         {"/css/[...]", cowboy_static, {priv_dir, riichi, "css"}},
                         {"/images/[...]", cowboy_static, {priv_dir, riichi, "images"}},
                         {"/websocket", server_websocket, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(
                my_http_listener,
                100,
                [{port, 8080}],
                [{env, [{dispatch, Dispatch}]}]
               ),
    riichi_sup:start_link().

stop(_State) ->
    ok.
