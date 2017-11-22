-module(game_action).

-export([new/2,
         new/3,
         player/1,
         action/1,
         arguments/1]).

-record(game_action, {player, action, arguments}).

-type game_action() :: #game_action{}
                     | exhaustive_draw.


new(Player, Action) ->
    #game_action{player=Player, action=Action}.

new(Player, Action, Arguments) ->
    #game_action{player=Player, action=Action, arguments=Arguments}.

player(Action) ->
    Action#game_action.player.

action(Action) ->
    Action#game_action.action.

arguments(Action) ->
    Action#game_action.arguments.
