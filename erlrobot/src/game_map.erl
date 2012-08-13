-module(game_map).
-export([read_map/1]).

read_map(MapName) ->
	{ok, S} = file:open(MapName, read),
	{LinesTrigger, _} = string:to_integer(io:get_line(S, '')),
	Triggers = lists:map(
		fun(_I) ->
			[Id_S, X_S, Y_S] = string:tokens(io:get_line(S, ''), " \n"),
			{Id, _} = string:to_integer(Id_S),
			{X, _} = string:to_float(X_S),
			{Y, _} = string:to_float(Y_S),
			{Id, {X, Y}}
		end, lists:seq(1, LinesTrigger)),
	{LinesPath, _} = string:to_integer(io:get_line(S, '')),
	Paths = lists:map(
		fun(_I) ->
			[Source_S, Target_S] = string:tokens(io:get_line(S, ''), " \n"),
			{Source, _} = string:to_integer(Source_S),
			{Target, _} = string:to_integer(Target_S),
			{Source, Target}
		end, lists:seq(1, LinesPath)),
	{Triggers, Paths}.