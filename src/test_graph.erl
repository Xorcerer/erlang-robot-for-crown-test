%% Author: Jackie
%% Created: 2012-9-1
%% Description: TODO: Add description to test_graph
-module(test_graph).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([read_map/1, find_path/3, test_path/0]).

%%
%% API Functions
%%
read_map(MapName) ->
	{ok, S} = file:open(MapName, read),
	{TriggerCount, _} = string:to_integer(io:get_line(S, '')),
	io:format("trigger count = ~p~n", [TriggerCount]),
	Triggers = lists:map(
		fun(_I) ->
			[Id_S, X_S, Y_S] = string:tokens(io:get_line(S, ''), " \n"),
			{Id, _} = string:to_integer(Id_S),
			{X, _} = string:to_float(X_S),
			{Y, _} = string:to_float(Y_S),
			{Id, {X, Y}}
		end, lists:seq(1, TriggerCount)),
	Paths = lists:map(
		fun(_I) ->
			[Target_S | Path_S] = string:tokens(io:get_line(S, ''), ": \n"),
			{Target, _} = string:to_integer(Target_S),
			Path = lists:map(fun(P_S) -> {A, _} = string:to_integer(P_S), A end, Path_S),
			{Target, Path}
		end, lists:seq(1, TriggerCount)),
	file:close(S),
	{Triggers, Paths}.

test_path() ->
	Map = read_map("yewai2x.grf"),
	{_, Paths} = Map,
	Path0 = find_path(Paths, 11, 31),
	io:format("path(11->31) is ~p~n", [Path0]),
	Path0 = [14,16,17,21,23,25,29,30,48,47,45,44,43,42,41,40,38],
	Path1 = find_path(Paths, 1, 4),
	Path1 = [],
	Path1 = find_path(Paths, 4, 1),
	Path2 = find_path(Paths, 1, 10),
	Path2 = [4, 5, 6],
	Path2 = find_path(Paths, 10, 1),
	Path3 = find_path(Paths, 6, 10),
	Path3 = [],
	Path4 = find_path(Paths, 8, 11),
	Path4 = [9,26,27,25,23,21,17,16,14],
	Path5 = find_path(Paths, 11, 8),
	Path5 = [14,16,17,21,23,25,27,26,9].

find_path(Paths, From, To) ->
	[{Trig0, _} |_] = Paths,
	{_, FromPath} = lists:keyfind(From, 1, Paths),
	{_, ToPath} = lists:keyfind(To, 1, Paths),
	if
		Trig0 == From -> ToPath;
		Trig0 == To -> FromPath;
		true ->
			extract_path(From, To, FromPath, ToPath, Trig0)
	end.

extract_path(From, To, [H|T1], [H|T2], _) -> extract_path(From, To, T1, T2, H);
extract_path(From, _, [], [From|T], _) -> T;
extract_path(_, To, [To|T], [], _) -> lists:reverse(T);
extract_path(_, _, FromPath, ToPath, Common) -> lists:reverse(FromPath) ++ [Common|ToPath].
	
%%
%% Local Functions
%%

