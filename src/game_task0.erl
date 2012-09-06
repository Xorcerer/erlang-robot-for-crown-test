%% Author: Jackie
%% Created: 2012-8-20
%% Description: TODO: Add description to game_task0
-module(game_task0).

%%
%% Include files
%%
-include("records.hrl").
-include("playerInfo.hrl").

%%
%% Exported Functions
%%
-export([task0/4]).

%%
%% API Functions
%%
-import(math_util, [dist_vec2d/2]).

-define(Task0Loc, {-1905.701, 1252.586}).

task0(GSPid, Context, TaskId, PlayerId) ->
	{_SessionId, _SId, _AId, _UserId} = Context,
	%io:format("task0, GSPid = ~p~n", [GSPid]),
	task0(init, GSPid, Context, TaskId, PlayerId).

%%
%% Local Functions
%%
task0(init, GSPid, Context, TaskId, PlayerId) ->
	{_SessionId, _SId, _AId, _UserId} = Context,
	{X, Y} = ?Task0Loc,
	%io:format("do_task0 send move (x=~p, y=~p)~n", [X, Y]),
	GSPid ! {move, #pose{x = X, y = Y, angle = 0.0}},
	task0(wait, GSPid, Context, TaskId, PlayerId);

task0(wait, GSPid, Context, TaskId, PlayerId) ->
	{_SessionId, _SId, _AId, _UserId} = Context,
	receive
		{GSPid, #msg_MoveNotif{
			id = PlayerId, x = X, y = Y}} = _Msg ->
			%io:format("do_task0 received msg: ~p~n", [_Msg]),
			Dist = dist_vec2d({X, Y}, ?Task0Loc),
			if
				Dist < ?NEAR_DIST ->
					%io:format("quit do_task0~n"),
					void;	% quit the loop
				true ->
					%io:format("do_task0, not near enough?~n"),
					task0(wait, GSPid, Context, TaskId, PlayerId)
			end
	end.

