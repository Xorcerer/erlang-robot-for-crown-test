%% Author: Jackie
%% Created: 2012-8-20
%% Description: TODO: Add description to game_task1
-module(game_task1).

%%
%% Include files
%%
-include("records.hrl").
-include("playerInfo.hrl").

%%
%% Exported Functions
%%
-export([task1/4]).

%%
%% API Functions
%%
-import(math_util, [dist_vec2d/2]).

-define(Task1Loc, {2021.341, 773.998}).

task1(GSPid, Context, TaskId, PlayerId) ->
	{_SessionId, _SId, _AId, _UserId} = Context,
	%io:format("task1, GSPid = ~p~n", [GSPid]),
	task1(init, GSPid, Context, TaskId, PlayerId).

%%
%% Local Functions
%%
task1(init, GSPid, Context, TaskId, PlayerId) ->
	{_SessionId, _SId, _AId, _UserId} = Context,
	{X, Y} = ?Task1Loc,
	%io:format("do_task1 send move (x=~p, y=~p)~n", [X, Y]),
	GSPid ! {move, #pose{x = X, y = Y, angle = 0.0}},
	task1(wait, GSPid, Context, TaskId, PlayerId);

task1(wait, GSPid, Context, TaskId, PlayerId) ->
	{_SessionId, _SId, _AId, _UserId} = Context,
	receive
		{GSPid, #msg_MoveNotif{
			id = PlayerId, x = X, y = Y}} = _Msg ->
			%io:format("do_task1 received msg: ~p~n", [_Msg]),
			Dist = dist_vec2d({X, Y}, ?Task1Loc),
			if
				Dist < ?NEAR_DIST ->
					%io:format("quit do_task1~n"),
					void;	% quit the loop
				true ->
					%io:format("do_task1, not near enough?~n"),
					task1(wait, GSPid, Context, TaskId, PlayerId)
			end
	end.

