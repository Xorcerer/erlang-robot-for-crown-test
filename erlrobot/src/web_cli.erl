-module(web_cli).
-include("records.hrl").
-include("playerInfo.hrl").
-export([start/0]).
-import(hades_wrap,
	[
		register_user/1,
		my_tasks/1,
		get_game_server/1,
		finish_task/2,
		accept_task/2,
		get_map_table/2,
		jump_to_task/3,
		task_is_over/1
	]).
-import(game_task,
	[
		task0/5,
		task1/5,
		task2/7
	]).
-import(lib_misc, [pmap/2, unconsult/2, wait/1]).
-import(game_map, [read_map/1]).

start() ->
	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),

	N = 1,
	Result = pmap(fun player/1, lists:seq(1, N)),
	inets:stop(),
	io:format("result = ~p~n", [Result]),
	unconsult("players.dat", Result),
	ok.

wait_player_id(GSPid) ->
	receive
		{GSPid, {logged, PlayerId}} ->
			PlayerId
	end.

wait_player_loc(GSPid) ->
	receive
		{GSPid, {playerPose, #pose{x = X, y = Y}}} ->
			{X, Y}
	end.

player(I) ->
	UserName = "aeo" ++ integer_to_list(I),
	io:format("username: ~p~n", [UserName]),
	Context = register_user(UserName),
	{_SessionId, SId, _AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	{TaskId0, _} = my_tasks(Context),
	io:format("task id = ~p~n", [TaskId0]),
	GameServer0 = get_game_server(Context),
	{Host0, Port0} = GameServer0,
	io:format("got game server0 location: host = ~p, port = ~p~n", [Host0, Port0]),
	ParentPid = self(),
	io:format("ParentPid = ~p~n", [ParentPid]),
	
	%% todo: kill this process and its inner player process when needed
	GSPid0 = spawn(
		fun() ->
			io:format("spawning cli:player~n"),
			cli:player_msg(ParentPid, GameServer0, SId, UserId)
		end),
	io:format("GSPid0 = ~p~n", [GSPid0]),
	PlayerId0 = wait_player_id(GSPid0),
	io:format("player0 id = ~p~n", [PlayerId0]),
	PlayerLoc0 = wait_player_loc(GSPid0),
	io:format("player0 loc = ~p~n", [PlayerLoc0]),
	
	task0(GSPid0, Context, TaskId0, UserId, PlayerId0),
	io:format("task0 finished~n"),

	{TaskId1, _} = finish_task(Context, TaskId0),
	GSPid0 ! {msg, #msg_Task{taskId = TaskId0, taskState = ?TASKSTATE_COMPLETE}},

	ok = accept_task(Context, TaskId1),
	GSPid0 ! {msg, #msg_Task{taskId = TaskId1, taskState = ?TASKSTATE_TAKEN}},
	task1(GSPid0, Context, TaskId1, UserId, PlayerId0),
	io:format("task1 finished~n"),

	%% finish the second task, meanwhile get the conditions of the next task.
	{TaskId2, _Status2} = finish_task(Context, TaskId1),
	GSPid0 ! {msg, #msg_Task{taskId = TaskId1, taskState = ?TASKSTATE_COMPLETE}},

	ok = accept_task(Context, TaskId2),
	GsId2 = get_map_table(Context, TaskId2),
	ok = jump_to_task(Context, TaskId2, GsId2),

	GSPid0 ! quit,
	io:format("wait quitAck~n"),
	wait({GSPid0, quitAck}),
	io:format("after quitAck~n"),

	GameServer2 = get_game_server(Context),
	{Host2, Port2} = GameServer2,
	io:format("got game server2 location: host = ~p, port = ~p~n", [Host2, Port2]),
	GameMap2 = read_map("yewai2.grf"),	% todo: fix the hardcoded map name here
	io:format("after read_map~n"),

	%% experiment: only tackle with the task related monsters' appearances
	GSPid2 = spawn(
		fun() ->
			io:format("spawning cli:player 2~n"),
			cli:player_msg(ParentPid, GameServer2, SId, UserId)
		end),
	PlayerId2 = wait_player_id(GSPid2),
	io:format("player2 id = ~p~n", [PlayerId2]),
	PlayerLoc2 = wait_player_loc(GSPid2),
	io:format("player2 loc = ~p~n", [PlayerLoc2]),

	task2(GSPid2, Context, TaskId2, UserId, PlayerId2, PlayerLoc2, GameMap2),
	io:format("task2 finished~n"),

	{_TaskId3, _Status3} = finish_task(Context, TaskId2),
	GSPid2 ! {msg, #msg_Task{taskId = TaskId2, taskState = ?TASKSTATE_COMPLETE}},

	io:format("great! task2 finished!!~n"),
	ok.


%% GET /account/logout
