-module(web_cli).
-include("records.hrl").
-include("playerInfo.hrl").
-export([start/0]).
-import(hades_wrap,
	[
		register_user/2,
		my_tasks/1,
		get_game_server/1,
		finish_task/2,
		accept_task/2,
		get_map_table/2,
		jump_to_task/3,
		task_is_over/1
	]).

-import(game_task0, [task0/5]).
-import(game_task1, [task1/5]).
-import(game_task2, [task2/7]).

-import(lib_misc, [pmap/2, unconsult/2, wait/1]).
-import(game_map, [read_map/1]).

start() ->
	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),

	N = 2,
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
	{ok, HttpPid} = inets:start(httpc, [{profile, self()}]),
	io:format("inets:start, pid=~p~n", [HttpPid]),
	
	httpc:set_options(
		[
			{cookies, enabled}
		], self()),

	UserName = "afr" ++ integer_to_list(I),
	io:format("username: ~p~n", [UserName]),
	Context = register_user(UserName, I),
	{_, _SessionId, SId, _AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	{TaskId0, _} = my_tasks(Context),
	io:format("task id = ~p~n", [TaskId0]),
	GameServer0 = get_game_server(Context),
	{Host0, Port0} = GameServer0,
	io:format("~p: got game server0 location: host = ~p, port = ~p~n", [I, Host0, Port0]),
	ParentPid = self(),
	io:format("~p: ParentPid = ~p~n", [UserName, ParentPid]),
	
	%% todo: kill this process and its inner player process when needed
	GSPid0 = spawn(
		fun() ->
			io:format("~p: spawning cli:player~n", [I]),
			cli:player_msg(ParentPid, GameServer0, SId, UserId)
		end),
	io:format("~p: GSPid0 = ~p~n", [I, GSPid0]),
	PlayerId0 = wait_player_id(GSPid0),
	io:format("~p: player0 id = ~p~n", [I, PlayerId0]),
	PlayerLoc0 = wait_player_loc(GSPid0),
	io:format("~p: player0 loc = ~p~n", [I, PlayerLoc0]),
	
	task0(GSPid0, Context, TaskId0, UserId, PlayerId0),
	io:format("~p: task0 finished~n", [I]),

	{TaskId1, _} = finish_task(Context, TaskId0),
	GSPid0 ! {msg, #msg_Task{taskId = TaskId0, taskState = ?TASKSTATE_COMPLETE}},

	ok = accept_task(Context, TaskId1),
	GSPid0 ! {msg, #msg_Task{taskId = TaskId1, taskState = ?TASKSTATE_TAKEN}},
	task1(GSPid0, Context, TaskId1, UserId, PlayerId0),
	io:format("~p: task1 finished~n", [I]),

	%% finish the second task, meanwhile get the conditions of the next task.
	{TaskId2, _Status2} = finish_task(Context, TaskId1),
	GSPid0 ! {msg, #msg_Task{taskId = TaskId1, taskState = ?TASKSTATE_COMPLETE}},

	ok = accept_task(Context, TaskId2),
	GsId2 = get_map_table(Context, TaskId2),
	ok = jump_to_task(Context, TaskId2, GsId2),

	GSPid0 ! quit,
	io:format("~p: wait quitAck~n", [I]),
	wait({GSPid0, quitAck}),
	io:format("~p: after quitAck~n", [I]),

	GameServer2 = get_game_server(Context),
	{Host2, Port2} = GameServer2,
	io:format("~p: got game server2 location: host = ~p, port = ~p~n", [I, Host2, Port2]),
	GameMap2 = read_map("yewai2.grf"),	% todo: fix the hardcoded map name here
	io:format("~p: after read_map~n", [I]),

	%% experiment: only tackle with the task related monsters' appearances
	GSPid2 = spawn(
		fun() ->
			io:format("~p: spawning cli:player 2~n", [I]),
			cli:player_msg(ParentPid, GameServer2, SId, UserId)
		end),
	PlayerId2 = wait_player_id(GSPid2),
	io:format("~p: player2 id = ~p~n", [I, PlayerId2]),
	PlayerLoc2 = wait_player_loc(GSPid2),
	io:format("~p: player2 loc = ~p~n", [I, PlayerLoc2]),

	task2(GSPid2, Context, TaskId2, UserId, PlayerId2, PlayerLoc2, GameMap2),
	io:format("~p: task2 finished~n", [I]),

	{_TaskId3, _Status3} = finish_task(Context, TaskId2),
	GSPid2 ! {msg, #msg_Task{taskId = TaskId2, taskState = ?TASKSTATE_COMPLETE}},

	inets:stop(httpc, self()),
	io:format("~p: great! task2 finished!!~n", [I]),
	ok.


%% GET /account/logout
