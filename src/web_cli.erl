-module(web_cli).
-include("records.hrl").
-include("playerInfo.hrl").
-export([start/0]).
-import(hades_wrap,
	[
		login_user/1,
		my_tasks/1,
		get_game_server/1,
		finish_task/2,
		accept_task/2,
		get_map_table/2,
		jump_to_task/3,
		task_is_over/1
	]).

-import(game_task0, [task0/4]).
-import(game_task1, [task1/4]).
-import(game_task2, [task2/7]).

-import(lib_misc,
	[
		pmap/2,
		unconsult/2,
		wait/1,
		get_time_stamp/0,
		get_username/0
	]).
-import(game_map, [read_map/1]).
-spec(player/1 :: (Args :: integer()) -> integer()).

-define(LogFileName, "logpost.txt").

start() ->
	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),
	
	AnnouncerPid = spawn(fun() -> announcer:announce() end),
	register(announcer_p, AnnouncerPid),

	CounterPid = spawn(fun count/0),
	register(counter_pid, CounterPid),
	Base = flags:extract_int(base, 1),
	N = flags:extract_int(count, 1),
	UBound = Base + N - 1,
	Result = pmap(fun player/1,
		lists:seq(?USERID_BASE + Base, ?USERID_BASE + UBound)),
	inets:stop(),

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

count() -> count(0).
count(Num) ->
	receive
		{FromPid, get_current_number} ->
			FromPid ! {self(), Num},
			count(Num + 1)
	end.

player(UserId) ->
%% 	counter_pid ! {self(), get_current_number},
%% 	receive {current_number, SerialNum} -> SerialNum end,
	SerialNum = lib_misc:rpc(counter_pid, get_current_number),
	ok = timer:sleep(SerialNum * 200),
	ProfileId = web_wrap:get_httpc_profile(self()),
	{ok, HttpPid} = inets:start(httpc, [{profile, ProfileId}]),
	io:format("inets:start, pid=~p~n", [HttpPid]),
	
	httpc:set_options(
		[
			{cookies, enabled}
		], HttpPid),

	Context = login_user(UserId),
%% 	% announcer_p ! {self(), report, UserId, logged_in},
	{_SessionId, SId, _AId, UserId} = Context,
%% 	io:format("~p:context ok~n", [UserId]),
%% 	{TaskId0, _} = my_tasks(Context),
%% 	io:format("task id = ~p~n", [TaskId0]),
%% 	GameServer0 = get_game_server(Context),
%% 	{Host0, Port0} = GameServer0,
%% 	io:format("~p: got game server0 location: host = ~p, port = ~p~n", [UserId, Host0, Port0]),
%% 	ParentPid = self(),
%% 	io:format("~p: ParentPid = ~p~n", [UserId, ParentPid]),
%% 	
%% 	%% todo: kill this process and its inner player process when needed
%% 	GSPid0 = spawn(
%% 		fun() ->
%% 			io:format("~p: spawning cli:player~n", [UserId]),
%% 			cli:player_msg(ParentPid, GameServer0, SId, UserId)
%% 		end),
%% 	io:format("~p: GSPid0 = ~p~n", [UserId, GSPid0]),
%% 	PlayerId0 = wait_player_id(GSPid0),
%% 	io:format("~p: player0 id = ~p~n", [UserId, PlayerId0]),
%% 	PlayerLoc0 = wait_player_loc(GSPid0),
%% 	io:format("~p: player0 loc = ~p~n", [UserId, PlayerLoc0]),
%% 	
%% 	task0(GSPid0, Context, TaskId0, PlayerId0),
%% 	io:format("~p: task0 finished~n", [UserId]),
%% 
%% 	{TaskId1, _} = finish_task(Context, TaskId0),
%% 	io:format("~p: task1 is known:~p~n", [UserId, TaskId1]),
%% 	GSPid0 ! {msg, #msg_Task{taskId = TaskId0, taskState = ?TASKSTATE_COMPLETE}},
%% 	% announcer_p ! {self(), report, UserId, fin_task0},
%% 
%% 	io:format("~p: pre accept task id:~p~n", [UserId, TaskId1]),
%% 	ok = accept_task(Context, TaskId1),
%% 	GSPid0 ! {msg, #msg_Task{taskId = TaskId1, taskState = ?TASKSTATE_TAKEN}},
%% 	task1(GSPid0, Context, TaskId1, PlayerId0),
%% 	io:format("~p: task1 finished~n", [UserId]),
%% 
%% 	%% finish the second task, meanwhile get the conditions of the next task.
%% 	{TaskId2, _Status2} = finish_task(Context, TaskId1),
%% 	GSPid0 ! {msg, #msg_Task{taskId = TaskId1, taskState = ?TASKSTATE_COMPLETE}},
%% 	% announcer_p ! {self(), report, UserId, fin_task1},
%% 
%% 	ok = accept_task(Context, TaskId2),
%% 	GsId2 = get_map_table(Context, TaskId2),
%% 	ok = jump_to_task(Context, TaskId2, GsId2),
%% 
%% 	GSPid0 ! quit,
%% 	io:format("~p: wait quitAck~n", [UserId]),
%% 	wait({GSPid0, quitAck}),
%% 	io:format("~p: after quitAck~n", [UserId]),
%% 
%% 	GameServer2 = get_game_server(Context),
	ParentPid = self(),
	_Status2 = 0,
	TaskId2 = 3,
	GameServer2 = {"172.16.8.210", 10001},
	
	{Host2, Port2} = GameServer2,
	io:format("~p: got game server2 location: host = ~p, port = ~p~n", [UserId, Host2, Port2]),
	timer:sleep(2000),
	GameMap2 = read_map("../data/yewai2.grf"),	% todo: fix the hardcoded map name here
	io:format("~p: after read_map~n", [UserId]),

	%% experiment: only tackle with the task related monsters' appearances
	GSPid2 = spawn(
		fun() ->
			io:format("~p: spawning cli:player 2~n", [UserId]),
			cli:player_msg(ParentPid, GameServer2, SId, UserId)
		end),
	PlayerId2 = wait_player_id(GSPid2),
	io:format("~p: player2 id = ~p~n", [UserId, PlayerId2]),
	PlayerLoc2 = wait_player_loc(GSPid2),
	io:format("~p: player2 loc = ~p~n", [UserId, PlayerLoc2]),

	task2(GSPid2, Context, TaskId2, PlayerId2, PlayerLoc2, GameMap2, _Status2),
	io:format("~p: task2 finished~n", [UserId]),

	{_TaskId3, _Status3} = finish_task(Context, TaskId2),
	GSPid2 ! {msg, #msg_Task{taskId = TaskId2, taskState = ?TASKSTATE_COMPLETE}},
	% announcer_p ! {self(), report, UserId, fin_task2},

	inets:stop(httpc, ProfileId),
	io:format("~p: great! task2 finished!!~n", [UserId]),
	
	GSPid2 ! quit,
	io:format("~p: wait quitAck2~n", [UserId]),
	wait({GSPid2, quitAck}),
	io:format("~p: gspid2: ~p, after quitAck2~n", [UserId, GSPid2]),

	UserId.


%% GET /account/logout
