-module(web_cli).
-include("../include/records.hrl").
-include("../include/playerInfo.hrl").
-export([start/0]).
-import(hades_wrap,
	[
		register_user/1,
		my_tasks/1,
		get_game_server/1,
		finish_task/2,
		accept_task/2,
		get_map_table/2,
		jump_to_task/3
	]).
-import(lib_misc, [pmap/2, unconsult/2, wait/1]).
-import(game_map, [read_map/1]).
%-import(cli, [player/2]).

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

player(I) ->
	UserName = "ach" ++ integer_to_list(I),
	Context = register_user(UserName),
	{SessionId, SId, AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	{Tid0, _} = my_tasks(Context),
	io:format("tid = ~p~n", [Tid0]),
	GameServer = get_game_server(Context),
	{Host, Port} = GameServer,
	io:format("got game server location: host = ~p, port = ~p~n", [Host, Port]),
	ParentPid = self(),
	%% todo: kill this process and its inner player process when needed
	Pid0 = spawn(
		fun() ->
			GSPlayerPid = self(),
			io:format("spawning cli:player"),
			cli:player(Host, Port, SId, UserId,
				fun(Msg) ->
					case Msg of
						{playerPose, #pose{
							x = _X,
							y = _Y,
							angle = _Angle}} ->
							io:format("in cli:player"),
							GSPlayerPid ! {move, #pose{
								x = -1905.701, y = 1252.586, angle = 0.105}},
							%% todo: we should have been waiting the player approaches the NPC
							GSPlayerPid ! {task, Tid0, ?TASKSTATE_COMPLETE},
							ParentPid ! {self(), finished};
						quitAck ->
							ParentPid ! {self(), quitAck};
						_ ->
							void
					end
				end)
		end),
	wait({Pid0, finished}),

	{Tid1, _} = finish_task(Context, Tid0),
	ok = accept_task(Context, Tid1),
	Pid0 ! {task, Tid1, ?TASKSTATE_TAKEN},
	Pid0 ! {move, #pose{
		x = 2021.341, y = 773.998, angle = 0.000}},
	Pid0 ! {task, Tid1, ?TASKSTATE_COMPLETE},
	
	%% finish the task number 2, meanwhile get the conditions of the next task.
	{Tid2, Status2} = finish_task(Context, Tid1),
	ok = accept_task(Context, Tid2),
	GsId2 = get_map_table(Context, Tid2),
	ok = jump_to_task(Context, Tid2, GsId2),
	Pid0 ! quit,
	io:format("wait quitAck~n"),

	wait({Pid0, quitAck}),

	io:format("after quitAck~n"),

	GameServer2 = get_game_server(Context),
	{Host2, Port2} = GameServer2,
	{Triggers, Paths} = read_map("yewai2.grf"),	% todo: fix the hardcoded map name here

	%% experiment: only tackle with the task related monsters' appearances
	Pid2 = spawn(
		fun() ->
			GSPlayerPid = self(),	% it is also Pid2
			io:format("spawning cli:player 2"),
			cli:player(Host, Port, SId, UserId,
				fun(Msg) ->
					case Msg of
						{logged, PlayerId} ->	% this must be the first message
							ParentPid ! {self(), {logged, PlayerId}};
						
						%% this must be the second message
						{playerPose, #pose{
							x = _X,
							y = _Y,
							angle = _Angle}} = PlayerPose ->
							io:format("in cli:player 2"),
							%% todo: we should have been waiting the player approaches the NPC
							%GSPlayerPid ! {task, Tid2, ?TASKSTATE_COMPLETE},
							ParentPid ! {self(), PlayerPose};
						#msg_CreatureAppearNotif{
							id = Id,
							userId = UserId,
							x = X,
							y = Y} ->
							io:format("monster appear(userId:~p, id:~p, x:~p, y:~p)~n)", [UserId, Id, X, Y]),
							ParentPid ! {self(), {monster, Id, X, Y}};
						quitAck ->
							ParentPid ! {self(), quitAck};
						_ ->
							void
					end
				end)
		end),
	
	PlayerId2 =
		receive
			{Pid2, {logged, PlayerId}} ->
				PlayerId
		end,
	
	%% find the nearest trigger point, return its id
	FindNearestTrigger =
		fun(X, Y) ->
			Distances = lists:map(
				fun({Id, X1, Y1}) ->
					DX = X1 - X,
					DY = Y1 - Y,
					DistSquared = DX*DX + DY*DY,
					{Id, DistSquared}
				end, Triggers),
			Sorted = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
			[{Id, _} | _] = Sorted,
			Id
		end,
	
	PlayerPose2 =
		receive
			{Pid2, {playerPose, #pose{x = X, y = Y}} ->
				FindNearestTrigger(X, Y)
		end,
	io:format("nearest trigger id: ~p~n", [PlayerPose2]),

	%% recursive lambda!
	HandleTask2 =
		fun(Monsters, DestPos, PlayerPose, F) ->
			%% if there is no monster appears, traverse the trigger points
			%% else, attack the first monster
			case Monsters of
				[] ->
					
				_ ->
					%% judge the nearest monster, approach it, get ready for attacking
					Pid2 ! {move, #pose{x = X,}
			end,
			receive
				{Pid2, {monster, Id, X, Y}} ->
					F([{Id, X, Y}} | Monsters], DestPos, F);
				{Pid2, {killed, Id}} ->
					%% if killed a monster, query the task status
					%% if task is over, quit
					
				{Pid2, quitAck} ->
					void	% quit loop
			end
		end,
	HandleTask2([], undefined, PlayerPose2, HandleTask2),
	Context.


%% GET /account/logout
