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
	UserName = "acf" ++ integer_to_list(I),
	Context = register_user(UserName),
	{SessionId, SId, AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	Tid0 = my_tasks(Context),
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
						#msg_CreatureAppearNotif{
							id = _PlayerId,
							x = _X,
							y = _Y,
							angle = _Angle} ->
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

	Tid1 = finish_task(Context, Tid0),
	ok = accept_task(Context, Tid1),
	Pid0 ! {task, Tid1, ?TASKSTATE_TAKEN},
	Pid0 ! {move, #pose{
		x = 2021.341, y = 773.998, angle = 0.000}},
	Pid0 ! {task, Tid1, ?TASKSTATE_COMPLETE},
	Tid2 = finish_task(Context, Tid1),
	ok = accept_task(Context, Tid2),
	GsId2 = get_map_table(Context, Tid2),
	ok = jump_to_task(Context, Tid2, GsId2),
	Pid0 ! quit,
	io:format("wait quitAck"),

	wait({Pid0, quitAck}),

	io:format("after quitAck"),

	GameServer2 = get_game_server(Context),
	{Host2, Port2} = GameServer2,
	{Triggers, Paths} = read_map("yewai2.grf"),	% todo: fix the hardcoded map name here
	Pid2 = spawn(
		fun() ->
			GSPlayerPid = self(),	% it is also Pid2
			io:format("spawning cli:player 2"),
			cli:player(Host, Port, SId, UserId,
				fun(Msg) ->
					case Msg of
						#msg_CreatureAppearNotif{
							id = _PlayerId,
							x = _X,
							y = _Y,
							angle = _Angle} ->
							io:format("in cli:player 2"),
							GSPlayerPid ! {move, #pose{
								x = -1905.701, y = 1252.586, angle = 0.105}},
							%% todo: we should have been waiting the player approaches the NPC
							%GSPlayerPid ! {task, Tid2, ?TASKSTATE_COMPLETE},
							ParentPid ! {self(), moved};
						quitAck ->
							ParentPid ! {self(), quitAck};
						_ ->
							void
					end
				end)
		end),
	wait({Pid2, quitAck}),
	Context.


%% GET /account/logout
