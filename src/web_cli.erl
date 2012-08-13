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
	UserName = "abq" ++ integer_to_list(I),
	Context = register_user(UserName),
	{SessionId, SId, AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	Tid0 = my_tasks(Context),
	io:format("tid = ~p~n", [Tid0]),
	GameServer = get_game_server(Context),
	{Host, Port} = GameServer,
	io:format("got game server location: host = ~p, port = ~p~n", [Host, Port]),
	Self = self(),
	%% todo: kill this process and its inner player process when needed
	Pid = spawn(
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
							Self ! {self(), finished};
						quit ->
							Self ! {self(), quit}
					end
				end)
		end),
	wait({Pid, finished}),
	Tid1 = finish_task(Context, Tid0),
	ok = accept_task(Context, Tid1),
	Pid ! {task, Tid1, ?TASKSTATE_TAKEN},
	Pid ! {move, #pose{
		x = 2021.341, y = 773.998, angle = 0.000}},
	Pid ! {task, Tid1, ?TASKSTATE_COMPLETE},
	Tid2 = finish_task(Context, Tid1),
	ok = accept_task(Context, Tid2),
	GsId2 = get_map_table(Context, Tid2),
	ok = jump_to_task(Context, Tid2, GsId2),
	Pid ! quit,
	wait({Pid, quit}),
	GameServer2 = get_game_server(Context),
	{Host2, Port2} = GameServer2,
	Pid2 = spawn(
		fun() ->
			GSPlayerPid = self(),
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
							Self ! {self(), moved};
						quit ->
							Self ! {self(), quit}
					end
				end)
		end),
	Context.


%% GET /account/logout
