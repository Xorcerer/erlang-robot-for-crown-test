-module(http_client).
-include("../include/playerInfo.hrl").
-export([start/0]).
-import(hades_wrap,
	[
		register_user/1,
		my_tasks/1,
		get_game_server/1,
		finish_task/2
	]).
-import(lib_misc, [pmap/2, unconsult/2]).
%-import(client, [player/2]).

start() ->
	inets:start(),
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
	UserName = "abf" ++ integer_to_list(I),
	Context = register_user(UserName),
	%{SessionId, SId, AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	Tid0 = my_tasks(Context),
	io:format("tid = ~p~n", [Tid0]),
	GameServer = get_game_server(Context),
	{Host, Port} = GameServer,
	Self = self(),
	Pid = spawn(
		fun() ->
			GSPlayerPid = self(),
			client:player(Host, Port,
				fun(_) ->
					GSPlayerPid ! {move, #pose{
						x = -1905.701, y = 1252.586, angle = 0.105}},
					%% todo: we should have been waiting the player approaches the NPC
					GSPlayerPid ! {task, Tid0, ?TASKSTATE_COMPLETE}
				end)
		end),
	Tid1 = finish_task(Context, Tid0),
	Context.


%% GET /account/logout
