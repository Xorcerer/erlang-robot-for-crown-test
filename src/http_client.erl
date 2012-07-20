-module(http_client).
-export([start/0]).
-import(hades_wrap,
	[
		register_user/1,
		my_tasks/1,
		finish_task/2
	]).
-import(lib_misc, [pmap/2, unconsult/2]).

start() ->
	inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),

	N = 10,
	Result = pmap(fun player/1, lists:seq(1, N)),
	inets:stop(),
	io:format("result = ~p~n", [Result]),
	unconsult("players.dat", Result),
	ok.

player(I) ->
	UserName = "abd" ++ integer_to_list(I),
	Context = register_user(UserName),
	%{SessionId, SId, AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	Tid = my_tasks(Context),
	finish_task(Context, Tid),
	Context.



%% GET /account/logout
