-module(http_client).
-export([start/0]).
-import(hades_wrap,
	[
		register_user/1
	]).
-import(lib_misc, [pmap/2]).

start() ->
	inets:start(),
	Result = pmap(fun player/1, ["aba1", "aba2"]),
	inets:stop(),
	io:format("result = ~p~n", [Result]),
	ok.

player(UserName) ->
	httpc:set_options(
		[
			{cookies, enabled}
		]),

	Context = register_user(UserName),
	{SessionId, SId, AId, UserId} = Context,
	io:format("new user id = ~p~n", [UserId]),
	Context.



%% GET /account/logout
