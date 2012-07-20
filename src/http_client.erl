-module(http_client).
-export([start/0]).
-import(hades_wrap,
	[
		register_user/1
	]).

start() ->
	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),

	Context = register_user("hi10"),
	{SessionId, SId, AId, UserId} = Context,
	io:format("new user id = ~p~n", [UserId]),

	inets:stop().

%% GET /account/logout
