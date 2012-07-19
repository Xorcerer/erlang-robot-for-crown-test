-module(http_client).
-export([start/0]).

-define(HOST, "http://192.168.0.106/").
-define(VERSION, {0,8,0,70}).

start() ->
	VersionStr = version_tostr(),
	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),
	%% login so as to get cookies
	{ok, {{"HTTP/1.1", 302, _ReasonPhrase}, Headers, _}} =
		post_url(
			"account/temp_login",
			"form_email=hi2%40hi.com&form_password=hihihi&user_login=%E7%99%BB%E5%BD%95"),
	get_url(""),
	get_url("static/game/res_" ++ VersionStr ++ "/res/ui/game_config.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/ui/config/level_config.json"),

	inets:stop().

version_tostr() ->
	{0, Main, Minor, Revision} = ?VERSION,
	L = [0, Main, Minor, Revision],
	L1 = lists:map(fun integer_to_list/1, L),
	string:join(L1, "_").

get_url(Url) ->
	httpc:request(
		get,
		{
			?HOST ++ Url,
			[]
		},
		[],
		[]).

post_url(Url, Content) ->
	httpc:request(
		post,
		{
			?HOST ++ Url,
			[],
			"application/x-www-form-urlencoded",
			Content
		}, [], []).
