-module(web_wrap).
-compile(export_all).
-include("hades.hrl").

version_tostr(Split) ->
	{0, Main, Minor, Revision} = ?VERSION,
	L = [0, Main, Minor, Revision],
	L1 = lists:map(fun integer_to_list/1, L),
	string:join(L1, Split).

get_httpc_profile(Pid) -> list_to_atom(pid_to_list(Pid)).

get_url(Url) ->
	io:format("get_url ~p~n", [Url]),
	httpc:request(
		get,
		{
			?HOST ++ Url,
			[]
		},
		[],
		[],
		get_httpc_profile(self())).

post_url(Url, Content) ->
	io:format("post_url ~p~n", [Url]),
	httpc:request(
		post,
		{
			?HOST ++ Url,
			[],
			"application/x-www-form-urlencoded",
			Content
		}, [], [], get_httpc_profile(self())).

extract_cookies() ->
	Cookies = httpc:which_cookies(get_httpc_profile(self())),
	{_, SessionCookies} = lists:keyfind(session_cookies, 1, Cookies),
	SessionId = get_cookie("session_id", SessionCookies),
	SId = get_cookie("sid", SessionCookies),
	AId = get_cookie("aid", SessionCookies),
	{SessionId, SId, AId}.

get_cookie(Name, Cookies) ->
	{_, _, _, Name, Value, _, _, _, _, _, _} =
		lists:keyfind(Name, 4, Cookies),
	Value.
