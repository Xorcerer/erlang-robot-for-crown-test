-module(http_client).
-export([start/0]).
-import(mochijson2, [decode/1]).

-define(HOST, "http://192.168.0.106/").
-define(VERSION, {0,8,0,71}).

start() ->
	VersionStr = version_tostr("_"),
	VersionStrDot = version_tostr("."),

	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),
	%% login so as to get cookies
	{ok, {{"HTTP/1.1", 302, _ReasonPhrase}, Headers, _}} =
		post_url("account/temp_login",
			"form_email=hi2%40hi.com&form_password=hihihi&user_login=%E7%99%BB%E5%BD%95"),
	{SessionId, SId, AId} = extract_cookies(),
	%io:format("cookies = ~p~n", [{SessionId, SId, AId}]),
	get_url(""),
	get_url("static/game/res_" ++ VersionStr ++ "/res/ui/game_config.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/ui/config/level_config.json"),
	get_url("account/get_player_list?client=flash&accountid=" ++ AId ++
		"&userid=0&format=json&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&sessionid=" ++ SId),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/zhanshi.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/fashi.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/mushi.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/common.xml"),
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		post_url("profile/register",
			"accountid=" ++ AId ++ "&account%5Fid=" ++ AId ++ "&gender=male&tag=" ++
			integer_to_list(get_time_stamp()) ++
			"&role=warrior&camp=0&client=flash&idcard=1234345656dfdf&userid=0&name=%E5%8C%97%E7%8E%84%E8%80%81%E4%BA%BA&format=json&sessionid=" ++
			SId),
	{struct, [{<<"res">>, <<"ok">>}, {<<"userid">>, UserId}]} = decode(Result),
	%UserId = 31681,
	io:format("user id = ~p~n", [UserId]),
	get_url("account/get_player_list?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++ "&format=json&tag=" ++
		integer_to_list(get_time_stamp()) ++
		"&sessionid=" ++ SId),
	get_url("equipment/get_data?accountid=" ++ AId ++ "&tag=" ++
		integer_to_list(get_time_stamp()) ++ "&client=flash&userid=" ++
		integer_to_list(UserId) ++ "&format=json&sessionid=" ++ SId),
	get_url("static/xml/consume_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/material_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/equipment_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/charm_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/passive_skill.xml?v=" ++ VersionStrDot),
	get_url("combine/combine_list?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&sessionid=" ++ SId),
	get_url("static/xml/monster_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/package_list.xml?v=" ++ VersionStrDot),
	get_url("skill/all_skill?format=json&v=" ++ VersionStrDot),
	get_url("static/game/res_" ++ VersionStr ++ "/res/ui/config/buff_list.xml"),
	get_url("system/timestamp?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&sessionid=" ++ SId),
	post_url("account/choose_player",
		"accountid=" ++ AId ++
		"&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&player=" ++ integer_to_list(UserId) ++
		"%20%E5%8C%97%E7%8E%84%E8%80%81%E4%BA%BA&client=flash&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId),
	%% this post has no response?
	%post_url("a/message/updates",
	%	"client=flash&accountid=" ++ AId ++
	%	"&userid=" ++ integer_to_list(UserId) ++
	%	"&format=json&tag=" ++ integer_to_list(get_time_stamp()) ++
	%	"&sessionid=" ++ SId),
	get_url("task/task_player_count?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&sessionid=" ++ SId),
	post_url("camp_task/get_owners",
		"accountid=" ++ AId ++
		"&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&tids=&client=flash&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId),
	post_url("task/finish",
		"accountid=" ++ AId ++
		"&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&client=flash&tid=1&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId),

	inets:stop().

version_tostr(Split) ->
	{0, Main, Minor, Revision} = ?VERSION,
	L = [0, Main, Minor, Revision],
	L1 = lists:map(fun integer_to_list/1, L),
	string:join(L1, Split).

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
	io:format("post_url ~p~n", [Url]),
	httpc:request(
		post,
		{
			?HOST ++ Url,
			[],
			"application/x-www-form-urlencoded",
			Content
		}, [], []).

extract_cookies() ->
	Cookies = httpc:which_cookies(),
	[{_, SessionCookies} | _] = lists:filter(
		fun(C) ->
			case C of
				{session_cookies, _} -> true;
				_ -> false
			end
		end, Cookies),
	SessionId = get_cookie("session_id", SessionCookies),
	SId = get_cookie("sid", SessionCookies),
	AId = get_cookie("aid", SessionCookies),
	{SessionId, SId, AId}.

get_cookie(Name, Cookies) ->
	{
		_, _, _, Name, Value,
		_, _, _, _, _, _
	} = lists:keyfind(Name, 4, Cookies),
	Value.
	
get_time_stamp() ->
	calendar:datetime_to_gregorian_seconds(erlang:localtime()).
