-module(hades_wrap).
-compile(export_all).
-include("../include/hades.hrl").
-import(mochijson2, [decode/1]).

-import(web_wrap,
	[
		version_tostr/1,
		get_url/1,
		post_url/2,
		extract_cookies/0,
		get_time_stamp/0
	]).

register_user(UserName) ->
	VersionStr = version_tostr("_"),
	VersionStrDot = version_tostr("."),

	post_url("account/signup",
		"invite_code=daydayup&form_email=" ++ UserName ++
		"%40hi.com&form_password=hihihi&form_name=" ++ UserName),
	{SessionId, SId, AId} = extract_cookies(),
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		post_url("profile/register",
			"accountid=" ++ AId ++
			"&account%5Fid=" ++ AId ++
			"&name=xxx&role=warrior&camp=0&idcard=1234345656dfdf&client=flash&tag=" ++
			integer_to_list(get_time_stamp()) ++
			"&format=json&userid=0&sessionid=" ++ SId ++
			"&gender=male"),
	{struct, [{<<"res">>, <<"ok">>}, {<<"userid">>, UserId}]} = decode(Result),
	{SessionId, SId, AId, UserId}.

acceptable_tasks(Context) -> ok.

%% by far, only return a single tid.
my_tasks(Context) ->
	{SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		get_url("task/mytask?client=flash&accountid=" ++ AId ++
			"&userid=" ++ integer_to_list(UserId) ++
			"&format=json&sessionid=" ++ SId ++
			"&tag=" ++ integer_to_list(get_time_stamp())),
	{struct, [{<<"my_task">>, [{struct, Props}]}]} = decode(Result),
	{_, Tid} = lists:keyfind(<<"tid">>, 1, Props),
	Tid.

get_game_server(Context) ->
	{SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		get_url("profile/locate?accountid=" ++ AId ++
			"&tag=" ++ integer_to_list(get_time_stamp()) ++
			"&client=flash&format=json&userid=" ++ integer_to_list(UserId) ++
			"&sessionid=" ++ SId),
	{struct, Props} = decode(Result),
	{_, HostB} = lists:keyfind(<<"host">>, 1, Props),
	{_, PortB} = lists:keyfind(<<"port">>, 1, Props),
	Host = binary_to_list(HostB),
	PortS = binary_to_list(PortB),
	{Port, _} = string:to_integer(PortS),
	{Host, Port}.

finish_task(Context, Tid) ->
	{SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		post_url("task/finish",
			"accountid=" ++ AId ++
			"&tag=" ++ integer_to_list(get_time_stamp()) ++
			"&client=flash&tid=" ++ integer_to_list(Tid) ++
			"&userid=" ++ integer_to_list(UserId) ++
			"&format=json&sessionid=" ++ SId),
	{struct, [{<<"res">>,<<"ok">>}|Props]} = decode(Result),
	%% only extract one of the next tasks
	{_, [{struct, AcceptableTaskB} | _]} = lists:keyfind(<<"accpetable_task">>, 1, Props),
	{_, NextTid} = lists:keyfind(<<"tid">>, 1, AcceptableTaskB),
	NextTid.

accept_task(Context, Tid) ->
	{SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		post_url("task/accept",
			"accountid=" ++ AId ++
			"&tag=" ++ integer_to_list(get_time_stamp()) ++
			"&tid=" ++ integer_to_list(Tid) ++
			"&client=flash&reduce%5Fcooldown=0&format=json&userid=" ++ integer_to_list(UserId) ++
			"&sessionid=" ++ SId),
	{struct, [{<<"res">>,<<"ok">>}|_]} = decode(Result),
	ok.

%% todo: wrap the followings
visit_register() ->
	VersionStr = version_tostr("_"),
	VersionStrDot = version_tostr("."),

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
		"&format=json&sessionid=" ++ SId).

visit_login() ->
	VersionStr = version_tostr("_"),
	VersionStrDot = version_tostr("."),

	{ok, {{"HTTP/1.1", 302, _ReasonPhrase}, Headers, _}} =
		post_url("account/temp_login",
			"form_email=hi2%40hi.com&form_password=hihihi&user_login=%E7%99%BB%E5%BD%95"),
	{SessionId, SId, AId} = extract_cookies(),
	get_url("account/game?accountid=" ++ AId ++
		"&sessionid=" ++ SId ++
		"&full_screen=3"),
	get_url("static/game/res_" ++ VersionStr ++
		"/res/ui/game_config.xml"),
	get_url("static/game/res_" ++ VersionStr ++
		"/res/ui/config/level_config.json"),
	get_url("account/get_player_list?client=flash&accountid=" ++ AId ++
		"&userid=0&format=json&sessionid=" ++ SId ++
		"&tag=" ++ integer_to_list(get_time_stamp())),

	UserId = 32081,

	%% for each player career do the following two gets
	get_url("equipment/get_data?accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&client=flash&format=json&sessionid=" ++ SId),
	get_url("profile/info?accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&client=flash&format=json&sessionid=" ++ SId),

	%get_url("static/game/res_" ++ VersionStr ++ "/res/archives.txt"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/character/career.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/zhanshi.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/fashi.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/mushi.xml"),
	get_url("static/game/res_" ++ VersionStr ++ "/res/equipment/common.xml"),

	%% after login
	get_url("static/xml/consume_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/material_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/equipment_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/charm_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/passive_skill.xml?v=" ++ VersionStrDot),
	get_url("combine/combine_list?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId ++
		"&tag=" ++ integer_to_list(get_time_stamp())),
	get_url("static/xml/monster_list.xml?v=" ++ VersionStrDot),
	get_url("static/xml/package_list.xml?v=" ++ VersionStrDot),
	get_url("skill/all_skill?format=json?v=" ++ VersionStrDot),
	get_url("static/game/res_" ++ VersionStr ++
		"/res/ui/config/buff_list.xml"),
	%get_url("system/timestamp?client=flash&accountid=" ++ AId ++
	%	"&userid=" ++ integer_to_list(UserId) ++
	%	"&format=json&sessionid=" ++ SId ++
	%	"&tag=" ++ integer_to_list(get_time_stamp())),
	post_url("account/choose_player",
		"accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&player=" ++ integer_to_list(UserId) ++
		"%20%E9%99%86%E5%89%8D%E8%BE%88&tag=" ++
		integer_to_list(get_time_stamp()) ++
		"&client=flash&format=json&sessionid=" ++ SId),
	%post_url("a/message/updates",
	%	"profile/info?accountid=" ++ AId ++
	%	"&userid=" ++ integer_to_list(UserId) ++
	%	"&tag=" ++ integer_to_list(get_time_stamp()) ++
	%	"&client=flash&format=json&sessionid=" ++ SId),
	get_url("shortcut/my_shortcut?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId ++
		"&tag=" ++ integer_to_list(get_time_stamp())),
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		get_url("task/acceptable_task?client=flash&accountid=" ++ AId ++
			"&userid=" ++ integer_to_list(UserId) ++
			"&format=json&sessionid=" ++ SId ++
			"&tag=" ++ integer_to_list(get_time_stamp())),
	io:format("acceptable task = ~p~n", [Result]),
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result1}} =
		get_url("task/mytask?client=flash&accountid=" ++ AId ++
			"&userid=" ++ integer_to_list(UserId) ++
			"&format=json&sessionid=" ++ SId ++
			"&tag=" ++ integer_to_list(get_time_stamp())),
	io:format("my task = ~p~n", [Result1]),
	%% the above response has "tid": 1
	get_url("task/task_player_count?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId ++
		"&tag=" ++ integer_to_list(get_time_stamp())),
	get_url("profile/locate?accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&tag=" ++ integer_to_list(get_time_stamp()) ++
		"&client=flash&format=json&sessionid=" ++ SId),
	get_url("passive_skill/get_data?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId ++
		"&tag=" ++ SId),
	%a/message/updates
	%% click the task npc
	get_url("task/task_player_count?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId ++
		"&tag=" ++ integer_to_list(get_time_stamp())),
	%{"finish_status": {"1": {"finished": false, "accepted_time": 1658045929.543228},
	%% click the complete button
	%post_url("task/finish",
	%	"accountid=" ++ AId ++
	%	"&tag=" ++ integer_to_list(get_time_stamp()) ++
	%	"&client=flash&tid=1&userid=" ++ integer_to_list(UserId) ++
	%	"&format=json&sessionid=" ++ SId),
	%{"res": "ok", "my_task": [], "accpetable_task": [{..., "tid": 2, ...
	%get_url("task/task_player_count?client=flash&accountid=" ++ AId ++
	%	"&userid=" ++ integer_to_list(UserId) ++
	%	"&format=json&sessionid=" ++ SId ++
	%	"&tag=" ++ integer_to_list(get_time_stamp())),
	true.
