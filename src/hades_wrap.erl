-module(hades_wrap).
-compile(export_all).
-include("playerInfo.hrl").
-include("hades.hrl").
-import(mochijson2, [decode/1]).

-import(web_wrap,
	[
		version_tostr/1,
		get_url/1,
		post_url/2,
		extract_cookies/0
	]).
-import(lib_misc, [get_time_stamp/0]).

-define(LogPassword, "123456").
-define(LogUserName, "testtest_dongyi").

login_user(UserId) ->
	UserSeq = UserId - ?USERID_BASE - 1,
	io:format("dongyi%40test~w.com~n", [UserSeq]),
	LogEmail = io_lib:format("dongyi%40test~w.com", [UserSeq]),
	{ok, {{"HTTP/1.1", _ResponseCode, _}, _, _ResponseContent}} =
		post_url("account/temp_login",
			"form_email=" ++ LogEmail ++
			"&form_password=" ++ ?LogPassword ++
			"&user_login=" ++ ?LogUserName),
	io:format("~p:logged in~n", [UserId]),
	{SessionId, SId, AId} = extract_cookies(),
	io:format("~p: get cookies:~p~n", [UserId, {SessionId, SId, AId}]),
	{SessionId, SId, AId, UserId}.

%acceptable_tasks(_Context) -> ok.

%% by far, only return a single tid.
my_tasks(Context) ->
	{_SessionId, SId, AId, UserId} = Context,
	io:format("~p:my_task~n", [UserId]),
	Url = "task/mytask?client=flash&accountid=" ++ AId ++
		"&userid=" ++ integer_to_list(UserId) ++
		"&format=json&sessionid=" ++ SId ++
		"&tag=" ++ integer_to_list(get_time_stamp()),
	io:format("~p:url=~p~n", [UserId, Url]),
	{ok, {{"HTTP/1.1", 200, "OK"}, _, Result}} = get_url(Url),
	{struct, [{<<"my_task">>, [{struct, Props}]}]} = decode(Result),
	{_, Tid} = lists:keyfind(<<"tid">>, 1, Props),
	{_, Conditions} = lists:keyfind(<<"current">>, 1, Props),
	Status = lists:map(
		fun({struct, Condition}) ->
			{_, Item} = lists:keyfind(<<"condition_item">>, 1, Condition),
			{_, Count} = lists:keyfind(<<"condition_count">>, 1, Condition),
			{Item, Count}
		end, Conditions),
	io:format("~p: conditions: ~p~n", [UserId, Conditions]),
	{Tid, Status}.

get_game_server(Context) ->
	{_SessionId, SId, AId, UserId} = Context,
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
	{_SessionId, SId, AId, UserId} = Context,
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
	{_, Conditions} = lists:keyfind(<<"condition">>, 1, AcceptableTaskB),
	Status = lists:map(
		fun({struct, Condition}) ->
			{_, Item} = lists:keyfind(<<"condition_item">>, 1, Condition),
			{_, Count} = lists:keyfind(<<"condition_count">>, 1, Condition),
			{Item, Count}
		end, Conditions),
	io:format("~p: conditions: ~p~n", [UserId, Conditions]),
	{NextTid, Status}.

task_is_over(Status) ->
	lists:all(
		fun({_Item, Count}) -> Count == 0 end, Status).
	

accept_task(Context, Tid) ->
	{_SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		post_url("task/accept",
			"accountid=" ++ AId ++
			"&tag=" ++ integer_to_list(get_time_stamp()) ++
			"&tid=" ++ integer_to_list(Tid) ++
			"&client=flash&reduce%5Fcooldown=0&format=json&userid=" ++ integer_to_list(UserId) ++
			"&sessionid=" ++ SId),
	{struct, [{<<"res">>, <<"ok">>}|_]} = decode(Result),
	ok.

get_map_table(Context, Tid) ->
	{_SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		get_url("scene/get_map_table_list?accountid=" ++ AId ++
			"&userid=" ++ integer_to_list(UserId) ++
			"&taskid=" ++ integer_to_list(Tid) ++
			"&client=flash&tag=" ++ integer_to_list(get_time_stamp()) ++
			"&format=json&sessionid=" ++ SId),
	 {struct, [{GsIdB, _}|_]} = mochijson2:decode(Result),
	 binary_to_list(GsIdB).

jump_to_task(Context, Tid, GsId) ->
	{_SessionId, SId, AId, UserId} = Context,
	{ok, {{"HTTP/1.1",200,"OK"}, _, Result}} =
		post_url("scene/jump_to_task_location",
			"accountid=" ++ AId ++
			"&userid=" ++ integer_to_list(UserId) ++
			"&table=%2D1&client=flash&tag=" ++ integer_to_list(get_time_stamp()) ++
			"&gsid=" ++ edoc_lib:escape_uri(GsId) ++
			"&tid=" ++ integer_to_list(Tid) ++
			"&format=json&sessionid=" ++ SId),
	{struct, [{<<"res">>, <<"ok">>}|_]} = decode(Result),
	ok.
