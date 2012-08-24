-module(lib_misc).
-compile(export_all).

pmap(F, L) ->
	S = self(),
	%% make_ref() returns a unique reference
	%% we'll match on this later
	Ref = erlang:make_ref(),
	Pids = lists:map(
		fun(I) ->
			spawn(fun() -> do_f(S, Ref, F, I) end)
		end, L),
	%% gather the results
	gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
	Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
	receive
		{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
	end;
gather([], _) ->
	[].

unconsult(File, L) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
	file:close(S).

timer(Time, Fun) ->
	receive
		Msg ->
			io:format("timer received msg: ~p~n", [Msg]),
			void
	after Time ->
		R = Fun(),
		if
			R -> timer(Time, Fun);
			true -> void
		end
	end.

wait(Msg) ->
	receive
		Msg -> void;
		_ -> wait(Msg)
	end.

	
get_time_stamp() ->
	calendar:datetime_to_gregorian_seconds(erlang:localtime()).

get_username() ->
	 %euuid:format(euuid:v5(euuid:ns_dns(), "example.com")).
	%S = euuid:format(euuid:v1()),
	%S1 = lists:filter(fun(C) -> C /= $- end, S),
	%S2 = lists:map(fun(C) -> if C >= $0 andalso C =< $9 -> C - $0 + $g; true -> C end end, S1),
	%S2.
	integer_to_list(get_time_stamp()).