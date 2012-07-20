-module(lib_misc).
-compile(export_all).
-import(lists, [map/2]).

pmap(F, L) ->
	S = self(),
	%% make_ref() returns a unique reference
	%% we'll match on this later
	Ref = erlang:make_ref(),
	Pids = map(
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