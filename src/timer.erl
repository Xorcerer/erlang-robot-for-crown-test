-module(timer).
-export([start/2]).

start(Time, Fun) -> spawn(fun() -> timer(Time, Fun) end).

timer(Time, Fun) ->
	receive
		after Time ->
			Fun(),
			timer(Time, Fun)
	end.