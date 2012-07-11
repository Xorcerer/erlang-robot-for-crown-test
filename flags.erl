-module(flags).
-compile(export_all).

extract_str(Flag, Default) ->
	case init:get_argument(Flag) of
		{ok, [[S]]} -> S;
		_ -> Default
	end.

extract_int(Flag, Default) ->
	case init:get_argument(Flag) of
		{ok, [[S]]} ->
			{ F, _ } = string:to_integer(S),
			F;
		_ -> Default
	end.
