-module(erl_msg_reader).
-compile(export_all).

read_str(Buff) ->
	{Len, T} = read_short(Buff),
	{S, T1} = split_binary(T, Len),
	{binary_to_list(S), T1}.

read_byte(Buff) ->
	{<<H>>, T} = split_binary(Buff, 1),
	{H, T}.

read_short(Buff) ->
	{<<H:16>>, T} = split_binary(Buff, 2),
	{H, T}.

read_int(Buff) ->
	{<<H:32>>, T} = split_binary(Buff, 4),
	{H, T}.

read_single(Buff) ->
	{<<H:32/float>>, T} = split_binary(Buff, 4),
	{H, T}.
