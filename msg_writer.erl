-module(erl_msg_writer).
-compile(export_all).

write_str(S) ->
	list_to_binary([write_short(length(S)), S]).

write_byte(B) -> <<B:8>>.

write_short(S) -> <<S:16>>.

write_int(I) -> <<I:32>>.

write_single(S) -> <<S:32/float>>.
