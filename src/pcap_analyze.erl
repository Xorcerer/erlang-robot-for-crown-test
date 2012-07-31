-module(pcap_analyze).
-compile(export_all).
-import(flags, [extract_str/2, extract_int/2]).

-define(FHEAD_LEN, 24).
-define(TCP_PROTO, 6).

-define(IP1, <<192,168,0,106>>).
-define(IP2, <<192,168,0,183>>).

%% 

start() ->
	FName = flags:extract_str(file, "abd11_finish_task2.dat"),
	io:format("file name = ~p~n", [FName]),
	{ok, Buff} = file:read_file(FName),

	io:format("length of Bin ~p~n", [byte_size(Buff)]), % ok!
	Content = read_head(Buff),
	io:format("length of Content ~p~n", [byte_size(Content)]),
	Packs = analyze_pack(Content),
	io:format("count of packs is ~p~n", [length(Packs)]),
	%% fortunately, all the ip packages have 'don't fragment' flags in them.
	TCPPacks = lists:map(
		fun({TS, Pack}) ->
			{<<_Dest:48, _Src:48, 16#0800:16>>, T} = split_binary(Pack, 14),
			{TS, tcp_pack(T)}
		end, Packs),
	%[TCPPack1, TCPPack2 | _] = TCPPacks,
	%{{S1, MS1}, {{SrcIP1, SrcPort1, DestIP1, DestPort1}, _}} = TCPPack1,
	%{{S2, MS2}, {{SrcIP2, SrcPort2, DestIP2, DestPort2}, _}} = TCPPack2,
	%io:format("~p.~p~n", [S1, MS1]), %ok
	%io:format("~p.~p~n", [S2, MS2]), %ok
	%io:format("src: ~p, ~p, dst: ~p, ~p~n",
	%	[show_ip(SrcIP1), SrcPort1, show_ip(DestIP1), DestPort1]), %ok
	%io:format("src: ~p, ~p, dst: ~p, ~p~n",
	%	[show_ip(SrcIP2), SrcPort2, show_ip(DestIP2), DestPort2]), %ok
	TcpPacks1 = lists:sort(
		fun({TS1, {Sock1, _}}, {TS2, {Sock2, _}}) ->
			comp_sock(Sock1, Sock2, TS1, TS2)
		end, TCPPacks),
	%lists:foreach(
	%	fun({TS, {{SrcIP, SrcPort, DestIP, DestPort}, _}}) ->
	%		io:format("ts: ~p, src: ~p, ~p, dst: ~p, ~p~n",
	%			[show_ts(TS), show_ip(SrcIP), SrcPort, show_ip(DestIP), DestPort])
	%	end, TcpPacks1),
	TcpPacks2 = assemble_tcp(TcpPacks1),
	io:format("len: ~p~n", [length(TcpPacks2)]),
	lists:foreach(
		fun({_Sock, L}) ->
			io:format("  len: ~p~n", [length(L)])
		end, TcpPacks2),
	TcpPacks3 = lists:map(
		fun({Sock, L}) ->
			L1 = lists:map(
				fun({_TS, P}) -> P end, L),
			{Sock, list_to_binary(L1)}
		end, TcpPacks2),
	{HttpPacks, UserPacks} = lists:partition(
		fun({Sock, _P}) ->
			{SrcIP, SrcPort, DestIP, DestPort} = Sock,
			if
				<<SrcIP:32>> == ?IP1 andalso SrcPort == 80 -> true;
				<<DestIP:32>> == ?IP1 andalso DestPort == 80 -> true;
				true -> false
			end
		end, TcpPacks3),
	io:format("http: ~p, user: ~p~n", [length(HttpPacks), length(UserPacks)]),
	ok.

read_head(Buff) ->
	{<<16#D4C3B2A1:32, _:16, _:16, _:32, _:32, _:32, _:32>>, T} = split_binary(Buff, 24),
	T.

analyze_pack(Buff) -> lists:reverse(analyze_pack(Buff, [])).

analyze_pack(<<>>, Acc) -> Acc;
analyze_pack(Buff, Acc) ->
	{<<Secs:32/native, MSecs:32/native, CapLen:32/native, _:32>>, T} = split_binary(Buff, 16),
	%io:format("pack len ~p~n", [CapLen]),
	{Pack, T1} = split_binary(T, CapLen),
	analyze_pack(T1, [{{Secs, MSecs}, Pack}|Acc]).

time_delta({S1, MS1}, {S2, MS2}) ->
	DS = S2 - S1,
	if
		MS2 >= MS1 -> {DS, MS2 - MS1};
		true -> {DS-1, math:pow(10, 6) + MS2 - MS1}
	end.

tcp_pack(Pack) ->
	{<<4:4, IpHLen:4, _TOS:8, _TLen:16, _ID:16, _Flags:3, _Offset:13,
		_TTL:8, ?TCP_PROTO:8, _IpChk:16, SrcIP:32, DestIP:32>>, T} = split_binary(Pack, 20),
	{_, T1} = split_binary(T, IpHLen*4 - 20),
	%% tcp
	{<<SrcPort:16, DestPort:16, _Seq:32, _AckSeq:32,
		TcpHLen:4, _:6, _Urg:1, _Ack:1, _Psh:1, _Rst:1, _Syn:1, _Fin:1,
		_WinSize:16, _TcpChk:16, _UrgPnt:16>>, T2} = split_binary(T1, 20),
	{_, T3} = split_binary(T2, TcpHLen*4 - 20),
	{{SrcIP, SrcPort, DestIP, DestPort}, T3}.

show_ts({Sec, MSec}) ->
	integer_to_list(Sec) ++ "." ++
	integer_to_list(MSec).

show_ip(IP) ->
	<<A:8, B:8, C:8, D:8>> = <<IP:32>>,
	integer_to_list(A) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(D).

comp_sock(Sock1, Sock2, TS1, TS2) ->
	{SrcIP1, SrcPort1, DestIP1, DestPort1} = Sock1,
	{SrcIP2, SrcPort2, DestIP2, DestPort2} = Sock2,
	if
		SrcIP1 < SrcIP2 -> true;
		SrcIP1 > SrcIP2 -> false;
		true ->
			if
				SrcPort1 < SrcPort2 -> true;
				SrcPort1 > SrcPort2 -> false;
				true ->
					if
						DestIP1 < DestIP2 -> true;
						DestIP1 > DestIP2 -> false;
						true ->
							if
								DestPort1 < DestPort2 -> true;
								DestPort1 > DestPort2 -> false;
								true ->
									comp_ts(TS1, TS2)
							end
					end
			end
	end.

comp_ts({Sec1, MSec1}, {Sec2, MSec2}) ->
	if
		Sec1 < Sec2 -> true;
		Sec1 > Sec2 -> false;
		true ->
			MSec1 =< MSec2
	end.

assemble_tcp(TcpPacks) ->
	L = assemble_tcp(TcpPacks, []),
	lists:map(
		fun({Sock, L1}) ->
			{Sock, lists:reverse(L1)}
		end, L).

assemble_tcp([], Acc) -> Acc;
assemble_tcp([{TS, {Sock, P}} | T], [{Sock, L} | T1]) ->
	assemble_tcp(T, [{Sock, [{TS, P} | L]} | T1]);
assemble_tcp([{TS, {Sock, P}} | T], Acc) ->
	assemble_tcp(T, [{Sock, [{TS, P}]} | Acc]).
