-module(erl_client).
-compile(export_all).
-include("records.hrl").
-import(lists, [reverse/1]).
-import(erl_msg, [write_msg/1, read_msg/2]).
-import(erl_timer, [start/2]).

start() ->
	N = 200,
	[ spawn(fun() -> player() end) || A <- lists:seq(1, N) ].

player() ->
    player("localhost", 9527).

player(Host, Port) ->
    {ok,Socket} = gen_tcp:connect(Host,Port,
		[binary, {packet, 0}, {nodelay, true}, {active, false}]), %% (1)
	UserId = 29321,
    ok = gen_tcp:send(Socket,
		binary_to_list(erl_msg:write_msg(#msg_login{
			session_key = "HELLO",
			user_id = UserId,
			table_id = -1,
			major = 0,
			minor = 8,
			revision = 0 }))),  %% (2)
	{ok, <<?MSG_LoginAck:32, MsgLen:32>>} = gen_tcp:recv(Socket, 8),
	{ok, Buff} = gen_tcp:recv(Socket, MsgLen),
	#msg_login_ack{err_code = 0, id = PlayerId} = erl_msg:read_msg(Buff, ?MSG_LoginAck),
	Pid = self(),
	erl_timer:start(3000,
		fun() ->
			%io:format("send internal ping~n"),
			Pid ! ping
		end),
	spawn(
		fun() ->
			socket_loop(Pid, Socket)
		end),
	%io:format("start loop~n"),
	loop(Socket, #player_info{
		user_id = UserId,
		player_id = PlayerId }, 0).

socket_loop(P, Socket) ->
	%io:format("enter socket_loop~n"),
	{ok, <<MsgId:32, MsgLen:32>>} = gen_tcp:recv(Socket, 8),
	%io:format("in socket_loop, msgid=~p, len=~p~n", [MsgId, MsgLen]),
	{ok, Buff} = gen_tcp:recv(Socket, MsgLen),
	%io:format("in socket_loop, buff received~n"),
	Msg = erl_msg:read_msg(Buff, MsgId),
	%io:format("in socket_loop, msg read:~p~n", [Msg]),
	P ! Msg,
	socket_loop(P, Socket).

loop(Socket, PlayerInfo, FrameNo) ->
	%io:format("in main loop~n"),
	#player_info{
		user_id = UserId,
		player_id = PlayerId,
		pose = Pose } = PlayerInfo,
	receive
		ping ->
			%io:format("in ping~n"),
			ok = gen_tcp:send(Socket,
				binary_to_list(erl_msg:write_msg(#msg_ping{data = FrameNo}))),
			loop(Socket, PlayerInfo, FrameNo + 1);
		move ->
			%io:format("in move~n"),
			#pose{
				x = X,
				y = Y} = Pose,
			DX = (random:uniform() - 0.5) * 250.0,
			DY = (random:uniform() - 0.5) * 250.0,
			X1 = X + DX,
			Y1 = Y + DY,
			Angle = math:atan2(DY, DX),
			ok = gen_tcp:send(Socket,
				binary_to_list(erl_msg:write_msg(#msg_move{
					state = 0,
					x = X1,
					y = Y1,
					angle = Angle}))),
			loop(Socket, PlayerInfo, FrameNo);
		#msg_move_ack{
			x = X,
			y = Y,
			angle = Angle} ->
			%io:format("move ack(x=~p, y=~p, angle=~p)~n", [X, Y, Angle]),
			PlayerInfo1 = #player_info{
				user_id = UserId,
				player_id = PlayerId,
				pose = #pose{
					x = X,
					y = Y,
					angle = Angle}},
			loop(Socket, PlayerInfo1, FrameNo);
		#msg_ping_ack{data = Data} ->
			%io:format("ping ack(~p)~n", [data]),
			loop(Socket, PlayerInfo, FrameNo);
		#msg_creature_appear_notif{
			id = PlayerId,
			x = X,
			y = Y,
			angle = Angle} ->
			%io:format("creature appear~n"),
			PlayerInfo1 = #player_info{
				user_id = UserId,
				player_id = PlayerId,
				pose = #pose{
					x = X,
					y = Y,
					angle = Angle}},
			Pid = self(),
			erl_timer:start(250,
				fun() ->
					%io:format("send internal move~n"),
					Pid ! move
				end),
			loop(Socket, PlayerInfo1, FrameNo);
		_ ->
			%io:format("unknown msg~n"),
			loop(Socket, PlayerInfo, FrameNo)
	end.
