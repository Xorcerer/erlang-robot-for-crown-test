-module(client).
-export([start/0]).
-include("../include/records.hrl").
-include("../include/playerInfo.hrl").
-import(msg, [write_msg/1, read_msg/2]).
-import(timer, [start/2]).
-import(flags, [extract_str/2, extract_int/2]).

start() ->
	N = flags:extract_int(count, 100),
	[ spawn(fun player/0) || _ <- lists:seq(1, N) ].

player() ->
	Host = flags:extract_str(host, "localhost"),
	Port = flags:extract_int(port, 9527),
    player(Host, Port).

player(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port,
		[binary, {packet, 0}, {nodelay, true}, {active, false}]),
	UserId = flags:extract_int(uid, 29921),
    ok = gen_tcp:send(Socket,
		binary_to_list(msg:write_msg(#msg_Login{
			sessionKey = "HELLO",
			userId = UserId,
			tableId = -1,
			major = 0,
			minor = 8,
			revision = 0 }))),
	{ok, <<?MSG_LoginAck:32, MsgLen:32>>} = gen_tcp:recv(Socket, 8),
	{ok, Buff} = gen_tcp:recv(Socket, MsgLen),
	#msg_LoginAck{errCode = 0, id = PlayerId} = msg:read_msg(Buff, ?MSG_LoginAck),
	Pid = self(),
	timer:start(3000,
		fun() ->
			%io:format("send internal ping~n"),
			Pid ! ping
		end),
	spawn(
		fun() ->
			socket_loop(Pid, Socket)
		end),
	%io:format("start loop~n"),
	loop(Socket, #playerInfo{
		userId = UserId,
		playerId = PlayerId }, 0).

socket_loop(P, Socket) ->
	%io:format("enter socket_loop~n"),
	{ok, <<MsgId:32, MsgLen:32>>} = gen_tcp:recv(Socket, 8),
	%io:format("in socket_loop, msgid=~p, len=~p~n", [MsgId, MsgLen]),
	{ok, Buff} = gen_tcp:recv(Socket, MsgLen),
	%io:format("in socket_loop, buff received~n"),
	Msg = msg:read_msg(Buff, MsgId),
	%io:format("in socket_loop, msg read:~p~n", [Msg]),
	P ! Msg,
	socket_loop(P, Socket).

loop(Socket, PlayerInfo, FrameNo) ->
	%io:format("in main loop~n"),
	#playerInfo{
		userId = UserId,
		playerId = PlayerId,
		pose = Pose } = PlayerInfo,
	receive
		ping ->
			%io:format("in ping~n"),
			ok = gen_tcp:send(Socket,
				binary_to_list(msg:write_msg(#msg_Ping{data = FrameNo}))),
			loop(Socket, PlayerInfo, FrameNo + 1);
		move ->
			%io:format("in move~n"),
			#pose{
				x = X,
				y = Y,
				angle = Angle} = Pose,

			Dist = random:uniform() * 250.0,
			DAngle = (math:pow(random:uniform(), 0.5) - 0.5) * math:pi(),
			Angle1 = Angle + DAngle,
			X1 = X + Dist * math:cos(Angle1),
			Y1 = Y + Dist * math:sin(Angle1),

			ok = gen_tcp:send(Socket,
				binary_to_list(msg:write_msg(#msg_Move{
					state = 0,
					x = X1,
					y = Y1,
					angle = Angle1}))),
			loop(Socket, PlayerInfo, FrameNo);
		#msg_MoveNotif{
			id = PlayerId,
			x = X,
			y = Y,
			angle = Angle} ->
			%io:format("move notif(x=~p, y=~p, angle=~p)~n", [X, Y, Angle]),
			PlayerInfo1 = #playerInfo{
				userId = UserId,
				playerId = PlayerId,
				pose = #pose{
					x = X,
					y = Y,
					angle = Angle}},
			loop(Socket, PlayerInfo1, FrameNo);
		#msg_PingAck{data = _Data} ->
			%io:format("ping ack(~p)~n", [_Data]),
			loop(Socket, PlayerInfo, FrameNo);
		#msg_CreatureAppearNotif{
			id = PlayerId,
			x = X,
			y = Y,
			angle = Angle} ->
			%io:format("creature appear~n"),
			PlayerInfo1 = #playerInfo{
				userId = UserId,
				playerId = PlayerId,
				pose = #pose{
					x = X,
					y = Y,
					angle = Angle}},
			Pid = self(),
			timer:start(250,
				fun() ->
					%io:format("send internal move~n"),
					Pid ! move
				end),
			loop(Socket, PlayerInfo1, FrameNo);
		_ ->
			%io:format("unknown msg~n"),
			loop(Socket, PlayerInfo, FrameNo)
	end.
