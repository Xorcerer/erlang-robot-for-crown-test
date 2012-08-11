-module(cli).
-compile(export_all).
-include("../include/records.hrl").
-include("../include/playerInfo.hrl").
-import(msg, [write_msg/1, read_msg/2]).
-import(flags, [extract_str/2, extract_int/2]).
-import(lib_misc, [timer/2]).

start() ->
	N = flags:extract_int(count, 100),
	[ spawn(fun player/0) || _ <- lists:seq(1, N) ].

player() ->
	Host = flags:extract_str(host, "localhost"),
	Port = flags:extract_int(port, 9527),
	UserId = flags:extract_int(uid, 29921),
	Self = self(),
    player(Host, Port, UserId,
		fun(_Msg) ->
			spawn(
				fun() ->
					timer(250,
						fun() ->
							%io:format("send internal move~n"),
							Self ! {move, random}
						end
					)
				end
			)
		end).

player(Host, Port, UserId, OnKnowSelfPos) ->
    {ok, Socket} = gen_tcp:connect(Host, Port,
		[binary, {packet, 0}, {nodelay, true}, {active, false}]),
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
	Self = self(),
	spawn(
		fun() ->
			timer(3000,
				fun() ->
					%io:format("send internal ping~n"),
					Self ! ping
				end)
			end
		),
	spawn(
		fun() ->
			socket_loop(Self, Socket)
		end),
	%io:format("start loop~n"),
	loop(
		Socket,
		#playerInfo{
			userId = UserId,
			playerId = PlayerId },
		0,
		OnKnowSelfPos).

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

loop(Socket, PlayerInfo, FrameNo, OnKnowSelfPos) ->
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
			loop(Socket, PlayerInfo, FrameNo + 1, OnKnowSelfPos);
		{move, random} ->
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
			self() ! {move, #pose{x = X1, y = Y1, angle = Angle1}},
			loop(Socket, PlayerInfo, FrameNo, OnKnowSelfPos);
		{move, #pose{
				x = X,
				y = Y,
				angle = Angle}} ->
			%io:format("in move pose~n"),
			ok = gen_tcp:send(Socket,
				binary_to_list(msg:write_msg(#msg_Move{
					state = 0,
					x = X,
					y = Y,
					angle = Angle}))),
			loop(Socket, PlayerInfo, FrameNo, OnKnowSelfPos);
		#msg_MoveNotif{
			id = PlayerId,
			x = X,
			y = Y,
			angle = Angle} ->
			%io:format("move notif(x=~p, y=~p, angle=~p)~n", [X, Y, Angle]),
			PlayerInfo1 = PlayerInfo#playerInfo{
				pose = #pose{
					x = X,
					y = Y,
					angle = Angle}},
			loop(Socket, PlayerInfo1, FrameNo, OnKnowSelfPos);
		#msg_PingAck{data = _Data} ->
			%io:format("ping ack(~p)~n", [_Data]),
			loop(Socket, PlayerInfo, FrameNo, OnKnowSelfPos);
		#msg_CreatureAppearNotif{
			id = PlayerId,
			x = X,
			y = Y,
			angle = Angle} = Msg ->
			%io:format("creature appear~n"),
			PlayerInfo1 = PlayerInfo#playerInfo{
				pose = #pose{
					x = X,
					y = Y,
					angle = Angle}},
			OnKnowSelfPos(Msg),
			loop(Socket, PlayerInfo1, FrameNo, OnKnowSelfPos);
		{task, TaskId, TaskState} ->
			ok = gen_tcp:send(Socket,
				binary_to_list(msg:write_msg(#msg_Task{
					taskId = TaskId, taskState = TaskState}))),
			loop(Socket, PlayerInfo, FrameNo, OnKnowSelfPos);
		_ ->
			%io:format("unknown msg~n"),
			loop(Socket, PlayerInfo, FrameNo, OnKnowSelfPos)
	end.
