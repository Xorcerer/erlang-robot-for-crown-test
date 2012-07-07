-module(msg).
-compile(export_all).
-include("records.hrl").
-import(msg_reader, [read_str/1, read_byte/1, read_short/1, read_int/1, read_int1/1, read_single/1]).
-import(msg_writer, [write_str/2, write_byte/2, write_short/2, write_int/2, write_single/2]).

write_msg(Msg) ->
	{ MsgId, Buff } =
		case Msg of
			#msg_login{
				session_key = SessionKey,
				user_id = UserId,
				table_id = TableId,
				major = Major,
				minor = Minor,
				revision = Revision } ->
				{
					?MSG_Login,
					list_to_binary([
						msg_writer:write_str(SessionKey),
						msg_writer:write_int(UserId),
						msg_writer:write_int(TableId),
						msg_writer:write_int(Major),
						msg_writer:write_int(Minor),
						msg_writer:write_int(Revision)])
				};
			#msg_login_ack{
				err_code = ErrCode,
				id = Id } ->
				{
					?MSG_LoginAck,
					list_to_binary([
						msg_writer:write_int(ErrCode),
						msg_writer:write_int(Id)])
				};
			#msg_ping{data = Data} ->
				{
					?MSG_Ping,
					msg_writer:write_int(Data)
				};
			#msg_ping_ack{data = Data} ->
				{
					?MSG_PingAck,
					msg_writer:write_int(Data)
				};
			#msg_move{
				state = State,
				x = X,
				y = Y,
				angle = Angle } ->
				{
					?MSG_Move,
					list_to_binary([
						msg_writer:write_int(State),
						msg_writer:write_single(X),
						msg_writer:write_single(Y),
						msg_writer:write_single(Angle)])
				};
			#msg_move_ack{
				x = X,
				y = Y,
				angle = Angle } ->
				{
					?MSG_MoveAck,
					list_to_binary([
						msg_writer:write_single(X),
						msg_writer:write_single(Y),
						msg_writer:write_single(Angle)])
				}
		end,
	list_to_binary([
		msg_writer:write_int(MsgId),
		msg_writer:write_int(size(Buff)),
		Buff]).

read_msg(Buff, MsgId) ->
	%io:format("in read_msg, msgid=~p~n", [MsgId]),
	case MsgId of
		?MSG_Login ->
			{ SessionKey, Buff1 } = msg_reader:read_str(Buff),
			{ UserId, Buff2 } = msg_reader:read_int(Buff1),
			{ TableId, Buff3 } = msg_reader:read_int(Buff2),
			{ Major, Buff4 } = msg_reader:read_int(Buff3),
			{ Minor, Buff5 } = msg_reader:read_int(Buff4),
			{ Revision, _ } = msg_reader:read_int(Buff5),
			#msg_login{
				session_key = SessionKey,
				user_id = UserId,
				table_id = TableId,
				major = Major,
				minor = Minor,
				revision = Revision };
		?MSG_LoginAck ->
			%io:format("receiving loginack, buff=~p~n", [Buff]),
			{ ErrCode, Buff1 } = msg_reader:read_int(Buff),
			{ Id, _ } = msg_reader:read_int(Buff1),
			#msg_login_ack{
				err_code = ErrCode,
				id = Id };
		?MSG_Ping ->
			{ Data, _ } = msg_reader:read_int(Buff),
			#msg_ping{data = Data};
		?MSG_PingAck ->
			{ Data, _ } = msg_reader:read_int(Buff),
			#msg_ping_ack{data = Data};
		?MSG_Move ->
			{ State, Buff1 } = msg_reader:read_int(Buff),
			{ X, Buff2 } = msg_reader:read_single(Buff1),
			{ Y, Buff3 } = msg_reader:read_single(Buff2),
			{ Angle, _ } = msg_reader:read_single(Buff3),
			#msg_move{
				state = State,
				x = X,
				y = Y,
				angle = Angle };
		?MSG_MoveAck ->
			{ X, Buff1 } = msg_reader:read_single(Buff),
			{ Y, Buff2 } = msg_reader:read_single(Buff1),
			{ Angle, _ } = msg_reader:read_single(Buff2),
			#msg_move_ack{
				x = X,
				y = Y,
				angle = Angle };
		?MSG_CreatureAppearNotif ->
			%io:format("in receiving creature app, buff=~p~n", [Buff]),
			{ Type, Buff1 } = msg_reader:read_int(Buff),
			%io:format("received type:~p~n", [Type]),
			{ Career, Buff2 } = msg_reader:read_int(Buff1),
			%io:format("received career~n"),
			{ Gender, Buff3 } = msg_reader:read_byte(Buff2),
			%io:format("received gender~n"),
			{ Name, Buff4 } = msg_reader:read_str(Buff3),
			%io:format("received name~n"),
			{ Id, Buff5 } = msg_reader:read_int(Buff4),
			{ UserId, Buff6 } = msg_reader:read_int(Buff5),
			{ X, Buff7 } = msg_reader:read_single(Buff6),
			{ Y, Buff8 } = msg_reader:read_single(Buff7),
			{ Angle, Buff9 } = msg_reader:read_single(Buff8),
			%io:format("received x,y,angle~n"),
			{ Radius, Buff10 } = msg_reader:read_single(Buff9),
			{ MovementSpeed, Buff11 } = msg_reader:read_single(Buff10),
			{ Health, Buff12 } = msg_reader:read_single(Buff11),
			{ Hp, Buff13 } = msg_reader:read_single(Buff12),
			{ Mana, Buff14 } = msg_reader:read_single(Buff13),
			{ Mp, Buff15 } = msg_reader:read_single(Buff14),
			{ CampId, Buff16 } = msg_reader:read_byte(Buff15),
			{ ForceId, Buff17 } = msg_reader:read_byte(Buff16),
			{ Attackable, Buff18 } = msg_reader:read_byte(Buff17),
			{ Talkable, Buff19 } = msg_reader:read_byte(Buff18),
			{ MonsterClass, Buff20 } = msg_reader:read_int(Buff19),
			{ TableId, Buff21 } = msg_reader:read_int(Buff20),
			{ State, Buff22 } = msg_reader:read_int(Buff21),
			{ Level, Buff23 } = msg_reader:read_int(Buff22),
			{ IsBuilding, _ } = msg_reader:read_byte(Buff23),
			#msg_creature_appear_notif{
				type = Type,
				career = Career,
				gender = Gender,
				name = Name,
				id = Id,
				user_id = UserId,
				x = X,
				y = Y,
				angle = Angle,
				radius = Radius,
				movement_speed = MovementSpeed,
				health = Health,
				hp = Hp,
				mana = Mana,
				mp = Mp,
				camp_id = CampId,
				force_id = ForceId,
				attackable = Attackable,
				talkable = Talkable,
				monster_class = MonsterClass,
				table_id = TableId,
				state = State,
				level = Level,
				is_building = IsBuilding };
		_ ->
			%io:format("receiving unknown msg~n"),
			void
	end.
