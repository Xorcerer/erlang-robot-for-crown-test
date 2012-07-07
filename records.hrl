
-define(MSG_Login, 2000).
-define(MSG_LoginAck, 2001).
-define(MSG_Ping, 2003).
-define(MSG_PingAck, 2004).
-define(MSG_Move, 2010).
-define(MSG_MoveAck, 2012).
-define(MSG_CreatureAppearNotif, 2030).

-record(msg_login, {
	session_key,
	user_id,
	table_id,
	major,
	minor,
	revision
	}).

-record(msg_login_ack, {
	err_code,
	id
	}).

-record(msg_ping, {
	data
	}).

-record(msg_ping_ack, {
	data
	}).

-record(msg_move, {
	state,
	x,
	y,
	angle
	}).

-record(msg_move_ack, {
	x,
	y,
	angle
	}).

-record(msg_creature_appear_notif, {
	type,
	career,
	gender,
	name,
	id,
	user_id,
	x,
	y,
	angle,
	radius,
	movement_speed,
	health,
	hp,
	mana,
	mp,
	camp_id,
	force_id,
	attackable,
	talkable,
	monster_class,
	table_id,
	state,
	level,
	is_building
	}).

-record(player_info, {
	user_id,
	player_id,
	pose
	}).

-record(pose, {
	x,
	y,
	angle
	}).
