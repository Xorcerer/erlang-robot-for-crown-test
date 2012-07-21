
-record(playerInfo, {
	userId,
	playerId,
	pose
	}).

-record(pose, {
	state,
	x,
	y,
	angle
	}).

-define(TASKSTATE_UNTAKE, 0).
-define(TASKSTATE_CANTAKE, 1).
-define(TASKSTATE_TAKEN, 2).
-define(TASKSTATE_COMMITABLE, 3).
-define(TASKSTATE_COMPLETE, 4).
