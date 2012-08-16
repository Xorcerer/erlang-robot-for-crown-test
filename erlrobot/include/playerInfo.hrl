
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

-define(EPSILON, 1.0e-5).

-define(NEAR_DIST, 10.0).
-define(ATTACK_DIST, 100.0).
-define(ATTACK_SKILL, 4).	% toxiphily
-define(SKILL_CD, 500).
-define(MOVE_CD, 2000).	% rest a period of time after a move request

-define(TASKSTATE_UNTAKE, 0).
-define(TASKSTATE_CANTAKE, 1).
-define(TASKSTATE_TAKEN, 2).
-define(TASKSTATE_COMMITABLE, 3).
-define(TASKSTATE_COMPLETE, 4).

% todo: need a spanning tree including the NPCs
-define(NPC_MAYOR, 0).
-define(NPC_BLACKSMITH, 1).
-define(NPC_SERGEANT, 2).
-define(NPC_MANAGER, 3).
-define(NPC_MAGICIAN, 4).
-define(NPC_BOSS, 5).

-define(NPC_LOCATIONS,
	[
		{2240.0, 819.0},
		{-2456.0, -613.0},
		{-3718.0, -63.0},
		{-1287.0, -1134.0},
		{-288.0, 904.0},
		{-2076.0, 1400.0}
	]).

