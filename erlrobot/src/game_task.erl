%% Author: Jackie
%% Created: 2012-8-16
%% Description: TODO: Add description to game_task
-module(game_task).

%%
%% Include files
%%
-include("records.hrl").
-include("playerInfo.hrl").

%%
%% Exported Functions
%%
-export(
	[
		task0/6,
		task1/6,
		task2/8
	]).

%%%
%%% Imported Functions
%%%
-import(hades_wrap,
	[
		register_user/1,
		my_tasks/1,
		get_game_server/1,
		finish_task/2,
		accept_task/2,
		get_map_table/2,
		jump_to_task/3,
		task_is_over/1
	]).
-import(math_util,
	[
		add_vec2d/2,
		sub_vec2d/2,
		scale_vec2d/2,
		norm_vec2d/1,
		len_squared_vec2d/1,
		len_vec2d/1,
		dist_squared_vec2d/2,
		dist_vec2d/2
	]).

-define(Task0Loc, {-1905.701, 1252.586}).
-define(Task1Loc, { 2021.341, 773.998}).


%%
%% API Functions
%%
task0(ParentPid, GSPid, Context, TaskId, UserId, PlayerId) ->
	do_task0(init, ParentPid, GSPid, Context, TaskId, UserId, PlayerId).

task1(ParentPid, GSPid, Context, TaskId, UserId, PlayerId) ->
	do_task1(init, ParentPid, GSPid, Context, TaskId, UserId, PlayerId).

task2(ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, _Paths} = GameMap) ->
	{TrigId, _} = find_nearest_trigger(PlayerLoc, Triggers),
	{_, {TrigX, TrigY} = TrigLoc} = lists:keyfind(TrigId, 1, Triggers),
	GSPid ! {move, #pose{state = 0, x = TrigX, y = TrigY, angle = 0.0}},
	do_task2({patrol, init, [TrigId], []}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap).

%%
%% Local Functions
%%
do_task0(Mode, ParentPid, GSPid, Context, TaskId, UserId, PlayerId) ->
	receive
		{GSPid, playerPose, #pose{}} ->
			{X, Y} = ?Task0Loc,
			GSPid ! {move, #pose{x = X, y = Y, angle = 0.0}},
			do_task0(wait_move, ParentPid, GSPid, Context, TaskId, UserId, PlayerId);
		{GSPid, #msg_MoveNotif{
			id = PlayerId, x = X, y = Y}} ->
			Dist = dist_vec2d({X, Y}, ?Task0Loc),
			if
				Dist < ?EPSILON ->
					ParentPid ! {self(), finished};
				true -> void
			end
	end.

do_task1(Mode, ParentPid, GSPid, Context, TaskId, UserId, PlayerId) ->
	case Mode of
		init ->
			{X, Y} = ?Task1Loc,
			GSPid ! {move, #pose{x = X, y = Y, angle = 0.0}},
			do_task1(wait_move, ParentPid, GSPid, Context, TaskId, UserId, PlayerId);
		wait_move ->
			receive
				{GSPid, #msg_MoveNotif{
					id = PlayerId, x = X, y = Y}} ->
					Dist = dist_vec2d({X, Y}, ?Task1Loc),
					if
						Dist < ?EPSILON ->
							ParentPid ! {self(), finished};
						true -> void
					end
			end
	end.

do_task2(Mode, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, Paths} = GameMap) ->
	case Mode of
		{patrol, SubMode, [H|T] = TriggerIds, VisitedTriggerIds} ->
			case SubMode of
				init ->
					IsVisited = lists:member(H, VisitedTriggerIds),
					if
						IsVisited ->
							do_task2({patrol, init, T, VisitedTriggerIds}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
						true ->
							{_, {TrigX, TrigY} = _Trig} = lists:keyfind(H, 1, Triggers),
							GSPid ! {move, #pose{state = 0, x = TrigX, y = TrigY, angle = 0.0}},
							%timer:sleep(?MOVE_CD),
							%% wait for arriving the target trigger point
							do_task2({patrol, wait, TriggerIds, VisitedTriggerIds}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
					end;
				wait ->
					receive
						{GSPid, #msg_MoveNotif{
							id = PlayerId, x = X, y = Y}} ->
							PlayerLoc1 = {X, Y},
							{_, {TrigX, TrigY} = TrigLoc} = lists:keyfind(H, 1, Triggers),
							Dist = dist_vec2d(PlayerLoc1, TrigLoc),
							if
								Dist < ?EPSILON ->
									do_task2({patrol, init, T, [H|VisitedTriggerIds]}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc1, GameMap);
								true ->
									do_task2({patrol, wait, TriggerIds, VisitedTriggerIds}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc1, GameMap)
							end;
						{GSPid, {monster, MonId, MonX, MonY}} ->
							do_task2({attack, init, [{MonId, {MonX, MonY}}], MonId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
					end
			end;
		{attack, SubMode, Monsters, TargetMonsterId} ->
			case SubMode of
				init ->
					{_, MonLoc} = lists:keyfind(TargetMonsterId, 1, Monsters),
					{TrigId, _} = find_nearest_trigger(MonLoc, Triggers),
					{LocX, LocY} = get_attack_loc(TrigId, GameMap),
					GSPid ! {move, #pose{x = LocX, y = LocY, angle = 0.0}},
					do_task2({attack, {wait, undefined}, Monsters, TargetMonsterId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
				{wait, AttackPid} ->
					receive
						{GSPid, {monster, MonId, MonX, MonY}} ->
							R = lists:keyfind(MonId, 1, Monsters),
							case R of
								false -> do_task2({attack, {wait, AttackPid}, [{MonId, {MonX, MonY}} | Monsters], TargetMonsterId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
								_ -> do_task2({attack, {wait, AttackPid}, Monsters, TargetMonsterId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
							end;
						{GSPid, {killed, MonId} = Msg} ->
							io:format("monster killed id=~p~n", [MonId]),
							case AttackPid of
								undefined -> void;
								_ -> AttackPid ! {self(), Msg}
							end,
							%% great! killed one enemy!
							%% query the task status to see if the task is over
							{_, Status} = my_tasks(Context),
							IsTaskOver = task_is_over(Status),
							if
								IsTaskOver ->
									ParentPid ! {self(), finished};	% and then quit this loop itself
								true ->
									%% since I have only attacked the TargetMonster, so this Id must be TargetMonster
									Monsters1 = lists:keydelete(MonId, 1, Monsters),

									case Monsters1 of
										[] ->
											%% start to patrol from the nearest trigger point
											{NearTrigId, _} = find_nearest_trigger(PlayerLoc, Triggers),
											do_task2({patrol, init, [NearTrigId], []}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
										[{MonId1, _, _} |_] ->
											io:format("plan to attack another monster id=~p~n", [MonId1]),
											%% plan to attack another monster
											do_task2({attack, init, Monsters1, MonId1}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
									end
							end;
						{GSPid, #msg_MoveNotif{
							id = Id,
							x = CX,
							y = CY}} ->
							CLoc = {CX, CY},
							if
								Id == PlayerId ->
									{_, MonLoc} = lists:keyfind(TargetMonsterId, 1, Monsters),
									{TrigId, _} = find_nearest_trigger(MonLoc, Triggers),
									AttackLoc = get_attack_loc(TrigId, GameMap),
									Dist = dist_vec2d(AttackLoc, CLoc),
									if
										Dist < ?ATTACK_DIST ->
											GSPid ! {msg, #msg_Casting{skillId = ?ATTACK_SKILL, skillSeq = 0, targetId = TargetMonsterId, x = 0.0, y = 0.0}},
											TaskPid = self(),
											AttackPid1 = spawn(
												fun() ->
													attack_monster(TaskPid, GSPid, TargetMonsterId)
												end),
											do_task2({attack, {wait, AttackPid1}, Monsters, TargetMonsterId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
										true ->
											do_task2({attack, {wait, AttackPid}, Monsters, TargetMonsterId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
									end;
								true ->
									Monsters1 = lists:keyreplace(Id, 1, Monsters, {Id, CX, CY}),
									do_task2({attack, {wait, AttackPid}, Monsters1, TargetMonsterId}, ParentPid, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
							end
					end
			end
	end.

attack_monster(TaskPid, GSPid, MonsterId) ->
	receive
		{TaskPid, {killed, MonId}} ->
			void	% quit the loop
	after
		?SKILL_CD ->
			GSPid ! {msg, #msg_Casting{skillId = ?ATTACK_SKILL, skillSeq = 0, targetId = MonsterId, x = 0.0, y = 0.0}},
			attack_monster(TaskPid, GSPid, MonsterId)
	end.

find_nearest_trigger(Loc, Triggers) ->
	Distances = lists:map(
		fun({Id, Trig}) ->
			{Id, dist_squared_vec2d(Loc, Trig)}
		end, Triggers),
	[H | _] = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
	H.

get_attack_loc(TrigId, {Triggers, Paths}) ->
	{_, Trig1} = lists:keyfind(TrigId, 1, Triggers),
	%% find an arbitrary neighbor trigger (there must be at least one) and
	%% move onto the line segment with min(100, segment len) away from the TrigId
	[TrigId2 | _] = get_trigger_neighbor_ids(TrigId, Paths),
	{_, {X2, Y2}} = lists:keyfind(TrigId2, 1, Triggers),
	Trig2 = {X2, Y2},
	SegVec = sub_vec2d(Trig1, Trig2),
	SegLen = len_vec2d(SegVec),
	DistTrig = min(100.0, SegLen),
	SegVec0 = norm_vec2d(SegVec),
	Loc = add_vec2d(Trig1, scale_vec2d(SegVec0, DistTrig)),
	Loc.

get_trigger_neighbor_ids(TriggerId, Paths) ->
	Neighbors = lists:filter(
		fun({Source, Target}) ->
			Source == TriggerId orelse Target == TriggerId
		end, Paths),
	NeighborIds = lists:map(
		fun({Source, Target}) ->
			if
				Source == TriggerId -> Target;
				true -> Source
			end
		end, Neighbors),
	NeighborIds.
