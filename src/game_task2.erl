%% Author: Jackie
%% Created: 2012-8-20
%% Description: TODO: Add description to game_task2
-module(game_task2).

%%
%% Include files
%%
-include("records.hrl").
-include("playerInfo.hrl").

%%
%% Exported Functions
%%
-export([task2/7]).

%%
%% API Functions
%%
-import(hades_wrap,
	[
		my_tasks/1,
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

task2(GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, _Paths} = GameMap) ->
	{_UserNo, _SessionId, _SId, _AId, UserId} = Context,
	%{TrigId, _} = find_nearest_trigger(PlayerLoc, Triggers),
	%{_, {TrigX, TrigY} = _TrigLoc} = lists:keyfind(TrigId, 1, Triggers),
	N = random:uniform(length(Triggers)),
	{TrigId, {TrigX, TrigY}} = lists:nth(N, Triggers),
	GSPid ! {move, #pose{state = 0, x = TrigX, y = TrigY, angle = 0.0}},
	task2({patrol, init, [TrigId], []}, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap).

%%
%% Local Functions
%%
task2({patrol, init, TriggerIds, VisitedTriggerIds}, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, Paths} = GameMap) ->
	{_UserNo, _SessionId, _SId, _AId, UserId} = Context,
	[TrigId | Ts] = TriggerIds,
	IsVisited = lists:member(TrigId, VisitedTriggerIds),
	if
		IsVisited ->
			task2({patrol, init, Ts, VisitedTriggerIds}, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
		true ->
			{_, {_TrigX, _TrigY} = Trig} = lists:keyfind(TrigId, 1, Triggers),
			io:format("~p: do_task2 send move x=~p, y=~p~n", [_UserNo, _TrigX, _TrigY]),
			{X, Y} = offset(_UserNo, Trig),
			GSPid ! {move, #pose{state = 0, x = X, y = Y, angle = 0.0}},

			%% pop H, push H's children
			NeighborIds = get_trigger_neighbor_ids(TrigId, Paths),
			TriggerIds1 = lists:append(NeighborIds, Ts),

			%% wait for arriving the target trigger point
			task2({patrol, wait, TriggerIds1, [TrigId | VisitedTriggerIds]},
				GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
	end;

task2({patrol, wait, TriggerIds, VisitedTriggerIds}, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, _Paths} = GameMap) ->
	{_UserNo, _SessionId, _SId, _AId, UserId} = Context,
	receive
		{GSPid, #msg_MoveNotif{
			id = PlayerId, x = X, y = Y}} ->
			[VTrigId | _Vs] = VisitedTriggerIds,
			PlayerLoc1 = {X, Y},
			{_, TrigLoc} = lists:keyfind(VTrigId, 1, Triggers),
			Loc2 = offset(_UserNo, TrigLoc),
			Dist = dist_vec2d(PlayerLoc1, Loc2),
			if
				Dist < ?NEAR_DIST ->
					timer:sleep(?MOVE_CD),
					io:format("~p: do_task2 arrived x=~p, y=~p~n", [_UserNo, X, Y]),
					task2({patrol, init, TriggerIds, VisitedTriggerIds},
						GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc1, GameMap);
				true ->
					{Loc2X, Loc2Y} = Loc2,
					GSPid ! {move, #pose{state = 0, x = Loc2X, y = Loc2Y, angle = 0.0}},
					task2({patrol, wait, TriggerIds, VisitedTriggerIds},
						GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc1, GameMap)
			end;
		{GSPid, #msg_CreatureAppearNotif{
			id = MonId,
			userId = _MonUserId,
			x = MonX,
			y = MonY}} ->
			io:format("~p: do_task2: discover a monster, monId=~p, x=~p, y=~p~n", [_UserNo, MonId, MonX, MonY]),
			task2({attack, init, [{MonId, {MonX, MonY}}], MonId},
				GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
	after 2000 ->
		%% timeout then resend move instruction
		io:format("~p: do_task2 patrol failed moving, retry~n", [_UserNo]),
		[VTrigId | _Vs] = VisitedTriggerIds,
		{_, TrigLoc} = lists:keyfind(VTrigId, 1, Triggers),
		Loc2 = offset(_UserNo, TrigLoc),
		{Loc2X, Loc2Y} = Loc2,
		GSPid ! {move, #pose{state = 0, x = Loc2X, y = Loc2Y, angle = 0.0}},
		task2({patrol, wait, TriggerIds, VisitedTriggerIds},
			GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
	end;

task2({attack, init, Monsters, TargetMonsterId}, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, _Paths} = GameMap) ->
	{_UserNo, _SessionId, _SId, _AId, UserId} = Context,
	%io:format("do_task2: attack init monsters=~p, target=~p~n", [Monsters, TargetMonsterId]),
	{_, MonLoc} = lists:keyfind(TargetMonsterId, 1, Monsters),
	{TrigId, _} = find_nearest_trigger(MonLoc, Triggers),
	AttackLoc = get_attack_loc(_UserNo, PlayerLoc, TrigId, GameMap),
	{LocX, LocY} = AttackLoc,
	io:format("~p: do_task2 send attack move x=~p, y=~p~n", [_UserNo, LocX, LocY]),
	GSPid ! {move, #pose{x = LocX, y = LocY, angle = 0.0}},
	task2({attack, {wait, undefined}, Monsters, TargetMonsterId, AttackLoc},
		GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);

task2({attack, {wait, AttackPid}, Monsters, TargetMonsterId, AttackLoc}, GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, {Triggers, _Paths} = GameMap) ->
	{_UserNo, _SessionId, _SId, _AId, UserId} = Context,
	receive
		{GSPid, #msg_CreatureAppearNotif{
			id = MonId,
			userId = _MonUserId,
			x = MonX,
			y = MonY}} ->
			%% don't change the original attack aim
			io:format("~p: do_task2: discover a monster when waiting attack, monId=~p, x=~p, y=~p~n", [_UserNo, MonId, MonX, MonY]),
			R = lists:keyfind(MonId, 1, Monsters),
			case R of
				false ->
					io:format("~p: do_task2: this monster(~p) is new~n", [_UserNo, MonId]),
					task2({attack, {wait, AttackPid}, [{MonId, {MonX, MonY}} | Monsters], TargetMonsterId, AttackLoc},
						GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
				_ ->
					task2({attack, {wait, AttackPid}, Monsters, TargetMonsterId, AttackLoc},
						GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
			end;
		{GSPid, #msg_CreatureDisappearNotif{id = MonId}} = _Msg ->
			io:format("~p: monster killed id=~p~n", [_UserNo, MonId]),
			case AttackPid of
				undefined -> void;
				_ ->
					if
						MonId == TargetMonsterId ->
							io:format("~p: do_task2 notify its timer(~p) monster(~p) killed~n", [_UserNo, AttackPid, MonId]),
							AttackPid ! {self(), {killed, MonId}};
						true -> void
					end
			end,
			%% perhaps count of enemy is decrimented!
			%% query the task status to see if the task is over
			{_, Status} = my_tasks(Context),
			IsTaskOver = task_is_over(Status),
			if
				IsTaskOver ->
					%io:format("do_task2 task is over~n"),
					%% notify attack monster process to quit regardless of if the killed monster is the target or not
					AttackPid ! {self(), {killed, MonId}},
					void;	% and then quit this loop itself
				true ->
					%% since I have only attacked the target monster, so this Id is supposed to be target monster
					Monsters1 = lists:keydelete(MonId, 1, Monsters),

					case Monsters1 of
						[] ->
							%io:format("do_task2 no monsters at the moment~n"),
							%% start to patrol from the nearest trigger point
							{NearTrigId, _} = find_nearest_trigger(PlayerLoc, Triggers),
							task2({patrol, init, [NearTrigId], []},
								GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
						_ ->
							%% plan to attack another monster
							%% let's choose the nearest one
							{TargetMonsterId1, _} = find_nearest_monster(PlayerLoc, Monsters1),
							io:format("~p: plan to attack another monster id=~p~n", [_UserNo, TargetMonsterId1]),
							task2({attack, init, Monsters1, TargetMonsterId1},
								GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
					end
			end;
		{GSPid, #msg_MoveNotif{
			id = Id,
			x = CX,
			y = CY}} ->
			CLoc = {CX, CY},
			if
				Id == PlayerId ->
					Dist = dist_vec2d(AttackLoc, CLoc),
					if
						Dist < ?NEAR_DIST andalso AttackPid == undefined ->
							io:format("~p: do_task2 in attack arrived loc=~p~n", [_UserNo, CLoc]),
							GSPid ! {msg, #msg_Casting{skillId = ?ATTACK_SKILL, skillSeq = 0, targetId = TargetMonsterId, x = 0.0, y = 0.0}},
							TaskPid = self(),
							AttackPid1 = spawn(
								fun() ->
									monitor(process, GSPid),
									attack_monster(TaskPid, _UserNo, GSPid, TargetMonsterId)
								end),
							task2({attack, {wait, AttackPid1}, Monsters, TargetMonsterId, AttackLoc},
								GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
						true ->
							io:format("~p: do_task2 in attack not near enough, or attck pid is ~p, dest=~p, player loc=~p~n", [_UserNo, AttackPid, AttackLoc, CLoc]),
							{AttackLocX, AttackLocY} = AttackLoc,
							GSPid ! {move, #pose{x = AttackLocX, y = AttackLocY, angle = 0.0}},
							task2({attack, {wait, AttackPid}, Monsters, TargetMonsterId, AttackLoc},
								GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
					end;
				true ->
					Monsters1 = lists:keyreplace(Id, 1, Monsters, {Id, {CX, CY}}),
					if
						Id == TargetMonsterId ->
							%% if the target monster moves, then re-aim it.
							task2({attack, init, Monsters1, TargetMonsterId},
								GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap);
						true ->
							%% if another monster moves, continue wait for arrive the original attack loc
							task2({attack, {wait, AttackPid}, Monsters1, TargetMonsterId, AttackLoc},
								GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
					end
			end
	after 2000 ->
		%% received this message denotes failure
		io:format("~p: do_task2 failed moving, retry~n", [_UserNo]),
		{AttackLocX, AttackLocY} = AttackLoc,
		GSPid ! {move, #pose{x = AttackLocX, y = AttackLocY, angle = 0.0}},
		task2({attack, {wait, AttackPid}, Monsters, TargetMonsterId, AttackLoc},
			GSPid, Context, TaskId, UserId, PlayerId, PlayerLoc, GameMap)
	end.

attack_monster(TaskPid, _UserNo, GSPid, MonsterId) ->
	receive
		{TaskPid, {killed, _MonId}} ->
			io:format("~p: attack_monster received monster(~p) killed! killing(~p)~n", [_UserNo, _MonId, MonsterId]),
			void;	% quit the loop
		Msg ->
			io:format("~p: attack_monster received msg: ~p~n", [_UserNo, Msg]),
			void
	after ?SKILL_CD ->
			io:format("~p: gspid: ~p, alive?~p, attack_monster send casting to monster(~p)~n", [_UserNo, GSPid, is_process_alive(GSPid), MonsterId]),
			GSPid ! {msg, #msg_Casting{skillId = ?ATTACK_SKILL, skillSeq = 0, targetId = MonsterId, x = 0.0, y = 0.0}},
			attack_monster(TaskPid, _UserNo, GSPid, MonsterId)
	end.

find_nearest_trigger(Loc, Triggers) ->
	Distances = lists:map(
		fun({Id, Trig}) ->
			{Id, dist_squared_vec2d(Loc, Trig)}
		end, Triggers),
	[H | _] = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
	H.

find_nearest_trigger(Loc, TriggerIds, Triggers) ->
	FilteredTriggers = lists:filter(
		fun({Id, _} = _Trig) ->
			lists:member(Id, TriggerIds)
		end, Triggers),
	Distances = lists:map(
		fun({Id, Trig}) ->
			{Id, dist_squared_vec2d(Loc, Trig)}
		end, FilteredTriggers),
	[H | _] = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
	H.

find_nearest_monster(Loc, Monsters) ->
	Distances = lists:map(
		fun({Id, MonLoc}) ->
			{Id, dist_squared_vec2d(Loc, MonLoc)}
		end, Monsters),
	[H | _] = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
	H.

%% find the nearest one among the neighbor triggers
%% that point must be available for destination of moving
get_attack_loc(_UserNo, PlayerLoc, TrigId, {Triggers, Paths}) ->
	{_, Trig1} = lists:keyfind(TrigId, 1, Triggers),

	%% find the neighbor trigger which is nearest to the player (there must be at least one) and
	%% move onto the line segment with min(100, segment len) away from the TrigId
	NeighborIds = get_trigger_neighbor_ids(TrigId, Paths),
	{TrigId2, _} = find_nearest_trigger(PlayerLoc, NeighborIds, Triggers),
	{_, Trig2} = lists:keyfind(TrigId2, 1, Triggers),
	SegVec = sub_vec2d(Trig1, Trig2),
	SegLen = len_vec2d(SegVec),
	DistTrig = min(?ATTACK_DIST, SegLen),
	SegVec0 = norm_vec2d(SegVec),
	Loc = add_vec2d(Trig1, scale_vec2d(SegVec0, DistTrig)),
	Loc1 = offset(_UserNo, Loc),
	Loc1.

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

offset(UserNo, Loc) ->
	Theta = math:pi() * UserNo / 6,
	Len = 50.0,
	OffsetVec = {Len * math:cos(Theta), Len * math:sin(Theta)},
	add_vec2d(Loc, OffsetVec).
