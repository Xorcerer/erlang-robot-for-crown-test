-module(web_cli).
-include("records.hrl").
-include("playerInfo.hrl").
-export([start/0]).
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
-import(lib_misc, [pmap/2, unconsult/2, wait/1]).
-import(game_map, [read_map/1]).
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

start() ->
	ok = inets:start(),
	httpc:set_options(
		[
			{cookies, enabled}
		]),

	N = 1,
	Result = pmap(fun player/1, lists:seq(1, N)),
	inets:stop(),
	io:format("result = ~p~n", [Result]),
	unconsult("players.dat", Result),
	ok.

player(I) ->
	UserName = "ade" ++ integer_to_list(I),
	io:format("username: ~p~n", [UserName]),
	Context = register_user(UserName),
	{_SessionId, SId, _AId, UserId} = Context,
	%io:format("new user id = ~p~n", [UserId]),
	{Tid0, _} = my_tasks(Context),
	io:format("tid = ~p~n", [Tid0]),
	GameServer = get_game_server(Context),
	{Host, Port} = GameServer,
	io:format("got game server location: host = ~p, port = ~p~n", [Host, Port]),
	ParentPid = self(),
	%% todo: kill this process and its inner player process when needed
	Pid0 = spawn(
		fun() ->
			GSPlayerPid = self(),
			io:format("spawning cli:player"),
			cli:player(Host, Port, SId, UserId,
				fun(Msg) ->
					case Msg of
						{playerPose, #pose{
							x = _X,
							y = _Y,
							angle = _Angle}} ->
							io:format("in cli:player"),
							GSPlayerPid ! {move, #pose{
								x = -1905.701, y = 1252.586, angle = 0.105}},
							%% todo: we should have been waiting the player approaches the NPC
							GSPlayerPid ! {task, Tid0, ?TASKSTATE_COMPLETE},
							ParentPid ! {self(), finished};
						quitAck ->
							ParentPid ! {self(), quitAck};
						_ ->
							void
					end
				end)
		end),
	wait({Pid0, finished}),

	{Tid1, _} = finish_task(Context, Tid0),
	ok = accept_task(Context, Tid1),
	Pid0 ! {task, Tid1, ?TASKSTATE_TAKEN},
	Pid0 ! {move, #pose{
		x = 2021.341, y = 773.998, angle = 0.000}},
	Pid0 ! {task, Tid1, ?TASKSTATE_COMPLETE},
	
	%% finish the task number 2, meanwhile get the conditions of the next task.
	{Tid2, _Status2} = finish_task(Context, Tid1),
	ok = accept_task(Context, Tid2),
	GsId2 = get_map_table(Context, Tid2),
	ok = jump_to_task(Context, Tid2, GsId2),
	Pid0 ! quit,
	io:format("wait quitAck~n"),

	wait({Pid0, quitAck}),

	io:format("after quitAck~n"),

	GameServer2 = get_game_server(Context),
	{Host2, Port2} = GameServer2,
	{Triggers, Paths} = read_map("yewai2.grf"),	% todo: fix the hardcoded map name here
	io:format("after read_map~n"),

	%% experiment: only tackle with the task related monsters' appearances
	Pid2 = spawn(
		fun() ->
			_GSPlayerPid = self(),	% it is also Pid2
			io:format("spawning cli:player 2"),
			cli:player(Host2, Port2, SId, UserId,
				fun(Msg) ->
					case Msg of
						{logged, PlayerId} ->	% this must be the first message
							ParentPid ! {self(), {logged, PlayerId}};
						
						%% this must be the second message
						{playerPose, #pose{
							x = _X,
							y = _Y,
							angle = _Angle}} = PlayerPose ->
							io:format("in cli:player 2~n"),
							%% todo: we should have been waiting the player approaches the NPC
							%GSPlayerPid ! {task, Tid2, ?TASKSTATE_COMPLETE},
							ParentPid ! {self(), PlayerPose};
						#msg_CreatureAppearNotif{
							id = Id,
							userId = _,
							x = X,
							y = Y} ->
							io:format("monster appear(userId:~p, id:~p, x:~p, y:~p)~n)", [UserId, Id, X, Y]),
							ParentPid ! {self(), {monster, Id, X, Y}};
						#msg_CreatureDisappearNotif{id = Id} ->
							io:format("monster disappear(id:~p)~n", [Id]),
							ParentPid ! {self(), {killed, Id}};
						quitAck ->
							ParentPid ! {self(), quitAck};
						#msg_MoveNotif{} ->
							ParentPid ! {self(), Msg};
						_ ->
							void
					end
				end)
		end),
	
	PlayerId2 =
		receive
			{Pid2, {logged, PlayerId}} ->
				PlayerId
		end,
	
	%% find the nearest trigger point, return its id
	FindNearestTrigger =
		fun(Loc) ->
			Distances = lists:map(
				fun({Id, Trig}) ->
					{Id, dist_squared_vec2d(Loc, Trig)}
				end, Triggers),
			[H | _] = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
			H
		end,
	
	GetTriggerNeighborIds =
		fun(TriggerId) ->
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
			NeighborIds
		end,
	
	PlayerLoc2 =
		receive
			{Pid2, {playerPose, #pose{x = X, y = Y}}} ->
				{X, Y}
		end,
	PlayerPos2 = FindNearestTrigger(PlayerLoc2),
	{PlayerPosId2, _PlayerPosD2} = PlayerPos2,
	io:format("nearest trigger id: ~p~n", [PlayerPosId2]),

	%% recursive lambda!
	HandleTask2 =
		fun(Mode, PlayerLoc, F) ->
			%io:format("handle task2: mode=~p, playerloc=~p~n", [Mode, PlayerLoc]),
			%% if there is no monster appears, traverse the trigger points
			%% else, attack the first monster
			case Mode of
				%% move toward the target trigger point by sending move msg
				{patrol, true, [H|T] = TriggerStack, VisitedTriggers} ->
					%% if H is a visited trigger, just ignore it
					Visited =
						lists:any(fun(Id) -> Id == H end, VisitedTriggers),
					if
						Visited ->
							F({patrol, true, T, VisitedTriggers}, PlayerLoc, F);
						true ->
							{_, {TrigX, TrigY} = _Trig} = lists:keyfind(H, 1, Triggers),
							Pid2 ! {move, #pose{state = 0, x = TrigX, y = TrigY, angle = 0.0}},
							timer:sleep(?MOVE_CD),
							%% wait for arriving the target trigger point
							F({patrol, false, TriggerStack, VisitedTriggers}, PlayerLoc, F)
					end;
				%% wait for arriving the target trigger point
				{patrol, false, [H|T] = TriggerStack, VisitedTriggers} ->
					receive
						{Pid2, {monster, Id, X1, Y1}} ->
							%% transit mode
							TargetMonster = {Id, X1, Y1},
							%% move toward the monster, prepare to attack
							F({attack, true, [TargetMonster], Id}, PlayerLoc, F);
						{Pid2, {playerPose, #pose{
							x = X1,
							y = Y1,
							angle = _Angle}}} ->
							F({patrol, false, TriggerStack, VisitedTriggers}, {X1, Y1}, F);
						{Pid2, #msg_MoveNotif{
							id = PlayerId2,
							x = X1,
							y = Y1}} ->
							PlayerLoc1 = {X1, Y1},
							%% if player has arrived the trigger H, then move to the next pos
							%% otherwise, move to the trigger H
							{_, Trig} = lists:keyfind(H, 1, Triggers),
							DistH = dist_squared_vec2d(PlayerLoc1, Trig),
							if
								DistH < ?EPSILON ->
									VisitedTriggers1 = [H | VisitedTriggers],
									NeighborIds = GetTriggerNeighborIds(H),
									TriggerStack1 = lists:append(NeighborIds, T),
									F({patrol, true, TriggerStack1, VisitedTriggers1}, PlayerLoc1, F);
								true ->
									F({patrol, false, TriggerStack, VisitedTriggers}, PlayerLoc1, F)
							end;	% if
						_ ->
							F({patrol, false, TriggerStack, VisitedTriggers}, PlayerLoc, F)
					end;	% receive
				%% move toward the monster, prepare to attack
				{attack, true, Monsters, TargetMonsterId} ->
					%% if approach the target monster enough, then start attacking,
					%% else, approach it, get ready for attacking
					{_, TargetX, TargetY} = lists:keyfind(TargetMonsterId, 1, Monsters),
					{TrigId, _} = FindNearestTrigger({TargetX, TargetY}),
					{_, Trig1} = lists:keyfind(TrigId, 1, Triggers),
					%% find an arbitrary neighbor trigger (there must be at least one) and
					%% move onto the line segment with min(100, segment len) away from the TrigId
					[TrigId2 | _] = GetTriggerNeighborIds(TrigId),
					{_, {X2, Y2}} = lists:keyfind(TrigId2, 1, Triggers),
					Trig2 = {X2, Y2},
					SegVec = sub_vec2d(Trig1, Trig2),
					SegLen = len_vec2d(SegVec),
					DistTrig = min(100.0, SegLen),
					SegVec0 = norm_vec2d(SegVec),
					{LocX, LocY} = add_vec2d(Trig1, scale_vec2d(SegVec0, DistTrig)),
					Pid2 ! {move, #pose{x = LocX, y = LocY, angle = 0.0}},
					F({attack, false, Monsters, TargetMonsterId}, PlayerLoc, F);
				%% wait for approaching the target monster enough, then begin attacking
				{attack, false, Monsters, TargetMonsterId} ->
					{TargetId, TargetX, TargetY} = lists:keyfind(TargetMonsterId, 1, Monsters),
					DistMonster = dist_vec2d(PlayerLoc, {TargetX, TargetY}),
					if
						DistMonster < ?ATTACK_DIST ->
							Pid2 ! {msg, #msg_Casting{skillId = 4, skillSeq = 0, targetId = TargetId, x = 0.0, y = 0.0}},
							io:format("attack targetId=~p~n", [TargetId]),
							%% after cooldown, give it another attack
							timer:sleep(?SKILL_CD);
						true -> void
					end,	% if

					receive
						{Pid2, {monster, Id, MonX, MonY}} ->
							%% avoid duplicate monster
							R = lists:keyfind(Id, 1, Monsters),
							case R of
								false -> F({attack, false, [{Id, MonX, MonY} | Monsters], TargetMonsterId}, PlayerLoc, F);
								_ -> F({attack, false, Monsters, TargetMonsterId}, PlayerLoc, F)
							end;
						{Pid2, {killed, Id}} ->
							io:format("monster killed id=~p~n", [Id]),
							%% great! killed one enemy!
							%% query the task status to see if the task is over
							{_, Status} = my_tasks(Context),
							TaskOver = task_is_over(Status),
							if
								TaskOver ->
									Pid2 ! quit;	% and then quit this loop itself
								true ->
									%% since I have only attacked the TargetMonster, so this Id must be TargetMonster
									Monsters1 = lists:keydelete(Id, 1, Monsters),

									case Monsters1 of
										[] ->
											%% start to patrol from the nearest trigger point
											{NearTrigId, _} = FindNearestTrigger(PlayerLoc),
											F({patrol, true, [NearTrigId], []}, PlayerLoc, F);
										[{MonId, _, _} |_] ->
											io:format("plan to attack another monster id=~p~n", [MonId]),
											%% plan to attack another monster
											F({attack, true, Monsters1, MonId}, PlayerLoc, F)
									end
							end;
						{Pid2, #msg_MoveNotif{
							id = Id,
							x = CX,
							y = CY}} ->
							CLoc = {CX, CY},
							if
								Id == PlayerId2 ->
									F({attack, false, Monsters, TargetMonsterId}, CLoc, F);
								true ->
									Monsters1 = lists:keyreplace(Id, 1, Monsters, {Id, CX, CY}),
									F({attack, false, Monsters1, TargetMonsterId}, CLoc, F)
							end;
						_ ->
							F({attack, false, Monsters, TargetMonsterId}, PlayerLoc, F)
					end	% receive
			end	% case Mode
		end,	% fun
	HandleTask2({patrol, true, [PlayerPosId2], []}, PlayerPos2, HandleTask2),
	Context.


%% GET /account/logout
