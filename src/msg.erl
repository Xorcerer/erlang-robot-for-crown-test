-module(msg).
-compile(export_all).
-include("records.hrl").
-import(msg_reader, [read_str/1, read_byte/1, read_short/1, read_int/1, read_int1/1, read_single/1]).
-import(msg_writer, [write_str/2, write_byte/2, write_short/2, write_int/2, write_single/2]).

write_msg(Msg) ->
    { MsgId, Buff } =
        case Msg of
            #msg_Login{
                sessionKey = SessionKey,
                userId = UserId,
                tableId = TableId,
                major = Major,
                minor = Minor,
                revision = Revision} ->
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
            #msg_LoginAck{
                errCode = ErrCode,
                id = Id} ->
                {
                    ?MSG_LoginAck,
                    list_to_binary([
                        msg_writer:write_int(ErrCode),
                        msg_writer:write_int(Id)])
                };
            #msg_Ping{
                data = Data} ->
                {
                    ?MSG_Ping,
                    list_to_binary([
                        msg_writer:write_int(Data)])
                };
            #msg_PingAck{
                data = Data} ->
                {
                    ?MSG_PingAck,
                    list_to_binary([
                        msg_writer:write_int(Data)])
                };
            #msg_Move{
                state = State,
                x = X,
                y = Y,
                angle = Angle} ->
                {
                    ?MSG_Move,
                    list_to_binary([
                        msg_writer:write_int(State),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle)])
                };
            #msg_MoveNotif{
                id = Id,
                state = State,
                x = X,
                y = Y,
                angle = Angle} ->
                {
                    ?MSG_MoveNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(State),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle)])
                };
            #msg_MoveAck{
                x = X,
                y = Y,
                angle = Angle} ->
                {
                    ?MSG_MoveAck,
                    list_to_binary([
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle)])
                };
            #msg_PropChangedIntNotif{
                id = Id,
                propertyId = PropertyId,
                value = Value,
                skillId = SkillId,
                modifierId = ModifierId,
                arg1 = Arg1} ->
                {
                    ?MSG_PropChangedIntNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(PropertyId),
                        msg_writer:write_int(Value),
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(ModifierId),
                        msg_writer:write_int(Arg1)])
                };
            #msg_PropChangedUintNotif{
                id = Id,
                propertyId = PropertyId,
                value = Value,
                skillId = SkillId,
                modifierId = ModifierId,
                arg1 = Arg1} ->
                {
                    ?MSG_PropChangedUintNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(PropertyId),
                        msg_writer:write_int(Value),
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(ModifierId),
                        msg_writer:write_int(Arg1)])
                };
            #msg_PropChangedSingleNotif{
                id = Id,
                propertyId = PropertyId,
                value = Value,
                skillId = SkillId,
                modifierId = ModifierId,
                arg1 = Arg1} ->
                {
                    ?MSG_PropChangedSingleNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(PropertyId),
                        msg_writer:write_single(Value),
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(ModifierId),
                        msg_writer:write_int(Arg1)])
                };
            #msg_ForceReposNotif{
                id = Id,
                x = X,
                y = Y,
                angle = Angle,
                speed = Speed,
                skillId = SkillId,
                modifierId = ModifierId} ->
                {
                    ?MSG_ForceReposNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle),
                        msg_writer:write_single(Speed),
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(ModifierId)])
                };
            #msg_CreatureProps{
                id = Id} ->
                {
                    ?MSG_CreatureProps,
                    list_to_binary([
                        msg_writer:write_int(Id)])
                };
            #msg_CreaturePropsAck{
                id = Id,
                name = Name,
                exp = Exp,
                preExp = PreExp,
                nextExp = NextExp,
                level = Level,
                hp = Hp,
                mp = Mp,
                health = Health,
                healthRegeneration = HealthRegeneration,
                mana = Mana,
                manaRegeneration = ManaRegeneration,
                attackDamage = AttackDamage,
                abilityPower = AbilityPower,
                armor = Armor,
                armorPenetration = ArmorPenetration,
                magicResistance = MagicResistance,
                magicPenetration = MagicPenetration,
                criticalStrikeChance = CriticalStrikeChance,
                criticalStrikeDamage = CriticalStrikeDamage,
                lifeSteal = LifeSteal,
                spellVamp = SpellVamp,
                tenacity = Tenacity,
                range = Range,
                movementSpeed = MovementSpeed,
                attackSpeed = AttackSpeed,
                cooldownReduction = CooldownReduction,
                gold = Gold,
                rMB = RMB,
                gM = GM} ->
                {
                    ?MSG_CreaturePropsAck,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_str(Name),
                        msg_writer:write_int(Exp),
                        msg_writer:write_int(PreExp),
                        msg_writer:write_int(NextExp),
                        msg_writer:write_int(Level),
                        msg_writer:write_single(Hp),
                        msg_writer:write_single(Mp),
                        msg_writer:write_single(Health),
                        msg_writer:write_single(HealthRegeneration),
                        msg_writer:write_single(Mana),
                        msg_writer:write_single(ManaRegeneration),
                        msg_writer:write_single(AttackDamage),
                        msg_writer:write_single(AbilityPower),
                        msg_writer:write_single(Armor),
                        msg_writer:write_single(ArmorPenetration),
                        msg_writer:write_single(MagicResistance),
                        msg_writer:write_single(MagicPenetration),
                        msg_writer:write_single(CriticalStrikeChance),
                        msg_writer:write_single(CriticalStrikeDamage),
                        msg_writer:write_single(LifeSteal),
                        msg_writer:write_single(SpellVamp),
                        msg_writer:write_single(Tenacity),
                        msg_writer:write_single(Range),
                        msg_writer:write_single(MovementSpeed),
                        msg_writer:write_single(AttackSpeed),
                        msg_writer:write_single(CooldownReduction),
                        msg_writer:write_int(Gold),
                        msg_writer:write_int(RMB),
                        msg_writer:write_int(GM)])
                };
            #msg_JumpNotif{
                id = Id,
                x = X,
                y = Y,
                angle = Angle} ->
                {
                    ?MSG_JumpNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle)])
                };
            #msg_SkillPropsNotif{
                id = Id,
                class = Class,
                level = Level,
                preCastTime = PreCastTime,
                castTime = CastTime,
                castingTime = CastingTime,
                cooldown = Cooldown,
                range = Range,
                smallCD = SmallCD,
                middleCD = MiddleCD,
                bigCD = BigCD,
                mP = MP,
                attackMin = AttackMin,
                attackMax = AttackMax,
                magicAttackMin = MagicAttackMin,
                magicAttackMax = MagicAttackMax,
                damage = Damage,
                arg1 = Arg1,
                arg2 = Arg2,
                arg3 = Arg3,
                arg4 = Arg4} ->
                {
                    ?MSG_SkillPropsNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(Class),
                        msg_writer:write_int(Level),
                        msg_writer:write_single(PreCastTime),
                        msg_writer:write_single(CastTime),
                        msg_writer:write_single(CastingTime),
                        msg_writer:write_single(Cooldown),
                        msg_writer:write_single(Range),
                        msg_writer:write_single(SmallCD),
                        msg_writer:write_single(MiddleCD),
                        msg_writer:write_single(BigCD),
                        msg_writer:write_int(MP),
                        msg_writer:write_int(AttackMin),
                        msg_writer:write_int(AttackMax),
                        msg_writer:write_int(MagicAttackMin),
                        msg_writer:write_int(MagicAttackMax),
                        msg_writer:write_int(Damage),
                        msg_writer:write_single(Arg1),
                        msg_writer:write_single(Arg2),
                        msg_writer:write_single(Arg3),
                        msg_writer:write_single(Arg4)])
                };
            #msg_Casting{
                skillId = SkillId,
                skillSeq = SkillSeq,
                targetId = TargetId,
                x = X,
                y = Y} ->
                {
                    ?MSG_Casting,
                    list_to_binary([
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(SkillSeq),
                        msg_writer:write_int(TargetId),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y)])
                };
            #msg_CastingNotif{
                skillId = SkillId,
                skillSeq = SkillSeq,
                playerId = PlayerId,
                targetId = TargetId,
                x = X,
                y = Y,
                lame = Lame} ->
                {
                    ?MSG_CastingNotif,
                    list_to_binary([
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(SkillSeq),
                        msg_writer:write_int(PlayerId),
                        msg_writer:write_int(TargetId),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_int(Lame)])
                };
            #msg_CastedNotif{
                skillId = SkillId,
                skillSeq = SkillSeq,
                playerId = PlayerId,
                targetId = TargetId,
                resultId = ResultId} ->
                {
                    ?MSG_CastedNotif,
                    list_to_binary([
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(SkillSeq),
                        msg_writer:write_int(PlayerId),
                        msg_writer:write_int(TargetId),
                        msg_writer:write_byte(ResultId)])
                };
            #msg_CastingAck{
                skillId = SkillId,
                skillSeq = SkillSeq,
                errorId = ErrorId} ->
                {
                    ?MSG_CastingAck,
                    list_to_binary([
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(SkillSeq),
                        msg_writer:write_int(ErrorId)])
                };
            #msg_SkillCooldownNotif{
                skillId = SkillId,
                cooldown = Cooldown} ->
                {
                    ?MSG_SkillCooldownNotif,
                    list_to_binary([
                        msg_writer:write_int(SkillId),
                        msg_writer:write_int(Cooldown)])
                };
            #msg_CreatureAppearNotif{
                type = Type,
                career = Career,
                gender = Gender,
                name = Name,
                id = Id,
                userId = UserId,
                x = X,
                y = Y,
                angle = Angle,
                radius = Radius,
                movementSpeed = MovementSpeed,
                health = Health,
                hp = Hp,
                mana = Mana,
                mp = Mp,
                campId = CampId,
                forceId = ForceId,
                attackable = Attackable,
                talkable = Talkable,
                monsterClass = MonsterClass,
                tableId = TableId,
                state = State,
                level = Level,
                isBuilding = IsBuilding} ->
                {
                    ?MSG_CreatureAppearNotif,
                    list_to_binary([
                        msg_writer:write_int(Type),
                        msg_writer:write_int(Career),
                        msg_writer:write_byte(Gender),
                        msg_writer:write_str(Name),
                        msg_writer:write_int(Id),
                        msg_writer:write_int(UserId),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle),
                        msg_writer:write_single(Radius),
                        msg_writer:write_single(MovementSpeed),
                        msg_writer:write_single(Health),
                        msg_writer:write_single(Hp),
                        msg_writer:write_single(Mana),
                        msg_writer:write_single(Mp),
                        msg_writer:write_byte(CampId),
                        msg_writer:write_byte(ForceId),
                        msg_writer:write_byte(Attackable),
                        msg_writer:write_byte(Talkable),
                        msg_writer:write_int(MonsterClass),
                        msg_writer:write_int(TableId),
                        msg_writer:write_int(State),
                        msg_writer:write_int(Level),
                        msg_writer:write_byte(IsBuilding)])
                };
            #msg_CreatureDisappearNotif{
                id = Id} ->
                {
                    ?MSG_CreatureDisappearNotif,
                    list_to_binary([
                        msg_writer:write_int(Id)])
                };
            #msg_CreatureOnNotif{
                name = Name,
                id = Id} ->
                {
                    ?MSG_CreatureOnNotif,
                    list_to_binary([
                        msg_writer:write_str(Name),
                        msg_writer:write_int(Id)])
                };
            #msg_CreatureOffNotif{
                id = Id} ->
                {
                    ?MSG_CreatureOffNotif,
                    list_to_binary([
                        msg_writer:write_int(Id)])
                };
            #msg_OperatorAppearNotif{
                id = Id,
                type = Type,
                x = X,
                y = Y,
                angle = Angle,
                subType = SubType,
                arg1 = Arg1,
                arg2 = Arg2} ->
                {
                    ?MSG_OperatorAppearNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(Type),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle),
                        msg_writer:write_int(SubType),
                        msg_writer:write_int(Arg1),
                        msg_writer:write_int(Arg2)])
                };
            #msg_OperatorDisappearNotif{
                id = Id} ->
                {
                    ?MSG_OperatorDisappearNotif,
                    list_to_binary([
                        msg_writer:write_int(Id)])
                };
            #msg_AddBuffNotif{
                playerId = PlayerId,
                buffId = BuffId,
                buffLevel = BuffLevel,
                buffTime = BuffTime} ->
                {
                    ?MSG_AddBuffNotif,
                    list_to_binary([
                        msg_writer:write_int(PlayerId),
                        msg_writer:write_int(BuffId),
                        msg_writer:write_int(BuffLevel),
                        msg_writer:write_int(BuffTime)])
                };
            #msg_UpdateBuffNotif{
                playerId = PlayerId,
                buffId = BuffId,
                buffLevel = BuffLevel,
                buffTime = BuffTime} ->
                {
                    ?MSG_UpdateBuffNotif,
                    list_to_binary([
                        msg_writer:write_int(PlayerId),
                        msg_writer:write_int(BuffId),
                        msg_writer:write_int(BuffLevel),
                        msg_writer:write_int(BuffTime)])
                };
            #msg_DelBuffNotif{
                playerId = PlayerId,
                buffId = BuffId} ->
                {
                    ?MSG_DelBuffNotif,
                    list_to_binary([
                        msg_writer:write_int(PlayerId),
                        msg_writer:write_int(BuffId)])
                };
            #msg_MoveEquip{
                sourceOwnerId = SourceOwnerId,
                positionFrom = PositionFrom,
                positionTo = PositionTo} ->
                {
                    ?MSG_MoveEquip,
                    list_to_binary([
                        msg_writer:write_int(SourceOwnerId),
                        msg_writer:write_byte(PositionFrom),
                        msg_writer:write_byte(PositionTo)])
                };
            #msg_MoveEquipAck{
                errorId = ErrorId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special} ->
                {
                    ?MSG_MoveEquipAck,
                    list_to_binary([
                        msg_writer:write_int(ErrorId),
                        msg_writer:write_str(EquipmentId),
                        msg_writer:write_str(IconId),
                        msg_writer:write_str(EquipmentRes),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(ItemType),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(EquipmentType),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(TemplateId),
                        msg_writer:write_int(Special)])
                };
            #msg_MoveEquipNotif{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special} ->
                {
                    ?MSG_MoveEquipNotif,
                    list_to_binary([
                        msg_writer:write_int(OwnerId),
                        msg_writer:write_str(EquipmentId),
                        msg_writer:write_str(IconId),
                        msg_writer:write_str(EquipmentRes),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(ItemType),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(EquipmentType),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(TemplateId),
                        msg_writer:write_int(Special)])
                };
            #msg_DropEquip{
                position = Position} ->
                {
                    ?MSG_DropEquip,
                    list_to_binary([
                        msg_writer:write_byte(Position)])
                };
            #msg_DropEquipAck{
                position = Position,
                errorId = ErrorId} ->
                {
                    ?MSG_DropEquipAck,
                    list_to_binary([
                        msg_writer:write_byte(Position),
                        msg_writer:write_int(ErrorId)])
                };
            #msg_UpdatedEquipNotif{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special} ->
                {
                    ?MSG_UpdatedEquipNotif,
                    list_to_binary([
                        msg_writer:write_int(OwnerId),
                        msg_writer:write_str(EquipmentId),
                        msg_writer:write_str(IconId),
                        msg_writer:write_str(EquipmentRes),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(ItemType),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(EquipmentType),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(TemplateId),
                        msg_writer:write_int(Special)])
                };
            #msg_ListEquip{
                ownerId = OwnerId,
                bag = Bag,
                equipment = Equipment} ->
                {
                    ?MSG_ListEquip,
                    list_to_binary([
                        msg_writer:write_int(OwnerId),
                        msg_writer:write_byte(Bag),
                        msg_writer:write_byte(Equipment)])
                };
            #msg_ListEquipAck{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special} ->
                {
                    ?MSG_ListEquipAck,
                    list_to_binary([
                        msg_writer:write_int(OwnerId),
                        msg_writer:write_str(EquipmentId),
                        msg_writer:write_str(IconId),
                        msg_writer:write_str(EquipmentRes),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(ItemType),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(EquipmentType),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(TemplateId),
                        msg_writer:write_int(Special)])
                };
            #msg_ListEquipNotif{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special} ->
                {
                    ?MSG_ListEquipNotif,
                    list_to_binary([
                        msg_writer:write_int(OwnerId),
                        msg_writer:write_str(EquipmentId),
                        msg_writer:write_str(IconId),
                        msg_writer:write_str(EquipmentRes),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(ItemType),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(EquipmentType),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(TemplateId),
                        msg_writer:write_int(Special)])
                };
            #msg_SimpleMessageNotif{
                n = N} ->
                {
                    ?MSG_SimpleMessageNotif,
                    list_to_binary([
                        msg_writer:write_int(N)])
                };
            #msg_Command{
                clientSideId = ClientSideId,
                command = Command} ->
                {
                    ?MSG_Command,
                    list_to_binary([
                        msg_writer:write_int(ClientSideId),
                        msg_writer:write_str(Command)])
                };
            #msg_CommandAck{
                clientSideId = ClientSideId,
                result = Result} ->
                {
                    ?MSG_CommandAck,
                    list_to_binary([
                        msg_writer:write_int(ClientSideId),
                        msg_writer:write_str(Result)])
                };
            #msg_Task{
                taskId = TaskId,
                taskState = TaskState} ->
                {
                    ?MSG_Task,
                    list_to_binary([
                        msg_writer:write_int(TaskId),
                        msg_writer:write_int(TaskState)])
                };
            #msg_ChangeTable{
                tableId = TableId} ->
                {
                    ?MSG_ChangeTable,
                    list_to_binary([
                        msg_writer:write_int(TableId)])
                };
            #msg_ChangeTableAck{
                tableId = TableId} ->
                {
                    ?MSG_ChangeTableAck,
                    list_to_binary([
                        msg_writer:write_int(TableId)])
                };
            #msg_AssignLeader{
                newLeaderId = NewLeaderId} ->
                {
                    ?MSG_AssignLeader,
                    list_to_binary([
                        msg_writer:write_int(NewLeaderId)])
                };
            #msg_AssignLeaderAck{
                resultId = ResultId} ->
                {
                    ?MSG_AssignLeaderAck,
                    list_to_binary([
                        msg_writer:write_int(ResultId)])
                };
            #msg_LockPlayer{
                lock = Lock} ->
                {
                    ?MSG_LockPlayer,
                    list_to_binary([
                        msg_writer:write_byte(Lock)])
                };
            #msg_WinnerNotif{
                forceId = ForceId} ->
                {
                    ?MSG_WinnerNotif,
                    list_to_binary([
                        msg_writer:write_int(ForceId)])
                };
            #msg_SpeakNotif{
                speakerId = SpeakerId,
                content = Content} ->
                {
                    ?MSG_SpeakNotif,
                    list_to_binary([
                        msg_writer:write_int(SpeakerId),
                        msg_writer:write_str(Content)])
                };
            #msg_Talk{
                targetId = TargetId} ->
                {
                    ?MSG_Talk,
                    list_to_binary([
                        msg_writer:write_int(TargetId)])
                };
            #msg_TalkAck{
                resultId = ResultId} ->
                {
                    ?MSG_TalkAck,
                    list_to_binary([
                        msg_writer:write_int(ResultId)])
                }
        end,
    list_to_binary([
        msg_writer:write_int(MsgId),
        msg_writer:write_int(size(Buff)),
        Buff]).

read_msg(Buff, MsgId) ->
    case MsgId of
        ?MSG_Login ->
            { SessionKey, Buff1 } = msg_reader:read_str(Buff),
            { UserId, Buff2 } = msg_reader:read_int(Buff1),
            { TableId, Buff3 } = msg_reader:read_int(Buff2),
            { Major, Buff4 } = msg_reader:read_int(Buff3),
            { Minor, Buff5 } = msg_reader:read_int(Buff4),
            { Revision, _ } = msg_reader:read_int(Buff5),
            #msg_Login{
                sessionKey = SessionKey,
                userId = UserId,
                tableId = TableId,
                major = Major,
                minor = Minor,
                revision = Revision};
        ?MSG_LoginAck ->
            { ErrCode, Buff1 } = msg_reader:read_int(Buff),
            { Id, _ } = msg_reader:read_int(Buff1),
            #msg_LoginAck{
                errCode = ErrCode,
                id = Id};
        ?MSG_Ping ->
            { Data, _ } = msg_reader:read_int(Buff),
            #msg_Ping{
                data = Data};
        ?MSG_PingAck ->
            { Data, _ } = msg_reader:read_int(Buff),
            #msg_PingAck{
                data = Data};
        ?MSG_Move ->
            { State, Buff1 } = msg_reader:read_int(Buff),
            { X, Buff2 } = msg_reader:read_single(Buff1),
            { Y, Buff3 } = msg_reader:read_single(Buff2),
            { Angle, _ } = msg_reader:read_single(Buff3),
            #msg_Move{
                state = State,
                x = X,
                y = Y,
                angle = Angle};
        ?MSG_MoveNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { State, Buff2 } = msg_reader:read_int(Buff1),
            { X, Buff3 } = msg_reader:read_single(Buff2),
            { Y, Buff4 } = msg_reader:read_single(Buff3),
            { Angle, _ } = msg_reader:read_single(Buff4),
            #msg_MoveNotif{
                id = Id,
                state = State,
                x = X,
                y = Y,
                angle = Angle};
        ?MSG_MoveAck ->
            { X, Buff1 } = msg_reader:read_single(Buff),
            { Y, Buff2 } = msg_reader:read_single(Buff1),
            { Angle, _ } = msg_reader:read_single(Buff2),
            #msg_MoveAck{
                x = X,
                y = Y,
                angle = Angle};
        ?MSG_PropChangedIntNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { PropertyId, Buff2 } = msg_reader:read_int(Buff1),
            { Value, Buff3 } = msg_reader:read_int(Buff2),
            { SkillId, Buff4 } = msg_reader:read_int(Buff3),
            { ModifierId, Buff5 } = msg_reader:read_int(Buff4),
            { Arg1, _ } = msg_reader:read_int(Buff5),
            #msg_PropChangedIntNotif{
                id = Id,
                propertyId = PropertyId,
                value = Value,
                skillId = SkillId,
                modifierId = ModifierId,
                arg1 = Arg1};
        ?MSG_PropChangedUintNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { PropertyId, Buff2 } = msg_reader:read_int(Buff1),
            { Value, Buff3 } = msg_reader:read_int(Buff2),
            { SkillId, Buff4 } = msg_reader:read_int(Buff3),
            { ModifierId, Buff5 } = msg_reader:read_int(Buff4),
            { Arg1, _ } = msg_reader:read_int(Buff5),
            #msg_PropChangedUintNotif{
                id = Id,
                propertyId = PropertyId,
                value = Value,
                skillId = SkillId,
                modifierId = ModifierId,
                arg1 = Arg1};
        ?MSG_PropChangedSingleNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { PropertyId, Buff2 } = msg_reader:read_int(Buff1),
            { Value, Buff3 } = msg_reader:read_single(Buff2),
            { SkillId, Buff4 } = msg_reader:read_int(Buff3),
            { ModifierId, Buff5 } = msg_reader:read_int(Buff4),
            { Arg1, _ } = msg_reader:read_int(Buff5),
            #msg_PropChangedSingleNotif{
                id = Id,
                propertyId = PropertyId,
                value = Value,
                skillId = SkillId,
                modifierId = ModifierId,
                arg1 = Arg1};
        ?MSG_ForceReposNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { X, Buff2 } = msg_reader:read_single(Buff1),
            { Y, Buff3 } = msg_reader:read_single(Buff2),
            { Angle, Buff4 } = msg_reader:read_single(Buff3),
            { Speed, Buff5 } = msg_reader:read_single(Buff4),
            { SkillId, Buff6 } = msg_reader:read_int(Buff5),
            { ModifierId, _ } = msg_reader:read_int(Buff6),
            #msg_ForceReposNotif{
                id = Id,
                x = X,
                y = Y,
                angle = Angle,
                speed = Speed,
                skillId = SkillId,
                modifierId = ModifierId};
        ?MSG_CreatureProps ->
            { Id, _ } = msg_reader:read_int(Buff),
            #msg_CreatureProps{
                id = Id};
        ?MSG_CreaturePropsAck ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { Name, Buff2 } = msg_reader:read_str(Buff1),
            { Exp, Buff3 } = msg_reader:read_int(Buff2),
            { PreExp, Buff4 } = msg_reader:read_int(Buff3),
            { NextExp, Buff5 } = msg_reader:read_int(Buff4),
            { Level, Buff6 } = msg_reader:read_int(Buff5),
            { Hp, Buff7 } = msg_reader:read_single(Buff6),
            { Mp, Buff8 } = msg_reader:read_single(Buff7),
            { Health, Buff9 } = msg_reader:read_single(Buff8),
            { HealthRegeneration, Buff10 } = msg_reader:read_single(Buff9),
            { Mana, Buff11 } = msg_reader:read_single(Buff10),
            { ManaRegeneration, Buff12 } = msg_reader:read_single(Buff11),
            { AttackDamage, Buff13 } = msg_reader:read_single(Buff12),
            { AbilityPower, Buff14 } = msg_reader:read_single(Buff13),
            { Armor, Buff15 } = msg_reader:read_single(Buff14),
            { ArmorPenetration, Buff16 } = msg_reader:read_single(Buff15),
            { MagicResistance, Buff17 } = msg_reader:read_single(Buff16),
            { MagicPenetration, Buff18 } = msg_reader:read_single(Buff17),
            { CriticalStrikeChance, Buff19 } = msg_reader:read_single(Buff18),
            { CriticalStrikeDamage, Buff20 } = msg_reader:read_single(Buff19),
            { LifeSteal, Buff21 } = msg_reader:read_single(Buff20),
            { SpellVamp, Buff22 } = msg_reader:read_single(Buff21),
            { Tenacity, Buff23 } = msg_reader:read_single(Buff22),
            { Range, Buff24 } = msg_reader:read_single(Buff23),
            { MovementSpeed, Buff25 } = msg_reader:read_single(Buff24),
            { AttackSpeed, Buff26 } = msg_reader:read_single(Buff25),
            { CooldownReduction, Buff27 } = msg_reader:read_single(Buff26),
            { Gold, Buff28 } = msg_reader:read_int(Buff27),
            { RMB, Buff29 } = msg_reader:read_int(Buff28),
            { GM, _ } = msg_reader:read_int(Buff29),
            #msg_CreaturePropsAck{
                id = Id,
                name = Name,
                exp = Exp,
                preExp = PreExp,
                nextExp = NextExp,
                level = Level,
                hp = Hp,
                mp = Mp,
                health = Health,
                healthRegeneration = HealthRegeneration,
                mana = Mana,
                manaRegeneration = ManaRegeneration,
                attackDamage = AttackDamage,
                abilityPower = AbilityPower,
                armor = Armor,
                armorPenetration = ArmorPenetration,
                magicResistance = MagicResistance,
                magicPenetration = MagicPenetration,
                criticalStrikeChance = CriticalStrikeChance,
                criticalStrikeDamage = CriticalStrikeDamage,
                lifeSteal = LifeSteal,
                spellVamp = SpellVamp,
                tenacity = Tenacity,
                range = Range,
                movementSpeed = MovementSpeed,
                attackSpeed = AttackSpeed,
                cooldownReduction = CooldownReduction,
                gold = Gold,
                rMB = RMB,
                gM = GM};
        ?MSG_JumpNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { X, Buff2 } = msg_reader:read_single(Buff1),
            { Y, Buff3 } = msg_reader:read_single(Buff2),
            { Angle, _ } = msg_reader:read_single(Buff3),
            #msg_JumpNotif{
                id = Id,
                x = X,
                y = Y,
                angle = Angle};
        ?MSG_SkillPropsNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { Class, Buff2 } = msg_reader:read_int(Buff1),
            { Level, Buff3 } = msg_reader:read_int(Buff2),
            { PreCastTime, Buff4 } = msg_reader:read_single(Buff3),
            { CastTime, Buff5 } = msg_reader:read_single(Buff4),
            { CastingTime, Buff6 } = msg_reader:read_single(Buff5),
            { Cooldown, Buff7 } = msg_reader:read_single(Buff6),
            { Range, Buff8 } = msg_reader:read_single(Buff7),
            { SmallCD, Buff9 } = msg_reader:read_single(Buff8),
            { MiddleCD, Buff10 } = msg_reader:read_single(Buff9),
            { BigCD, Buff11 } = msg_reader:read_single(Buff10),
            { MP, Buff12 } = msg_reader:read_int(Buff11),
            { AttackMin, Buff13 } = msg_reader:read_int(Buff12),
            { AttackMax, Buff14 } = msg_reader:read_int(Buff13),
            { MagicAttackMin, Buff15 } = msg_reader:read_int(Buff14),
            { MagicAttackMax, Buff16 } = msg_reader:read_int(Buff15),
            { Damage, Buff17 } = msg_reader:read_int(Buff16),
            { Arg1, Buff18 } = msg_reader:read_single(Buff17),
            { Arg2, Buff19 } = msg_reader:read_single(Buff18),
            { Arg3, Buff20 } = msg_reader:read_single(Buff19),
            { Arg4, _ } = msg_reader:read_single(Buff20),
            #msg_SkillPropsNotif{
                id = Id,
                class = Class,
                level = Level,
                preCastTime = PreCastTime,
                castTime = CastTime,
                castingTime = CastingTime,
                cooldown = Cooldown,
                range = Range,
                smallCD = SmallCD,
                middleCD = MiddleCD,
                bigCD = BigCD,
                mP = MP,
                attackMin = AttackMin,
                attackMax = AttackMax,
                magicAttackMin = MagicAttackMin,
                magicAttackMax = MagicAttackMax,
                damage = Damage,
                arg1 = Arg1,
                arg2 = Arg2,
                arg3 = Arg3,
                arg4 = Arg4};
        ?MSG_Casting ->
            { SkillId, Buff1 } = msg_reader:read_int(Buff),
            { SkillSeq, Buff2 } = msg_reader:read_int(Buff1),
            { TargetId, Buff3 } = msg_reader:read_int(Buff2),
            { X, Buff4 } = msg_reader:read_single(Buff3),
            { Y, _ } = msg_reader:read_single(Buff4),
            #msg_Casting{
                skillId = SkillId,
                skillSeq = SkillSeq,
                targetId = TargetId,
                x = X,
                y = Y};
        ?MSG_CastingNotif ->
            { SkillId, Buff1 } = msg_reader:read_int(Buff),
            { SkillSeq, Buff2 } = msg_reader:read_int(Buff1),
            { PlayerId, Buff3 } = msg_reader:read_int(Buff2),
            { TargetId, Buff4 } = msg_reader:read_int(Buff3),
            { X, Buff5 } = msg_reader:read_single(Buff4),
            { Y, Buff6 } = msg_reader:read_single(Buff5),
            { Lame, _ } = msg_reader:read_int(Buff6),
            #msg_CastingNotif{
                skillId = SkillId,
                skillSeq = SkillSeq,
                playerId = PlayerId,
                targetId = TargetId,
                x = X,
                y = Y,
                lame = Lame};
        ?MSG_CastedNotif ->
            { SkillId, Buff1 } = msg_reader:read_int(Buff),
            { SkillSeq, Buff2 } = msg_reader:read_int(Buff1),
            { PlayerId, Buff3 } = msg_reader:read_int(Buff2),
            { TargetId, Buff4 } = msg_reader:read_int(Buff3),
            { ResultId, _ } = msg_reader:read_byte(Buff4),
            #msg_CastedNotif{
                skillId = SkillId,
                skillSeq = SkillSeq,
                playerId = PlayerId,
                targetId = TargetId,
                resultId = ResultId};
        ?MSG_CastingAck ->
            { SkillId, Buff1 } = msg_reader:read_int(Buff),
            { SkillSeq, Buff2 } = msg_reader:read_int(Buff1),
            { ErrorId, _ } = msg_reader:read_int(Buff2),
            #msg_CastingAck{
                skillId = SkillId,
                skillSeq = SkillSeq,
                errorId = ErrorId};
        ?MSG_SkillCooldownNotif ->
            { SkillId, Buff1 } = msg_reader:read_int(Buff),
            { Cooldown, _ } = msg_reader:read_int(Buff1),
            #msg_SkillCooldownNotif{
                skillId = SkillId,
                cooldown = Cooldown};
        ?MSG_CreatureAppearNotif ->
            { Type, Buff1 } = msg_reader:read_int(Buff),
            { Career, Buff2 } = msg_reader:read_int(Buff1),
            { Gender, Buff3 } = msg_reader:read_byte(Buff2),
            { Name, Buff4 } = msg_reader:read_str(Buff3),
            { Id, Buff5 } = msg_reader:read_int(Buff4),
            { UserId, Buff6 } = msg_reader:read_int(Buff5),
            { X, Buff7 } = msg_reader:read_single(Buff6),
            { Y, Buff8 } = msg_reader:read_single(Buff7),
            { Angle, Buff9 } = msg_reader:read_single(Buff8),
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
            #msg_CreatureAppearNotif{
                type = Type,
                career = Career,
                gender = Gender,
                name = Name,
                id = Id,
                userId = UserId,
                x = X,
                y = Y,
                angle = Angle,
                radius = Radius,
                movementSpeed = MovementSpeed,
                health = Health,
                hp = Hp,
                mana = Mana,
                mp = Mp,
                campId = CampId,
                forceId = ForceId,
                attackable = Attackable,
                talkable = Talkable,
                monsterClass = MonsterClass,
                tableId = TableId,
                state = State,
                level = Level,
                isBuilding = IsBuilding};
        ?MSG_CreatureDisappearNotif ->
            { Id, _ } = msg_reader:read_int(Buff),
            #msg_CreatureDisappearNotif{
                id = Id};
        ?MSG_CreatureOnNotif ->
            { Name, Buff1 } = msg_reader:read_str(Buff),
            { Id, _ } = msg_reader:read_int(Buff1),
            #msg_CreatureOnNotif{
                name = Name,
                id = Id};
        ?MSG_CreatureOffNotif ->
            { Id, _ } = msg_reader:read_int(Buff),
            #msg_CreatureOffNotif{
                id = Id};
        ?MSG_OperatorAppearNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { Type, Buff2 } = msg_reader:read_int(Buff1),
            { X, Buff3 } = msg_reader:read_single(Buff2),
            { Y, Buff4 } = msg_reader:read_single(Buff3),
            { Angle, Buff5 } = msg_reader:read_single(Buff4),
            { SubType, Buff6 } = msg_reader:read_int(Buff5),
            { Arg1, Buff7 } = msg_reader:read_int(Buff6),
            { Arg2, _ } = msg_reader:read_int(Buff7),
            #msg_OperatorAppearNotif{
                id = Id,
                type = Type,
                x = X,
                y = Y,
                angle = Angle,
                subType = SubType,
                arg1 = Arg1,
                arg2 = Arg2};
        ?MSG_OperatorDisappearNotif ->
            { Id, _ } = msg_reader:read_int(Buff),
            #msg_OperatorDisappearNotif{
                id = Id};
        ?MSG_AddBuffNotif ->
            { PlayerId, Buff1 } = msg_reader:read_int(Buff),
            { BuffId, Buff2 } = msg_reader:read_int(Buff1),
            { BuffLevel, Buff3 } = msg_reader:read_int(Buff2),
            { BuffTime, _ } = msg_reader:read_int(Buff3),
            #msg_AddBuffNotif{
                playerId = PlayerId,
                buffId = BuffId,
                buffLevel = BuffLevel,
                buffTime = BuffTime};
        ?MSG_UpdateBuffNotif ->
            { PlayerId, Buff1 } = msg_reader:read_int(Buff),
            { BuffId, Buff2 } = msg_reader:read_int(Buff1),
            { BuffLevel, Buff3 } = msg_reader:read_int(Buff2),
            { BuffTime, _ } = msg_reader:read_int(Buff3),
            #msg_UpdateBuffNotif{
                playerId = PlayerId,
                buffId = BuffId,
                buffLevel = BuffLevel,
                buffTime = BuffTime};
        ?MSG_DelBuffNotif ->
            { PlayerId, Buff1 } = msg_reader:read_int(Buff),
            { BuffId, _ } = msg_reader:read_int(Buff1),
            #msg_DelBuffNotif{
                playerId = PlayerId,
                buffId = BuffId};
        ?MSG_MoveEquip ->
            { SourceOwnerId, Buff1 } = msg_reader:read_int(Buff),
            { PositionFrom, Buff2 } = msg_reader:read_byte(Buff1),
            { PositionTo, _ } = msg_reader:read_byte(Buff2),
            #msg_MoveEquip{
                sourceOwnerId = SourceOwnerId,
                positionFrom = PositionFrom,
                positionTo = PositionTo};
        ?MSG_MoveEquipAck ->
            { ErrorId, Buff1 } = msg_reader:read_int(Buff),
            { EquipmentId, Buff2 } = msg_reader:read_str(Buff1),
            { IconId, Buff3 } = msg_reader:read_str(Buff2),
            { EquipmentRes, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { ItemType, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { EquipmentType, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { TemplateId, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_MoveEquipAck{
                errorId = ErrorId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special};
        ?MSG_MoveEquipNotif ->
            { OwnerId, Buff1 } = msg_reader:read_int(Buff),
            { EquipmentId, Buff2 } = msg_reader:read_str(Buff1),
            { IconId, Buff3 } = msg_reader:read_str(Buff2),
            { EquipmentRes, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { ItemType, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { EquipmentType, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { TemplateId, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_MoveEquipNotif{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special};
        ?MSG_DropEquip ->
            { Position, _ } = msg_reader:read_byte(Buff),
            #msg_DropEquip{
                position = Position};
        ?MSG_DropEquipAck ->
            { Position, Buff1 } = msg_reader:read_byte(Buff),
            { ErrorId, _ } = msg_reader:read_int(Buff1),
            #msg_DropEquipAck{
                position = Position,
                errorId = ErrorId};
        ?MSG_UpdatedEquipNotif ->
            { OwnerId, Buff1 } = msg_reader:read_int(Buff),
            { EquipmentId, Buff2 } = msg_reader:read_str(Buff1),
            { IconId, Buff3 } = msg_reader:read_str(Buff2),
            { EquipmentRes, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { ItemType, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { EquipmentType, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { TemplateId, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_UpdatedEquipNotif{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special};
        ?MSG_ListEquip ->
            { OwnerId, Buff1 } = msg_reader:read_int(Buff),
            { Bag, Buff2 } = msg_reader:read_byte(Buff1),
            { Equipment, _ } = msg_reader:read_byte(Buff2),
            #msg_ListEquip{
                ownerId = OwnerId,
                bag = Bag,
                equipment = Equipment};
        ?MSG_ListEquipAck ->
            { OwnerId, Buff1 } = msg_reader:read_int(Buff),
            { EquipmentId, Buff2 } = msg_reader:read_str(Buff1),
            { IconId, Buff3 } = msg_reader:read_str(Buff2),
            { EquipmentRes, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { ItemType, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { EquipmentType, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { TemplateId, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_ListEquipAck{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special};
        ?MSG_ListEquipNotif ->
            { OwnerId, Buff1 } = msg_reader:read_int(Buff),
            { EquipmentId, Buff2 } = msg_reader:read_str(Buff1),
            { IconId, Buff3 } = msg_reader:read_str(Buff2),
            { EquipmentRes, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { ItemType, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { EquipmentType, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { TemplateId, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_ListEquipNotif{
                ownerId = OwnerId,
                equipmentId = EquipmentId,
                iconId = IconId,
                equipmentRes = EquipmentRes,
                position = Position,
                itemType = ItemType,
                count = Count,
                equipmentType = EquipmentType,
                grade = Grade,
                templateId = TemplateId,
                special = Special};
        ?MSG_SimpleMessageNotif ->
            { N, _ } = msg_reader:read_int(Buff),
            #msg_SimpleMessageNotif{
                n = N};
        ?MSG_Command ->
            { ClientSideId, Buff1 } = msg_reader:read_int(Buff),
            { Command, _ } = msg_reader:read_str(Buff1),
            #msg_Command{
                clientSideId = ClientSideId,
                command = Command};
        ?MSG_CommandAck ->
            { ClientSideId, Buff1 } = msg_reader:read_int(Buff),
            { Result, _ } = msg_reader:read_str(Buff1),
            #msg_CommandAck{
                clientSideId = ClientSideId,
                result = Result};
        ?MSG_Task ->
            { TaskId, Buff1 } = msg_reader:read_int(Buff),
            { TaskState, _ } = msg_reader:read_int(Buff1),
            #msg_Task{
                taskId = TaskId,
                taskState = TaskState};
        ?MSG_ChangeTable ->
            { TableId, _ } = msg_reader:read_int(Buff),
            #msg_ChangeTable{
                tableId = TableId};
        ?MSG_ChangeTableAck ->
            { TableId, _ } = msg_reader:read_int(Buff),
            #msg_ChangeTableAck{
                tableId = TableId};
        ?MSG_AssignLeader ->
            { NewLeaderId, _ } = msg_reader:read_int(Buff),
            #msg_AssignLeader{
                newLeaderId = NewLeaderId};
        ?MSG_AssignLeaderAck ->
            { ResultId, _ } = msg_reader:read_int(Buff),
            #msg_AssignLeaderAck{
                resultId = ResultId};
        ?MSG_LockPlayer ->
            { Lock, _ } = msg_reader:read_byte(Buff),
            #msg_LockPlayer{
                lock = Lock};
        ?MSG_WinnerNotif ->
            { ForceId, _ } = msg_reader:read_int(Buff),
            #msg_WinnerNotif{
                forceId = ForceId};
        ?MSG_SpeakNotif ->
            { SpeakerId, Buff1 } = msg_reader:read_int(Buff),
            { Content, _ } = msg_reader:read_str(Buff1),
            #msg_SpeakNotif{
                speakerId = SpeakerId,
                content = Content};
        ?MSG_Talk ->
            { TargetId, _ } = msg_reader:read_int(Buff),
            #msg_Talk{
                targetId = TargetId};
        ?MSG_TalkAck ->
            { ResultId, _ } = msg_reader:read_int(Buff),
            #msg_TalkAck{
                resultId = ResultId};
        _ ->
            %io:format("receiving unknown msg~n"),
            void
    end.
