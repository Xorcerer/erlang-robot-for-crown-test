-module(msg).
-compile(export_all).
-include("records.hrl").
-import(msg_reader, [read_str/1, read_byte/1, read_short/1, read_int/1, read_int1/1, read_single/1]).
-import(msg_writer, [write_str/2, write_byte/2, write_short/2, write_int/2, write_single/2]).

write_msg(Msg) ->
    { MsgId, Buff } =
        case Msg of
            #msg_Login{
                sessionKey = Sessionkey,
                userId = Userid,
                tableId = Tableid,
                major = Major,
                minor = Minor,
                revision = Revision} ->
                {
                    ?MSG_Login,
                    list_to_binary([
                        msg_writer:write_str(Sessionkey),
                        msg_writer:write_int(Userid),
                        msg_writer:write_int(Tableid),
                        msg_writer:write_int(Major),
                        msg_writer:write_int(Minor),
                        msg_writer:write_int(Revision)])
                };
            #msg_LoginAck{
                errCode = Errcode,
                id = Id} ->
                {
                    ?MSG_LoginAck,
                    list_to_binary([
                        msg_writer:write_int(Errcode),
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
                propertyId = Propertyid,
                value = Value,
                skillId = Skillid,
                modifierId = Modifierid,
                arg1 = Arg1} ->
                {
                    ?MSG_PropChangedIntNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(Propertyid),
                        msg_writer:write_int(Value),
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Modifierid),
                        msg_writer:write_int(Arg1)])
                };
            #msg_PropChangedUintNotif{
                id = Id,
                propertyId = Propertyid,
                value = Value,
                skillId = Skillid,
                modifierId = Modifierid,
                arg1 = Arg1} ->
                {
                    ?MSG_PropChangedUintNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(Propertyid),
                        msg_writer:write_int(Value),
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Modifierid),
                        msg_writer:write_int(Arg1)])
                };
            #msg_PropChangedSingleNotif{
                id = Id,
                propertyId = Propertyid,
                value = Value,
                skillId = Skillid,
                modifierId = Modifierid,
                arg1 = Arg1} ->
                {
                    ?MSG_PropChangedSingleNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_int(Propertyid),
                        msg_writer:write_single(Value),
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Modifierid),
                        msg_writer:write_int(Arg1)])
                };
            #msg_ForceReposNotif{
                id = Id,
                x = X,
                y = Y,
                angle = Angle,
                speed = Speed,
                skillId = Skillid,
                modifierId = Modifierid} ->
                {
                    ?MSG_ForceReposNotif,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle),
                        msg_writer:write_single(Speed),
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Modifierid)])
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
                preExp = Preexp,
                nextExp = Nextexp,
                level = Level,
                hp = Hp,
                mp = Mp,
                health = Health,
                healthRegeneration = Healthregeneration,
                mana = Mana,
                manaRegeneration = Manaregeneration,
                attackDamage = Attackdamage,
                abilityPower = Abilitypower,
                armor = Armor,
                armorPenetration = Armorpenetration,
                magicResistance = Magicresistance,
                magicPenetration = Magicpenetration,
                criticalStrikeChance = Criticalstrikechance,
                criticalStrikeDamage = Criticalstrikedamage,
                lifeSteal = Lifesteal,
                spellVamp = Spellvamp,
                tenacity = Tenacity,
                range = Range,
                movementSpeed = Movementspeed,
                attackSpeed = Attackspeed,
                cooldownReduction = Cooldownreduction,
                gold = Gold,
                rMB = Rmb,
                gM = Gm} ->
                {
                    ?MSG_CreaturePropsAck,
                    list_to_binary([
                        msg_writer:write_int(Id),
                        msg_writer:write_str(Name),
                        msg_writer:write_int(Exp),
                        msg_writer:write_int(Preexp),
                        msg_writer:write_int(Nextexp),
                        msg_writer:write_int(Level),
                        msg_writer:write_single(Hp),
                        msg_writer:write_single(Mp),
                        msg_writer:write_single(Health),
                        msg_writer:write_single(Healthregeneration),
                        msg_writer:write_single(Mana),
                        msg_writer:write_single(Manaregeneration),
                        msg_writer:write_single(Attackdamage),
                        msg_writer:write_single(Abilitypower),
                        msg_writer:write_single(Armor),
                        msg_writer:write_single(Armorpenetration),
                        msg_writer:write_single(Magicresistance),
                        msg_writer:write_single(Magicpenetration),
                        msg_writer:write_single(Criticalstrikechance),
                        msg_writer:write_single(Criticalstrikedamage),
                        msg_writer:write_single(Lifesteal),
                        msg_writer:write_single(Spellvamp),
                        msg_writer:write_single(Tenacity),
                        msg_writer:write_single(Range),
                        msg_writer:write_single(Movementspeed),
                        msg_writer:write_single(Attackspeed),
                        msg_writer:write_single(Cooldownreduction),
                        msg_writer:write_int(Gold),
                        msg_writer:write_int(Rmb),
                        msg_writer:write_int(Gm)])
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
                preCastTime = Precasttime,
                castTime = Casttime,
                castingTime = Castingtime,
                cooldown = Cooldown,
                range = Range,
                smallCD = Smallcd,
                middleCD = Middlecd,
                bigCD = Bigcd,
                mP = Mp,
                attackMin = Attackmin,
                attackMax = Attackmax,
                magicAttackMin = Magicattackmin,
                magicAttackMax = Magicattackmax,
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
                        msg_writer:write_single(Precasttime),
                        msg_writer:write_single(Casttime),
                        msg_writer:write_single(Castingtime),
                        msg_writer:write_single(Cooldown),
                        msg_writer:write_single(Range),
                        msg_writer:write_single(Smallcd),
                        msg_writer:write_single(Middlecd),
                        msg_writer:write_single(Bigcd),
                        msg_writer:write_int(Mp),
                        msg_writer:write_int(Attackmin),
                        msg_writer:write_int(Attackmax),
                        msg_writer:write_int(Magicattackmin),
                        msg_writer:write_int(Magicattackmax),
                        msg_writer:write_int(Damage),
                        msg_writer:write_single(Arg1),
                        msg_writer:write_single(Arg2),
                        msg_writer:write_single(Arg3),
                        msg_writer:write_single(Arg4)])
                };
            #msg_Casting{
                skillId = Skillid,
                skillSeq = Skillseq,
                targetId = Targetid,
                x = X,
                y = Y} ->
                {
                    ?MSG_Casting,
                    list_to_binary([
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Skillseq),
                        msg_writer:write_int(Targetid),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y)])
                };
            #msg_CastingNotif{
                skillId = Skillid,
                skillSeq = Skillseq,
                playerId = Playerid,
                targetId = Targetid,
                x = X,
                y = Y,
                lame = Lame} ->
                {
                    ?MSG_CastingNotif,
                    list_to_binary([
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Skillseq),
                        msg_writer:write_int(Playerid),
                        msg_writer:write_int(Targetid),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_int(Lame)])
                };
            #msg_CastedNotif{
                skillId = Skillid,
                skillSeq = Skillseq,
                playerId = Playerid,
                targetId = Targetid,
                resultId = Resultid} ->
                {
                    ?MSG_CastedNotif,
                    list_to_binary([
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Skillseq),
                        msg_writer:write_int(Playerid),
                        msg_writer:write_int(Targetid),
                        msg_writer:write_byte(Resultid)])
                };
            #msg_CastingAck{
                skillId = Skillid,
                skillSeq = Skillseq,
                errorId = Errorid} ->
                {
                    ?MSG_CastingAck,
                    list_to_binary([
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Skillseq),
                        msg_writer:write_int(Errorid)])
                };
            #msg_SkillCooldownNotif{
                skillId = Skillid,
                cooldown = Cooldown} ->
                {
                    ?MSG_SkillCooldownNotif,
                    list_to_binary([
                        msg_writer:write_int(Skillid),
                        msg_writer:write_int(Cooldown)])
                };
            #msg_CreatureAppearNotif{
                type = Type,
                career = Career,
                gender = Gender,
                name = Name,
                id = Id,
                userId = Userid,
                x = X,
                y = Y,
                angle = Angle,
                radius = Radius,
                movementSpeed = Movementspeed,
                health = Health,
                hp = Hp,
                mana = Mana,
                mp = Mp,
                campId = Campid,
                forceId = Forceid,
                attackable = Attackable,
                talkable = Talkable,
                monsterClass = Monsterclass,
                tableId = Tableid,
                state = State,
                level = Level,
                isBuilding = Isbuilding} ->
                {
                    ?MSG_CreatureAppearNotif,
                    list_to_binary([
                        msg_writer:write_int(Type),
                        msg_writer:write_int(Career),
                        msg_writer:write_byte(Gender),
                        msg_writer:write_str(Name),
                        msg_writer:write_int(Id),
                        msg_writer:write_int(Userid),
                        msg_writer:write_single(X),
                        msg_writer:write_single(Y),
                        msg_writer:write_single(Angle),
                        msg_writer:write_single(Radius),
                        msg_writer:write_single(Movementspeed),
                        msg_writer:write_single(Health),
                        msg_writer:write_single(Hp),
                        msg_writer:write_single(Mana),
                        msg_writer:write_single(Mp),
                        msg_writer:write_byte(Campid),
                        msg_writer:write_byte(Forceid),
                        msg_writer:write_byte(Attackable),
                        msg_writer:write_byte(Talkable),
                        msg_writer:write_int(Monsterclass),
                        msg_writer:write_int(Tableid),
                        msg_writer:write_int(State),
                        msg_writer:write_int(Level),
                        msg_writer:write_byte(Isbuilding)])
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
                subType = Subtype,
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
                        msg_writer:write_int(Subtype),
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
                playerId = Playerid,
                buffId = Buffid,
                buffLevel = Bufflevel,
                buffTime = Bufftime} ->
                {
                    ?MSG_AddBuffNotif,
                    list_to_binary([
                        msg_writer:write_int(Playerid),
                        msg_writer:write_int(Buffid),
                        msg_writer:write_int(Bufflevel),
                        msg_writer:write_int(Bufftime)])
                };
            #msg_UpdateBuffNotif{
                playerId = Playerid,
                buffId = Buffid,
                buffLevel = Bufflevel,
                buffTime = Bufftime} ->
                {
                    ?MSG_UpdateBuffNotif,
                    list_to_binary([
                        msg_writer:write_int(Playerid),
                        msg_writer:write_int(Buffid),
                        msg_writer:write_int(Bufflevel),
                        msg_writer:write_int(Bufftime)])
                };
            #msg_DelBuffNotif{
                playerId = Playerid,
                buffId = Buffid} ->
                {
                    ?MSG_DelBuffNotif,
                    list_to_binary([
                        msg_writer:write_int(Playerid),
                        msg_writer:write_int(Buffid)])
                };
            #msg_MoveEquip{
                sourceOwnerId = Sourceownerid,
                positionFrom = Positionfrom,
                positionTo = Positionto} ->
                {
                    ?MSG_MoveEquip,
                    list_to_binary([
                        msg_writer:write_int(Sourceownerid),
                        msg_writer:write_byte(Positionfrom),
                        msg_writer:write_byte(Positionto)])
                };
            #msg_MoveEquipAck{
                errorId = Errorid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special} ->
                {
                    ?MSG_MoveEquipAck,
                    list_to_binary([
                        msg_writer:write_int(Errorid),
                        msg_writer:write_str(Equipmentid),
                        msg_writer:write_str(Iconid),
                        msg_writer:write_str(Equipmentres),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(Itemtype),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(Equipmenttype),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(Templateid),
                        msg_writer:write_int(Special)])
                };
            #msg_MoveEquipNotif{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special} ->
                {
                    ?MSG_MoveEquipNotif,
                    list_to_binary([
                        msg_writer:write_int(Ownerid),
                        msg_writer:write_str(Equipmentid),
                        msg_writer:write_str(Iconid),
                        msg_writer:write_str(Equipmentres),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(Itemtype),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(Equipmenttype),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(Templateid),
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
                errorId = Errorid} ->
                {
                    ?MSG_DropEquipAck,
                    list_to_binary([
                        msg_writer:write_byte(Position),
                        msg_writer:write_int(Errorid)])
                };
            #msg_UpdatedEquipNotif{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special} ->
                {
                    ?MSG_UpdatedEquipNotif,
                    list_to_binary([
                        msg_writer:write_int(Ownerid),
                        msg_writer:write_str(Equipmentid),
                        msg_writer:write_str(Iconid),
                        msg_writer:write_str(Equipmentres),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(Itemtype),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(Equipmenttype),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(Templateid),
                        msg_writer:write_int(Special)])
                };
            #msg_ListEquip{
                ownerId = Ownerid,
                bag = Bag,
                equipment = Equipment} ->
                {
                    ?MSG_ListEquip,
                    list_to_binary([
                        msg_writer:write_int(Ownerid),
                        msg_writer:write_byte(Bag),
                        msg_writer:write_byte(Equipment)])
                };
            #msg_ListEquipAck{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special} ->
                {
                    ?MSG_ListEquipAck,
                    list_to_binary([
                        msg_writer:write_int(Ownerid),
                        msg_writer:write_str(Equipmentid),
                        msg_writer:write_str(Iconid),
                        msg_writer:write_str(Equipmentres),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(Itemtype),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(Equipmenttype),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(Templateid),
                        msg_writer:write_int(Special)])
                };
            #msg_ListEquipNotif{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special} ->
                {
                    ?MSG_ListEquipNotif,
                    list_to_binary([
                        msg_writer:write_int(Ownerid),
                        msg_writer:write_str(Equipmentid),
                        msg_writer:write_str(Iconid),
                        msg_writer:write_str(Equipmentres),
                        msg_writer:write_byte(Position),
                        msg_writer:write_byte(Itemtype),
                        msg_writer:write_int(Count),
                        msg_writer:write_str(Equipmenttype),
                        msg_writer:write_str(Grade),
                        msg_writer:write_int(Templateid),
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
                clientSideId = Clientsideid,
                command = Command} ->
                {
                    ?MSG_Command,
                    list_to_binary([
                        msg_writer:write_int(Clientsideid),
                        msg_writer:write_str(Command)])
                };
            #msg_CommandAck{
                clientSideId = Clientsideid,
                result = Result} ->
                {
                    ?MSG_CommandAck,
                    list_to_binary([
                        msg_writer:write_int(Clientsideid),
                        msg_writer:write_str(Result)])
                };
            #msg_Task{
                taskId = Taskid,
                taskState = Taskstate} ->
                {
                    ?MSG_Task,
                    list_to_binary([
                        msg_writer:write_int(Taskid),
                        msg_writer:write_int(Taskstate)])
                };
            #msg_ChangeTable{
                tableId = Tableid} ->
                {
                    ?MSG_ChangeTable,
                    list_to_binary([
                        msg_writer:write_int(Tableid)])
                };
            #msg_ChangeTableAck{
                tableId = Tableid} ->
                {
                    ?MSG_ChangeTableAck,
                    list_to_binary([
                        msg_writer:write_int(Tableid)])
                };
            #msg_AssignLeader{
                newLeaderId = Newleaderid} ->
                {
                    ?MSG_AssignLeader,
                    list_to_binary([
                        msg_writer:write_int(Newleaderid)])
                };
            #msg_AssignLeaderAck{
                resultId = Resultid} ->
                {
                    ?MSG_AssignLeaderAck,
                    list_to_binary([
                        msg_writer:write_int(Resultid)])
                };
            #msg_LockPlayer{
                lock = Lock} ->
                {
                    ?MSG_LockPlayer,
                    list_to_binary([
                        msg_writer:write_byte(Lock)])
                };
            #msg_WinnerNotif{
                forceId = Forceid} ->
                {
                    ?MSG_WinnerNotif,
                    list_to_binary([
                        msg_writer:write_int(Forceid)])
                };
            #msg_SpeakNotif{
                speakerId = Speakerid,
                content = Content} ->
                {
                    ?MSG_SpeakNotif,
                    list_to_binary([
                        msg_writer:write_int(Speakerid),
                        msg_writer:write_str(Content)])
                };
            #msg_Talk{
                targetId = Targetid} ->
                {
                    ?MSG_Talk,
                    list_to_binary([
                        msg_writer:write_int(Targetid)])
                };
            #msg_TalkAck{
                resultId = Resultid} ->
                {
                    ?MSG_TalkAck,
                    list_to_binary([
                        msg_writer:write_int(Resultid)])
                }
        end,
    list_to_binary([
        msg_writer:write_int(MsgId),
        msg_writer:write_int(size(Buff)),
        Buff]).

read_msg(Buff, MsgId) ->
    case MsgId of
        ?MSG_Login ->
            { Sessionkey, Buff1 } = msg_reader:read_str(Buff),
            { Userid, Buff2 } = msg_reader:read_int(Buff1),
            { Tableid, Buff3 } = msg_reader:read_int(Buff2),
            { Major, Buff4 } = msg_reader:read_int(Buff3),
            { Minor, Buff5 } = msg_reader:read_int(Buff4),
            { Revision, _ } = msg_reader:read_int(Buff5),
            #msg_Login{
                sessionKey = Sessionkey,
                userId = Userid,
                tableId = Tableid,
                major = Major,
                minor = Minor,
                revision = Revision};
        ?MSG_LoginAck ->
            { Errcode, Buff1 } = msg_reader:read_int(Buff),
            { Id, _ } = msg_reader:read_int(Buff1),
            #msg_LoginAck{
                errCode = Errcode,
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
            { Propertyid, Buff2 } = msg_reader:read_int(Buff1),
            { Value, Buff3 } = msg_reader:read_int(Buff2),
            { Skillid, Buff4 } = msg_reader:read_int(Buff3),
            { Modifierid, Buff5 } = msg_reader:read_int(Buff4),
            { Arg1, _ } = msg_reader:read_int(Buff5),
            #msg_PropChangedIntNotif{
                id = Id,
                propertyId = Propertyid,
                value = Value,
                skillId = Skillid,
                modifierId = Modifierid,
                arg1 = Arg1};
        ?MSG_PropChangedUintNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { Propertyid, Buff2 } = msg_reader:read_int(Buff1),
            { Value, Buff3 } = msg_reader:read_int(Buff2),
            { Skillid, Buff4 } = msg_reader:read_int(Buff3),
            { Modifierid, Buff5 } = msg_reader:read_int(Buff4),
            { Arg1, _ } = msg_reader:read_int(Buff5),
            #msg_PropChangedUintNotif{
                id = Id,
                propertyId = Propertyid,
                value = Value,
                skillId = Skillid,
                modifierId = Modifierid,
                arg1 = Arg1};
        ?MSG_PropChangedSingleNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { Propertyid, Buff2 } = msg_reader:read_int(Buff1),
            { Value, Buff3 } = msg_reader:read_single(Buff2),
            { Skillid, Buff4 } = msg_reader:read_int(Buff3),
            { Modifierid, Buff5 } = msg_reader:read_int(Buff4),
            { Arg1, _ } = msg_reader:read_int(Buff5),
            #msg_PropChangedSingleNotif{
                id = Id,
                propertyId = Propertyid,
                value = Value,
                skillId = Skillid,
                modifierId = Modifierid,
                arg1 = Arg1};
        ?MSG_ForceReposNotif ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { X, Buff2 } = msg_reader:read_single(Buff1),
            { Y, Buff3 } = msg_reader:read_single(Buff2),
            { Angle, Buff4 } = msg_reader:read_single(Buff3),
            { Speed, Buff5 } = msg_reader:read_single(Buff4),
            { Skillid, Buff6 } = msg_reader:read_int(Buff5),
            { Modifierid, _ } = msg_reader:read_int(Buff6),
            #msg_ForceReposNotif{
                id = Id,
                x = X,
                y = Y,
                angle = Angle,
                speed = Speed,
                skillId = Skillid,
                modifierId = Modifierid};
        ?MSG_CreatureProps ->
            { Id, _ } = msg_reader:read_int(Buff),
            #msg_CreatureProps{
                id = Id};
        ?MSG_CreaturePropsAck ->
            { Id, Buff1 } = msg_reader:read_int(Buff),
            { Name, Buff2 } = msg_reader:read_str(Buff1),
            { Exp, Buff3 } = msg_reader:read_int(Buff2),
            { Preexp, Buff4 } = msg_reader:read_int(Buff3),
            { Nextexp, Buff5 } = msg_reader:read_int(Buff4),
            { Level, Buff6 } = msg_reader:read_int(Buff5),
            { Hp, Buff7 } = msg_reader:read_single(Buff6),
            { Mp, Buff8 } = msg_reader:read_single(Buff7),
            { Health, Buff9 } = msg_reader:read_single(Buff8),
            { Healthregeneration, Buff10 } = msg_reader:read_single(Buff9),
            { Mana, Buff11 } = msg_reader:read_single(Buff10),
            { Manaregeneration, Buff12 } = msg_reader:read_single(Buff11),
            { Attackdamage, Buff13 } = msg_reader:read_single(Buff12),
            { Abilitypower, Buff14 } = msg_reader:read_single(Buff13),
            { Armor, Buff15 } = msg_reader:read_single(Buff14),
            { Armorpenetration, Buff16 } = msg_reader:read_single(Buff15),
            { Magicresistance, Buff17 } = msg_reader:read_single(Buff16),
            { Magicpenetration, Buff18 } = msg_reader:read_single(Buff17),
            { Criticalstrikechance, Buff19 } = msg_reader:read_single(Buff18),
            { Criticalstrikedamage, Buff20 } = msg_reader:read_single(Buff19),
            { Lifesteal, Buff21 } = msg_reader:read_single(Buff20),
            { Spellvamp, Buff22 } = msg_reader:read_single(Buff21),
            { Tenacity, Buff23 } = msg_reader:read_single(Buff22),
            { Range, Buff24 } = msg_reader:read_single(Buff23),
            { Movementspeed, Buff25 } = msg_reader:read_single(Buff24),
            { Attackspeed, Buff26 } = msg_reader:read_single(Buff25),
            { Cooldownreduction, Buff27 } = msg_reader:read_single(Buff26),
            { Gold, Buff28 } = msg_reader:read_int(Buff27),
            { Rmb, Buff29 } = msg_reader:read_int(Buff28),
            { Gm, _ } = msg_reader:read_int(Buff29),
            #msg_CreaturePropsAck{
                id = Id,
                name = Name,
                exp = Exp,
                preExp = Preexp,
                nextExp = Nextexp,
                level = Level,
                hp = Hp,
                mp = Mp,
                health = Health,
                healthRegeneration = Healthregeneration,
                mana = Mana,
                manaRegeneration = Manaregeneration,
                attackDamage = Attackdamage,
                abilityPower = Abilitypower,
                armor = Armor,
                armorPenetration = Armorpenetration,
                magicResistance = Magicresistance,
                magicPenetration = Magicpenetration,
                criticalStrikeChance = Criticalstrikechance,
                criticalStrikeDamage = Criticalstrikedamage,
                lifeSteal = Lifesteal,
                spellVamp = Spellvamp,
                tenacity = Tenacity,
                range = Range,
                movementSpeed = Movementspeed,
                attackSpeed = Attackspeed,
                cooldownReduction = Cooldownreduction,
                gold = Gold,
                rMB = Rmb,
                gM = Gm};
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
            { Precasttime, Buff4 } = msg_reader:read_single(Buff3),
            { Casttime, Buff5 } = msg_reader:read_single(Buff4),
            { Castingtime, Buff6 } = msg_reader:read_single(Buff5),
            { Cooldown, Buff7 } = msg_reader:read_single(Buff6),
            { Range, Buff8 } = msg_reader:read_single(Buff7),
            { Smallcd, Buff9 } = msg_reader:read_single(Buff8),
            { Middlecd, Buff10 } = msg_reader:read_single(Buff9),
            { Bigcd, Buff11 } = msg_reader:read_single(Buff10),
            { Mp, Buff12 } = msg_reader:read_int(Buff11),
            { Attackmin, Buff13 } = msg_reader:read_int(Buff12),
            { Attackmax, Buff14 } = msg_reader:read_int(Buff13),
            { Magicattackmin, Buff15 } = msg_reader:read_int(Buff14),
            { Magicattackmax, Buff16 } = msg_reader:read_int(Buff15),
            { Damage, Buff17 } = msg_reader:read_int(Buff16),
            { Arg1, Buff18 } = msg_reader:read_single(Buff17),
            { Arg2, Buff19 } = msg_reader:read_single(Buff18),
            { Arg3, Buff20 } = msg_reader:read_single(Buff19),
            { Arg4, _ } = msg_reader:read_single(Buff20),
            #msg_SkillPropsNotif{
                id = Id,
                class = Class,
                level = Level,
                preCastTime = Precasttime,
                castTime = Casttime,
                castingTime = Castingtime,
                cooldown = Cooldown,
                range = Range,
                smallCD = Smallcd,
                middleCD = Middlecd,
                bigCD = Bigcd,
                mP = Mp,
                attackMin = Attackmin,
                attackMax = Attackmax,
                magicAttackMin = Magicattackmin,
                magicAttackMax = Magicattackmax,
                damage = Damage,
                arg1 = Arg1,
                arg2 = Arg2,
                arg3 = Arg3,
                arg4 = Arg4};
        ?MSG_Casting ->
            { Skillid, Buff1 } = msg_reader:read_int(Buff),
            { Skillseq, Buff2 } = msg_reader:read_int(Buff1),
            { Targetid, Buff3 } = msg_reader:read_int(Buff2),
            { X, Buff4 } = msg_reader:read_single(Buff3),
            { Y, _ } = msg_reader:read_single(Buff4),
            #msg_Casting{
                skillId = Skillid,
                skillSeq = Skillseq,
                targetId = Targetid,
                x = X,
                y = Y};
        ?MSG_CastingNotif ->
            { Skillid, Buff1 } = msg_reader:read_int(Buff),
            { Skillseq, Buff2 } = msg_reader:read_int(Buff1),
            { Playerid, Buff3 } = msg_reader:read_int(Buff2),
            { Targetid, Buff4 } = msg_reader:read_int(Buff3),
            { X, Buff5 } = msg_reader:read_single(Buff4),
            { Y, Buff6 } = msg_reader:read_single(Buff5),
            { Lame, _ } = msg_reader:read_int(Buff6),
            #msg_CastingNotif{
                skillId = Skillid,
                skillSeq = Skillseq,
                playerId = Playerid,
                targetId = Targetid,
                x = X,
                y = Y,
                lame = Lame};
        ?MSG_CastedNotif ->
            { Skillid, Buff1 } = msg_reader:read_int(Buff),
            { Skillseq, Buff2 } = msg_reader:read_int(Buff1),
            { Playerid, Buff3 } = msg_reader:read_int(Buff2),
            { Targetid, Buff4 } = msg_reader:read_int(Buff3),
            { Resultid, _ } = msg_reader:read_byte(Buff4),
            #msg_CastedNotif{
                skillId = Skillid,
                skillSeq = Skillseq,
                playerId = Playerid,
                targetId = Targetid,
                resultId = Resultid};
        ?MSG_CastingAck ->
            { Skillid, Buff1 } = msg_reader:read_int(Buff),
            { Skillseq, Buff2 } = msg_reader:read_int(Buff1),
            { Errorid, _ } = msg_reader:read_int(Buff2),
            #msg_CastingAck{
                skillId = Skillid,
                skillSeq = Skillseq,
                errorId = Errorid};
        ?MSG_SkillCooldownNotif ->
            { Skillid, Buff1 } = msg_reader:read_int(Buff),
            { Cooldown, _ } = msg_reader:read_int(Buff1),
            #msg_SkillCooldownNotif{
                skillId = Skillid,
                cooldown = Cooldown};
        ?MSG_CreatureAppearNotif ->
            { Type, Buff1 } = msg_reader:read_int(Buff),
            { Career, Buff2 } = msg_reader:read_int(Buff1),
            { Gender, Buff3 } = msg_reader:read_byte(Buff2),
            { Name, Buff4 } = msg_reader:read_str(Buff3),
            { Id, Buff5 } = msg_reader:read_int(Buff4),
            { Userid, Buff6 } = msg_reader:read_int(Buff5),
            { X, Buff7 } = msg_reader:read_single(Buff6),
            { Y, Buff8 } = msg_reader:read_single(Buff7),
            { Angle, Buff9 } = msg_reader:read_single(Buff8),
            { Radius, Buff10 } = msg_reader:read_single(Buff9),
            { Movementspeed, Buff11 } = msg_reader:read_single(Buff10),
            { Health, Buff12 } = msg_reader:read_single(Buff11),
            { Hp, Buff13 } = msg_reader:read_single(Buff12),
            { Mana, Buff14 } = msg_reader:read_single(Buff13),
            { Mp, Buff15 } = msg_reader:read_single(Buff14),
            { Campid, Buff16 } = msg_reader:read_byte(Buff15),
            { Forceid, Buff17 } = msg_reader:read_byte(Buff16),
            { Attackable, Buff18 } = msg_reader:read_byte(Buff17),
            { Talkable, Buff19 } = msg_reader:read_byte(Buff18),
            { Monsterclass, Buff20 } = msg_reader:read_int(Buff19),
            { Tableid, Buff21 } = msg_reader:read_int(Buff20),
            { State, Buff22 } = msg_reader:read_int(Buff21),
            { Level, Buff23 } = msg_reader:read_int(Buff22),
            { Isbuilding, _ } = msg_reader:read_byte(Buff23),
            #msg_CreatureAppearNotif{
                type = Type,
                career = Career,
                gender = Gender,
                name = Name,
                id = Id,
                userId = Userid,
                x = X,
                y = Y,
                angle = Angle,
                radius = Radius,
                movementSpeed = Movementspeed,
                health = Health,
                hp = Hp,
                mana = Mana,
                mp = Mp,
                campId = Campid,
                forceId = Forceid,
                attackable = Attackable,
                talkable = Talkable,
                monsterClass = Monsterclass,
                tableId = Tableid,
                state = State,
                level = Level,
                isBuilding = Isbuilding};
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
            { Subtype, Buff6 } = msg_reader:read_int(Buff5),
            { Arg1, Buff7 } = msg_reader:read_int(Buff6),
            { Arg2, _ } = msg_reader:read_int(Buff7),
            #msg_OperatorAppearNotif{
                id = Id,
                type = Type,
                x = X,
                y = Y,
                angle = Angle,
                subType = Subtype,
                arg1 = Arg1,
                arg2 = Arg2};
        ?MSG_OperatorDisappearNotif ->
            { Id, _ } = msg_reader:read_int(Buff),
            #msg_OperatorDisappearNotif{
                id = Id};
        ?MSG_AddBuffNotif ->
            { Playerid, Buff1 } = msg_reader:read_int(Buff),
            { Buffid, Buff2 } = msg_reader:read_int(Buff1),
            { Bufflevel, Buff3 } = msg_reader:read_int(Buff2),
            { Bufftime, _ } = msg_reader:read_int(Buff3),
            #msg_AddBuffNotif{
                playerId = Playerid,
                buffId = Buffid,
                buffLevel = Bufflevel,
                buffTime = Bufftime};
        ?MSG_UpdateBuffNotif ->
            { Playerid, Buff1 } = msg_reader:read_int(Buff),
            { Buffid, Buff2 } = msg_reader:read_int(Buff1),
            { Bufflevel, Buff3 } = msg_reader:read_int(Buff2),
            { Bufftime, _ } = msg_reader:read_int(Buff3),
            #msg_UpdateBuffNotif{
                playerId = Playerid,
                buffId = Buffid,
                buffLevel = Bufflevel,
                buffTime = Bufftime};
        ?MSG_DelBuffNotif ->
            { Playerid, Buff1 } = msg_reader:read_int(Buff),
            { Buffid, _ } = msg_reader:read_int(Buff1),
            #msg_DelBuffNotif{
                playerId = Playerid,
                buffId = Buffid};
        ?MSG_MoveEquip ->
            { Sourceownerid, Buff1 } = msg_reader:read_int(Buff),
            { Positionfrom, Buff2 } = msg_reader:read_byte(Buff1),
            { Positionto, _ } = msg_reader:read_byte(Buff2),
            #msg_MoveEquip{
                sourceOwnerId = Sourceownerid,
                positionFrom = Positionfrom,
                positionTo = Positionto};
        ?MSG_MoveEquipAck ->
            { Errorid, Buff1 } = msg_reader:read_int(Buff),
            { Equipmentid, Buff2 } = msg_reader:read_str(Buff1),
            { Iconid, Buff3 } = msg_reader:read_str(Buff2),
            { Equipmentres, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { Itemtype, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { Equipmenttype, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { Templateid, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_MoveEquipAck{
                errorId = Errorid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special};
        ?MSG_MoveEquipNotif ->
            { Ownerid, Buff1 } = msg_reader:read_int(Buff),
            { Equipmentid, Buff2 } = msg_reader:read_str(Buff1),
            { Iconid, Buff3 } = msg_reader:read_str(Buff2),
            { Equipmentres, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { Itemtype, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { Equipmenttype, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { Templateid, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_MoveEquipNotif{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special};
        ?MSG_DropEquip ->
            { Position, _ } = msg_reader:read_byte(Buff),
            #msg_DropEquip{
                position = Position};
        ?MSG_DropEquipAck ->
            { Position, Buff1 } = msg_reader:read_byte(Buff),
            { Errorid, _ } = msg_reader:read_int(Buff1),
            #msg_DropEquipAck{
                position = Position,
                errorId = Errorid};
        ?MSG_UpdatedEquipNotif ->
            { Ownerid, Buff1 } = msg_reader:read_int(Buff),
            { Equipmentid, Buff2 } = msg_reader:read_str(Buff1),
            { Iconid, Buff3 } = msg_reader:read_str(Buff2),
            { Equipmentres, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { Itemtype, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { Equipmenttype, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { Templateid, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_UpdatedEquipNotif{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special};
        ?MSG_ListEquip ->
            { Ownerid, Buff1 } = msg_reader:read_int(Buff),
            { Bag, Buff2 } = msg_reader:read_byte(Buff1),
            { Equipment, _ } = msg_reader:read_byte(Buff2),
            #msg_ListEquip{
                ownerId = Ownerid,
                bag = Bag,
                equipment = Equipment};
        ?MSG_ListEquipAck ->
            { Ownerid, Buff1 } = msg_reader:read_int(Buff),
            { Equipmentid, Buff2 } = msg_reader:read_str(Buff1),
            { Iconid, Buff3 } = msg_reader:read_str(Buff2),
            { Equipmentres, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { Itemtype, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { Equipmenttype, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { Templateid, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_ListEquipAck{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special};
        ?MSG_ListEquipNotif ->
            { Ownerid, Buff1 } = msg_reader:read_int(Buff),
            { Equipmentid, Buff2 } = msg_reader:read_str(Buff1),
            { Iconid, Buff3 } = msg_reader:read_str(Buff2),
            { Equipmentres, Buff4 } = msg_reader:read_str(Buff3),
            { Position, Buff5 } = msg_reader:read_byte(Buff4),
            { Itemtype, Buff6 } = msg_reader:read_byte(Buff5),
            { Count, Buff7 } = msg_reader:read_int(Buff6),
            { Equipmenttype, Buff8 } = msg_reader:read_str(Buff7),
            { Grade, Buff9 } = msg_reader:read_str(Buff8),
            { Templateid, Buff10 } = msg_reader:read_int(Buff9),
            { Special, _ } = msg_reader:read_int(Buff10),
            #msg_ListEquipNotif{
                ownerId = Ownerid,
                equipmentId = Equipmentid,
                iconId = Iconid,
                equipmentRes = Equipmentres,
                position = Position,
                itemType = Itemtype,
                count = Count,
                equipmentType = Equipmenttype,
                grade = Grade,
                templateId = Templateid,
                special = Special};
        ?MSG_SimpleMessageNotif ->
            { N, _ } = msg_reader:read_int(Buff),
            #msg_SimpleMessageNotif{
                n = N};
        ?MSG_Command ->
            { Clientsideid, Buff1 } = msg_reader:read_int(Buff),
            { Command, _ } = msg_reader:read_str(Buff1),
            #msg_Command{
                clientSideId = Clientsideid,
                command = Command};
        ?MSG_CommandAck ->
            { Clientsideid, Buff1 } = msg_reader:read_int(Buff),
            { Result, _ } = msg_reader:read_str(Buff1),
            #msg_CommandAck{
                clientSideId = Clientsideid,
                result = Result};
        ?MSG_Task ->
            { Taskid, Buff1 } = msg_reader:read_int(Buff),
            { Taskstate, _ } = msg_reader:read_int(Buff1),
            #msg_Task{
                taskId = Taskid,
                taskState = Taskstate};
        ?MSG_ChangeTable ->
            { Tableid, _ } = msg_reader:read_int(Buff),
            #msg_ChangeTable{
                tableId = Tableid};
        ?MSG_ChangeTableAck ->
            { Tableid, _ } = msg_reader:read_int(Buff),
            #msg_ChangeTableAck{
                tableId = Tableid};
        ?MSG_AssignLeader ->
            { Newleaderid, _ } = msg_reader:read_int(Buff),
            #msg_AssignLeader{
                newLeaderId = Newleaderid};
        ?MSG_AssignLeaderAck ->
            { Resultid, _ } = msg_reader:read_int(Buff),
            #msg_AssignLeaderAck{
                resultId = Resultid};
        ?MSG_LockPlayer ->
            { Lock, _ } = msg_reader:read_byte(Buff),
            #msg_LockPlayer{
                lock = Lock};
        ?MSG_WinnerNotif ->
            { Forceid, _ } = msg_reader:read_int(Buff),
            #msg_WinnerNotif{
                forceId = Forceid};
        ?MSG_SpeakNotif ->
            { Speakerid, Buff1 } = msg_reader:read_int(Buff),
            { Content, _ } = msg_reader:read_str(Buff1),
            #msg_SpeakNotif{
                speakerId = Speakerid,
                content = Content};
        ?MSG_Talk ->
            { Targetid, _ } = msg_reader:read_int(Buff),
            #msg_Talk{
                targetId = Targetid};
        ?MSG_TalkAck ->
            { Resultid, _ } = msg_reader:read_int(Buff),
            #msg_TalkAck{
                resultId = Resultid};
        _ ->
            %io:format("receiving unknown msg~n"),
            void
    end.
