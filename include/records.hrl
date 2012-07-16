-define(MSG_Login, 2000).
-define(MSG_LoginAck, 2001).
-define(MSG_Ping, 2003).
-define(MSG_PingAck, 2004).
-define(MSG_Move, 2010).
-define(MSG_MoveNotif, 2011).
-define(MSG_MoveAck, 2012).
-define(MSG_PropChangedIntNotif, 2013).
-define(MSG_PropChangedUintNotif, 2014).
-define(MSG_PropChangedSingleNotif, 2015).
-define(MSG_ForceReposNotif, 2016).
-define(MSG_CreatureProps, 2017).
-define(MSG_CreaturePropsAck, 2018).
-define(MSG_JumpNotif, 2019).
-define(MSG_SkillPropsNotif, 2020).
-define(MSG_Casting, 2022).
-define(MSG_CastingNotif, 2023).
-define(MSG_CastedNotif, 2024).
-define(MSG_CastingAck, 2025).
-define(MSG_SkillCooldownNotif, 2026).
-define(MSG_CreatureAppearNotif, 2030).
-define(MSG_CreatureDisappearNotif, 2031).
-define(MSG_CreatureOnNotif, 2032).
-define(MSG_CreatureOffNotif, 2033).
-define(MSG_OperatorAppearNotif, 2034).
-define(MSG_OperatorDisappearNotif, 2035).
-define(MSG_AddBuffNotif, 2040).
-define(MSG_UpdateBuffNotif, 2041).
-define(MSG_DelBuffNotif, 2042).
-define(MSG_MoveEquip, 2050).
-define(MSG_MoveEquipAck, 2060).
-define(MSG_MoveEquipNotif, 2070).
-define(MSG_DropEquip, 2080).
-define(MSG_DropEquipAck, 2081).
-define(MSG_UpdatedEquipNotif, 2110).
-define(MSG_ListEquip, 2120).
-define(MSG_ListEquipAck, 2130).
-define(MSG_ListEquipNotif, 2140).
-define(MSG_SimpleMessageNotif, 2150).
-define(MSG_Command, 3101).
-define(MSG_CommandAck, 3102).
-define(MSG_Task, 4120).
-define(MSG_ChangeTable, 4130).
-define(MSG_ChangeTableAck, 4131).
-define(MSG_AssignLeader, 4132).
-define(MSG_AssignLeaderAck, 4133).
-define(MSG_LockPlayer, 4200).
-define(MSG_WinnerNotif, 4300).
-define(MSG_SpeakNotif, 4401).
-define(MSG_Talk, 4402).
-define(MSG_TalkAck, 4403).

-record(msg_Login, {
    sessionKey,
    userId,
    tableId,
    major,
    minor,
    revision
    }).

-record(msg_LoginAck, {
    errCode,
    id
    }).

-record(msg_Ping, {
    data
    }).

-record(msg_PingAck, {
    data
    }).

-record(msg_Move, {
    state,
    x,
    y,
    angle
    }).

-record(msg_MoveNotif, {
    id,
    state,
    x,
    y,
    angle
    }).

-record(msg_MoveAck, {
    x,
    y,
    angle
    }).

-record(msg_PropChangedIntNotif, {
    id,
    propertyId,
    value,
    skillId,
    modifierId,
    arg1
    }).

-record(msg_PropChangedUintNotif, {
    id,
    propertyId,
    value,
    skillId,
    modifierId,
    arg1
    }).

-record(msg_PropChangedSingleNotif, {
    id,
    propertyId,
    value,
    skillId,
    modifierId,
    arg1
    }).

-record(msg_ForceReposNotif, {
    id,
    x,
    y,
    angle,
    speed,
    skillId,
    modifierId
    }).

-record(msg_CreatureProps, {
    id
    }).

-record(msg_CreaturePropsAck, {
    id,
    name,
    exp,
    preExp,
    nextExp,
    level,
    hp,
    mp,
    health,
    healthRegeneration,
    mana,
    manaRegeneration,
    attackDamage,
    abilityPower,
    armor,
    armorPenetration,
    magicResistance,
    magicPenetration,
    criticalStrikeChance,
    criticalStrikeDamage,
    lifeSteal,
    spellVamp,
    tenacity,
    range,
    movementSpeed,
    attackSpeed,
    cooldownReduction,
    gold,
    rMB,
    gM
    }).

-record(msg_JumpNotif, {
    id,
    x,
    y,
    angle
    }).

-record(msg_SkillPropsNotif, {
    id,
    class,
    level,
    preCastTime,
    castTime,
    castingTime,
    cooldown,
    range,
    smallCD,
    middleCD,
    bigCD,
    mP,
    attackMin,
    attackMax,
    magicAttackMin,
    magicAttackMax,
    damage,
    arg1,
    arg2,
    arg3,
    arg4
    }).

-record(msg_Casting, {
    skillId,
    skillSeq,
    targetId,
    x,
    y
    }).

-record(msg_CastingNotif, {
    skillId,
    skillSeq,
    playerId,
    targetId,
    x,
    y,
    lame
    }).

-record(msg_CastedNotif, {
    skillId,
    skillSeq,
    playerId,
    targetId,
    resultId
    }).

-record(msg_CastingAck, {
    skillId,
    skillSeq,
    errorId
    }).

-record(msg_SkillCooldownNotif, {
    skillId,
    cooldown
    }).

-record(msg_CreatureAppearNotif, {
    type,
    career,
    gender,
    name,
    id,
    userId,
    x,
    y,
    angle,
    radius,
    movementSpeed,
    health,
    hp,
    mana,
    mp,
    campId,
    forceId,
    attackable,
    talkable,
    monsterClass,
    tableId,
    state,
    level,
    isBuilding
    }).

-record(msg_CreatureDisappearNotif, {
    id
    }).

-record(msg_CreatureOnNotif, {
    name,
    id
    }).

-record(msg_CreatureOffNotif, {
    id
    }).

-record(msg_OperatorAppearNotif, {
    id,
    type,
    x,
    y,
    angle,
    subType,
    arg1,
    arg2
    }).

-record(msg_OperatorDisappearNotif, {
    id
    }).

-record(msg_AddBuffNotif, {
    playerId,
    buffId,
    buffLevel,
    buffTime
    }).

-record(msg_UpdateBuffNotif, {
    playerId,
    buffId,
    buffLevel,
    buffTime
    }).

-record(msg_DelBuffNotif, {
    playerId,
    buffId
    }).

-record(msg_MoveEquip, {
    sourceOwnerId,
    positionFrom,
    positionTo
    }).

-record(msg_MoveEquipAck, {
    errorId,
    equipmentId,
    iconId,
    equipmentRes,
    position,
    itemType,
    count,
    equipmentType,
    grade,
    templateId,
    special
    }).

-record(msg_MoveEquipNotif, {
    ownerId,
    equipmentId,
    iconId,
    equipmentRes,
    position,
    itemType,
    count,
    equipmentType,
    grade,
    templateId,
    special
    }).

-record(msg_DropEquip, {
    position
    }).

-record(msg_DropEquipAck, {
    position,
    errorId
    }).

-record(msg_UpdatedEquipNotif, {
    ownerId,
    equipmentId,
    iconId,
    equipmentRes,
    position,
    itemType,
    count,
    equipmentType,
    grade,
    templateId,
    special
    }).

-record(msg_ListEquip, {
    ownerId,
    bag,
    equipment
    }).

-record(msg_ListEquipAck, {
    ownerId,
    equipmentId,
    iconId,
    equipmentRes,
    position,
    itemType,
    count,
    equipmentType,
    grade,
    templateId,
    special
    }).

-record(msg_ListEquipNotif, {
    ownerId,
    equipmentId,
    iconId,
    equipmentRes,
    position,
    itemType,
    count,
    equipmentType,
    grade,
    templateId,
    special
    }).

-record(msg_SimpleMessageNotif, {
    n
    }).

-record(msg_Command, {
    clientSideId,
    command
    }).

-record(msg_CommandAck, {
    clientSideId,
    result
    }).

-record(msg_Task, {
    taskId,
    taskState
    }).

-record(msg_ChangeTable, {
    tableId
    }).

-record(msg_ChangeTableAck, {
    tableId
    }).

-record(msg_AssignLeader, {
    newLeaderId
    }).

-record(msg_AssignLeaderAck, {
    resultId
    }).

-record(msg_LockPlayer, {
    lock
    }).

-record(msg_WinnerNotif, {
    forceId
    }).

-record(msg_SpeakNotif, {
    speakerId,
    content
    }).

-record(msg_Talk, {
    targetId
    }).

-record(msg_TalkAck, {
    resultId
    }).

