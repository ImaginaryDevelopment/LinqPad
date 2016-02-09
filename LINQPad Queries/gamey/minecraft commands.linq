<Query Kind="FSharpExpression" />

//minecraft commands

// /tp maxinerd imaginarydev

// /setblock ~ ~ ~ minecraft:mob_spawner 0 replace {EntityId:Spider}

// /give imaginarydev minecraft:command_block 1
let players = ["ImaginaryDev";"Maxinerd"]
let blocks = ["mob_spawner"; "command_block"]
let getBlock () = Util.ReadLine("block type?", blocks.[1], blocks)
let entities = ["Spider";"Creeper"]
let getEntity () = Util.ReadLine("entity type?", entities.[0], entities)
    
match Util.ReadLine() with
| "tp" -> 
    let target = Util.ReadLine("tp whom?",players.[1], players)
    let toPlayer = Util.ReadLine("to whom?", players.[0],players.Except([ target]))
    sprintf "/tp %s %s" target toPlayer
| "setblock" ->
    let blockType = getBlock()
    let entity = getEntity()
    sprintf "/setblock ~ ~ ~ minecraft:%s 0 replace {EntityId:%s}" blockType entity