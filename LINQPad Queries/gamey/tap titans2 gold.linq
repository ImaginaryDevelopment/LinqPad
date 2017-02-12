<Query Kind="FSharpProgram" />

// tap titans 2 maths
///http://stackoverflow.com/questions/105770/net-string-format-to-add-commas-in-thousands-place-for-a-number
let format fmt (d:double) = d.ToString(format=fmt)
[<AutoOpen>]
module Formula =
    let getTitanHp hpModifier stage = 
        let stage = float stage
        17.5 * Math.Pow(1.39,Math.Min(stage,115.)) * Math.Pow(1.13,Math.Max(stage - 115.,0.)) * hpModifier
    
    let getBossConstant stage = 
        [2;3;4;5;8].[(stage - 1) % 5]
        
    let getBossHp hpModifier stage = 
        let constant = getBossConstant stage
        float constant * getTitanHp hpModifier stage * Math.Min(2.5,Math.Pow(1.1, float stage / 200.))
    let getBaseGold hpModifier goldBonus atBoss stage = 
        // should this be titan or boss? not sure
        let monsterHp = 
            stage 
            |> (if atBoss then getBossHp else getTitanHp) hpModifier
        (monsterHp * 0.008 + 0.0002 * Math.Min(float stage, 150.)) * goldBonus
    let getTitanGold titanGoldBonus hpModifier baseGold=  baseGold * titanGoldBonus
    let getBossGold bossGoldBonus hpModifier baseGold = baseGold * 3. * bossGoldBonus
    
type StageInfo = { Stage:int; TitanHp:double; BossConstant:int; BossHp: double; BaseTitanGold: double; BaseBossGold: double; TitanGold:double; BossGold:double} 

// 3-4 coins drop per kill, so it may look like titan gold is different, plus there's likely rounding in the ui display on the coin amounts
let stageInfoFromStage hpModifier goldBonus titanGoldBonus bossGoldBonus stage = 
    let getBaseGold = getBaseGold hpModifier goldBonus
    let baseTGold,baseBGold = getBaseGold false stage, getBaseGold true stage
    {
        Stage= stage
        TitanHp = getTitanHp hpModifier stage
        BossConstant = getBossConstant stage
        BossHp = getBossHp hpModifier stage
        BaseTitanGold= baseTGold
        BaseBossGold= baseBGold
        TitanGold = getTitanGold hpModifier titanGoldBonus baseTGold
        BossGold = getBossGold hpModifier bossGoldBonus baseBGold
    }
 //   let 
 
//titansHp 50 |> format "n"
//[1..10]
//|> Seq.map getBossConstant
//[1..10]
//|> Seq.map (getBossesHp >> format "n")
//|> Dump
//|> ignore
// these results looked shady
//[1..12]
//|> Seq.map (getBaseGold true 1.)

let goldBonus = 1. * 1.5 (* bubbles *) * 1.29 (* polly *) * 11.0 (* legendary chest*)
let titanGoldBonus = 1.
let bossGoldBonus = 1.
let hpModifier = 1.
[
    [1..15]
    [31;32]
    
    [100]
    [800]
]
|> Seq.collect id
|> Seq.map (stageInfoFromStage hpModifier goldBonus titanGoldBonus bossGoldBonus)
|> Dump
