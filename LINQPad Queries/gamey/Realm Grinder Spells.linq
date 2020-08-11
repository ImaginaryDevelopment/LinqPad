<Query Kind="FSharpProgram" />

// Realm Grinder spell bonus maximization problem solver
// spell data from https://realm-grinder.wikia.com/wiki/Spells

let buildingsOwned = 10_407
let ascension,reincarnation = 0,34
let excavations = 260
let dumpR: obj -> unit =
    let dc = DumpContainer()
    let mutable items = List.empty
    dc.Content <- items
    dc.Dump()
    (fun x ->
        items<-x::items
        dc.Content <- items
    )
    
module Option =
    let getOrDefault v= 
        function
        | Some x -> x
        | None -> v
        
module Alignment =
        
    
    type GoodAlly = | Dwarf
        with
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = x.ToString()
    type EvilAlly = | Drow
        with
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = x.ToString()
    type GoodRace = | Elven | Fairy | Angel
        with
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = x.ToStrin            
    type Good = GoodRace * (GoodAlly option)
    type Evil =
        | Goblin of EvilAlly option
        | Undead of EvilAlly option
        | Demon of EvilAlly option
        with
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = x.ToString()
    type Neutral =
        | Titan
        | Druid
        | Faceless
        with
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = x.ToString()
            
    type Align =
        | Good of       Good option
        | Neutral of    Neutral option
        | Evil of       Evil option
        with 
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = x.ToString()
    type GoodSpell = | GRace of GoodRace | GAlly of GoodAlly
    type NeutralSpell = Neutral
    type EvilSpell = | ERace of EvilRace | EAlly of EvilAlly
    type SpellAlign = 
        |GA of GoodSpell 
        |NA of NeutralSpell 
        |EA of EvilSpell 
    let (|IsGood|_|) = function | Good x -> Some x | _ -> None
    let (|IsEvil|_|) = function | Evil x -> Some x | _ -> None
    let (|IsNeutral|_|) = function | Neutral x -> Some x | _ -> None
    let (|FR|EL|AN|Dwairy|Dwelf|Dwangel|) =
        function
        | Fairy, None -> FR
        | Fairy, Some Dwarf -> Dwairy
        | Elven, None -> EL
        | Elven, Some Dwarf -> Dwelf
        | GoodRace.Angel, None -> AN
        | GoodRace.Angel, Some Dwarf -> Dwangel
    
    let pureFairy = (Fairy,None) |> Some |> Align.Good
    let dwairy = (Fairy, Some Dwarf) |> Some |> Align.Good

type Spell = {Name:string; Mana:int; Bonus:int; Align:Alignment.SpellAlign option}

let getSSBonus a r = Math.Pow(float 10_000 * Math.Pow(1.05, float r),(1.0 + 0.5 * float a))


let spells = 
    let all = None
    let allGood = Some (Alignment.Good None)
    let allEvil = Some (Alignment.Evil None)
    let allNeutral = Some (Alignment.Neutral None)
    [
        //{Name="TC"; Mana=200; Bonus =
        {Name="CtA"; Mana=400; Bonus= int <| Math.Pow(float buildingsOwned * 0.3,0.975); Align= all}
        {Name="SS"; Mana=2500; Bonus= getSSBonus (float ascension) reincarnation |> int; Align= all}
        {Name="Holy Light"; Mana=900; Bonus = 1750; Align = allGood}
        {Name="Blood Frenzy"; Mana=600; Bonus=1250; Align = allEvil}
        {Name="Gem Grinder"; Mana=1000; Bonus=5000; Align = allNeutral}
        {Name="Fairy Chanting"; Mana=1000; Bonus=50000; Align= Some (Alignment.pureFairy)}
        {Name="Diamond Pickaxe"; Mana=1000; Bonus= 0.25 * excavations; Align = }
    ]
    
dumpR spells

dumpR <| sprintf "Mana needed for all = %i" (spells |> Seq.sumBy (fun x -> x.Mana))

// get all combinations of n items picked from l
let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
let getIsSpellAllowed alignment spell =
    match spell,alignment with
    | {Align= None},_ -> true
    | {Align= Some x}, Some y when x = y -> true
    | {Align= Some (Alignment.IsGood _)}, Some (Alignment.IsGood _) -> true
    | _ -> false
let calcBonus = 
    List.fold(fun bonusOpt spell ->
        match bonusOpt with
        | Some bonus -> bonus
        | None -> 1.0
        |> (*) (float spell.Bonus)
        |> Some
    ) None
let getCastableCombos mana spells = 
    let combinations = comb 2 spells
    let r = 
        combinations
        |> List.map(fun x -> 
            x
            |> List.fold(fun (casts:Spell list) (spell:Spell) ->
                let spent = casts |> List.sumBy(fun s -> s.Mana)
                if mana - spent > spell.Mana then
                    let r:Spell list = spell::casts
                    r
                else casts
            ) List.empty
        )
    r  

let alignment = Some Alignment.dwairy
let mana = 5605

spells 
|> Seq.filter (getIsSpellAllowed alignment)
|> List.ofSeq
|> getCastableCombos mana
|> List.map (fun x -> calcBonus x |> Option.getOrDefault 0.0, x)
|> List.sortBy(fst >> ((*) -1.0))
|> List.map(fun (b,x) -> sprintf "%g" b,x)

|> dumpR
    
