<Query Kind="FSharpProgram" />

let dumpt (t:string) x = 
    x.Dump(t)

type Trade = 
    | Armorcrafting
    | Metalworking
    | Magic
    | Jewelry
    | Woodworking
    | Weaponcrafting
    | RuneWriting
    | TexttileWorking
    | Tinkering
    | Alchemy
    | ArtsAndCrafts
    with override x.ToString() = sprintf "%A" x
    
    
module Workers =

    type Worker = {Name:string; Trades:Trade*Trade} with override x.ToString() = sprintf "%A" x
    let getTrades w = [fst w.Trades; snd w.Trades]
    let workers =
        [
            {Name="Armorer"         ;Trades=Trade.Metalworking, Trade.Armorcrafting}
            {Name="Sorceress"       ;Trades=Trade.Alchemy, Trade.RuneWriting}
            {Name="Jeweler"         ;Trades=Trade.Metalworking, Trade.Jewelry}
            {Name="Fletcher"        ;Trades=Trade.Weaponcrafting, Trade.ArtsAndCrafts}
            {Name="Luthier"         ;Trades=Trade.Woodworking, Trade.TexttileWorking}
            {Name="Armorer"         ;Trades=Trade.Metalworking, Trade.Armorcrafting}
            {Name="Blacksmith"      ;Trades=Trade.Metalworking, Trade.Weaponcrafting}
            {Name="Carpenter"       ;Trades=Trade.Woodworking, Trade.Weaponcrafting}
            {Name="Leatherworker"   ;Trades=Trade.TexttileWorking, Trade.Armorcrafting}
            {Name="Druid"           ;Trades=Trade.Woodworking, Trade.Alchemy}
            {Name="Tailor"          ;Trades=Trade.TexttileWorking, Trade.ArtsAndCrafts}
            {Name="Artificier"      ;Trades=Trade.Armorcrafting, Trade.Tinkering}
            {Name="Enchanter"       ;Trades= Trade.Magic, Trade.RuneWriting}
        ]
        
    let availTrades = workers |> Seq.map getTrades |> Seq.collect id |> Seq.distinct
    
    availTrades |> Seq.length
    |> dumpt "TradeCount"
    
    let score (elements:Worker list) =
        elements
        |> Seq.map getTrades
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.length
        |> (*) -1
    
    let has t = List.exists(fun x -> x = t)
    
    type WorkerCombo = 
        {Workers:string list; MissedTrades:Trade list} 
            
    let workerCombosByScore =
        seq{
            for i in [0.. workers.Length - 1] do
            for j in [i..workers.Length - 1] do
            for k in [j..workers.Length - 1] do
            for l in [k..workers.Length - 1] do
            for m in [l..workers.Length - 1] do
                let result = 
                    [workers.[i];workers.[j]; workers.[k]; workers.[l]; workers.[m]]
                    |> List.sortBy (fun w -> w.Name)
                yield result
        }
        |> Seq.sortBy score
        |> Seq.map (fun w -> 
                let names = w |> List.map (fun x -> x.Name)
                let trades = w |> Seq.map getTrades |> Seq.collect id
                {  Workers = names
                   MissedTrades = availTrades.Except(trades) (* |> Seq.map string *) |> List.ofSeq})
    //|> Seq.map (List.map(fun w -> w.Name))
                            
        |> Seq.take 20
        
module Items = 
    type ItemCategory = 
        | HeavyGlove
        | LightGlove
        | Thrown
        | Pistol
        | HeavyArmor
        | MediumArmor
        | LightArmor
        | HeavyBoot
        | LightBoot
        | Shield
        | Hammer
        | Axe
        | Instrument
        | Archery
        | Sword
        | HeavyHelm
        | Polearm
        | Ring
        | LightHelm
        | Knife
        | Potion
        | Herb
        | Stave
        | Necklace
        | Scroll
        with override x.ToString() = sprintf "%A" x
        
    type Quality =
        | Common
        | Good
        | Great
        | Flawless
        | Epic
        
        
    type Item = {Name:string; Category:ItemCategory; SkillsRequired: Trade Set; Level: System.Byte; EquipValueOpt: System.UInt16 option (*; Unlocks: Item seq *)}
    type Workers.WorkerCombo with
            static member CanCraft (w:Workers.WorkerCombo) (item:Item) =
                item.SkillsRequired
                |> Seq.exists (fun sr -> w.MissedTrades |> Seq.exists ((=) sr))
                |> not
                
    let ``hawk``=               {Level=16uy;    Name="Hawk";                Category= ItemCategory.Axe;             EquipValueOpt=None; SkillsRequired= Set [Trade.Metalworking; Trade.Woodworking; Trade.Weaponcrafting]}
    let ``fiery talisman``=     {Level=16uy;    Name="Fiery Talisman";      Category= ItemCategory.Necklace;        EquipValueOpt=None; SkillsRequired= Set [Trade.TexttileWorking; Trade.Magic; Trade.ArtsAndCrafts; Trade.Jewelry; Trade.RuneWriting]}
    let ``freezing scroll``=    {Level=16uy;    Name="Freezing Scroll";     Category= ItemCategory.Scroll;          EquipValueOpt=None; SkillsRequired= Set [Trade.TexttileWorking; Trade.Alchemy; Trade.Magic; Trade.ArtsAndCrafts; Trade.RuneWriting]}
    let ``shuriken`` =          {Level=18uy;    Name="Shuriken";            Category= ItemCategory.Thrown;          EquipValueOpt=None; SkillsRequired= Set [Trade.Metalworking; Trade.Weaponcrafting]}
    let ``double barrel`` =     {Level=19uy;    Name="Double Barrel";       Category= ItemCategory.Pistol;          EquipValueOpt=None; SkillsRequired= Set [Trade.Metalworking; Trade.Woodworking; Trade.Alchemy; Trade.Weaponcrafting; Trade.Tinkering] (*; Unlock=[]*)}
    let ``steel vambrace``=     {Level=19uy;    Name="Steel Vambrace";      Category= ItemCategory.HeavyGlove;      EquipValueOpt=None; SkillsRequired= Set [Trade.Metalworking; Trade.Armorcrafting]}
    let ``magic top``=          {Level=19uy;    Name="Magic Top";           Category= ItemCategory.LightHelm;       EquipValueOpt=None; SkillsRequired= Set [Trade.TexttileWorking; Trade.Magic; Trade.ArtsAndCrafts; Trade.RuneWriting]}
    let ``anti-venom``=         {Level=19uy;    Name="Anti-venom";          Category= ItemCategory.Herb;            EquipValueOpt=None; SkillsRequired= Set [Trade.Alchemy; Trade.Magic; Trade.RuneWriting]}
    let ``venomous buckler``=   {Level=20uy;    Name="Venomous Buckler";    Category= ItemCategory.Shield;          EquipValueOpt=None; SkillsRequired= Set [Trade.Woodworking; Trade.Alchemy; Trade.Armorcrafting]}

    let ``knight riders``=      {Level=21uy;    Name="Knight Riders";       Category= ItemCategory.HeavyBoot;       EquipValueOpt=None; SkillsRequired= Set [Trade.Metalworking; Trade.TexttileWorking; Trade.Armorcrafting]}
    let ``bear armor``=         {Level=21uy;    Name="Bear Armor";          Category= ItemCategory.MediumArmor;     EquipValueOpt=None; SkillsRequired= set [Trade.TexttileWorking; Trade.Armorcrafting; Trade.ArtsAndCrafts; Trade.Jewelry]}
    let ``life stealer``=       {Level=22uy;    Name="Life Stealer";        Category= ItemCategory.Knife;           EquipValueOpt=None; SkillsRequired= set [Trade.Metalworking; Trade.Magic; Trade.Weaponcrafting; Trade.RuneWriting]}
    let ``alchemist gloves`` =  {Level=22uy;    Name="Alchemist Gloves";    Category= ItemCategory.LightGlove;      EquipValueOpt=None; SkillsRequired= set [Trade.TexttileWorking; Trade.ArtsAndCrafts] (*; Unlock= []*)}
    let ``arcane guard``=       {Level=22uy;    Name="Arcane Guard";        Category= ItemCategory.HeavyArmor;      EquipValueOpt=None; SkillsRequired= set [Trade.Metalworking; Trade.TexttileWorking; Trade.Armorcrafting; Trade.ArtsAndCrafts; Trade.Jewelry]}
    let ``demolisher``=         {Level=25uy;    Name="Demolisher";          Category= ItemCategory.Hammer;          EquipValueOpt=None; SkillsRequired= set [Trade.Woodworking; Trade.Weaponcrafting]}
    
    let ``military tap``=       {Level=25uy;    Name="Military Tap";        Category = ItemCategory.Instrument;     EquipValueOpt=None; SkillsRequired= set [Trade.Woodworking; Trade.TexttileWorking; Trade.ArtsAndCrafts]}
    let ``scimitar``=           {Level=23uy;    Name="Scimitar";            Category = ItemCategory.Sword;          EquipValueOpt=None; SkillsRequired= set [Trade.Metalworking; Trade.Weaponcrafting]}
    
    let favorites = 
        [   ``alchemist gloves``
            ``double barrel``
            ``shuriken``
            ``arcane guard``
            ``knight riders``
            ``bear armor``
            ``venomous buckler``
            ``demolisher``
            ``steel vambrace``
            ``military tap``
            ``scimitar``
            ``life stealer``
            ``hawk``
            ``magic top``
            ``fiery talisman``
            ``freezing scroll``
            //``bone spear``
        ] |> set
        
    let favoritesByMinLevel=   
        favorites
        |> Seq.sortBy(fun i -> i.Level)
        
//    favoritesByMinLevel
//    |> Seq.map(fun i -> i.Name, i.Level, i.SkillsRequired |> List.map string)
//    |> dumpt "Skills required for favorites"

open Items

let optimizeForCategoryImprovement() = 
    let craftToLevelCategories = Items.favoritesByMinLevel |> Seq.take 6
    //let itemNeededSkills = craftToLevelCategories |> Seq.map (fun i -> i.SkillsRequired)  //|> Seq.collect id // |> Set.ofSeq
    //let workersByScore = 
    Workers.workerCombosByScore
    |> Seq.map(fun wc -> wc, craftToLevelCategories |> Seq.filter (Workers.WorkerCombo.CanCraft wc))
    |> Seq.sortBy (snd >> Seq.length >> (*) -1)
    |> Seq.map (fun (wc,craftable) -> wc.Workers, wc.MissedTrades |> Seq.map string, craftable |> Seq.map (fun c -> c.Name,c.Level))
    
optimizeForCategoryImprovement()
|> dumpt "Workers optimized for category improvement"

//    workerCombosByScore
//    |> Dump
//    |> ignore