<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

let before (delimiter:string) (text:string) = text.Substring(0,text.IndexOf(delimiter))
let trim (text:string) = text.Trim()
let after (delimiter:string) (text:string) = text.Substring(text.IndexOf(delimiter) + delimiter.Length)

type Color = |Blue |Green |Black |White |Red
type Power = |Power of int |Multiplier
type Toughness = |Toughness of int |Multiplier
type Cost = |ColorCost of Color*int |ManaCost of int |ColorMultiplier  // empty list is valid for things that have no cost, or require solely tapping something
type Ability = string //TODO: flesh out abilities with a type rather than just text
type OnEntry = Ability
type OnUpkeep = Ability
type Tag = string //hold tags like Legendary,Human, Elf, Equipment, Artifact (Creature)
type Archetype = 
    |Creature of Power * Toughness * Tag list 
    |Land of Color list
    |Planeswalker of Toughness
    |Artifact of Ability * Tag list
    |Sorcery 
    |Enchantment of IsAura:bool
    |Instant //flash does not fall under this heading

type Card = {Name:string; Type:Archetype; Cost : (Cost list) ;Color:Color list; OnEntry:OnEntry option; OnUpkeep : OnUpkeep option; Ability: Ability option}
type Draw = |Draw
type Main = |Main
type Attack = |Attack
type Turn = |Turn of (Draw -> Main -> Attack -> Main)

module Green = //hold green cards
    let greenCard colorList name arch = {Name=name;Type=arch;Cost=colorList;Color=[Green];OnEntry = None; OnUpkeep = None; Ability = None} 
    let forest = Land([Green]) |> greenCard []
    let ``Axebane Stag`` = Creature(Power.Power 6,Toughness.Toughness 7,List.empty) |> greenCard [ColorCost (Green,1);ManaCost(6)]
    let ``Animist's Awakening`` = Sorcery

type Library = Card list
type Graveyard = Card list
type Exile = Card list
type Hand = Card list
type Player = {Hand:Hand;Library:Library;Graveyard:Graveyard ; Exile:Exile ;Life:int}
type Game = |Game of Player*Player

let readCards url =
    let htmlDoc = Util.Cache(fun () -> printfn "fetching fresh"; HtmlAgilityPack.HtmlWeb().Load url)
    //htmlWeb.Dump()
    let extractCost (infoElement:HtmlNode) = 
        let (|CMC|_|) (item:string) = 
            if item.StartsWith("CMC") then 
                match item.Substring(3) with |x when x |> Seq.forall Char.IsNumber -> Convert.ToInt32(x) |> ManaCost |> Some | "X" -> ColorMultiplier |> Some
            else None
        let (|Color|_|) (item:string) = 
            if item.StartsWith("Color ") then
                match item.Substring(6) with | "B" -> Black | "G" -> Green |"R" -> Red |"U" -> Blue |"W" -> White |x ->failwithf "Unknown color %A" x
                |> Some
            else None
        infoElement.ChildNodes 
        |> Seq.takeWhile(fun c-> c.Name <> "br")
        |> Seq.skipWhile (fun c -> c.Name <> "img") 
        |> Seq.filter (fun c -> c.Name="img")
        |> Seq.map (fun img -> match img.Attributes.["alt"].Value with |CMC cost -> cost |Color c -> ColorCost(c,1) |x -> failwithf "color image not matched %A in element %A" x img.OuterHtml)
        |> List.ofSeq
        
    let extractArch (infoElem:HtmlNode) =
        let (|LandText|_|) infoList = 
            match infoList with
            |["Land";"Gate"] -> Land [] |> Some
            |["Land"] -> Land [] |> Some
            |["Basic Land";"Forest"]
            |["Land";"Forest Plains"] -> Land [Green] |> Some
            |["Basic Land"; "Mountain"]
            |["Land"; "Mountain Forest"] -> Land [Red;Green] |> Some
            |["Basic Land"; "Island"] -> Land [Blue] |> Some
            |["Basic Land"; "Plains"] -> Land [White] |> Some
            |["Land"; "Plains Island"] -> Land [Blue;White] |> Some
            |["Basic Land";"Swamp"] -> Land [Black] |> Some
            |["Land";"Island Swamp"] -> Land [Blue;Black] |> Some
            |["Land"; "Swamp Mountain"] -> Land [Black;Red] |> Some
            | _ -> None
        let (|CreatureText|_|) infoList = 
            let extractPowerToughness text = 
                try
                    text 
                    |> after "(" 
                    |> before ")" 
                    |> fun t-> t.Split([| "/" |], StringSplitOptions.None)
                    |> List.ofSeq
                    |> function
                        | ["*";"*"] -> Power.Multiplier,Toughness.Multiplier
                        | ["*";toughness] -> Power.Multiplier, Toughness.Toughness (System.Int32.Parse toughness)
                        | [power;toughness] -> Power.Power (System.Int32.Parse power), Toughness.Toughness (System.Int32.Parse toughness)
                        | _ -> failwithf "Failed to extract list for %A" text
                with ex -> failwithf "Failed to extract for %A" text
            match infoList with
            |["Creature";info] -> (info,[]) |> Some
            |["Legendary Creature";info] -> (info,["Legendary"]) |> Some
            |["Artifact Creature";info] -> (info,["Artifact"]) |> Some 
            | _ -> None
            |> Option.bind( fun (info,tags) -> 
                let power,toughness = extractPowerToughness info
                Creature(power,toughness, tags)
                |> Some
            )
        infoElem.ChildNodes
        |> Seq.skipWhile(fun c-> c.Name <> "br")
        |> Seq.skip 1
        |> Seq.head
        |> fun elem -> elem.InnerText
        |> (fun text ->
            match text.Split([| "—" |],StringSplitOptions.None) |> Seq.map trim |> List.ofSeq with
            |["Planeswalker";typeInfo] -> typeInfo |> after "(" |> before ")" |> System.Int32.Parse |> Toughness.Toughness |> Planeswalker
            |["Enchantment";"Aura"] ->  Enchantment true
            |["Enchantment"] -> Enchantment false
            |["Sorcery"] -> Sorcery
            |["Artifact"] -> Artifact("Unknown", [])
            |["Artifact";"Equipment"] -> Artifact("Unknown",["Equipment"])
            |["Instant"] -> Instant
            |LandText l -> l
            |CreatureText(c) -> c
            
            |x -> failwithf "no match for %A in %s" x text
        )
        
    let articleElement = htmlDoc.GetElementbyId("WikiaArticle")//.Dump()
    articleElement.ChildNodes.First(fun e-> e.Id = "mw-content-text").ChildNodes
    |> Seq.find (fun e -> e.Name="table" && not <| isNull e.Attributes.["class"] && e.Attributes.["class"].Value.Contains("CardRow"))
    |> fun table -> table.Element("tbody")::(table.Elements("tr") |> List.ofSeq)
    |> Seq.choose (fun e -> if isNull e then None else Some e)
    |> Seq.map(fun cardRow -> cardRow.Elements("td") |> List.ofSeq)
    |> Seq.map (fun cardTds -> 
                match cardTds with
                | [_imageTd;infoTd;_originTd] -> 
                    try
                        infoTd.Element("a").InnerText, (* cost images *) extractCost infoTd, extractArch infoTd
                    with ex -> 
                        ex.Data.Add("InnerHtml",infoTd.InnerHtml)
                        reraise()
                    |> fun (desc, cost, arch) -> {Card.Type = arch; Card.Ability = None; Card.Cost= cost; Card.OnEntry= None; Card.OnUpkeep = None;Color=[]}
                | x -> failwithf "failed to decompose td list %A" x
            )

let cards = readCards "http://magicduels.wikia.com/wiki/Cards" |>List.ofSeq
cards |> Dump

let randomDeck seedOpt deckLimit =   
    let rnd =  match seedOpt with |Some x -> Random x | None -> Random()
    let len = Seq.length cards
    [1..deckLimit] 
    |> Seq.map (fun _ ->rnd.Next(len))  
    |> Seq.map (fun i -> cards.Item i)
    |> List.ofSeq
//randomDeck (Some 1) 60
////|> Dump

let randomPlayer seedOpt =
    let rnd =  match seedOpt with |Some x -> Random x | None -> Random()
    {Player.Library=randomDeck seedOpt 60;Hand=[];Exile= [];Graveyard= [];Life= 20}
randomPlayer (Some 1)
|> sprintf "%A"
|> Dump