<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Data</NuGetReference>
</Query>

// parse parts of tales of maj'eyal to look for synergies
open FSharp.Data

module Helpers =
    
    let before (delim:string) (x:string) = x.[0.. x.IndexOf delim - 1 ]
    let after (delim:string) (x:string) = 
        let i = x.IndexOf delim + delim.Length
        x.[i.. ]
    let getIndexOf (delim:string) (x:string) =
        match x.IndexOf delim with
        | i when i >= 0 -> Some i
        | _ -> None

    let tryBefore delim x =
        getIndexOf delim x
        |> Option.map(fun i -> x.[0.. i - 1])
    
    let tryAfter delim x =
        getIndexOf delim x
        |> Option.map(fun i -> x.[ i + delim.Length .. ])
        
    let trim (x:string) = x.Trim()
    let (|Split2|_|) delim x =
        match tryBefore delim x, tryAfter delim x with
        | Some b, Some a -> Some (b,a)
        | _ -> None
    let tryParse f x =
        match f x with
        | true, v -> Some v
        | _ -> None
    let (|ParseDecimal|_|) (x:string) = x |> tryParse Decimal.TryParse
    let regGet title pattern f input =
            let m = Regex.Match(input, pattern)
            if m.Success then f m, input |> after m.Value
            else
                input.Dump("failing")
                failwith "could not find %s" title

open Helpers

type Category = {
    Broad: string
    Name: string
    Mastery: decimal
}
type Talent = {
    Name:string
    Points: int
    Cap: int // there are probably some that could invest less, should we sort by % complete?
    UseMode: string
    Is: string
    Description: string
    EffectiveLevel: decimal
}
type TalentEntry = {
    Category: Category
    Talents: Talent list // (string * string) list
}
module CharSheets =
    [<Literal>]
    let srcUrl =
        //"https://en.wikipedia.org/wiki/2017_FIA_Formula_One_World_Championship"
        //"https://te4.org/characters/145893/tome/719b1e9e-4480-4dea-a0e9-558b57b31527"
        "https://te4.org/characters/64849/tome/fbc079ed-79b7-49c2-861f-e4f7a55fc87e"

    // headers hold first category, followed by skills, then more categories
    type T4Char = HtmlProvider<srcUrl>
    let tryGetCat text masteryText =
        match text,masteryText with
        | Split2 "/" (b,n), ParseDecimal m ->
            Some {
                Broad = trim b
                Name = trim n
                Mastery = m
                }
        | _ -> None

    let getFirstCatHeaders (x:string[] option) =
        let h = Option.get x
        tryGetCat h.[0] h.[1]
        |> function
            | None -> failwithf "Could not get first cat headers from %A" x
            | Some y -> y
            
    // Effective talent level: 9.1 Use mode: Passive Is: a mind power Description: Each time you take damage, you roll 82% of your mental save against it. A successful saving throw can crit and will reduce the damage by at least 50%. The first talent point invested will also increase the amount of Psi you gain from Willpower by 0.5, but reduce the amount of life you gain from Constitution by 0.25. The first talent point also increases your solipsism threshold by 10% (currently 70%).Dismissal
    let extractTalent text v =
        

        let effective,text' = text |> regGet "effective talent level" "Effective talent level:\s+(\d+\.?\d?)\s+" (fun m -> m.Groups.[1].Value |> decimal)
        let useMode, text' = text' |> regGet "use mode" "Use mode:\s+(\w+)\s+" (fun m -> m.Groups.[1].Value)
        let is, text' = text' |> regGet "Is A" "Is:\s+([a-zA-CE-Z ]+)" (fun m -> m.Groups.[1].Value)
        let desc, text' = text' |> regGet "description" "Description:\s+(.*)\." (fun m -> m.Groups.[1].Value)
        
        //text,v
        let p,c = 
            match v with 
            | Split2 "/" (b,n) -> int b, int n
            | _ -> v.Dump("fail"); failwith "Failing to extract points"
        {
            Name= trim text'
            UseMode= useMode
            Is= is
            Description= desc
            EffectiveLevel= effective
            Points= p
            Cap= c
        }

    ("Effective talent level: 9.1 Use mode: Passive Is: a mind power Description: You believe that your mind is the center of everything. Permanently increases the amount of psi you gain per level by 5 and reduces your life rating (affects life at level up) by 50% (one time only adjustment). You also have learned to overcome damage with your mind alone, and convert 57% of all damage you receive into Psi damage and 57% of your healing and life regen now recovers Psi instead of life. Converted Psi damage you take will be further reduced by 47.1% (38.9% from character level with the remainder further reduced by 13.3% from talent level). The first talent point invested will also increase the amount of Psi you gain from Willpower by 0.5, but reduce the amount of life you gain from Constitution by 0.25. The first talent point also increases your solipsism threshold by 20% (currently 70%), reducing your global speed by 1% for each percentage your current Psi falls below this threshold.Solipsism","5/5")
    ||> extractTalent
    |> Dump
    |> ignore
    //failwith "stop"
    let foldTalents entries (text,v) =
        match entries, tryGetCat text v with
        | _, Some cat -> {Category=cat;Talents = List.empty}::entries
        | h::rem, _ -> {h with Talents= (extractTalent text v)::h.Talents}::rem
        | _ -> failwithf "not a cat, and no head cat"
        
    let getTalents firstCat rows =
        let toTE x = { Category = x; Talents = List.empty}
        (firstCat |> toTE |> List.singleton, rows)
        ||> Seq.fold foldTalents
        
    let getCharInfo (url:string) = 
        let classTalents = T4Char.Load(url).Tables.``Class Talents``
        classTalents.Rows 
        |> Seq.map (fun r -> r.Deconstruct())
        |> getTalents (getFirstCatHeaders classTalents.Headers)
        |> List.filter(fun x -> x.Talents |> List.exists(fun t -> t.Points > 0))
        |> List.sortByDescending(fun x -> x.Category.Mastery,x.Talents |> Seq.sumBy(fun t ->float t.Points / float t.Cap), x.Category.Broad)
        
let dumpCharSheetExample urlOpt =
    urlOpt
    |> Option.defaultValue CharSheets.srcUrl
    |> CharSheets.getCharInfo
    |> Dump
    |> ignore
    
dumpCharSheetExample None // (Some "https://te4.org/characters/145893/tome/719b1e9e-4480-4dea-a0e9-558b57b31527")