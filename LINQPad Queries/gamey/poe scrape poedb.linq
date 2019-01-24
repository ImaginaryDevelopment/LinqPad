<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll">C:\projects\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>PathOfSupporting</Namespace>
</Query>

// scrape poedb.tw
let waitForAttach = false
open System.Net.Http
open HtmlAgilityPack
open PathOfSupporting.Parsing.Html
open PathOfSupporting.Parsing.Html.Impl.Html
open PathOfSupporting.Parsing.Html.PoeDb.Tw
open PathOfSupporting.Parsing.Html.PoeDb.Tw.Impl.Munge
open PathOfSupporting.Parsing.Trees.Gems

type Page = | Page of string

module Helpers =
    let delimit d x = String.Join(d,value=Array.ofSeq x)
    let fValString f = function | null | "" -> None | x -> f x |> Some
    let fValueString f = function | null | "" as x -> x | x -> f x
    let fMultiString f = fValueString (fun x ->
        x.Split([| "\r\n";"\n";"\r" |], StringSplitOptions.None)
        |> Seq.map f
        |> delimit "\r\n"
    )
    let indent = fMultiString (sprintf "  %s")
    let replace (d:string) r = fValueString(fun x -> x.Replace(d,r))
    let remove d = replace d String.Empty
    let rReplace dp rp = fValueString(fun x -> Regex.Replace(x,pattern=dp,replacement=rp))
    let rRemove dp = rReplace dp String.Empty
    let (|RMatch|_|) p x =
        let m = Regex.Match(x,p)
        if m.Success then Some m
        else None
    let trim = fValueString(fun x -> x.Trim())
    let afterOrSelf (d:string) (x:string) =
        let i = x.IndexOf d
        if i >= 0 then 
            x.[i+d.Length..]
        else x
    let beforeOrSelf (d:string) (x:string) =
        let i = x.IndexOf d
        if i >= 0 then
            x.[0..i - 1]
        else x
    let after (d:string) (x:string) =
        let i = x.IndexOf d
        x.[i+d.Length..]
    let before (d:string) (x:string) =
        let i = x.IndexOf d
        x.[0..i - 1]
    let containsI y =
        fValString(fun x -> x.Contains(y,StringComparison.InvariantCultureIgnoreCase)) >> Option.defaultValue false
    module Tuple2 =
        let mapSnd f (x,y) = x, f y
        
open Helpers
let parseNodeForChildren =
    sprintf "<span>%s</span>"
    >> HtmlAgilityPack.HtmlNode.CreateNode
    >> wrap 
    >> getChildNodes
module Async =
    let map f x =
        async{
            let! result = x
            return f result
        }
    let bind f x =
        async{
            let! result = x
            return! f result
        }
open PathOfSupporting.Parsing.Impl.FsHtml
let targets : (_*Page*_*string option) list =
    let openModalDiv i ident style =
        div [A.id <| sprintf "openModal%i" i;A.className "modalDialog"][
                div[][
                    a[A.href"#close";A.title "Close";A.className "close"] %("x")
                    div[yield A.id ident; if String.IsNullOrWhiteSpace style then () else yield "style"%=style ] %" Content "
                ]
            ]
    let genStandardCorruption ident =
        [div [A.className "item"] [ a [A.href "#openModal1000"] %("Corruption")
                                    openModalDiv 1000 ident String.Empty
            ]]
    let setThisColor c = sprintf "this.style.color='%s'" c
    let complex =
        let combos = // cn, an start, corruption, triple file name, enchantment
            [
                "Body Armour",Page "ch","chestcorr", Some "garb",None
                "Helmet",Page "hm","helmcorr",None,Some "hm-enchant.html"
                "Boots",Page "bt","corrboots",None,Some "gl-enchant.html"
                "Gloves",Page "gl","corrgloves",None,Some "gl-enchant.html"
                "Shield",Page "sh","corrshield",None,None
            ]
        let stats =[ "str","ar";"dex","ev";"int","es"]
        let genAn isShield items =
            let words = items |> List.map fst |> delimit"_"
            let an =  words |> sprintf "%s_armour"
            if isShield then
                match words with
                | "int" -> "focus"
                | x -> sprintf "%s_shield" x
                |> sprintf "%s,%s" an
            else an
        let genFn cn = List.map snd >> delimit String.Empty >> sprintf "%s-%s" cn
        let genComplex cn (Page slot) (corr,enchantOpt) items =  {cn=cn;an= items |> genAn (cn="Shield")},Page <| genFn slot items, genStandardCorruption corr,enchantOpt
        [
            for (cn,Page slot,corr, tripleName,enchOpt) in combos do
                match tripleName with
                | Some trip ->
                    yield {cn=cn;an=genAn (cn="Shield") stats}, Page <| sprintf "%s-%s" slot trip, genStandardCorruption corr,None
                | None -> ()
                
                for i in [0..stats.Length - 1] do
                    let stat1 = stats.[i]
                    yield genComplex cn (Page slot) (corr,enchOpt) [stat1]
                    for stat2 in stats.[i+1 ..] do
                        yield genComplex cn (Page slot) (corr,enchOpt) [stat1;stat2]
        ]
    [
        // poedb name, local file name, corruption element
        "Amulet","ac-amulet", 
                            [   div [A.className "item"] [
                                    a [A.href "#openModal1000";"onMouseOver"%= setThisColor "#06ef89";"onMouseOut"%=setThisColor"#000"] %("Talisman")
                                    openModalDiv 1000 "talismanimplicit" "text-align:left; line-height:20px; font-size:17px"
                                    a [A.href "#openModal0"] %("Corruption")
                                    openModalDiv 0 "corrammy" "text-align:left" ]]
        "Bow","2h-bow", genStandardCorruption "bowcorr"
        "Belt","ac-belt",genStandardCorruption "corrbelt"
        "Claw","1h-claw",genStandardCorruption "clawcorr"
        "Dagger","1h-dagger",genStandardCorruption "daggcorr"
        "FishingRod","2h-fish",genStandardCorruption "fishcorr"
        "One Hand Axe","1h-axe",genStandardCorruption "1haxecorr"
        "One Hand Mace","1h-mace",genStandardCorruption "1hmacecorr"
        "One Hand Sword","1h-sword",genStandardCorruption "1hswordcorr"
        "Quiver","ac-quiver",genStandardCorruption "corrquiv"
        "Ring","ac-ring",genStandardCorruption "corrring"
        "Sceptre","1h-sceptre",genStandardCorruption "sceptrecorr"
        "Staff","2h-staff",genStandardCorruption "staffcorr"
        "Two Hand Axe", "2h-axe",genStandardCorruption "2haxecorr"
        "Two Hand Mace","2h-mace",genStandardCorruption "2hmacecorr"
        "Two Hand Sword", "2h-sword", genStandardCorruption "2hswordcorr"
        "Wand","1h-wand", genStandardCorruption "wandcorr"
    ]
    |> List.map(fun (title,pg,corr) -> title, Page pg,corr)
    |> List.map(fun (x,pg,corr) -> {cn=x;an=null},pg,corr,None)
    |> List.append complex

open PathOfSupporting.Parsing.Html.PoeAffix
let getIds x=
    let rec getIt x =
        match x with
        | Text _ -> []
        | Comment _ -> []
        | Element (_,attrs,children) ->
            [
                let idAttrs= attrs |> Seq.choose (fun (Attr(name,v)) -> if name = "id" then Some v else None)
                yield! idAttrs
                yield! List.collect getIt children
            ]
    getIt x
let generateAffix i (item:AffixTierContainer<TieredAffix>) =
    let cleanAffixDisplay =
        let pattern = @"data-tooltip=""(<[^""]+)"" " 
        // unwrap values
        rReplace @"\+\((\d+&ndash;\d+)\)" "$1"
        >> afterOrSelf "Adds"
        >> fun x ->
            let fossilInfo =
                match x with
                |RMatch pattern m ->
                    m.Groups.[1].Value
                    |> Some
                | _ -> None
            fossilInfo, rReplace pattern String.Empty x
    let cleanMeta = 
        rReplace ".*\d" "<strong>$0</strong>"
    let generateAffixHead title =
        div[A.className "mod";"onclick"%= sprintf "toggle('mod%i')" i] [
            a[A.href "#/";"type"%="changecolor"] %(title)
        ]
            
    let generateAffixBlock {ILvl=ilvl;DisplayHtml=innerHtml;Tier=tier;Meta=meta} =
        let meta = cleanMeta meta
        let tier = tier |> replace "Tier " "T"
        let fossilOpt,display=cleanAffixDisplay innerHtml
        let attrs = [
            yield "data-ilvl"%=string ilvl
            match fossilOpt with
            | Some fo ->
                let fo =
                    parseNodeForChildren fo
                    |> Seq.filter(isNodeType HtmlNodeType.Element)
                    |> List.ofSeq
                    |> Seq.map(fun n -> sprintf "%s=%s" (getAttrValueOrNull "data-tooltip" n) (getInnerText n))
                    |> delimit"&#10;"
                yield "title"%=fo
            | None -> ()
        ]
        li attrs %(sprintf "iLvl %i: %s (%s)%s" ilvl display meta tier)
    
    let affixDescr= parseNodeForChildren item.Display |> Seq.map(getInnerText>>trim) |> Seq.filter(String.IsNullOrWhiteSpace >> not) |> delimit" "
    div ["data-descr"%=affixDescr;A.className "modWrapper";"data-modfor"%=string i][
        generateAffixHead item.Display
        div [A.id <| sprintf "mod%i" i;A.className "VAL"]
            [
                br[]
                ol [] (item.Children |> List.map generateAffixBlock)
            ]
    ]

type FixDivisor<'t> = {Prefixes:'t list;Suffixes:'t list} with
    static member map f x = {Prefixes = f x.Prefixes;Suffixes=f x.Suffixes}
    static member mapi f x = 
        let foldIx f (i:int) items =
            ((i,List.empty),items)
            ||> List.fold(fun (i,ixes) ix ->
                let (j,ix) = f i ix
                if j < i then invalidOp <| sprintf "i should have been greater or equal (%i, %i)" j i
                (j,ix :: ixes)
            )
            |> fun (i,items) -> i,List.rev items
            
        let i,prefixes = foldIx f 0 x.Prefixes
        let _i,suffixes = foldIx f i x.Suffixes
        {Prefixes = prefixes;Suffixes=suffixes}
    static member ofContainers x =
        let isPre x = x.EffixType |> containsI "prefix"
        let result = {Prefixes=List.filter isPre x;Suffixes=List.filter (isPre>>not) x}
//        (result.Prefixes.Length,result.Suffixes.Length).Dump("pref,suff")
        if result.Prefixes.Length = x.Length then invalidOp "bad filter"
        if result.Suffixes.Length = x.Length then invalidOp "bad filter"
        result
let guardIds title x =
    let allIds = getIds x
    let badSeeds =
        allIds
        |> Seq.groupBy id
        |> Seq.map (fun (x,y) -> x, Seq.length y)
        |> Seq.filter(fun (_,y) -> y > 1)
        |> List.ofSeq
    
    if Seq.exists(fun _ -> true) badSeeds then
        badSeeds.Dump(sprintf "%s is bad" title)
        (toString x).Dump()
        invalidOp "duplicate"
    x
let processOne =
    let generateAffixChild i (x:AffixContainer<AffixTierContainer<TieredAffix>>) =
        let j,children =
            ((i,List.empty),x.Children)
            ||> List.fold(fun (i,items) ix ->
                    let child = generateAffix i ix
                    i+1,child::items
                    
            )
            |> Tuple2.mapSnd List.rev
            
        j,{   EffixType = x.EffixType
              Children = children
                    
        }
    let toHtmlModBucket (x:FixDivisor<AffixContainer<Element>>) =
        let makeSideBucket (x:AffixContainer<Element>) =
            div [A.className "modContainer";"data-bucket"%=x.EffixType][
                yield br []
                yield div [A.className "seperator"][
                    strong [] %(x.EffixType)
                ]
                yield div [A.className "affix"] x.Children
            ]
        let left = x.Prefixes |> List.map makeSideBucket
        guardIds "left" (element "bah" [] left) |> ignore
        let right = x.Suffixes |> List.map makeSideBucket
        guardIds "right" (element "bah" [] right) |> ignore
        br [] :: left,br [] :: right
    let mapItemAffixContainer pg corruption enchOpt {ItemType=item;Children=children} =
        if waitForAttach then
            printfn "doing item %s,%i" item <| System.Diagnostics.Process.GetCurrentProcess().Id
            Util.ReadLine() |> ignore
        let left,right =
            children
                |> FixDivisor<_>.ofContainers
                |> FixDivisor<_>.mapi (generateAffixChild )
                |> toHtmlModBucket
        generateAffixPage item 
            {   Main=[h2[] %("Prefix")]
                Main2=[h2[] %(item)]
                Main3=[h2[] %("Suffix")]
                Corruption=corruption
                EnchantPage=enchOpt
                Left=left
                Right=right
                Updated=DateTime.UtcNow
            }
        |> guardIds "fin"
        |> toString
        |> sprintf "<!doctype html>\r\n%s"
        |> fun x -> File.WriteAllText(sprintf @"C:\projects\poeaffix.github.io\%s.html" pg,x)
                            
    fun (anCn,pg, corruption,enchOpt) ->
        anCn 
        |> parseModPhp
        |> Async.map(
            Result.map(List.map (mapItemAffixContainer pg corruption enchOpt))
        )
let wrapProcess (cn,Page pg,corr,enchOpt) =
    processOne (cn,pg,corr,enchOpt)
    |> Async.Catch
    |> Async.map(
        function
        |Choice1Of2 (Error e) -> Some e
        |Choice1Of2 _x -> None
        |Choice2Of2 y ->
            y.Data.Add("cn",box cn)
            y.Data.Add("pg",box pg)
            let pe =PathOfSupporting.Internal.Helpers.PoSException.Exception y
            Some("processOne error",Some pe) // PathOfSupporting.Internal.Helpers
    )
let findEnchantMatch (skills:Parsing.Trees.Gems.Gem list) (htmlText:string) =
    let inline isPair (sk:Gem) htmlDelim skillName = (htmlText |> containsI htmlDelim) && sk.Name=skillName
    skills
    |> List.tryFind(fun sk ->
        let inline isPair d sn = isPair sk d sn
    
        htmlText.Contains(value=sk.Name,comparison=StringComparison.InvariantCultureIgnoreCase)
        || (isPair "Charged Slam" "Tectonic Slam")
        || (isPair "Skeletons" "Summon Skeleton")
        || (isPair "Animated Guardian" "Animate Guardian")
        || (isPair "Animated Weapons" "Animate Weapon")
        || (isPair "Holy Relic" "Summon Holy Relic")
        || (isPair "Spectre" "Raise Spectre")
        || (isPair "Zombie" "Raise Zombie")
        || (isPair "Chaos Golem" "Summon Chaos Golem")
        || (isPair "Lightning Golem" "Summon Lightning Golem")
        || (isPair "Flame Golem" "Summon Flame Golem")
        || (isPair "Ice Golem" "Summon Flame Golem")
        || (isPair "Stone Golem" "Summon Stone Golem")
        || (isPair "Fire Nova" "Fire Nova Mine")
        || (isPair "Agony Crawler" "Herald of Agony")
        || (isPair "Sentinels of Dominance" "Dominating Blow")
        || (isPair "Sentinel of Dominance" "Dominating Blow")
        || (isPair "Sentinels of Purity" "Herald of Purity")
        || (isPair "Converted Enemies" "Conversion Trap")
        || (isPair "Raging Spirits" "Summon Raging Spirit")
    )
    |> Option.map(fun x -> x.Name)
    
let runHelmetEnchant() =
    let skills =
        match PathOfSupporting.Parsing.Trees.Gems.getSkillGems {ResourceDirectory="C:\projects\PathOfSupporting\PoS";Filename=None} with
        | Ok x -> x
        | Error e -> e.Dump("skills error"); List.empty
    PathOfSupporting.Internal.Helpers.Api.fetch "http://poedb.tw/us/mod.php?type=enchantment&an=helmet"
    |> Async.map (
        Result.map parse
        >> Result.map(fun n ->
            let n =
        
                n
                |> selectNodes ".//table//tbody/tr/td[2]"
                |> Seq.choose(fun x ->
                    let text = getInnerText x
                    if text ="&nbsp;" then None
                    else Some (findEnchantMatch skills text,x)
                )
                |> List.ofSeq
            (n |> Seq.filter(fst >> Option.isNone) |> Seq.map snd).Dump("unmatched")
            n
            |> Seq.choose(fun (x,y) -> x |> Option.map(fun x -> x,y))
            |> Seq.groupBy fst
            |> Seq.map(fun (k,items) -> k, items |> Seq.map snd |> Seq.map getInnerText)
            |> Seq.sortBy fst
        )
    )
    |> Async.RunSynchronously
    |> Dump
    |> ignore
let runAffixes() =
    targets
    |> List.map wrapProcess
    |> List.map Async.RunSynchronously
    |> List.choose id
    |> fun x ->
        Util.ClearResults()
        x.Dump(depth=1)
    |> ignore

let generateIndex() =
    html [] [
        PathOfSupporting.Parsing.Html.PoeAffix.generateHead "PoE Affix" []
        PathOfSupporting.Parsing.Html.PoeAffix.Index.generateIndexBody DateTime.UtcNow []
    ]
    |> guardIds "fin"
    |> toString
    |> sprintf "<!doctype html>\r\n%s"
    |> fun x -> File.WriteAllText(sprintf @"C:\projects\poeaffix.github.io\%s.html" "index",x)
generateIndex()
runHelmetEnchant()
//runAffixes()