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
let targets =
    let openModalDiv i ident style =
        div [A.id <| sprintf "openModal%i" i;A.className "modalDialog"][
                div[][
                    a[A.href"#close";A.title "Close";A.className "close"] %("x")
                    div[yield A.id ident; if String.IsNullOrWhiteSpace style then () else yield "style"%=style ] %" Content "
                ]
            ]
    let genStandardCorruption ident =
        [div [A.id "item"] [    a [A.href "#openModal1000"] %("Corruption")
                                openModalDiv 1000 ident String.Empty
            ]]
    let setThisColor c = sprintf "this.style.color='%s'" c
    let complex = [
        {cn="Body Armour";an="str_armour"},"ch-ar", genStandardCorruption"chestcorr"
        {cn="Body Armour";an="dex_armour"},"ch-ev", genStandardCorruption"chestcorr"
        {cn="Body Armour";an="int_armour"},"ch-es", genStandardCorruption"chestcorr"
    ]
    [
        // poedb name, local file name, corruption element
        "Amulet","ac-amulet", 
                            [   div [A.id "item"] [
                                    a [A.href "#openModal1000";"onMouseOver"%= setThisColor "#06ef89";"onMouseOut"%=setThisColor"#000"] %("Talisman")
                                    openModalDiv 1000 "talismanimplicit" "text-align:left; line-height:20px; font-size:17px"
                                    a [A.href "#openModal0"] %("Corruption")
                                    openModalDiv 0 "corrammy" "text-align:left" ]]
        "Bow","2h-bow", genStandardCorruption "bowcorr"
        "Claw","1h-claw",genStandardCorruption "clawcorr"
        "Dagger","1h-dagger",genStandardCorruption "daggcorr"
        "FishingRod","2h-fish",genStandardCorruption "fishcorr"
        "One Hand Axe","1h-axe",genStandardCorruption "1haxecorr"
        "One Hand Mace","1h-mace",genStandardCorruption "1hmacecorr"
        "One Hand Sword","1h-sword",genStandardCorruption "1hswordcorr"
        "Ring","ac-ring",genStandardCorruption "corrring"
        "Sceptre","1h-sceptre",genStandardCorruption "sceptrecorr"
        "Staff","2h-staff",genStandardCorruption "staffcorr"
        "Two Hand Axe", "2h-axe",genStandardCorruption "2haxecorr"
        "Two Hand Mace","2h-mace",genStandardCorruption "2hmacecorr"
        "Two Hand Sword", "2h-sword", genStandardCorruption "2hswordcorr"
        "Wand","1h-wand", genStandardCorruption "wandcorr"
    ]
    |> List.map(fun (x,pg,corr) -> {cn=x;an=null},pg,corr)
    |> List.append complex
    // just get the one for now
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
        // unwrap values
        rReplace @"\+\((\d+&ndash;\d+)\)" "$1"
        >> afterOrSelf "Adds"
    let cleanMeta = 
        rReplace ".*\d" "<strong>$0</strong>"
    let generateAffixHead title =
        div[A.className "mod";"onclick"%= sprintf "toggle('mod%i')" i] [
            a[A.href "#/";"type"%="changecolor"] %(title)
        ]
            
    let generateAffixBlock {ILvl=ilvl;DisplayHtml=innerHtml;Tier=tier;Meta=meta} =
        let meta = cleanMeta meta
        let tier = tier |> replace "Tier " "T"
        li [] %(sprintf "iLvl %i: %s (%s)%s" ilvl (cleanAffixDisplay innerHtml) meta tier)
    
    div [A.className "modWrapper"][
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
        let i,suffixes = foldIx f i x.Suffixes
        printfn "Finished mapi index Count=%i" i
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
            div [A.className "modContainer"][
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
    let mapItemAffixContainer pg corruption {ItemType=item;Children=children} =
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
//                          <div id="item"><a href="#openModal1000" onMouseOver="this.style.color='#06ef89'"
//   onMouseOut="this.style.color='#000'">Talisman</a>
                Corruption=corruption
                Left=left
                Right=right
            }
        |> guardIds "fin"
        |> toString
        |> sprintf "<!doctype html>\r\n%s"
        |> fun x -> File.WriteAllText(sprintf @"C:\projects\poeaffix.github.io\%s.html" pg,x)
                            
    fun (anCn,pg, corruption) ->
        anCn 
        |> parseModPhp
        |> Async.map(
            Result.map(List.map (mapItemAffixContainer pg corruption))
    )
let wrapProcess (cn,pg,corr) =
    processOne (cn,pg,corr)
    |> Async.Catch
    |> Async.map(
        function
        |Choice1Of2 _x -> null
        |Choice2Of2 y -> 
            y.Data.Add("cn",box cn)
            y.Data.Add("pg",box pg)
            y)
let runAll() =
    targets
    |> List.map wrapProcess
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Dump
    |> ignore

let generateIndex() =
    html [] [
        PathOfSupporting.Parsing.Html.PoeAffix.generateHead "PoE Affix" []
        PathOfSupporting.Parsing.Html.PoeAffix.Index.generateIndexBody []
    ]
    |> guardIds "fin"
    |> toString
    |> sprintf "<!doctype html>\r\n%s"
    |> fun x -> File.WriteAllText(sprintf @"C:\projects\poeaffix.github.io\%s.html" "index",x)
generateIndex()
