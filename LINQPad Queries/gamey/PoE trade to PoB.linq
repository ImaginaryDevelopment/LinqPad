<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

// purpose: take a poe.trade page and make items that can be pasted into the create custom of poe.trade
// wip: start with weapons
// getting first 6 rows, not all items?

let debug = false
module Helpers =
    let replace (d:string) r =
        function
        | null | "" as x -> x
        | x -> x.Replace(d,r)
    let rReplace (d:string) (r:string) =
        function
        | null | "" as x -> x
        | x -> Regex.Replace(x,d,r)
    let contains (d:string) =
        function
        | null | "" -> false
        | x -> x.Contains d
    let truncate i x =
        match x with
        | null | "" -> x
        | x -> x.[0..i]
    let (|RMatch|_|) p x =
        let m = Regex.Match(x,pattern=p)
        if m.Success then Some m else None
    let (|MValue|) (m:Match) = m.Value
    let (|RMatches|_|) p x =
        let mc = Regex.Matches(x,pattern=p)
        if mc.Count > 0 && mc |> Seq.cast<Match> |> Seq.forall(fun m -> m.Success) then
            mc
            |> Seq.cast<Match>
            |> List.ofSeq
            |> Some
        else None
    let (|RsValue|_|) (i:int) (m:MatchCollection) =
        if m.Count>i then
            m.[i].Value |> Some
        else None
    let (|RValue|_|) (i:int) (m:Match) =
        if not m.Success then None
        elif i = 0 then m.Value |> Some
        elif m.Groups.Count >= i then m.Groups.[i].Value |> Some
        else None
    let delimit (d:string) items = String.Join(d,value=Array.ofList items)
        
open Helpers
module IO =
    let combine y x =
        Path.Combine(x,y)
    let combine' items x = x::items |> Array.ofList |> Path.Combine
module Html =
    open HtmlAgilityPack
    let getHasClass n (x:HtmlNode) =
        x.HasClass n
    let getChildren (x:HtmlNode) =
        x.ChildNodes
        |> Seq.cast<HtmlNode>
        |> List.ofSeq
    let prettify (x:HtmlNode) =
        let toRemove = ResizeArray()
        x.DescendantsAndSelf()
        |> Seq.iter(fun x ->
            if x.NodeType = HtmlNodeType.Text && String.IsNullOrWhiteSpace x.InnerText then
                toRemove.Add x
        )
        toRemove
        |> Seq.iter(fun x -> x.Remove())
        x
        
    module Impl =
        let removeComments (x:HtmlNode) =
            let i' = ref 0
            x.SelectNodes(".//comment()")
            |> function
                |null -> ()
                | x -> x
                    |> Seq.iteri(fun i x ->
                        i' := i
                        x.Remove())
            printfn "removed %i comments" (!i' + 1)
            x
        open IO
        // not exactly cache, not exactly perm
        let getRawPath targetDirPath =
            if debug then
                printfn "getting cachie"
            let p = targetDirPath |> combine "page.html"
            if Path.GetDirectoryName p |> Directory.Exists |> not then
                p |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
                printfn "Created dir %s" <| Path.GetDirectoryName p
            p
        let getDoc text =
            let hd = HtmlDocument()
            hd.LoadHtml text
            hd
        let getDescendant (name:string) (node:HtmlNode) =
            node.Descendants name
            |> Seq.tryHead
        let removeNodes title (items:HtmlNode seq) = 
            match items with
            | null -> ()
            | items -> 
                let items = List.ofSeq items
                if debug then printfn "found %i %s" items.Length title
                items
                |> List.iter(fun x -> x.Remove())
        let getAll (doc:HtmlDocument) (nodeType:string) =
            doc.DocumentNode.Descendants nodeType
        let getMungePath targetDirPath =
            IO.combine "munged.html" targetDirPath
        let targetFolder =
            Environment.GetFolderPath Environment.SpecialFolder.ApplicationData
            |> combine "poe.trade"
        let rawPath = lazy(getRawPath targetFolder)
        let mungedPath = lazy(getMungePath targetFolder)
        if debug then printfn "made lazy cachie"
    open Impl
    let writeCachie contents = 
        File.WriteAllText(rawPath.Value,contents)
    let munge() =
        let cachie= rawPath.Value
        cachie.Dump("target file")
        let mungePath = Path.GetDirectoryName cachie |> getMungePath
        let doc,hDump =
            let html = 
                File.ReadAllText cachie
                |> replace "/static/" "http://poe.trade/static/"
            getDoc html, fun () -> html.Dump("raw")
        try
            getAll doc "script" |> removeNodes "script"
            getAll doc "iframe" |> removeNodes "iframe"
            doc.DocumentNode.SelectSingleNode("//div[@id='dynamic']")
            |> function
                |null -> printfn "dynamic not found"
                |x -> x.Remove()
            doc.DocumentNode.SelectNodes("//comment()")
            |> Seq.iter(fun x -> x.Remove())
            doc.DocumentNode |> prettify |> ignore
            getDescendant "head" doc.DocumentNode
            |> function
                |None ->
                    if debug then eprintfn"Headless?"
                | Some h ->
                    getDescendant "base" h
                    |> function
                        |None ->
                            let baseNode=HtmlNode(HtmlNodeType.Element,h.OwnerDocument,h.ChildNodes.Count)
                            baseNode.Name<-"base"
                            baseNode.Attributes.Add("href","http://poe.trade/search/foobar/")
                            baseNode.Attributes.Add("target","_blank")
                            h.AppendChild baseNode
                            |> ignore<HtmlNode>
                        |Some b ->
                            if debug then printfn "base: %A" b.Attributes
            doc.Save mungePath
        with _ ->
            hDump()
            reraise()
            
        doc
    let clearMunge() = 
        let mp = mungedPath.Value
        if File.Exists mp then mp |> File.Delete
    let getDocOrMunge() =
        let doc = 
            if File.Exists mungedPath.Value |> not then
                munge()
            else mungedPath.Value |> File.ReadAllText |> getDoc
        if debug then 
            Process.Start mungedPath.Value |> ignore
            doc
            |> fun x -> x.DocumentNode
            |> removeComments
            |> getDescendant "body"
            |> Option.map prettify
            |> function
                |Some x -> x.OuterHtml
                |None -> doc.DocumentNode.OuterHtml
            |> truncate 255
            |> fun x -> x.Dump("result")
            |> ignore
            
            match doc.DocumentNode.InnerHtml with
            | _ -> ()
        doc
    let getAttrValue name (x:HtmlNode) =
        x.Attributes
        |> Seq.tryFind(fun x -> x.Name=name)
        |> Option.map(fun x -> x.Value)
        |> Option.defaultValue null
let (|Words|_|) =
    function
    | RMatches "([\w-]+)" m ->
        Some m
    | _ -> None

let (|FullName|Contracted|NoName|) =
    function
    | Words words ->
        match words with
        | MValue prefix :: MValue suffix :: MValue baseName::[] ->
            Contracted ((prefix,suffix),baseName)
        | MValue prefix :: MValue suffix :: MValue basePrefix :: MValue baseSuffix::[] ->
            FullName ((prefix,suffix),(basePrefix,baseSuffix))
        | _ -> NoName
    | _ -> NoName
    
let (|NameBase|) =
    function
    | null | "" -> null,null
    // "Woe Barb Jewelled Foil"
    | FullName ((p,s),(bp,bs)) ->
        sprintf "%s %s" p s, sprintf "%s %s" bp bs
    | Contracted ((p,s),bn) ->
        sprintf "%s %s" p s, bn
    | x ->
        eprintfn "No name? %s" x
        null,null
let printCount title x =
    printfn "%s has %i item(s)" title (Seq.length x)
    x
let getItems (doc:HtmlAgilityPack.HtmlDocument) =
    doc.DocumentNode.Descendants()
    |> printCount "Desc"
    |> Seq.filter(fun x -> x.Name = "table")
    |> printCount "tables"
    |> Seq.filter(Html.getAttrValue "id" >> contains "search-results")
    |> Seq.collect(Html.getChildren >> List.filter(fun cn -> cn.Name="tbody"))
    
type ModType =
    |Implicit
    |Prefix
    |Suffix
    with member private x.ToDump() = sprintf "%A" x
// text field should exactly match what would go into PoB I think?
type Mod = {Attrib:string;Value:string;ModType:ModType option;Text:string;Raw:string}
let getItemMods=
    Html.getChildren
    >> List.choose(fun td -> td.Descendants() |> Seq.tryFind(fun n -> n.Name="ul" && n.HasClass "item-mods"))
    >> List.collect Html.getChildren
    >> List.collect Html.getChildren
    >> List.collect (fun ul ->
        if not <| Html.getHasClass "mods" ul then failwithf"bad node %s" ul.OuterHtml
        let modType = if Html.getHasClass "withline" ul then Some Implicit else None
        ul
        |> Html.getChildren
        |> List.map(fun x ->
            match modType with
            |Some mt -> Some mt
            | None ->
                let affixSpan = x.SelectSingleNode("*[contains(@class,'item-affix')]") |> Option.ofObj
                let mt = affixSpan |> Option.bind(fun x ->
                            if x.HasClass "item-affix-S" then Some Suffix
                            elif x.HasClass "item-affix-P" then Some Prefix
                            else None
                        )
                mt
            |> fun mt ->
                let attrib = Html.getAttrValue "data-name" x
                let raw = x.OuterHtml
                let v = x.ChildNodes |> Seq.tryFind(fun cn -> cn.Name = "b") |> Option.map(fun cn -> cn.InnerText) |> Option.defaultValue null
                // trim displayText by way of mutation
                let text = 
                    x.ChildNodes
                    |> Seq.filter(fun cn -> cn.Name="span")
                    |> List.ofSeq
                    |> List.iter(fun cn -> cn.Remove())
                    x.InnerText
                
                {ModType=mt;Attrib=attrib;Value=v;Text=text;Raw=raw}
        )
    )
type Item = {Name:string;Base:string;ItemQ:string;Mods:Mod list; Meta:string; Rows:string list;Raw:string}

let mapItem x=
    match Html.getAttrValue "data-name" x with
    | null -> x.OuterHtml.Dump("failing"); failwithf "Bad item"
    | NameBase (n,b) ->
        let rows = Html.getChildren x
        let metaRow = rows.[1]
        // this stuff is for dps, phy dps, armour, energy shield, etc...
        let _findQualityNodes classNames (x:HtmlAgilityPack.HtmlNode) =
            let selector = sprintf "*[@data-value,(%s)]" (delimit "|" classNames)
            x.SelectNodes selector
        let _getQualityValue className x =
            if Html.getAttrValue "data-name" x = className then
               Html.getAttrValue "data-value" x
            else null
            
//        let qualities = findQualityNodes ["quality_shield";"quality_armour";
        let iq = 
            match rows.[0].SelectSingleNode(".//*[@data-name='q']") with
            | null -> "0"
            | td -> Html.getAttrValue "data-value" td
        ()
        let mods = getItemMods rows.[0]// |> List.sortBy(fun x -> )
        {Name=n;Base=b;ItemQ=iq;Mods=mods;Meta=metaRow.OuterHtml;Rows=rows |> List.map(fun x -> x.OuterHtml);Raw=x.OuterHtml}
let toPoB {Name=n;Base=b;Mods=mods} =
//    type Mod = {Attrib:string;Value:string;ModType:ModType option;Text:string;Raw:string}
    let getModText {Text=t}=
        t
        
    [ n;b;]@(mods |> List.map getModText)
    |> delimit "\r\n"
if not <| File.Exists Html.Impl.rawPath.Value then
    Util.ReadLine("copy html to the clipboard and hit enter here")
    |> ignore
    System.Windows.Forms.Clipboard.GetText()
    |> Html.writeCachie

Html.clearMunge()
Html.getDocOrMunge()
|> getItems
|> List.ofSeq
|> List.map mapItem
//    |> Seq.map(fun x -> x.OuterHtml)
//|> Dump
|> List.map toPoB
|> Dump
|> ignore