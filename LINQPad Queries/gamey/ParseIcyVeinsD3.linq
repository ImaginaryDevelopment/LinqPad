<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
  <Namespace>Microsoft.FSharp.Core</Namespace>
  <Namespace>System.Net.Http</Namespace>
  <DisableMyExtensions>true</DisableMyExtensions>
</Query>

let debug = false
let icic = StringComparison.InvariantCultureIgnoreCase
type System.String with
    static member trim (s:string) = s.Trim()
    static member after (delimiter:string) (s:string) = 
        s.Substring <| s.IndexOf delimiter + delimiter.Length |> String.trim
    static member containsI (d:string) (x:string) =
        x.IndexOf(d,StringComparison.InvariantCultureIgnoreCase) >= 0
    static member beforeI (delimiter:string) (s:string) =
        s.[0..s.IndexOf(delimiter,icic)]
    static member endsWithI (d:string) (x:string) =
        x.EndsWith(d,icic)
    
        
type Link = {Title:string; Link:string}
//    with member x.ToDump() = LINQPad.Hyperlinq(x.Link,x.Title)
    with member x.ToDump() = sprintf "%s - %s" x.Title x.Link
type Item = Link
module Map =
    let addListItem (k:'key) (v:'v) (m:Map<_,'v list>) =
        let items =
            if Map.containsKey k m then
                let items = m.[k]
                v::items
            else [v]
        m
        |> Map.add k items
    let addSetItem (k:'key) (v:'v) (m:Map<_,'v Set>) =
        let s =
            if Map.containsKey k m then
                let items = m.[k]
                items |> Set.add v
            else Set [v]
        m
        |> Map.add k s
    let keys = 
        Map.toSeq >> Seq.map fst >> Set.ofSeq

module Parsey =
    let mapLinkNode (node:HtmlNode) = {Title=node.InnerText;Link=node.Attributes.["href"].Value}
    let mapLinkNodes (nodes:HtmlNode seq) = 
        nodes
        |> Seq.map mapLinkNode 
        |> List.ofSeq
    let extractLinks containerIdOpt (xpath:string) (x:string) =
        let doc = HtmlAgilityPack.HtmlDocument()
        doc.LoadHtml x
        match containerIdOpt with
        | Some id' -> doc.GetElementbyId id'
        | None -> doc.DocumentNode
        |> fun x -> x.SelectNodes xpath
        |> mapLinkNodes
        
module Fetch =
    let asyncFetchCatch (path:string) =
        let baseAddress = Uri("http://www.icy-veins.com")
        let target = sprintf "/d3%s" path
        async {
            try
                use client = new HttpClient(BaseAddress=baseAddress)
                let! content = 
                    client.GetStringAsync(target) // ("http://fake-response.appspot.com/?sleep=30")
                          .ContinueWith(fun (t:System.Threading.Tasks.Task<string>) -> t.Result)
                    |> Async.AwaitTask
                return Choice1Of2 content
            with ex ->
                ex.Data.Add("path",path)
                ex.Data.Add("target",target)
                // Does not catch client-side timeout exception
                return Choice2Of2 ex
        }
        
    let fetchCache path =
        let fetch() =
            let response = 
                asyncFetchCatch path
                |> Async.RunSynchronously
                |> function
                    | Choice1Of2 msg -> 
                        if debug then
                            ([msg]).Dump(description=sprintf "full html - %s" path,depth=Nullable 0)
                            |> ignore
                        else printfn "fetched and cached %s" path
                        msg
                    | Choice2Of2 ex -> ex.Dump(); failwithf "bad response?"
            response
        let response = Util.Cache(fetch,path)
        response
    
    
    
    let doc,linkContainer = 
        printfn "about to wait"
        let response = fetchCache "/"
        printfn "waited"
        let doc = new HtmlAgilityPack.HtmlDocument()
        doc.LoadHtml(response)
        printfn "loaded html? %A" (not <| isNull doc.Text)
        // #nav must be javascript loaded after the fact
        match doc.GetElementbyId("footer") with
        | null -> 
            response.Dump("response")
            failwithf "could not find nav"
        | r -> doc,r
open Fetch    

module IcyLinks =
    let d3AHrefXpath = "//a[starts-with(@href,'//www.icy-veins.com/d3/')]"
    let itemLink = "//a[starts-with(@href,'http://us.battle.net/d3/en/item')]"
        
open IcyLinks
module D3 =
    // we get : class links
    type Class = Class of Link with member x.ToDump() = match x with |Class l -> l
    // for each we get a class' guides
    type Guide = Guide of Link with member x.ToDump() = match x with |Guide l -> l
    type ClassBuildContainer = ClassBuildContainer of Class * Guide list
            with member x.ToDump() = match x with ClassBuildContainer(c,gl) -> c,gl
    // for each we get build items
    type Item = Item of Link with member x.ToDump() = match x with Item l -> l
    type GuideDetail = GuideDetail of Class * Guide * Item Set
            with member x.ToDump() = match x with GuideDetail (c,g,items) -> c,g,items
    
//type BuildListLink = {Title:string;BuildsLink:string;Links:Link list}
open D3

module Inversion = // Item -> class -> builds that use
    type ClassMap = Map<Class,Guide Set>
    type ItemMap = Map<Item,ClassMap>
    let addGuide c (g:Guide) (m:ClassMap) :ClassMap =
        Map.addSetItem c g m
    let addItem c g (m:ItemMap) item :ItemMap =
        let cm = 
            if Map.containsKey item m then
                m.[item]
                |> addGuide c g
            else
                Map [ c,Set [g]]
            
        Map.add item cm m
            
    let invert (gdl:GuideDetail list) : ItemMap =
        gdl
        |> List.fold(fun (m:ItemMap) (GuideDetail (c,g,items)) ->
            items
            |> Set.fold(addItem c g) m
        ) Map.empty
    
open Inversion
    
let extractBuilds (Class classLink) txt : ClassBuildContainer=
    let links = Parsey.extractLinks None d3AHrefXpath txt
    let guides =
        let titleFilter =
            if classLink.Title.EndsWith(" builds",StringComparison.InvariantCultureIgnoreCase) then
                classLink.Title |> String.beforeI " builds"
            else classLink.Title
            |> fun x -> if x |> String.containsI " " then x.Replace(' ','-') else x
        links
        |> List.filter(fun l -> String.containsI titleFilter l.Link)
        |> List.filter(fun l -> not <| l.Title.EndsWith "builds")
        |> List.filter(fun l -> not <| l.Link.EndsWith "builds")
        |> List.filter (fun l -> l.Title <> "Leveling and Fresh 70 Guide")
        |> List.distinct
        |> List.map Guide
    if guides.Length < 4 then
        links.Dump("uhoh")
        failwithf "could not filter links"
    ClassBuildContainer(Class classLink,guides)
    
let processBuildsLink (Class classLink as cl) =
    let target = classLink.Link |> String.after "/d3"
    let result = fetchCache target
    result
    |> extractBuilds cl
//type Build = {Title:string;Link:string;Items:Item list}
let getGuideItems (Guide g) =
    let target = g.Link |> String.after "/d3"
    let result = fetchCache target
    let items =
        result
        |> Parsey.extractLinks None IcyLinks.itemLink
        |> List.map Item
    items
    |> Set.ofList
    
let processBuildLink (ClassBuildContainer (c,gl)) : GuideDetail list =
//    {Title=x.Title;Link=x.Link;Items=items}
    gl
    |> List.map(fun g ->
         GuideDetail(c,g,getGuideItems g)
     )
        
    
let buildLinks  =
    // build links in footer
    match linkContainer.SelectNodes d3AHrefXpath with
    | null -> linkContainer.InnerHtml.Dump("no links?"); failwithf "bad container"
    | links -> links |> Parsey.mapLinkNodes
    |> List.filter(fun x -> x.Link.EndsWith"builds")
    |> List.map Class
    
let r =
    buildLinks
    |> List.distinctBy(fun (Class x) -> x.Link)
    |> fun x -> x.Dump("ClassLinks"); x
//    |> List.truncate 1
    |> List.map processBuildsLink
//    |> List.truncate 1
    |> List.collect processBuildLink
    |> fun r -> 
        r
        |> List.groupBy(function GuideDetail(c,_,_) -> c)
        |> fun r -> r.Dump("class-guide-items",Nullable 0)
        r
let itemMap =
    r
    // invert items -> class -> build
    |> Inversion.invert
let autoComplete = Map.keys itemMap |> Set.map(fun (Item l) -> l.Title)
let mutable command = null
let getNextCommand () =
    let cmd = Util.ReadLine("Item?",null,autoComplete)
    command <- cmd
    if isNull cmd or cmd = "" then
        false
    else true
    
let dumpResult =
    let dc = DumpContainer()
    dc.Dump()
    fun (x:obj) -> dc.Content <- x
while getNextCommand() do
    if Set.contains command autoComplete then
        let k = itemMap |> Map.findKey(fun (Item k) _ -> k.Title = command)
        let users = itemMap.[k]
        dumpResult (k,users)
    else dumpResult (sprintf "Could not find item %s" command)
        
        
    