<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// scrape https://mightyparty.fandom.com/wiki/Hero_List and create a tracker app, like for COTLI
// > 34 seconds to run, why?

type ImageMode =
    | Display
    | Raw
    | NoValue
    
module Settings =
    let displayHeroImages = ImageMode.Raw
    let diagnoseImageTd = false
    let displayMetaUrl = false
    let displayCacheMessages = false
    let displayRequests = false
    let writeJson = true
    let findOrphans = false
    
open Settings
let target =
    let gamey = Util.CurrentQueryPath |> Path.GetDirectoryName
    if gamey.EndsWith("gamey") |> not then
        failwithf "unexpected path: %s" gamey
    // assumes directory exists
    Path.Combine(gamey,"data", "mightypartyheroes.json")
    
//target.Dump()

let remappings = Map [
    "Ball&#39;Zt, the Warden", "Ball'Zt, the Warden"
    "Caesar&#39;s Head", "Caesar's Head"
    "D&#39;Arc, Iron Maiden", "D'Arc, Iron Maiden"
    "Evil %22Santa%22", "Evil \"Santa\""
    "Evil&#39;s Helper", "Evil's Helper"
    "General Zor&#39;Ma", "General Zor'Ma"
    "Madam Lo&#39;Trix", "Madam Lo'Trix"
    "Melia, Forest&#39;s Daughter", "Melia, Forest's Daughter"
    "Old god&#39;s servant", "Old god's servant"
    //"Scare Doctor", ??
    "sporelok", "Sporelok"
    "Ysh&#39;Tmala, The Old God","Ysh'Tmala, The Old God"
]
module Helpers =
    let inline dumpt (title:string) (x:obj) = x.Dump(description=title)
    let inline is y x = y = x
    let (|ValueString|NonValueString|) x = if String.IsNullOrWhiteSpace x then NonValueString else ValueString x
    let startsWith (delimiter:string) (x:string) =
        x.StartsWith(delimiter)
    let trim =
        function
        | ValueString x -> x.Trim()
        | x -> x
    let afterOrSelf (delimiter:string) =
        function
        |ValueString x ->
            match x.IndexOf(delimiter) with
            | i when i < 0 -> x
            | i -> x.[i + delimiter.Length ..]
        | x -> x
    let after (delimiter:string) =
        function
        | ValueString x -> x.[x.IndexOf delimiter + delimiter.Length ..]
        | x -> x
        
    module Option =
        let getOrFail msg = 
            function
            | Some x -> x
            | None -> failwith msg
    module List =
        let partitionResults items =
            ((List.empty,List.empty),items |> List.indexed)
            ||> List.fold( fun (oks,errs) ->
                    function
                    | i,Ok x -> (i,x)::oks,errs
                    | i,Error e -> oks, (i,e)::errs
            )
            
    module Async =
        let map f x =
            async {
                let! x = x
                return f x
            }
        let catch x =
            async{
                try
                    let! x = x
                    return Ok x
                with ex ->
                    return Error ex
            }
        // if the Async<'t> is Async<Result<_,_>> make sure it is because of a catch instead of just returning a Result
        // the perf hit of a try/catch is about 99% weighted on the damage being done in failure, no throw means almost no hit
        let catchResult x =
            catch x
            |> map(function | Ok x -> x | Error ex -> Error ex)
            
    module Html =
        let inline getChildNodes (node:HtmlNode) = node.ChildNodes
        let inline getNodeName (x:HtmlNode) = x.Name
        let inline getChildElements node =
            node |> getChildNodes |> Seq.filter(getNodeName >> startsWith "#" >> not)
        module Attr =
            let inline getAttrName (x:HtmlAttribute) = x.Name
        let inline getDescs (x:HtmlNode) = x.Descendants()
        let inline getDescsByName (name:string) (x:HtmlNode) = x.Descendants(name)
        let inline getFollowingSiblings (x:HtmlNode) =
            x
            |> Seq.unfold(fun x ->
                match x.NextSibling with
                | null -> None
                | value -> Some(value,value)
            )
            
        
        let getAttrValue attrName (node:HtmlNode) =
            node.Attributes
            |> Seq.tryFind(Attr.getAttrName >> is attrName)
            |> Option.map(fun attr -> attr.Value)
            
        let tryGetChildAttrValues (tag:string) (attr:string) (node:HtmlNode) =
            node
            |> getDescsByName tag
            |> Seq.choose (getAttrValue attr >> Option.map trim)
        let tryGetChildAttrValue tag attr node =
            node
            |> tryGetChildAttrValues tag attr
            |> Seq.tryHead
            
        module Patterns =
            let (|AttrVal|_|) name x = x |> getAttrValue name
            let (|AttrValIs|_|) (name,value) x =
                match x |> getAttrValue name with
                | Some v -> if v = value then Some () else None
                | _ -> None
            let (|IsTable|) x =
                if getNodeName x = "table" then x else failwithf "was not given a table"
        
        let filterByAttribute name valueOpt nodes =
            nodes
            |> Seq.filter(fun attr ->
                let v = attr |> getAttrValue name 
                match v, valueOpt with
                | Some _, None -> true
                | Some v, Some x -> v = x
                | None, _ -> false
            )
            
        let findByName name nodes =
            try
                nodes
                |> Seq.find(fun (x:HtmlNode) -> x.Name = name)
            with _ ->
                eprintfn "Failed to find name: %s" name
                reraise()
        let inline getOuterHtml (node:HtmlNode) = node.OuterHtml
        let inline getInnerHtml (node:HtmlNode) = node.InnerHtml
        let inline getInnerText (node:HtmlNode) = node.InnerText |> trim
        
        let loadHtml text =
            let hd = HtmlDocument()
            hd.LoadHtml(text)
            hd
        let getBody fAssertions text =
            let htmlNode =
                text
                |> loadHtml
                |> fun hd -> hd.DocumentNode
                |> getChildNodes
                |> findByName "html"
            fAssertions htmlNode
            htmlNode
            |> getChildNodes
            |> findByName "body"
            
        let getTables (parent:HtmlNode) =
            parent
            |> getDescsByName "table"
    module Serial =
        open Newtonsoft.Json
        let serialize<'t> indent (x:'t) =
            if indent then
                JsonConvert.SerializeObject(x, Formatting.Indented)
            else JsonConvert.SerializeObject(x)
        let inline deserialize<'t> (x:string) = JsonConvert.DeserializeObject<'t>(x)
            
open Helpers
open Helpers.Html.Patterns

        
module Http =
    open System.Net.Http
    type HtmlMsgHandler() as me =
        inherit System.Net.Http.DelegatingHandler()
        do
            me.InnerHandler <- new System.Net.Http.HttpClientHandler()
            
        override x.SendAsync(req,token) =
            match req.RequestUri.Query with
            | ValueString _ -> 
                if Settings.displayRequests then
                    printfn "Requesting %A" req.RequestUri
                    printfn "Requesting query: %A" req.RequestUri.Query
                base.SendAsync(req,token)
            | _ ->
                if Settings.displayRequests then
                    printfn "Requesting %A" req.RequestUri
                let resp = base.SendAsync(req,token)
                resp
                
    let getHtml = // relpath off of /wiki/...
        let httpClient = new HttpClient(new HtmlMsgHandler(),true)
        let baseAddr = Uri "https://mightyparty.fandom.com/"
        httpClient.BaseAddress <- baseAddr
        fun (relPath:string) ->
            let uri = Uri(baseAddr, sprintf "wiki/%s" relPath)
            async{
                let! resp = Async.AwaitTask <| httpClient.GetStringAsync(uri)
                let mutable fromCache = false
                let key = string uri
                let resp = Util.Cache((fun () -> resp), key, &fromCache)
                if Settings.displayCacheMessages then
                    if not fromCache then
                            printfn "Freshly fetched from %s"  key
                    else printfn "Fetched %s from cache" key
                return resp
            }
            |> Async.StartAsTask
    
let runHtmlAssertions (html:HtmlNode) = 
    // look for <meta name="twitter:url" content="https://mightyparty.fandom.com/wiki/Hero_List">
    // debug log the url
    if displayMetaUrl then
        html
        |> Html.getChildNodes
        |> Html.findByName "head"
        |> Html.getChildNodes
        |> Seq.filter (Html.getNodeName >> is "meta")
        //|> Seq.map Html.getOuterHtml
        |> Seq.choose(
            function
            | AttrValIs("name","twitter:url") & AttrVal "content" content -> Some content
            | _ -> None
        )
        |> Seq.tryHead
        
        |> dumpt "meta:url"
    
type HeroTableRow = {
    Image:obj
    ID:string // int
    Name:string
    Rel:string
    Rarity:string
    Alignment: string
    Gender:string
    Type:string
    //ImageSrc:string
}

// based on two tables on different pages (weak evidence). it appears they use the same general format
let processATable title fAssertHeaders fMap (IsTable table) =
    // assuming they never remove tbody and violate html
    table
    |> Html.getChildNodes
    |> Html.findByName "tbody"
    // elements not nodes
    |> Html.getChildElements // now we have trs
    |> Seq.choose(fun node ->
        match Html.getNodeName node with
        | "tr" -> Some (Html.getChildElements node |> List.ofSeq)
        | _ ->
            (Html.getOuterHtml node, node.Name, node.NodeType)
            |> dumpt "Not a tr"
            failwithf "unexpected node in table" 
    )
    |> Seq.mapi (fun i x ->
        match i with
        | 0 ->
            //printfn "%s header? %s" title (x |> Seq.map Html.getInnerText |> String.concat ",")
            fAssertHeaders x
            None
        | _ -> Some(fMap x)
    )
    |> Seq.skip 1
    |> Seq.choose id
    
let mapHeroRow (tds:HtmlNode seq) =
    let extractImage (imagetd:HtmlNode) =
        //let image = Html.getInnerHtml image // wip
        let raw = imagetd |> Html.tryGetChildAttrValue "a" "href" |> Option.defaultValue null  // |> Html.getDescsByName "a" |> Seq.map (Html.getAttrValue "href") |> Seq.head |> Option.map trim |> Option.defaultValue null
        // scaled is usually on img data-src, sometimes on img src
        let scaled =
            imagetd |> Html.tryGetChildAttrValue "img" "data-src"
            |> function
                | Some (ValueString v) -> Some v
                | None -> imagetd |> Html.tryGetChildAttrValue "img" "src"
                | _ -> failwithf "scaled: Some without value"
            |> Option.defaultValue null
        match diagnoseImageTd, displayHeroImages,raw, scaled with
        | true, _, _, _ -> Html.getInnerHtml imagetd, null
        | _, ImageMode.Display, _, ValueString src -> src, Util.Image(src) |> box
        // propagate the image data for serialization
        | _, ImageMode.Raw, _, ValueString src -> src, src |> box
        | _ -> raw, null
        
    tds
    |> List.ofSeq
    |> function
        | id::nameInfo::image::r::a::g::t::[] ->
            let id = int id.InnerText
            let nameInfo = nameInfo |> Html.getDescsByName "a" |> Seq.tryExactlyOne |> Option.getOrFail "multiple a's"
            let rel = nameInfo |> Html.getAttrValue "href" |> Option.map (trim >> afterOrSelf "/wiki/") |> Option.defaultValue null
            let name = nameInfo.InnerText.Trim()
            let image = extractImage image
            
            let getATitle (tr:HtmlNode) = tr |> Html.getDescsByName "a" |> Seq.head |> Html.getAttrValue "title" |> Option.map trim |> Option.defaultValue null
            
            let r = r |> getATitle
            let a = a |> getATitle
            let g = g |> getATitle
            let t = t |> getATitle
            
            {
                ID=string id
                Name=name
                Rel=rel
                Image=snd image
                //ImageSrc=fst image
                Rarity=r
                Alignment=a
                Gender=g
                Type=t
                }
        | x -> failwithf "hero: tr of length %i was unexpected" <| Seq.length x

let processHeroTable = processATable "heroTable" ignore mapHeroRow

type SoulBindRow = {
    Level:string
    Required:string list // (string * int) list?
    RequiredLvl:string // ^
}

let tryRemap x =
    match remappings |> Map.tryFind x with
    | Some v -> v
    | None -> x
let mapSoulBindRow (tds: HtmlNode list) =
    let mapReq req =
        req
        |> Html.getDescsByName "li"
        |> Seq.map (
            Html.getChildElements
            >> Seq.choose (Html.getAttrValue "data-hero")
            >> Seq.tryHead
            >> Option.map tryRemap
            >> Option.defaultValue "eh?"
        )
        |> List.ofSeq
    tds
    |> List.ofSeq
    |> function
        | level::req::reqLvl:: _ -> // cost::bonus::might::strength
            {
                Level=
                    match Html.getInnerText level |> trim with
                    | "I" -> 1
                    | "II" -> 2
                    | "III" -> 3
                    | "IV" -> 4
                    | _ -> -1
                    |> string
                    
                Required= mapReq req
                RequiredLvl = Html.getInnerText reqLvl |> afterOrSelf "Level" |> trim
            }
        | x -> failwithf "soulBind:tr of length %i was unexpected" <| Seq.length x

let processSoulBindTable table = processATable "soulBindTable" ignore mapSoulBindRow table
    
let getHeroInfo (hero:HeroTableRow) =
    async {
        let! html = Async.AwaitTask <| Http.getHtml hero.Rel
        let body = Html.getBody runHtmlAssertions html
        //return Html.getOuterHtml body
        // we need the table.wikitable that has a previous sibling 'h3 > #Required_Heroes_to_Soulbind_Adherent
        let soulBindH3 =
            let potentials =
                body.Descendants("h3")
                |> List.ofSeq
            potentials
            |> Seq.tryPick(fun h3 ->
                Html.getDescs h3
                |> Seq.tryFind (fun node ->
                    node.Id.StartsWith("Required_Heroes_to_Soulbind")
                )
                |> function
                    | Some node -> Some node
                    | None ->
                        potentials |> List.map Html.getOuterHtml |> dumpt "potentials" |> ignore
                        failwith "Could not find an h3 with a soulbind child"
            )
            |> function
                | Some x -> x
                | None ->
                    Html.getOuterHtml body
                    |> dumpt "bad body"
                    |> ignore
                    failwithf "Unable to find soulbinding info for %s:%s" hero.Name hero.Rel
            |> fun span -> span.ParentNode
        let soulBindTable = soulBindH3 |> Html.getFollowingSiblings |> Seq.find (Html.getNodeName >> is "table")
        
        return soulBindTable |> processSoulBindTable
    }
    |> Async.catch

let GetHeroList () =
    Http.getHtml "Hero_List"
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> Html.getBody runHtmlAssertions 
    |> Html.getTables 
    |> Seq.map (processHeroTable>>List.ofSeq)
    |> List.ofSeq
type Soulbind = {
    Requirements: string list
    ReqLvl: int
}
type HeroInfo = {
    Image:obj
    ID:string // int
    Name:string
    //Rel:string
    Rarity:string
    Alignment: string
    Gender:string
    Type:string
    Soulbinds: Soulbind list
}
let heroes = GetHeroList()
Util.ElapsedTime.Dump("getHeroList done")

let showSingleHero skip =
    let firstHero =
        heroes
        |> Seq.head
        |> Seq.skip skip
        |> Seq.head
        
    firstHero |> Dump |> ignore    

    firstHero
    |> getHeroInfo
    |> Async.catchResult
    |> Async.RunSynchronously
    |> Dump
    |> ignore
heroes
|> List.concat
|> Seq.map (fun h -> getHeroInfo h |> Async.map(function | Ok x -> Ok(h,List.ofSeq x) | Error ex -> Error(h,ex)) |> Async.RunSynchronously)
//|> Seq.truncate 4
|> Seq.map(function
    | Ok (h,hi) -> Ok {
            Image= h.Image
            ID = h.ID
            Name = h.Name
            Rarity = h.Rarity
            Alignment = h.Alignment
            Gender = h.Gender
            Type = h.Type
            Soulbinds =
                hi
                |> List.map(fun r ->
                    {
                        Requirements = r.Required
                        ReqLvl = int r.RequiredLvl
                    }
                )
        }
    | Error ex -> Error ex
)
|> List.ofSeq
|> fun x ->
    Util.ElapsedTime.Dump("pre-partition")
    x
|> List.partitionResults

|> fun (o,e) ->
    Util.ElapsedTime.Dump("post-part")
    e.Dump("errors")
    let unindexed = o |> List.map snd
    if Settings.findOrphans then
        let nameMap = unindexed |> Seq.map(fun x -> x.Name, x.ID) |> Map.ofSeq
        //let heroMap = unindexed |> Seq.map(fun x -> x.ID, x) |> Map.ofSeq
        // validation of soulbind names as a key
        unindexed
        |> List.choose(fun hi ->
               hi.Soulbinds
               |> Seq.collect(fun sb -> sb.Requirements)
               |> Seq.choose(fun n -> if Map.containsKey n nameMap then None else Some n)
               |> List.ofSeq
               |> function
                | [] -> None
                | bad -> Some(hi,bad)
        )
        |> dumpt "bad apples"
    //let allHeroes = unindexed |> Seq.collect (fun x -> x.Name::(x.Soulbinds |> List.collect(fun sb -> sb.Requirements))) |> Set.ofSeq
    unindexed
|> fun x ->
    if Settings.writeJson then
        // should we verify the image field isn't holding a real image first?
        let data = Serial.serialize true x
        File.WriteAllText(target,contents=data)
        ()
    else
        x |> Dump |> ignore

Util.ElapsedTime.Dump("fin")