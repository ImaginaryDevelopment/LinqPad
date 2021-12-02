<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

// scrape https://mightyparty.fandom.com/wiki/Hero_List and create a tracker app, like for COTLI
let displayHeroImages = true
let diagnoseImageTd = false
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

    module Async =
        let map f x =
            async {
                let! x = x
                return f x
            }
        
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
                printfn "Requesting %A" req.RequestUri
                printfn "Requesting query: %A" req.RequestUri.Query
                base.SendAsync(req,token)
            | _ ->
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
                let resp = Util.Cache((fun () -> resp), uri.ToString(), &fromCache)
                if not fromCache then
                   printfn "Freshly fetched" 
                return resp
            }
            |> Async.StartAsTask
    
let runHtmlAssertions (html:HtmlNode) = 
    // look for <meta name="twitter:url" content="https://mightyparty.fandom.com/wiki/Hero_List">
    // debug log the url
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
        | "tr" -> Some <| Html.getChildElements node
        | _ ->
            (Html.getOuterHtml node, node.Name, node.NodeType)
            |> dumpt "Not a tr"
            failwithf "unexpected node in table" 
    )
    |> Seq.mapi (fun i x ->
        match i with
        | 0 ->
            printfn "%s header? %s" title (x |> Seq.map Html.getInnerText |> String.concat ",")
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
        | _, true, _, ValueString src -> src,Util.Image(src) |> box
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
        | x -> failwithf "tr of length %i was unexpected" <| Seq.length x

let processHeroTable (IsTable table) =
    // assuming they never remove tbody and violate html
    table
    |> Html.getChildNodes
    |> Html.findByName "tbody"
    |> Html.getChildElements // now we have trs
    |> Seq.choose(fun node ->
        match Html.getNodeName node with
        | "tr" -> Some <| Html.getChildElements node
        | _ ->
            (Html.getOuterHtml node, node.Name, node.NodeType)
            |> dumpt "Not a tr"
            failwithf "unexpected node in table" 
    )
    |> Seq.mapi (fun i x ->
        match i with
        | 0 ->
            printfn "header? %s" (x |> Seq.map Html.getInnerText |> String.concat ",")
            None
        | _ -> Some(x |> mapHeroRow)
    )
    |> Seq.skip 1
    |> Seq.choose id
let mapSoulBindRow () = ()    
let processSoulBindTable (IsTable table) = processHeroTable table
    
let getHeroInfo (hero:HeroTableRow) =
    async {
        let! html = Async.AwaitTask <| Http.getHtml hero.Rel
        let body = Html.getBody runHtmlAssertions html
        //return Html.getOuterHtml body
        // we need the table.wikitable that has a previous sibling 'h3 > #Required_Heroes_to_Soulbind_Adherent
        let soulBindH3 =
            body.Descendants()
            |> Seq.find(fun node -> node.Id = "Required_Heroes_to_Soulbind_Adherent")
            |> fun span -> span.ParentNode
        let soulBindTable = soulBindH3 |> Html.getFollowingSiblings |> Seq.find (Html.getNodeName >> is "table")
        
        return Html.getOuterHtml soulBindTable
    }

let GetHeroList () =
    Http.getHtml "Hero_List"
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> Html.getBody runHtmlAssertions 
    |> Html.getTables 
    |> Seq.map (processHeroTable>>List.ofSeq)
    |> List.ofSeq
    
let firstHero =
    GetHeroList()
    |> Seq.head
    |> Seq.head
    
firstHero |> Dump |> ignore    

firstHero
|> getHeroInfo
|> Async.Catch
|> Async.RunSynchronously
|> Dump
|> ignore
    