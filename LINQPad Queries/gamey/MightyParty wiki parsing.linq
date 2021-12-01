<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

// scrape https://mightyparty.fandom.com/wiki/Hero_List and create a tracker app, like for COTLI
module Helpers =
    let inline dumpt (title:string) (x:obj) = x.Dump(description=title)
    let inline is y x = y = x
    let (|ValueString|NonValueString|) x = if String.IsNullOrWhiteSpace x then NonValueString else ValueString x
    let startsWith (delimiter:string) (x:string) =
        x.StartsWith(delimiter)
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
        let inline getAttrName (x:HtmlAttribute) = x.Name
        
        let getAttrValue attrName (node:HtmlNode) =
            node.Attributes
            |> Seq.tryFind(getAttrName >> is attrName)
            |> Option.map(fun attr -> attr.Value)
            
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
            parent.Descendants()
            |> Seq.filter(getNodeName >> is "table")
            
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
    ID:string // int
    Name:string
    Rel:string
    Image:string
    Rarity:string
    Alignment: string
    Gender:string
    Type:string
}
let mapHeroRow (tds:HtmlNode seq) =
    tds
    |> List.ofSeq
    |> function
        | id::nameInfo::image::r::a::g::t::[] ->
            let id = int id.InnerText
            let nameInfo = nameInfo.Descendants("a") |> Seq.tryExactlyOne |> Option.getOrFail "multiple a's"
            let rel = nameInfo |> Html.getAttrValue "href" |> Option.defaultValue null
            let name = nameInfo.InnerText.Trim()
            let image = Html.getInnerHtml image
            let r = Html.getInnerHtml r
            let a = Html.getInnerHtml a
            let g = Html.getInnerHtml g
            let t = Html.getInnerHtml t
            {
                ID=string id
                Name=name
                Rel=rel
                Image=image
                Rarity=r
                Alignment=a
                Gender=g
                Type=t
                }
let processHeroTable (IsTable table) =
    // assuming they never remove tbody and violate html
    table
    |> Html.getChildNodes
    |> Html.findByName "tbody"
    |> Html.getChildElements // now we have trs
    |> Seq.choose(fun node ->
        match Html.getNodeName node with
        | "tr" -> Some <| Html.getChildElements node
        //| "#text" -> None
        | _ ->
            (Html.getOuterHtml node, node.Name, node.NodeType)
            |> dumpt "Not a tr"
            failwithf "unexpected node in table" 
    )
    //|> Seq.map (Html.getChildNodes >> Seq.map Html.getOuterHtml)
    |> Seq.mapi (fun i x ->
        match i with
        | 0 ->
            printfn "header? %O" x
            None
        | _ -> Some(i,x |> mapHeroRow)
    )
    |> Seq.skip 1
    |> Seq.choose id
    
Http.getHtml "Hero_List"
|> Async.AwaitTask
|> Async.RunSynchronously
|> Html.getBody runHtmlAssertions 
|> Html.getTables 
|> Seq.map (processHeroTable>>List.ofSeq)
|> List.ofSeq
|> Dump
|> ignore
    
    