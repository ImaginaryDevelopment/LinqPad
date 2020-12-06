<Query Kind="FSharpExpression">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

let path = "characters-vault"
let flip f x y = f y x
let trim (x:string) = x.Trim()
let encodeQueryValue (x:string) =
    Uri.EscapeDataString x
let httpClient () =
    let c = new System.Net.Http.HttpClient()
    c.BaseAddress <- Uri "https://te4.org/"
    c
    
let getPage (path:string) =
    async {
        use c = httpClient()
        try
            let! text = Async.AwaitTask <| c.GetStringAsync path 
            return text
        with ex -> // retry
            (path,ex).Dump("path failure")
            let! text = Async.AwaitTask <| c.GetStringAsync path 
            return text
    }
let cache (key:string) (f:unit -> 't) =
    printfn "Caching %s" key
    Util.Cache(Func<_> f, key)
    
let parseHtml (x:string) =
    let d = HtmlAgilityPack.HtmlDocument()
    d.LoadHtml x
    d
let getElementById x (d:HtmlAgilityPack.HtmlDocument) = d.GetElementbyId x
let getOuterHtml (x:HtmlAgilityPack.HtmlNode) = x.OuterHtml
let getText (x:HtmlAgilityPack.HtmlNode) = x.InnerText
let getChildren (x:HtmlAgilityPack.HtmlNode) = x.ChildNodes |> List.ofSeq
let getElement (key:string) (x:HtmlAgilityPack.HtmlNode) =
    x.ChildNodes
    |> Seq.tryFind (fun n -> n.Name = key)
let getElements (key:string) (x:HtmlAgilityPack.HtmlNode) =
    x.ChildNodes
    |> Seq.filter(fun n -> n.Name = key)
    
let toQueryValues k items =
    items
    |> Seq.map(encodeQueryValue >>  sprintf "%s=%s" k)
    
let getCharacters (kvs:Map<string,string list>) =
    let query = (List.empty,kvs) ||> Map.fold(fun s k -> toQueryValues k >> String.concat "&" >> fun v -> v::s) |> String.concat "&"
    async{
        use c = httpClient()
        let fullpath = sprintf "%s?%s" path query
        fullpath.Dump("query uri")
        
        let! chars = Async.AwaitTask <| c.GetStringAsync fullpath
        return parseHtml chars |> getElementById "characters" 
    }
let setupInputs () =
    let getForm (d:HtmlAgilityPack.HtmlDocument) =
        d
        |> getElementById "selectors"
        |> fun el -> el.OuterHtml
    cache "formpage" <| fun () ->
            getPage path
            |> Async.RunSynchronously
    |> parseHtml
    |> getForm
    
let alwaysInputs =
    Map [
        "tag_official_addons", ["1"] // only querying characters using only official addons
        "tag_winner", ["winner"] // only query winners
    ]
setupInputs()
|> ignore

let (| Children |) (x:HtmlAgilityPack.HtmlNode) =
    x.ChildNodes
    |> List.ofSeq
let (| GetElement |_|) (name:string) x =
    getElement name x
let (|NodeName|_|) name (x:HtmlAgilityPack.HtmlNode) =
    if x.Name = name then Some x else None
    
let getCharacter path =
    async {
        try
            let text = cache path (fun () -> getPage path |> Async.RunSynchronously)
            let chz = parseHtml text |> getElementById "charsheet_zones"
            return chz |> Ok
        with ex -> return Error ex
    }
    
let dissectCharacterZones (chz:HtmlAgilityPack.HtmlNode) =
    chz.ChildNodes |> Seq.filter(fun x -> x.Name = "div")
let dissectCharsheetHead =
    getElement "table"
    
let mapTalents el =
    el
    |> getElement "table"
    |> Option.get
    |> getElements "tr"
    |> Seq.chunkBySize 5
    |> Seq.map (List.ofArray >> List.map getChildren >> (fun x ->
        let inline dump title =
                (x |> List.map (List.map getOuterHtml)).Dump(description=title)
        let (|StrongText|_|) =
            function
            |  GetElement "strong" x ->
               x |> getText |> Some
            | x ->  printfn "StrongText didn't match %A" (getOuterHtml x);None
            
        let (|Title|_|) (nodes: _ list) =
            match nodes with
            | NodeName "td" (StrongText n)::NodeName "td" el ::[] -> Some(n, getText el)
            | _ ->
                nodes |> List.map getOuterHtml |> fun x -> x.Dump("title did not match")
                None
        try
            match x with
            | Title (cat,pwr) :: rem ->
                rem |> List.map (List.map getOuterHtml) |> fun x -> x.Dump("rem")
                cat,pwr // , getText talentLevelEl, powers
            | _ ->
                dump "failing uhoh"
                "",""
            
        with ex ->
            dump "failing "
            ex.Dump()
            failwithf "Failed to map talents"
    ))
    
let mutable unmappedKeys = Set.empty
let addAll items s =
    (s,items) ||> Seq.fold(fun s v -> Set.add v s)
    
let mapCharacterZones (items : HtmlAgilityPack.HtmlNode seq) =
    let m = 
        items
        |> Seq.map(fun d -> getElement "h4" d |> Option.get|> getText |> trim, d)
        |> Seq.map(fun (k,el) ->
            match k with
            | "Character" -> None // dissectCharsheetHead el |> Some
            | "Prodigies" -> None // Some el
            | "Generic Talents" -> mapTalents el |> Some
            | _ -> None
            |> fun v -> k, v // Option.map getOuterHtml v
        )
        |> Map.ofSeq
    unmappedKeys <-
        m
        |> Map.filter(fun _ -> Option.isNone)
        |> Map.toSeq |> Seq.map fst
        |> flip addAll unmappedKeys
    m
    
    
let rowToCharacter (x:HtmlAgilityPack.HtmlNode) =
    x
    |> getElements "td"
    |> List.ofSeq
    |> function
        | userTr::charTr::_versionTr::_winnerTr::_lastUpdatedTr::[] ->
            let _userhref,uname = userTr |> getElement "a" |> Option.get |> fun a -> a.GetAttributeValue("href", String.Empty), a.InnerText
            let cHref,cName = charTr |> getElement "a" |> Option.get |> fun a -> a.GetAttributeValue("href",String.Empty), a.InnerText
            cHref,sprintf "%s: %s = %s" uname cName cHref
        | bad -> failwithf "unexpected number of elements %i in %s" bad.Length (bad |> List.map getOuterHtml |> String.concat "")
alwaysInputs
|> getCharacters
|> Async.RunSynchronously
|> getElement "table"
|> Option.get
|> getElement "tbody"
|> Option.get
|> getElements "tr"
|> Seq.map (rowToCharacter >> fun (h,disp) -> disp, getCharacter h |> Async.RunSynchronously |> Result.map (dissectCharacterZones >> mapCharacterZones))
|> Seq.truncate 2
//|> Seq.map getOuterHtml