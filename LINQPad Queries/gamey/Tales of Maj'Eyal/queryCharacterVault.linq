<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

let path = "characters-vault"
let flip f x y = f y x
let trim (x:string) = x.Trim()
let before (delim:string) (x:string) = x.[0.. x.IndexOf delim - 1]
let after (delim:string) (x:string) = x.[x.IndexOf delim + delim.Length ..]
let chunkBy f x =
    let rec loop chunk chunks list = 
        match list with
        | [] -> List.rev ((List.rev chunk)::chunks)
        | x::xs when f x && List.isEmpty chunk -> loop [x] chunks xs
        | x::xs when f x -> loop [x] ((List.rev chunk)::chunks) xs
        | x::xs -> loop (x::chunk) chunks xs
    loop [] [] x
        
let logErrors title items =
    let collected = ResizeArray()
    let result =
        items
        |> Seq.choose(
            function
            | Ok x -> Some x
            | Error x -> collected.Add x; None
        )
        |> Seq.toList
    collected.Dump(description=title)
    result
    
let logKeyed title (items:(string*Result<_,_>) seq) =
    let collected = ResizeArray()
    let result =
        items
        |> Seq.choose(fun (x,y) ->
            match y with
            | Ok y -> Some (x,y)
            | Error y -> collected.Add((x,y)); None
        )
        |> Seq.toList
    collected.Dump(description=title)
    result
       
let cache (key:string) (f:unit -> 't) =
    printfn "Caching %s" key
    Util.Cache(Func<_> f, key)
module Async =
    let map f x = 
        async {
            let! x2 = x
            return f x2
        }
        
module Fetch =     
    let httpClient () =
        let c = new System.Net.Http.HttpClient()
        c.BaseAddress <- Uri "https://te4.org/"
        c
    let encodeQueryValue (x:string) =
        Uri.EscapeDataString x
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
        
    let toQueryValues k items =
        items
        |> Seq.map(encodeQueryValue >>  sprintf "%s=%s" k)
        
    let queryPage path kvs =
        let query = (List.empty,kvs) ||> Map.fold(fun s k -> toQueryValues k >> String.concat "&" >> fun v -> v::s) |> String.concat "&"
        let fullpath = sprintf "%s?%s" path query
        getPage fullpath
        
type Talent = {Name:string;Points:int}
    with member x.Dump() = sprintf "%s-%i" x.Name x.Points
type TalentCategory = {Name:string; Power:string;Talents:Talent list}

module Parse =
    open HtmlAgilityPack
    let parseHtml (x:string) =
        let d = HtmlAgilityPack.HtmlDocument()
        d.LoadHtml x
        d
    let getElementById x (d:HtmlDocument) = d.GetElementbyId x
    let getOuterHtml (x:HtmlNode) = x.OuterHtml
    let dumpOuter title x = (getOuterHtml x).Dump(description=title)
    let getText (x:HtmlNode) = x.InnerText
    let getChildren (x:HtmlNode) = x.ChildNodes |> List.ofSeq
    let getElement (key:string) (x:HtmlNode) =
        x.ChildNodes
        |> Seq.tryFind (fun n -> n.Name = key)
    let getElements (key:string) (x:HtmlNode) =
        x.ChildNodes
        |> Seq.filter(fun n -> n.Name = key)

    let (| GetElement |_|) (name:string) x =
        getElement name x
    
    let (|NodeName|_|) name (x:HtmlAgilityPack.HtmlNode) =
        if x.Name = name then Some x else None
    
    let getCharacters (kvs:Map<string,string list>) =
        Fetch.queryPage path kvs
        |> Async.map (parseHtml >> getElementById "characters")
    
    let setupInputs () =
        let getForm (d:HtmlAgilityPack.HtmlDocument) =
            d
            |> getElementById "selectors"
            |> fun el -> el.OuterHtml
        cache "formpage" <| fun () ->
                Fetch.getPage path
                |> Async.RunSynchronously
        |> parseHtml
        |> getForm
        
    let parseTalent (data,spentTd) =
        try
            match data with
            | NodeName "td" (GetElement "ul" ul) ->
                let talentName =
                    let last = getElements "li" ul |> Seq.last
                    last |> getOuterHtml |> after "</div>" |> before "<" |> trim
                Some {Name=talentName;Points= spentTd |> getText |> before "/5" |> int}
            | _ -> data |> getOuterHtml |> fun x -> x.Dump("failing to get talent data"); None
        with ex -> (ex.Message,getOuterHtml data,getOuterHtml spentTd).Dump("parseTalent fail"); None
        
    let mapTalents el =
        el
        |> getElement "table"
        |> Option.get
        |> getElements "tr"
        |> List.ofSeq
        |> chunkBy (function | GetElement "td" (GetElement "strong" _) -> true | _ -> false)
        |> Seq.choose (List.map getChildren >> (fun x ->
            let inline dump title =
                    (x |> List.map (List.map getOuterHtml)).Dump(description=title)
            let (|StrongText|_|) =
                function
                |  GetElement "strong" x ->
                   x |> getText |> Some
                | x ->  printfn "StrongText didn't match %A" (getOuterHtml x);None
            let getTitle (nodes: _ list) =
                match nodes with
                | NodeName "td" (StrongText n)::NodeName "td" el ::[] -> Some(n, getText el)
                | _ ->
                    nodes |> List.map getOuterHtml |> fun x -> x.Dump("title did not match")
                    None
            let (|Title|_|) (nodes: _ list) = getTitle nodes
            try
                match x with
                | Title (cat,pwr) :: rem ->
                    
                    let paired = rem |> List.map(function | a::b::[] -> a,b | x -> x.Dump("pairing failed"); failwith "pairing failed" )
                    Some {Name=cat;Power=pwr;Talents= List.choose parseTalent paired}
                | _ ->
                    dump "failing uhoh"
                    None
                
            with ex ->
                dump "failing "
                ex.Dump()
                failwithf "Failed to map talents"
        ))
    let parseZoneTitle = getElement "h4" >> Option.map (getText >> trim)
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
            
module Retrieval =    
    let alwaysInputs =
        Map [
            "tag_official_addons", ["1"] // only querying characters using only official addons
            "tag_winner", ["winner"] // only query winners
        ]
        
    Parse.setupInputs()
    |> ignore
    
        
    let getCharacter path =
        async {
            try
                let text = cache path (fun () -> Fetch.getPage path |> Async.RunSynchronously)
                let chz = Parse.parseHtml text |> Parse.getElementById "charsheet_zones"
                return chz |> Some
            with ex -> 
                ex.Dump(path)
                return None
        }
        
    let dissectCharacterZones (chz:HtmlAgilityPack.HtmlNode) =
        chz.ChildNodes |> Seq.filter(fun x -> x.Name = "div")
        
    //let dissectCharsheetHead =
    //    getElement "table"
        
        
        
    let mutable unmappedKeys = Set.empty
    let addAll items s =
        (s,items) ||> Seq.fold(fun s v -> Set.add v s)
        
    let mapCharacterZones (items : HtmlAgilityPack.HtmlNode seq) =
        let m = 
            items
            |> Seq.choose(fun d -> Parse.parseZoneTitle d |> Option.map(fun x -> x, d))
            |> Seq.choose(fun (k,el) ->
                match k with
                | "Character" -> None // dissectCharsheetHead el |> Some
                | "Prodigies" -> None // Some el
                | "Generic Talents" -> Parse.mapTalents el |> List.ofSeq |> Some
                | "Class Talents" -> Parse.mapTalents el |> List.ofSeq |> Some
                | _ ->
                    unmappedKeys <- unmappedKeys |> Set.add k
                    None
                |> Option.map (fun v -> k, v) // Option.map getOuterHtml v
            )
            |> Map.ofSeq
            
        m
        
    let fetchCharacters inputs =
        Parse.getCharacters inputs
        |> Async.RunSynchronously
        |> Parse.getElement "table"
        |> Option.bind (Parse.getElement "tbody")
        |> Option.map (Parse.getElements "tr")
        
    //type FetchType = | Path of string | Values of Map<string, string list>
    let fetchMapped v =
        (alwaysInputs,v)
        ||> Map.fold(fun m k v ->
            Map.add k v m
        )
        |> fetchCharacters
        |> Option.map (
            Seq.map (Parse.rowToCharacter >> fun (h,disp) -> disp, getCharacter h |> Async.RunSynchronously |> Option.map(dissectCharacterZones >> mapCharacterZones) |> Option.defaultValue Map.empty)
        )
        
    let sampleFetch () =
        fetchMapped Map.empty
        |> Option.map(
            Seq.map (fun x -> x)
            //|> Seq.map(fun (x,y) -> x, y |> logErrors x)
            >> Seq.truncate 1
            >> Dump
            >> ignore
        )
        
Retrieval.fetchMapped Map.empty
|> Option.iter(fun x ->
    x
    |> Seq.map (fun x -> x)
    |> Seq.truncate 1
    |> Dump
    |> ignore
)