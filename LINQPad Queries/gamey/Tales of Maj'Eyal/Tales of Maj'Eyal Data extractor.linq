<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>System.Windows.Media</Namespace>
</Query>

// purpose: extract raw data into json about Tales of Maj'Eyal winners

// does not handle pagination decently
// need to handle or filter unicode talent names :( - Character zone Size field may be an easy indicator


let authority = "https://te4.org/"
let path = "characters-vault"

type MappedClass = 
    | Adventurer
    | ArcaneBlade
    | Archmage
    | Doombringer
    | Marauder
    | Solipsist
    | SunPaladin
    with
        member x.FormValue =
            match x with
            | Adventurer -> string x, "104"
            | ArcaneBlade -> "Arcane Blade", "22"
            | Archmage -> string x, "7"
            | Doombringer -> string x, "23313"
            | Marauder -> string x, "71"
            | Solipsist -> string x, "102"
            | SunPaladin ->  "Sun Paladin", "27"
let classOpt = [Doombringer.FormValue] |> Some
type Difficulty = 
    | Normal
    | Insane
    with
        member x.FormValue =
            match x with
            | Normal -> string x, "6"
            | Insane -> string x, "36"
let alwaysInputs =
    Map [
        "tag_official_addons", ["1"] // only querying characters using only official addons
        //"tag_permadeath[]", [ Permadeath.Roguelike.FormValue |> snd]
        //"tag_difficulty[]", [Difficulty.Normal.FormValue  |> snd]
        "tag_winner", ["winner"] // only query winners
        //"tag_level_min", ["50"]
        //"tag_dead",["dead"] // I query dead ppl
    ]
    
[<Struct>]
type OptionalBuilder =
  member __.Bind(opt, binder) =
    match opt with
    | Some value -> binder value
    | None -> None
  member __.Return(value) =
    Some value
  member __.ReturnFrom x =
    match x with
    | Some (Some x) -> Some x
    | _ -> None
    
let option = OptionalBuilder()
let charDump, showChars =
    let chars: ResizeArray<string*obj> = ResizeArray()
    let dm = DumpContainer()
    let add = fun (name:string,path:string) ->
        let uri = Uri(Uri authority, relativeUri=path) |> string
        chars.Add(name,box <| LINQPad.Hyperlinq uri)
        dm.Content <- null
        dm.Content <- chars
    add, (fun () -> dm.Dump("chars"))
let urlDump,showUrls =
    let urls: ResizeArray<string*obj> = ResizeArray()
    let dm = DumpContainer()
    let add = fun (x:string) ->
        let uri = Uri(Uri authority,relativeUri=x) |> string
        urls.Add (x,box <| LINQPad.Hyperlinq uri)
        dm.Content <- null
        dm.Content <- urls
    add, (fun () -> dm.Dump("urls"))
    
    
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
    //printfn "Caching %s" key
    Util.Cache(Func<_> f, key)
    
module Map =
    let addItem k x (m:Map<_,'t list>) =
        if m.ContainsKey k then
            let existing = m.[k]
            m |> Map.add k (x::existing)
        else
            m |> Map.add k [x]
            
    // merge the data in m2 into m1, using m2's values as additional items in m1's list
    let mergeAsList (m2:Map<'tk,'tv >) (m:Map<'tk, 'tv list>) : Map<'tk, 'tv list> =
        (m,m2)
        ||> Map.fold(fun m k v ->
            m |> addItem k v
        )
        
module Async =
    let map f x = 
        async {
            let! x2 = x
            return f x2
        }
        
module Fetch =     
    let httpClient () =
        let c = new System.Net.Http.HttpClient()
        c.BaseAddress <- Uri authority
        c
    let encodeQueryValue (x:string) =
        Uri.EscapeDataString x
    let getPage (path:string) =
        async {
            use c = httpClient()
            try
                let! text = Async.AwaitTask <| c.GetStringAsync path 
                //(path).Dump("Fetched")
                return text
            with ex -> // retry
                (path,ex).Dump("path failure")
                do! Async.Sleep 1500 // rest up for next attempt
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
        
[<NoComparison>]
type CharacterLink = {User:string;Name:string;Path:string;Link:obj}
module Charsheets =
    type Prodigies = Map<string,int> // assuming prodigies only go 1/1 for now
    type StatSummary = { StatName:string; Base:int; Effective:int}
    type StatCharsheet = StatSummary list
    type CharacterCharsheet = {Campaign:string;Mode:string*string;Race:string;Class:string}
    
type Talent = {Name:string;Points:int}
    with member x.Dump() = sprintf "%s-%i" x.Name x.Points
type TalentCategory = {Name:string; Power:string;Talents:Talent list}
type TalentCategoryType =
    | Class
    | Generic
    with member x.ToDump() = string x
type Err = Err of string list    
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
    let getAttrValue key (x:HtmlNode): string option=
        match x.GetAttributeValue(key,null) with
        | null -> None
        | x when String.IsNullOrWhiteSpace x -> None
        | x -> Some x
        
    let getElement (key:string) (x:HtmlNode) =
        x.ChildNodes
        |> Seq.tryFind (fun n -> n.Name = key)
    let getElements (key:string) (x:HtmlNode) =
        x.ChildNodes
        |> Seq.filter(fun n -> n.Name = key)
        |> List.ofSeq

    let (| GetElement |_|) (name:string) x =
        getElement name x
    
    let (|NodeName|_|) name (x:HtmlAgilityPack.HtmlNode) =
        if x.Name = name then Some x else None
    
    let getCharacters (kvs:Map<string,string list>) =
        Fetch.queryPage path kvs
        |> Async.map (parseHtml >> getElementById "characters")
    
    let setupInputs () = // this was to persue the feature of an interactive script, instead hard-coded values have been used.
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
                Ok {Name=talentName;Points= spentTd |> getText |> before "/5" |> int}
            | _ -> data |> getOuterHtml |> fun x -> x.Dump("failing to get talent data"); Error ("nonmatch",getOuterHtml data)
        with ex ->
            //(ex.Message,getOuterHtml data,getOuterHtml spentTd).Dump("parseTalent fail")
           	Error(ex.Message,getOuterHtml data)
        
    let (|ProdigyTitle|_|) x =
        match x with
        | NodeName "td" (GetElement "ul" (GetElement "li" li)) ->
            getText li |> Some
        | _ ->
            dumpOuter "prodigy title not found" x
            None
    let mapTalents el: (TalentCategory * (_ )) seq=
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
                    let talents = paired |> List.map parseTalent
                    let good = talents |> List.choose (function | Ok x -> Some x | _ -> None)
                    let bad = talents |> List.choose (function | Error x -> Some x | _ -> None) |> List.map(fun (e1,e2) -> Err [e1;e2])
                    Some ({Name=cat;Power=pwr;Talents= good},bad)
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
        |> function // columns aren't always consistent
            //| userTd::charTd::_ -> (userTd,charTd)
            | userTd::charTd::_ -> Some (userTd,charTd)
            | td :: [] when (getText td).Contains("No characters available") -> None
            | bad -> 
                (bad |> List.map getOuterHtml).Dump("character row failure")
                failwithf "unexpected number of elements %i in %s" bad.Length (bad |> List.map getOuterHtml |> String.concat "")
        |> Option.map (fun (userTd,charTd) ->
                let getHrefWithInner x = x |> getElement "a" |> Option.map (fun a -> a |> getAttrValue "href" |> Option.defaultValue null, a.InnerText) |> Option.defaultValue (String.Empty,null)
                let _userhref,uname = userTd |> getHrefWithInner
                let cHref,cName = charTd |> getHrefWithInner
                charDump (cName,cHref)
                {User=uname;Name=cName;Path=cHref;Link=null}
            )
                //cHref,sprintf "%s: %s = %s" uname cName cHref
module Assert =
    open HtmlAgilityPack
    let hasClass (name:string) (x:HtmlNode) =
        x
        |> Parse.getAttrValue "class"
        |> function
            | Some c ->
                if not <| c.Split(Array.singleton " ",StringSplitOptions.RemoveEmptyEntries).Contains(name) then
                    (Parse.getOuterHtml x).Dump(sprintf "class '%s' not found" name)
            | None -> ()
type TalentPower = TalentPower of string with member x.ToDump() = match x with | TalentPower v -> v
type CategoryInvestment = CategoryInvestment of int with member x.ToDump() = match x with | CategoryInvestment v -> v
type CategoryAnaly = { TotalPoints: int; TalentsObtained:int}

//type CDisp = CDisp of string with member x.ToDump() = match x with | CDisp v -> v
type TalentFetchResult =  Map<TalentCategoryType,(TalentCategory * Err list) list>
module Retrieval =    
        
    Parse.setupInputs() // not needed/used, featureset deferred
    |> ignore
        
    let getCharacter path =
        urlDump path
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
        
    let dissectCharsheetHead =
        Parse.getElement "table"
    let dissectProdigies x =
        option{
            let! tbl = Parse.getElement "table" x
            Assert.hasClass "talents" tbl
            let! tbody = Parse.getElement "tbody" tbl
            let trs = Parse.getElements "tr" tbody
            return trs |> List.choose (
                Parse.getElements "td"
                >> function
                | Parse.ProdigyTitle t :: Parse.NodeName "td" _el ::[] -> Some(t,1)
                | s -> s |> List.map(Parse.getOuterHtml) |> fun x -> x.Dump("prodigy shape unexpected"); None
                )
        }
        |> Option.defaultValue List.empty
        |> Map.ofSeq
                    
        
    let addAll items s =
        (s,items) ||> Seq.fold(fun s v -> Set.add v s)
    [<NoComparison;NoEquality>]
    type MappedCharacter = { Character:CharacterLink;Prodigies:Charsheets.Prodigies; ClassTalents: TalentCategory list;GenericTalents: TalentCategory list}
    let mapCharacterZones ch (items : HtmlAgilityPack.HtmlNode seq) =
        let m = 
            items
            |> Seq.choose(fun d -> Parse.parseZoneTitle d |> Option.map(fun x -> x, d))
            |> List.ofSeq
            |> fun zones ->
                let tfZone x = zones |> Seq.tryFind(fst>>(=) x) |> Option.map snd
                match tfZone "Character", tfZone "Prodigies", tfZone "Generic Talents", tfZone "Class Talents" with
                | Some ch2, Some prod, Some gens, Some cls ->
                    let ch2 = dissectCharsheetHead ch2
                    let prodigies = prod |> dissectProdigies
                    let extractTalents x =
                        let t =  x |> Parse.mapTalents |> List.ofSeq
                        t |> List.map fst, t |> List.collect snd
                        
                    let cls,errs = extractTalents cls
                    let gens,errs2 = extractTalents gens
                    if errs.Length > 0 || errs2.Length > 0 then
                        (errs,errs2).Dump("cls,gens errors")
                    
                    if cls.Length > 0 && cls |> Seq.exists(fun ct -> ct.Name |> Seq.exists(fun c -> int c > 255) |> not) then
                    
                        {   Character = ch; Prodigies= prodigies
                            ClassTalents = cls
                            GenericTalents= gens
                            
                        }
                        |> Some
                    else None
                | _ -> None
                
            
        m
    let extractTalentsOnly m =
        (Map.empty,m)
        ||> Map.fold(fun m k v ->
            match k with 
            | "Generic Talents" -> 
                m |> Map.add Generic v
            | "Class Talents" ->
                m |> Map.add Class v
            | _ -> m
        )
        
    let fetchCharacters pg inputs =
        Parse.getCharacters inputs
        |> Async.RunSynchronously
        |> Parse.getElement "table"
        |> Option.bind (Parse.getElement "tbody")
        |> Option.map (Parse.getElements "tr")
        |> Option.map(fun x ->
            printfn "%i characters found on page %i" x.Length (pg + 1)
            x
        )
        
    //type FetchType = | Path of string | Values of Map<string, string list>
    //let fetchMapped pageOpt v : Map<string,(Character * TalentFetchResult) seq option> =
    let fetchMapped pageOpt v =
        let fItem =
            Parse.rowToCharacter
            >> Option.bind(fun ch ->
                let m = getCharacter ch.Path |> Async.RunSynchronously |> Option.bind(dissectCharacterZones >> mapCharacterZones ch)
                m)
        let r = 
            let always,pg =
                match pageOpt with
                | Some pg -> alwaysInputs |> Map.add "page" ([string pg]), pg
                | None -> alwaysInputs, 0
            (always,v)
            ||> Map.fold(fun m k v ->
                Map.add k v m
            )
            |> fetchCharacters pg
            |> Option.map (Seq.choose fItem)
        r
        
        
    let sampleFetch () =
        fetchMapped None Map.empty
        |> Option.map(
            Seq.map (fun x -> x)
            //|> Seq.map(fun (x,y) -> x, y |> logErrors x)
            >> Seq.truncate 1
            >> Dump
            >> ignore
        )
let retrieveSample () =        
    [
        let opts = Map["tag_class[]",["12"]]
        Retrieval.fetchMapped None opts
        Retrieval.fetchMapped (Some 1) opts
    ]
    |> List.choose id
    
    |> Seq.map (fun x -> x)
    |> Seq.truncate 2
    |> Dump
    |> ignore
    
let paging = [None;Some 1]
paging
|> List.map(flip Retrieval.fetchMapped Map.empty)
|> Seq.choose id
|> Seq.collect id
|> List.ofSeq // need to do this now so the progress bar change ahead isn't misleading
|> Dump

showChars()
showUrls()