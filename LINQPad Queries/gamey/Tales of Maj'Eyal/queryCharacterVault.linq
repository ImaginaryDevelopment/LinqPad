<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>System.Windows.Media</Namespace>
</Query>

// does not handle pagination decently
// need to handle or filter unicode talent names :( - Character zone Size field may be an easy indicator
// need to pull up points spent in category for category analysis

type MappedClass = 
    | Adventurer
    | ArcaneBlade
    | Archmage
    | Marauder
    | Solipsist
    | SunPaladin
    with
        member x.FormValue =
            match x with
            | Adventurer -> string x, "104"
            | ArcaneBlade -> "Arcane Blade", "22"
            | Archmage -> string x, "7"
            | Marauder -> string x, "71"
            | Solipsist -> string x, "102"
            | SunPaladin ->  "Sun Paladin", "27"
let classOpt = Solipsist.FormValue |> Some
let authority = "https://te4.org/"
let path = "characters-vault"
let alwaysInputs =
    Map [
        "tag_official_addons", ["1"] // only querying characters using only official addons
        //"tag_permadeath[]",["66"] // roguelike - 1 life
        "tag_permadeath[]",["65"] // adventure - x lives
        "tag_difficulty[]",["36"] // insane
        //"tag_level_min", ["50"]
        //"tag_dead",["dead"] // I query dead ppl
        "tag_winner", ["winner"] // only query winners
    ]
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
                urlDump path
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
type Character = {User:string;Name:string;Path:string;Link:obj}
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
            | userTd::charTd::_ -> (userTd,charTd)
            //| userTd::charTd::_ -> Some (userTd,charTd)
            //| td :: [] when (getText td).Contains("No characters available") -> None
            | bad -> 
                (bad |> List.map getOuterHtml).Dump("character row failure")
                failwithf "unexpected number of elements %i in %s" bad.Length (bad |> List.map getOuterHtml |> String.concat "")
        |> (fun (userTd,charTd) ->
                let _userhref,uname = userTd |> getElement "a" |> Option.get |> fun a -> a.GetAttributeValue("href", String.Empty), a.InnerText
                let cHref,cName = charTd |> getElement "a" |> Option.get |> fun a -> a.GetAttributeValue("href",String.Empty), a.InnerText
                {User=uname;Name=cName;Path=cHref;Link=null}
            )
                //cHref,sprintf "%s: %s = %s" uname cName cHref
            
type TalentPower = TalentPower of string with member x.ToDump() = match x with | TalentPower v -> v
type CategoryInvestment = CategoryInvestment of int with member x.ToDump() = match x with | CategoryInvestment v -> v
type CategoryAnaly = { TotalPoints: int; TalentsObtained:int}

//type CDisp = CDisp of string with member x.ToDump() = match x with | CDisp v -> v
type TalentFetchResult =  Map<TalentCategoryType,(TalentCategory * Err list) list>
module Retrieval =    
        
    Parse.setupInputs() // not needed/used, featureset deferred
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
    let fetchMapped pageOpt v : (Character * TalentFetchResult) seq option =
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
            |> Option.map (
                Seq.map (Parse.rowToCharacter >> fun ch -> ch, getCharacter ch.Path |> Async.RunSynchronously |> Option.map(dissectCharacterZones >> mapCharacterZones >> extractTalentsOnly) |> Option.defaultValue Map.empty)
            )
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
    
type SequenceStatData = { Mean:decimal;Median:int; Min:int;Max:int;Population:int}

module Invert =
    // correct shape of data for munging with other characters, is the shaping of a single character's data
    type CCategoryAnalysis = Map<TalentCategoryType,Map<string, Character*TalentPower*CategoryAnaly>>
    // analysis of how popular a talent categories are by talent category type (generic vs class) with characters
    type CategoryAnalysis = Map<TalentCategoryType,Map<string, (Character*TalentPower*CategoryAnaly) list>>
    
    let analyzeCategories (ch:Character,m:Map<TalentCategoryType,TalentCategory list>) : CCategoryAnalysis = 
        m
        |> Map.map(fun _tct tcs ->
            let m': Map<string,Character * TalentPower*CategoryAnaly> = Map.empty
            (m',tcs)
            ||> List.fold(fun m tc ->
                let ci = tc.Talents |> List.sumBy(fun t -> t.Points)
                let cCount = tc.Talents |> List.filter(fun t -> t.Points > 0) |> List.length
                let ca = {TotalPoints = ci; TalentsObtained = cCount}
                m |> Map.add tc.Name (ch,TalentPower tc.Power, ca)
            )
        )
    // analyze multiple characters
    let analyzeCCategories (items:CCategoryAnalysis list) : CategoryAnalysis =
        let m:CategoryAnalysis = Map.empty
        let r : CategoryAnalysis = 
            (m,items)
            ||> List.fold(fun m (ca:CCategoryAnalysis) ->
                (m,ca)
                ||> Map.fold(fun m tct v ->
                    let nextV =
                        if m.ContainsKey tct then
                            m.[tct] |> Map.mergeAsList v
                        else v |> Map.map (fun _ v -> [v])
                    m |> Map.add tct nextV
                    
                )
            )
        r
        
[<NoComparison>]
type TalentError = {Tct:TalentCategoryType; Ch:Character; Errors: (TalentCategory*Err list) list}
let liftFetchErrors (talents:(TalentCategory * Err list) list) : TalentCategory list * ((TalentCategory*Err list) list) =
    let init = List.empty, List.empty
    (init,talents)
    ||> List.fold(fun (c,e) (tc,errs) ->
        tc::c, if List.isEmpty errs then e else (tc,errs)::e
    )
    
// pull out the errors for aggregation with other character errors        
let extractFetchErrors (ch,m:TalentFetchResult) =
    let cl,dirty = 
        let init = Map.empty, List.empty
        (init,m)
        ||> Map.fold(fun (cl,d) tct talents ->
            let clean,dirty = liftFetchErrors talents
            cl |> Map.add tct clean, if List.isEmpty dirty then d else (ch,dirty) :: d
        )
    (ch,cl), dirty
        
let filterErrors (items : _ seq) =
    Util.Progress <- Nullable 50
    let init = List.empty, List.empty 
    let clean,dirty =
        (init,items)
        ||> Seq.fold(fun (cl,d) item ->
            let clean,dirty = extractFetchErrors item
            clean::cl, if List.isEmpty dirty then d else dirty::d
        )
    dirty.Dump("failed items")
    clean

let opts =
    Map[
        match classOpt with
        | Some (_name,v) -> yield "tag_class[]",[v]
        | None -> ()
    ]
let paging = [None;Some 1]
paging
|> List.map(flip Retrieval.fetchMapped opts)
|> Seq.choose id
|> Seq.collect id
|> List.ofSeq // need to do this now so the progress bar change ahead isn't misleading
|> Seq.map (fun x -> x)
//|> Seq.truncate 2
|> filterErrors
|> List.ofSeq
|> Seq.map Invert.analyzeCategories
|> List.ofSeq
|> Invert.analyzeCCategories
|> fun x ->
    // potential sample data shape for charting:
    // Talent Category line
    // Sub Talent lines
    [ "Spell / Fire", 5.0 / 30.0] |> ignore
    let characters =
        let s = x |> Map.toSeq |> Seq.map snd |> Seq.collect Map.toSeq |> Seq.collect snd |> Seq.map(fun (ch,_,_) -> ch.Name) |> Seq.distinct |> Seq.length
        s
    let gens = x.[Generic]
    let chartf f m =
        let data: (string*decimal) seq = f m 
        data.Chart(Func<_,_>(fst >> box),Func<_,_>(snd>>box), Util.SeriesType.Bar)//.DumpInline(heading=title)
        
    let chartTPtsAvg (m:Map<string,(Character*TalentPower*CategoryAnaly) list>) =
        m |> chartf (fun m ->
            m
            |> Map.toSeq
            |> Seq.filter(fst >> fun name -> int name.[0] < 255)
            |> Seq.map(fun (k,v) ->
                let cas = v |> List.map(fun (_,_,ca) -> ca)
                let total = cas |> List.map(fun ca -> ca.TotalPoints) |> Seq.sum
                k, decimal total / decimal characters
            )
            |> Seq.sortByDescending snd
            |> Seq.truncate 34 // adventurer can be too many for charting
        )
    let chartPtsExist (m:Map<string,(Character*TalentPower*CategoryAnaly) list>) =
        m |> chartf (fun m ->
            m
            |> Map.toSeq
            |> Seq.filter(fst >> fun name -> int name.[0] < 255)
            |> Seq.map(fun (k,v) ->
                let cas = v |> List.map(fun (_,_,ca) -> ca)
                let total = cas |> List.filter(fun ca -> ca.TotalPoints > 0) |> Seq.length
                k, decimal total / decimal characters
            )
            |> Seq.sortByDescending snd
            |> Seq.truncate 34 // adventurer can be too many for charting
            
        )
    let pagingText = sprintf "attempted %i characters on %i page(s)" characters paging.Length 
    let ctptsavg (cls,gens) =  
        let fullTitle = (classOpt |> (function | Some (name,_) -> sprintf "%s " name | None -> String.Empty) |> sprintf "Avg Pts per character - %s of %s" pagingText ) 
        fullTitle, chartTPtsAvg cls, chartTPtsAvg gens
    let ctptex (cls,gens) =
        let fullTitle = (classOpt |> (function | Some (name,_) -> sprintf "%s " name | None -> String.Empty) |> sprintf "Avg Characters Did Invest - %s of %s" pagingText) 
        fullTitle,chartPtsExist cls, chartPtsExist gens
        
    let cls = x.[Class]
    let getBitMap (ch:LINQPadChart) = ch.ToBitmap()
    (
        let title,clsd,gend = ctptsavg (cls,gens)
        //[clsd;gend] |> List.map getBitMap |> fun x -> x.Dump(title)
        Util.HorizontalRun(false, clsd |> getBitMap,gend |> getBitMap).Dump(title)
    )
    (
        let title,clsd,gend = ctptex (cls,gens)
        Util.HorizontalRun(false, clsd |> getBitMap,gend |> getBitMap).Dump(title)
    )
        
    x
//|> Dump
|> ignore

// heat not included yet
module Heat =
    let colorIntensity (v:float) min max (colorRange:Color seq) =
        if v < min || v > max then failwithf "value %0.3f should be between min (%0.3f) and max (%0.3f)" v min max
        let nV = (v - min) / (max - min) |> float32 // between 0..1
        let steps = colorRange |> Seq.length
        let step = 1.f / float32 steps
        let minV = nV - step
        let maxV = nV + step
        let (<->) x (a,b) = x>=a && x<=b //between operator
        seq {for i in 0..steps -> (float32 i) * step} 
        |> Seq.windowed 2 
        |> Seq.map(fun r -> r.[0],r.[1])
        |> Seq.map(fun (a,b) ->
            if nV <-> (a,b) then abs((a-b)/2.0f-nV)
            elif minV <-> (a,b) then step-(nV-b)
            elif maxV <-> (a,b) then step-(a-nV)
            else 0.0f)
        |> Seq.zip colorRange 
        |> Seq.map (fun (c,s) -> s,Color.FromScRgb(1.f, c.ScR*s, c.ScG*s, c.ScB*s))
        |> Seq.filter (fun (a,_) -> a>0.f)
        |> Seq.map (fun (_,c)-> c)
        |> Seq.fold (+) (Color.FromScRgb(1.f,0.f,0.f,0.f))
        
()
showUrls()