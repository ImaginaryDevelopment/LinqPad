<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// retrieve own stashes and possibly search
// for help see https://np.reddit.com/r/pathofexiledev/

(* 
    doesn't work without authorization =/
    let url = "https://www.pathofexile.com/character-window/get-stash-items?league=Harbinger&tabs=1&tabIndex=1&accountName=DevelopersDevelopersDevelopers" 
*)

//let target = "http://www.pathofexile.com/api/public-stash-tabs"
let origin = Uri "https://www.pathofexile.com"
let path = "/character-window/get-stash-items"
let stashStartIndex = 0

type SearchType =
    | FindEnchanted

let searchTypeOpt = Some FindEnchanted

let reportUrls = false

type [<Measure>] ms
type [<Measure>] s
type [<Measure>] min

module Seq = 
    // cast one by one, return result only if all items were sucessful
    // not setup for efficiently handling a large number of items
    // result will be reversed if built efficiently?
    let tryCast<'T> (items: IEnumerable) = 
        items
        |> Seq.cast<obj>
        |> Seq.fold (fun castedItems nextItem ->
            match castedItems with
            | Some items ->
                match nextItem with
                | :? 'T as t -> 
                    Some (t :: items)
                | _ -> 
                    None
            | None -> None
                
            
        ) (Some List.Empty)
        |> Option.map (List.ofSeq>> List.rev)
        
    let catch (substr:string) (x:_ seq) =
        let e = x.GetEnumerator()
        e
        |> Seq.unfold(fun e ->
            try
                if e.MoveNext() then
                    Some (e.Current,e)
                else None
            with ex ->
                if ex.Message.Contains substr then None
                else reraise()
        )
        
        
        
        
module Helpers =
    let isNotNull =
        function
        | null -> false
        | _ -> true
    let (|NullString|EmptyString|WhiteSpace|ValueString|) (x:string) =
        match x with
        | null -> NullString
        | x when String.IsNullOrEmpty x -> EmptyString
        | x when String.IsNullOrWhiteSpace x -> WhiteSpace
        | _ -> ValueString
    let delimit (d:string) x = 
        let v = x |> Array.ofSeq
        String.Join(d,value= v)
    let contains (x:'t) (items: 't seq) = items.Contains(x)
    let containsSubStr (x:string) (v: string) = isNotNull v && v.Contains(x)
    let toLower (x:string) = if isNotNull x then x.ToLower() else x
    let containsAnySubStr (subs:string seq) (items: string seq) =
        items
        |> Seq.exists(fun item ->
           subs
           |> Seq.exists(fun sub -> item.Contains sub)
        )
    let indexOf (delimiter:string) (s:string) :int = 
        s.IndexOf delimiter
    let lastIndexOf (delimiter:string) (s:string) :int =
        s.LastIndexOf delimiter
    let trim (s:string) = s.Trim()
    let split (delimiters: string[]) (options:StringSplitOptions) (s:string) = 
        s.Split(delimiters,options)
    
    let private b4 f (s:string) = 
        s.Substring(0,f s)
    let private aft f (s:string) = 
        s.Substring(f s)
        
    let before (delimiter:string) (s:string) :string =
        b4 <| indexOf delimiter <| s
        
    let after (delimiter:string) (s:string) :string = 
        indexOf delimiter >> (+) delimiter.Length
        |> aft <| s
    
    let beforeLast (delimiter:string) (s:string) :string = 
        if s.Contains delimiter then 
            b4 <| lastIndexOf delimiter <| s
        else s
        
    let afterLast delimiter (s:string) = 
        lastIndexOf delimiter >> (+) delimiter.Length
        |> aft <| s
    let afterLastOrSelf delimiter s = 
        if lastIndexOf delimiter s > -1 then
            afterLast delimiter s
        else s
    let beforeLastOrNone delimiter s = 
        if lastIndexOf delimiter s > -1 then
            beforeLast delimiter s
        else String.Empty
    let beforeLastInclusiveOrNone delimiter s =
        match beforeLastOrNone delimiter s with
        | NullString 
        | EmptyString as x -> x
        | (WhiteSpace as x)
        | (ValueString as x) -> 
            let result = sprintf "%s%s" x delimiter
            //printfn "d:%s,s:%s,value:%s" delimiter s result
            result
    let sleep (delay:decimal<ms>) x =
        let intDelay = delay |> decimal |> int
        async {
            do! Async.Sleep intDelay
            return x
        }
 
open Helpers
(*
Including your session ID ("logged in") will change how certain policies track your limits.
In the case of the trade search policy, logging in will rate-limit you based on your account instead of your IP (with the same limits) 
        and the rate-limit based on your IP will allow double the requests per interval.
You can see this change in the headers if you try making requests with your session ID.
*)

// if you can make {limit} requests per minute, what should the delay be between requests?
// yes it is sloppy and doesn't include the actual processing time of the previous request
let sToMs = 1000m<ms/s>
let calculateDelay (limit:decimal<1/min>) : decimal<ms> = // for the purposes of PoE's api it says you can't make more than 20 every 5 seconds
    let mnToS = 60m<s/min>
    //let limit = 160m<1/min>
    let getDelayMsFromMax (maxPerMinute:decimal<1/min>) : decimal<ms> = 
        1m / (maxPerMinute / mnToS / sToMs)
    limit |> getDelayMsFromMax
    // buffer + int truncation helper
    |> (+) 50.8m<ms>
    
module Cereal =
    open Newtonsoft.Json
    let (|JValueArray|_|) (jt:Linq.JToken) = 
        match jt with 
        | :? Linq.JArray as jv -> 
            match jv.ToArray() |> Seq.tryCast<Linq.JValue> with
            | Some values -> Some values
            | _ -> None
        | _ -> None
    let deserialize (t:Type) (x:string) = JsonConvert.DeserializeObject(x,t)
    let deserializeT<'T> (x:string) =
        try
            JsonConvert.DeserializeObject<'T>(x)
        with _ ->
            if x.Length < 400 then x.Dump("failed deserialize")
            reraise()
    let deserializeWithRawT<'T> f (x:string) = 
        let r:'T = deserializeT x
        f x r
    //TODO: we need some f such that it does the transform to x with raw filled in
    //let deserializeRecursively f


open System.Runtime.CompilerServices
open Cereal
    
let maybeCache f cacheKey useCache = 
    if useCache then
        let result = Util.Cache((fun _ -> f()), cacheKey)
        result
    else
        f()
type SequenceState = 
    | Start
    | Continue of nextTabId:int
    | Finished
    
let fFetch = 
    let dcFetchStatus = DumpContainer()
    let mutable textBuffer = List.empty
    let mutable dumped = false
    (fun (s:string) ->
        if dumped |> not then
            dumped <- true
            dcFetchStatus.Dump("Fetch status")
        match textBuffer with
        | [] -> 
            textBuffer <- [s]
        | a::[] -> 
            textBuffer <- s::[a]
        | a::b::[] ->
            textBuffer <- s::a::b::[]
        | a::b::c::[] ->
            textBuffer <- s::a::b::c::[]
        | _ -> textBuffer <- List.empty
        dcFetchStatus.Content <- textBuffer
    )
type SecureStashParams = {
    League:string
    TabCountRequested:int
    TabStartIndex:int
}

let httpclient =
        // https://stackoverflow.com/questions/12373738/how-do-i-set-a-cookie-on-httpclients-httprequestmessage
        let c = System.Net.CookieContainer()
        let handler = new System.Net.Http.HttpClientHandler (CookieContainer=c)
        let client = new System.Net.Http.HttpClient(handler,BaseAddress=origin)
        c.Add(origin, System.Net.Cookie("POESESSID",Util.GetPassword("POESESSID")))
        client
        
let fetch useCache (fKey:string option -> string) (fContinue:int*string -> 'T option * SequenceState) ssp = 
    let makeQuery(path:string) opts = 
        let path = if path.EndsWith("?") then path else (path + "?")
        opts
        |> Seq.map(fun (k,v) ->
            sprintf "%s=%s" k v
        )
        |> String.concat "&"
        |> sprintf "%s%s" path
    let makeStashQuery ssp =
        makeQuery path [ //     league=Harbinger&tabs=1&tabIndex=1&accountName=DevelopersDevelopersDevelopers" 
            "league",ssp.League
            "tabs", string<int> ssp.TabCountRequested
            "tabIndex", ssp.TabStartIndex |> string<int>
            "accountName", "DevelopersDevelopersDevelopers"        
        ]
 
    let fetch tabIndex =
        let target = makeStashQuery {ssp with TabStartIndex=tabIndex}
        if reportUrls then
            target.Dump("url")
        let raw = Async.RunSynchronously(async{
                        return! Async.AwaitTask <| httpclient.GetStringAsync(target)
                    })
        tabIndex,raw
    
    SequenceState.Start
    |> Seq.unfold(
        function 
        | Start -> 
            fFetch "getting first item"
            let fetchOrSleepRetry f =
                try
                    f()
                with ex ->
                    if ex.Message.Contains("429") then
                        printfn "Retrying request"
                        let delay = 60m<s> * sToMs
                        sleep delay ()
                        |> Async.RunSynchronously 
                        f()
                    else reraise()
                
            let result = maybeCache (fun _ -> fetch ssp.TabStartIndex) (fKey None) useCache
            fFetch "finished fetch"
            fContinue result
            |> Some
        | SequenceState.Continue nextTabId ->
            fFetch (sprintf "getting %i" nextTabId)
            // if what you get back is empty, we reached the end
            let result = maybeCache(fun _ -> fetch nextTabId) (string nextTabId |> Some |> fKey) useCache
            fFetch "finished fetch"
            fContinue result
            |> Some
        | SequenceState.Finished ->
            None
    )
    |> Seq.choose id
    
let formatBytes (x:System.Int64) = 
    let k = 1000L
    let mb = k * 1000L
    let gb = mb * 1000L
    if x >= gb then
        float x / float gb |> sprintf "%.2f GB"
    elif x >= mb then
        float x / float mb |> sprintf "%.2f MB"
    elif x >= k then
        float x / float k |> sprintf "%.2f KB"
    else sprintf "%d" x
    
type FetchStat = {Fetches:int; TextLength: int64;Memory:int64} with
    member x.LengthDisplay = formatBytes x.TextLength
    member x.MemoryDisplay = formatBytes x.Memory
    
let fStatsOut = 
    let dc = DumpContainer()
    let mutable fetchStat = {Fetches=0; TextLength=0L;Memory=GC.GetTotalMemory true}
    
    dc.Dump("length")
    dc.Content <- fetchStat
    (fun l -> 
        fetchStat <- {Fetches = fetchStat.Fetches + 1; TextLength = l + fetchStat.TextLength; Memory = GC.GetTotalMemory true}
        dc.Content <- fetchStat
    )
        
let deserializeJToken text = 
    //text.Dump()
    let x = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string,JToken>>(text)
    fStatsOut (text.Length |> int64)
    x    
    
(* 
    {   "verified":false,"w":1,"h":1,"icon":"https:\/\/web.poecdn.com\/image\/Art\/2DItems\/Currency\/CurrencyRerollRare.png?v=c60aa876dd6bab31174df91b1da1b4f9&w=1&h=1&scale=1",
        "league":"Standard",
        "id":"d825910720c5d2d00de634a34d3ba9367ad972edf73f84abad2878cddc7dda4c",
        "name":"",
        "typeLine":"Chaos Orb",
        "identified":true,
        "ilvl":0,
        "properties":[
            {"name":"Stack Size","values":[["60\/10",0]],"displayMode":0,"type":32}
            ],
        "explicitMods":["Reforges a rare item with new random modifiers"],
        "descrText":"Right click this item then left click a rare item to apply it.",
        "frameType":5,"stackSize":60,"maxStackSize":5000,"x":2,"y":0,"inventoryId":"Stash1"}
*)
type Requirement = { Name:string;Values:int[][];DisplayMode:int;Suffix:string}

[<NoComparison;NoEquality>]
type Item = {   Name:string;NamePrefix:string; TypeLine:string; InventoryId:string; Requirements:Requirement[];TypeLinePrefix:string; ILvl:int
                EnchantMods:string[];ImplicitMods:string[];FracturedMods:string[];ExplicitMods:string[];CraftedMods:string[]
                DescrText:string; (*Verified:bool;*) Identified:bool; Corrupted:bool; (*League:string;*) Icon:obj
                SocketedItems:Item[]
                } with 
    member x.ToDump() =
        // if the thing has already been transformed, don't do it again
        match x.Icon with
        | :? string as _uri ->
            let icon = match x.Icon with | :? string as uri -> lazy(Util.Image(uri)) |> box | i -> i
            let sanitizeWord = afterLastOrSelf ">"
            let getPrefixOrNone = beforeLastInclusiveOrNone ">"
            let nPrefix = x.Name |> getPrefixOrNone
            let r = {   x with 
                            Icon = icon
                            Name=x.Name |> sanitizeWord
                            NamePrefix= nPrefix
                            TypeLine= x.TypeLine |> sanitizeWord
                            TypeLinePrefix=x.TypeLine |> getPrefixOrNone}
            match r.TypeLinePrefix with
            | NullString
            | EmptyString -> ()
            | WhiteSpace as s -> printfn "should this whitespace be here?'%s'" s
            | ValueString as s when s.Contains("<") -> ()
            | _ -> printfn "Bad typeline prefix result: %A -> %A" x r
            
            // this creates a new one, which means the new one will have dump called again (makes this recursive)
            r
        | _ -> 
            //printfn "Recursed :("
            x
        
type StashType = 
    | Blight
    | Currency
    | Delirium
    | Delve
    | Divination
    | Fragment
    | Essence
    | Map
    | Metamorph
    | Quad
    | Standard
    | Unique
    | Unknown
    with
        static member All =
            [
                Blight
                Currency
                Delirium
                Delve
                Divination
                Fragment
                Essence
                Map
                Metamorph
                Quad
                Standard
                Unique
                Unknown
            ]
        static member GetLayout (x:StashType) =
            string x
            |> toLower
            |> sprintf "%sLayout"
        static member GetLayouts () =
            StashType.All
            |> List.map StashType.GetLayout
    
type Colour = {
    R:int
    G:int
    B:int
}
[<NoComparison;NoEquality>]
type Tab = {
    N:string
    I:int
    Id:string
    Type:string
    Hidden:bool
    Selected:bool
    colour:Colour
    //srcL:string
    //srcC:string
    //srcR:string
}

[<NoComparison;NoEquality>]
type StashResponseRaw = {
   NumTabs:int // how many tabs total are there 
   Tabs: Tab[]
   CurrencyLayout: obj
   EssenceLayout: obj
   MetamorphLayout:obj
   BlightLayout:obj
   //Items: Item[] // skip items we are handling those special
}

let dontDumpKeys = Set[
    yield "numTabs"
    yield "tabs"
    yield "items"
    yield! StashType.GetLayouts()
]

type StashStat = {Included:int;Excluded:int} with 
    member x.AllStashes = x.Included + x.Excluded
    
module List = 
    let maxOpt =
        function
        | [] -> None
        | x -> x |> Seq.max |> Some
    let maxByOpt f = 
        function
        | [] -> None
        | x -> x |> Seq.maxBy f |> Some
        
let fStashStats = 
    let dc = DumpContainer()
    let mutable stats = {Included = 0; Excluded=0}
    dc.Dump("ItemStats")
    dc.Content<- stats
    (fun (items: (Item*obj) list) (filtered: (Item*obj) list) ->
        
        stats <- {Included = stats.Included + filtered.Length ;Excluded= stats.Excluded + items.Length - filtered.Length}
        dc.Content <- stats
    )
let wrapRaw x = Util.OnDemand("Raw", fun _ -> x)

let remembral<'t,'tExpensive> () =
    let mutable x:'t option = None
    let getter (f:'tExpensive -> 't) a =
        match x with
        | None ->
            let value = f a
            x <- Some value
            value
        | Some v -> v
    getter
        
type FetchedStash = {
    Index:int
    Name:string
    StashType:StashType
    FilteredItems: (Item * obj) list
}

// league was Harbinger at the time of the original code here
let fetchStashes league useCache:seq<_>= 
    let getMap = remembral<Tab[], IDictionary<string,JToken>>()

    fetch useCache
        (function | None -> "public-stash-tabs" | Some tabId -> sprintf "public-stash-tabs,%s" tabId )
        (function
            | _,null
            | _,WhiteSpace
            | _,EmptyString -> None, Finished
            | i,raw ->
                //raw.Dump("Yay data?")
                let data = deserializeJToken raw // we need to figure out where the data is that shows how many stash tabs remain, or what index we just requested?
                //let experiment = deserializeT<StashResponseRaw> raw
                //experiment.Dump(sprintf "tab %i?" i)
                let delay = calculateDelay ( 18m*5m<1/min> )
                sleep delay ()
                |> Async.RunSynchronously
                // diagnostic dumping of unseen keys
                data.Keys
                |> Seq.filter(fun x -> dontDumpKeys |> Set.contains x |> not)
                |> Seq.iter(fun k ->
                    data.[k].Dump(description=sprintf "found unusual key %s %i" k i,depth=Nullable 1)
                    |> ignore<JToken>
                )
                // turn off the visualization helpers that the api sends for us to draw the stash tab
                //data.Remove("currencyLayout") |> ignore<bool>
                
                Some (i,data,raw), Continue (i+1) // this needs to change if the api is willing to retrieve more than 1 stash tab at once
        ) {
            League = league
            TabCountRequested = 1 // possible error if there aren't enough remaining also may return less than requested
            TabStartIndex = stashStartIndex 
        }
    |> Seq.map(fun (i,dic,raw) -> 
        let keys = dic.Keys |> Seq.map string |> Set
        let tabMap = getMap (fun dic -> dic.["tabs"] |> string |> deserializeT) dic
        if isNull dic.["items"] || (string dic.["items"]).Length < 10 then invalidOp "items missing"
        let stashType =
            [
                "blightLayout", Blight
                "currencyLayout", Currency
                "essenceLayout", Essence
                "fragmentLayout", Fragment
                "metamorphLayout", Metamorph
            ]
            |> List.choose(fun (k,t) ->
                if keys |> Set.contains k then t |> Some
                elif keys = Set ["numTabs";"tabs";"items"] then Some Standard
                else None
            )
            |> List.tryHead
            |> Option.defaultValue Unknown
           
        let filter(item:Item) = 
            let filters: (unit -> bool) list =
                let isNotType t () = String.IsNullOrWhiteSpace item.TypeLine || item.TypeLine <> t
                let typeHasNo (t:string) () = String.IsNullOrWhiteSpace item.TypeLine || not <| item.TypeLine.Contains t
                let hasEnchant () = isNotNull item.EnchantMods && item.EnchantMods.Length > 0
                let hasEnchantT (x:string) () = hasEnchant() && item.EnchantMods |> Seq.exists (containsSubStr x)
                let hasInName (x:string) () = isNotNull item.Name && item.Name.Contains(x)
                [
                    //fun stash -> isNull stash.AccountName |> not
                    //fun stash -> stash.Items |> Seq.exists(fun item -> item.League = league)
                    //fun item -> 
                    //    isNotNull item.SocketedItems && item.SocketedItems.Length > 0 
                    //hasEnchant
                    //hasEnchantT "Hatred"
                    //hasEnchantT "Blight Area"
                    //typeHasNo "Ring"
                    //typeHasNo "Amulet"
                    //typeHasNo "Shield"
                    //typeHasNo "Vise"
                    //fun item -> isNull item.TypeLine || not <| item.TypeLine.Contains "Amulet"
                    //fun item -> isNotNull item.ImplicitMods && item.ImplicitMods.Length > 0 && item.ImplicitMods.[0] <> "Item sells for much more to vendors"
                    hasInName "Vixen"
                    
                    //        ()
                ]
            filters
            |> Seq.fold(fun x f -> 
                x && f ()) true
        let items = 
            dic.["items"] :?> JArray
            |> Seq.cast<JObject>
            |> Seq.map(fun jo -> jo |> string |> deserializeT<Item>, jo |> string |> wrapRaw |> box)
            |> List.ofSeq
        // we should really only ever do this once
        let filtered = 
            items
            |> List.filter(fst >> filter)
        fStashStats items filtered
        {
            Index = i
            Name = tabMap |> Seq.tryFind(fun t -> t.I = i) |> Option.map(fun t -> t.N) |> Option.defaultValue "-1"
            StashType = stashType
            FilteredItems = filtered
        }, raw
        //stashType,filtered,raw
    )
let useCache = true
let deserializeStashResp raw =
    deserializeT<StashResponseRaw> raw
    
fetchStashes "Standard" useCache
//|> Seq.truncate 2
|> Seq.filter(fun (s,_) ->
    s.FilteredItems.Length > 0
    //match s.StashType with
    //| Currency -> false
    ////| Fragment -> false
    //| Essence -> false
    //| Standard -> false
    //| _ -> true
)
//|> Seq.map(fun (s, _) ->
//    string s
//)
|> Seq.map fst
|> Seq.catch "404"
|> Seq.truncate 1
|> Dump
|> ignore