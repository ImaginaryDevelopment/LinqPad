<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// play with stash tab api

let target = "http://www.pathofexile.com/api/public-stash-tabs"

module Helpers =
    let (|NullString|EmptyString|WhiteSpace|ValueString|) (x:string) =
        match x with
        | null -> NullString
        | x when String.IsNullOrEmpty x -> EmptyString
        | x when String.IsNullOrWhiteSpace x -> WhiteSpace
        | _ -> ValueString
    let delimit (d:string) x = 
        let v = x |> Array.ofSeq
        String.Join(d,value= v)
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
        
open Helpers
        
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
    let deserializeT<'T> (x:string) = JsonConvert.DeserializeObject<'T>(x)
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
    | Continue of nextChangeId:string
    | Finished
let fetch useCache (fKey:string option -> string) (fContinue:string -> 'T option * SequenceState) = 
    let fetch changeIdOpt =
        use client = new System.Net.Http.HttpClient()
        let target = match changeIdOpt with | None -> target | Some x -> sprintf "%s?id=%s" target x
        Async.RunSynchronously(async{
                return client.GetStringAsync(target)
            }).Result
    
    //maybeCache fetch "public-stash-tabs" useCache
    SequenceState.Start
    |> Seq.unfold(
        function 
        | Start -> 
            printfn "getting first item"
            let result = maybeCache (fun _ -> fetch None) (fKey None) useCache
            printfn "finished fetch"
            fContinue result
            |> Some
        | SequenceState.Continue changeId ->
            printfn "getting %s" changeId
            // if what you get back is empty, we reached the end
            let result = maybeCache(fun _ -> Some changeId |> fetch ) (Some changeId |> fKey) useCache
            printfn "finished fetch"
            fContinue result
            |> Some
        | SequenceState.Finished ->
            None
    )
    |> Seq.choose id
type FetchStat = {Fetches:int; TextLength: int64} with
    member x.LengthDisplay =         
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
        formatBytes x.TextLength
let fStatsOut = 
    let dc = DumpContainer()
    let mutable fetchStat = {Fetches=0; TextLength=0L}
    
    dc.Dump("length")
    dc.Content <- fetchStat
    (fun l -> 
        fetchStat <- {Fetches = fetchStat.Fetches + 1; TextLength = l + fetchStat.TextLength}
        dc.Content <- fetchStat
    )
        
let deserial text = 
    //text.Dump()
    let x = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string,JToken>>(text)
    fStatsOut (text.Length |> int64)
    x    
type Item = {Name:string;NamePrefix:string; TypeLine:string; TypeLinePrefix:string; Verified:bool; Identified:bool; Corrupted:bool; Icon:obj; } with 
    member x.ToDump() =
        // if the thing has already been transformed, don't do it again
        match x.Icon with
        | :? string as uri ->
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
            //printfn "Name:%s,NP:%s,X:%A" x.Name nPrefix x
            // this creates a new one, which means the new one will have dump called again
            r
        | _ -> 
            //printfn "Recursed :("
            x
        
        
        
type Stash = {AccountName:string;LastCharacterName:string;Id:string; Stash:string;League:string; StashType:string;Public:bool; Items:Item[];} //with
//    member x.ToDump():obj =
//        printfn "preparing to dump"
//        {x with Items = x.Items |> Seq.cast<JObject> |> Seq.map(fun item -> item.Properties() |> Seq.map(fun p -> box(p.Name, p.Value)) |> Array.ofSeq) |> Seq.map box |> Array.ofSeq  }
//        |> box
let pairOfF f x = f x, x

let rawPair f x = f x, Util.OnDemand("Raw", fun _ -> x)
fetch true 
    (function | None -> "public-stash-tabs" | Some changeId -> sprintf "public-stash-tabs,%s" changeId)
    (function
        | null
        | WhiteSpace
        | EmptyString -> None, Finished
        | raw ->
            let data = deserial raw
            let nextChangeId = data.["next_change_id"]
            Some data, Continue (nextChangeId |> string)
    )
|> Seq.collect(fun dic -> 
    dic.["stashes"] :?> JArray
)
|> Seq.cast<JObject>
|> Seq.map(fun x -> x |> string |> deserializeT<Stash>, Util.OnDemand("Raw", fun _ -> x |> string))
|> Seq.takeWhile (fun (x,_) -> x.LastCharacterName <> "DontLetMeGetMe")
//|> Seq.filter(fun (x,_) -> x.LastCharacterName = "DontLetMeGetMe")
|> Seq.maxBy(fun (x,_) -> if isNull x.AccountName then 0 else x.AccountName.Length)
|> fun (x,raw) -> x.AccountName.Length, x, raw
//|> Seq.filter(fun (x,_) -> x.Items.Length > 0)
//|> Seq.take 2
|> Dump
|> ignore