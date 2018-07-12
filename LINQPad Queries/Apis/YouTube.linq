<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// youtube api
open System.Net.Http
type DumpRevType = 
    |Final
    |Cont
    
module Helpers =
    let trim (x:string) = x.Trim()  
    let tee<'T> f (x:'T) = 
        f x |> ignore;
        x
    let dumpReverse  =
        let dcF = DumpContainer() |> Dump
        let dc = DumpContainer()
        dc.Dump() |> ignore
        (fun dumpRevType o -> 
            match dumpRevType with
            | Cont -> 
                match dc.Content with
                | :? (obj list) as items -> List.Cons(o,items)
                | _ -> [ o ]
                |> fun content -> dc.Content <- content
            | Final ->
                dcF.Content <- o
        )
open Helpers

module Serialization = 
    open Newtonsoft.Json
    let deserialize (t:Type) (x:string) = JsonConvert.DeserializeObject(x,t)
    let deserializeT<'T> (x:string) = JsonConvert.DeserializeObject<'T>(x)
    // consider this to ignore serialization of properties when the name = raw
    //type private JsonIgnoreRawResolver =
    let deserializeObjectReflectively<'T>(x:string) = 
        let rec deserializeR(t:Type) x = 
            let isRecord = Microsoft.FSharp.Reflection.FSharpType.IsRecord t
            let props = t.GetProperties() |> List.ofSeq
            if t.IsClass then
                let dObj = 
                    try
                        JsonConvert.DeserializeObject<Linq.JToken>(x)
                    with ex -> 
                        printfn "Failed to deserialize %s" x
                        reraise()
                match dObj with
                | :? Linq.JValue as jv -> 
                    printfn "JValue: %A" jv.Value
                    jv.Value 
                | :? Linq.JObject as jo ->
                    
                    let raw = jo.ToString()
                    printfn "JObject: %s" raw
                    let result =  raw |> deserialize t
                    
                    match props |> Seq.tryFind(fun p -> p.Name = "Raw" && p.PropertyType = typeof<string> || p.PropertyType = typeof<obj>) with
                    | Some pRaw when pRaw.PropertyType = typeof<string> -> 
                        if isRecord then
                            printfn "Making record"
                            let nextProps = 
                                try
                                    props 
                                    |> Seq.map(fun p -> 
                                            if p=pRaw then 
                                                box raw
                                            else
                                                printfn "Attempting to deserialize to %s:%s from %A" p.Name p.PropertyType.Name raw
                                                p.GetValue result |> box
                                    )
                                    |> Array.ofSeq
                                with ex ->
                                    printfn "Failed to get nextProps"
                                    reraise()
                            printfn "Properties = "
                            try
                                let result = FSharp.Reflection.FSharpValue.MakeRecord(t,nextProps)
                                result
                            with ex ->
                                nextProps.Dump(sprintf "Failed to make record for %s" t.Name)
                                printfn "Failed to get properties for %s from %A" t.Name nextProps
                                reraise()
                        else        
                            pRaw.SetValue(result,raw)
                            result
                    | Some p -> 
                        p.SetValue(result,Util.OnDemand("Raw",fun () -> raw))
                        result
                    | None -> 
                        result.Dump("item!")
                        result
            else
                failwith "oopsies"
        let t = typeof<'T>
        deserializeR t x :?> 'T
            
    
    let deserialize2<'T>(x:string) : 'T =
        deserializeObjectReflectively<'T> x
open Serialization

// may only be working for me because of oauth grant when run from https://developers.google.com/apis-explorer/#p/youtube/v3/
let apiKey = Util.GetPassword("GoogleApiServerKey") |> trim
let userName = Util.Cache((fun () -> Util.ReadLine("Youtube username?")),"username")
let getData :string -> string = 
    let mutable counter = 0
    fun(uri:string) -> 
        if String.IsNullOrEmpty uri then 
            failwithf "uri was null or empty"
        printfn "Getting uri %i: %s" counter uri
        counter <- counter + 1
        use client = new HttpClient(BaseAddress=Uri("https://www.googleapis.com"))
        client.DefaultRequestHeaders.Add("Accept", "application/json")
        client.GetStringAsync(uri).Result
let getCachedData key uri = 
    Util.Cache((fun () -> getData uri),key)
module YouTubes = 
    type PageInfo = {TotalResults:int; ResultsPerPage:int}
    
    type PlaylistIdentifier = {Kind:string; ETag:string; Id:string}
    
    type PlaylistIdContainer = {Kind:string; ETag:string; PageInfo:PageInfo; Items:PlaylistIdentifier[]; Raw:string } // might be that list is only working because there was a single item
    type Thumbnail = {Url:string;Width:int;Height:string}
    type ThumbnailContainer = {Default: Thumbnail; Image:obj}
    type ChannelPlaylistSnippet = {PublishedAt:string; ChannelId:string; Title:string; Description:string; Thumbnails: ThumbnailContainer}
    type ChannelPlaylistId = {Kind:string; ETag:string; Id:string; Snippet:ChannelPlaylistSnippet}
    type ChannelPlaylistContainer = {Kind:string; ETag:string; PageInfo:PageInfo; Items:ChannelPlaylistId list}
    
    type Resource = {Kind:string;VideoId:string}
    type PlaylistItemSnippet = {PublishedAt:DateTime; ChannelId:string; Title:string; Description:string; Thumbnails: ThumbnailContainer; Position:int; ResourceId:Resource}
    type PlaylistItem = {Kind:string; ETag:string; Id:string; Snippet:PlaylistItemSnippet}
    type PlaylistItemContainer = {Kind:string; ETag:string; NextPageToken:string; PageInfo:PageInfo; Items:PlaylistItem list}
    
open YouTubes

type PlaylistItemDisplay = {Title:string; Image:obj; Position:int; PublishedAt:DateTime; ShortDescription:string; Description:obj;Raw:obj}
let getChannels () = // includes playlists? maybe?
    getCachedData "channels" (sprintf "/youtube/v3/channels?part=id&forUsername=%s&key=%s" userName apiKey)
    |> (fun raw -> 
        try
            raw
        
            |> deserialize2<PlaylistIdContainer> 
            |> fun x -> {x with Raw=raw}
        with ex ->
            raw.Dump("Failed to convert to PlaylistIdContainer")
            reraise();
        )
let getLists channelId = 
    getCachedData "playlists" (sprintf "/youtube/v3/playlists?part=%s&channelId=%s&key=%s" ("snippet%2CcontentDetails") channelId apiKey)
    
let getPartialList (nextPageTokenOpt:string option) (listId:string) = 
    // can't cache? pageToken in responses may vary
    let getData (extra:string) = 
        getData(sprintf "/youtube/v3/playlistItems?part=snippet&playlistId=%s%s&key=%s" listId extra apiKey)
    //printfn "getPartialList %A" nextPageTokenOpt
    match nextPageTokenOpt with 
    | None -> getData null
    | Some pageToken -> 
        Util.Cache((fun () -> getData (sprintf "&pageToken=%s" pageToken)),sprintf "getPartialList:%s" pageToken)
    |> deserializeT<PlaylistItemContainer>
type PagingState =
    | Initial
    | Progress of nextToken:string option
let elipsize (x:string) = 
    if not <| String.IsNullOrEmpty x && x.Length > 150 then
        true,sprintf "%s..." (x.Substring(0,147))
    else false,x

let getWholeList listId = 
    Initial
    |> Seq.unfold(fun (pState:PagingState) ->
        // we are ignoring the total results field expecting the nextPageToken will be empty or null on the last page
        match pState with
        | Initial -> 
            printfn "Initial"
            getPartialList None listId
            |> Some
            
        | Progress (Some t) ->
            printfn "Getting page"
            getPartialList (Some t) listId
            |> Some
        | Progress None -> 
            printfn "Stopping"
            None
        |> Option.map(fun x ->
            printfn "container found! %A" (x.PageInfo, x.NextPageToken)
            let toDisplay (y:PlaylistItem) = 
                let shortened,sd = elipsize y.Snippet.Description
                let d = if shortened then Util.OnDemand("Full Description", fun () -> y.Snippet.Description) else null
                let t = if isNull <| box y.Snippet.Thumbnails then Unchecked.defaultof<_> else Util.Image(y.Snippet.Thumbnails.Default.Url)
                {Title=y.Snippet.Title; Position=y.Snippet.Position; PublishedAt=y.Snippet.PublishedAt;Image=t; ShortDescription=sd; Description=d; Raw=Util.OnDemand("Raw", null)}
            let itemized = 
                x.Items
                |> Seq.map (fun x -> 
                    try
                        toDisplay x
                    with ex ->
                        x.Dump("Failing item")
                        reraise()
                )
                |> List.ofSeq
            
            if String.IsNullOrWhiteSpace x.NextPageToken then
                //printfn "no next token! %A" x
                
                itemized, Progress None
            else
                let nextState = x.NextPageToken |> Some |> Progress
                itemized, nextState
        )
    )
    |> Seq.concat
    
getChannels()
//|> tee dumpReverse
|> fun x -> x.Items
|> Seq.map(fun x -> x.Id |> getLists)
//|> tee dumpReverse
|> Seq.mapi(fun i x -> 
    printfn "Deserializing object %i" i
    let r = deserializeObjectReflectively<ChannelPlaylistContainer> x
    printfn "Finished object %i" i
    r
)
|> Seq.collect (fun x -> x.Items)
|> Seq.map (fun x -> x.Snippet.Title, {x.Snippet.Thumbnails with Image = Util.Image(x.Snippet.Thumbnails.Default.Url)},x.Id)
//|> Seq.truncate 1
|> List.ofSeq
//|> tee dumpReverse
|> Seq.map(fun (title, data, listId) -> 
    //printfn "doing it and doing it and doing it well"
    data.Image, title, getWholeList listId )
// without this here it looped endlessly, why?
|> List.ofSeq
|> dumpReverse Final
|> ignore