<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net.Http</Namespace>
</Query>

// type /api in hypixel
let key = Util.GetPassword("hypixelapikey")
let debug = false
type int64 = System.Int64

let viewAuctionCommand key = sprintf "/viewauction %s" key

type [<Measure>] ms
type [<Measure>] s
type [<Measure>] min
let mapLift f x =
    x, f x
let after (d:string) (x:string) =
    if String.IsNullOrEmpty d then failwithf "Bad delimiter"
    match x.IndexOf d with
    | i when i < 0 -> None
    | i ->
        Some x.[i+d.Length..]
        
let (|After|_|) d x =
    after d x
let equalsI x y = String.Equals(x,y,StringComparison.InvariantCultureIgnoreCase)
    
module Tuple =
    let flip (x,y) = y, x
    let mapFst f (x,y) = f x, y
    
//module Seq =
//    let tryAverage title (items:'t seq) : 't option =
//        try
//            items |> Seq.average |> Some
//        with ex ->
//            printfn "Error in %s: %s" title ex.Message
//            None
            
module Map =
    let ofMultikey (items:_ seq) =
        (Map[],items)
        ||> Seq.fold(fun map (k,v) ->
            match map |> Map.tryFind k with
            | None -> map |> Map.add k [v]
            | Some items ->
                map |> Map.add k (v::items)
        )
    let keys (m:Map<'t,_>) : 't seq =
        m
        |> Map.toSeq
        |> Seq.map fst
        
module Async =
    let map f x =
        async{
            let! x' = x
            return f x'
        }
        
let delay : decimal<ms> = 
    let mnToS = 60m<s/min>
    let sToMs = 1000m<ms/s>
    let limit = 160m<1/min>
    let getDelayMsFromMax (maxPerMinute:decimal<1/min>) : decimal<ms> = 
        1m / (maxPerMinute / mnToS / sToMs)
    limit |> getDelayMsFromMax
    // buffer + int truncation helper
    |> (+) 50.8m<ms>
delay.Dump()

let target = "https://api.hypixel.net"
let upper (x:string) = x.ToUpperInvariant()
let lower (x:string) = x.ToLowerInvariant()
let trim(x:string) = x.Trim()
let (|EndsWith|_|) (d:string) (x:string) = if x.EndsWith(d) then Some x else None
let formatCurrency (x:decimal) = x.ToString("C")
let formatNumber (x:decimal) = x.ToString("n0")
let inline dumpt title (x:'t) =
    x.Dump(description=title)
    x
let ddumpt title x =
    if debug then
        dumpt title x
    else x
let horizontal (x:'t seq) =
    Util.HorizontalRun(false,elements=x)
let dumpPrompt,setPrompt =
    let dc = DumpContainer()
    let redump () = dc.Dump("productIds")
    
    redump()
    let mutable prompt = None
    redump,fun (x:obj) ->
        prompt <- Option.ofObj x
        match prompt with
        | Some x -> dc.Content <- x
        | _ -> dc.Content <- null
let redump,display =
    let dc = DumpContainer()
    //let mutable last : obj = null
    let redump () = dc.Dump()
    
    redump()
    redump,fun (x:obj) ->
        //last <- x
        dc.Content <- x
        
module Cereal =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    let serialize (x:obj) = JsonConvert.SerializeObject(x)
    let deserialize<'t> x = JsonConvert.DeserializeObject<'t>(x)

type Bid = {
    auction_id: string
    bidder: string
    profile_id: string
    amount: int
    timestamp: int64
}
type Auction = {
    uuid: Guid
    auctioneer: Guid
    profile_id: Guid
    coop: string []
    start: int64
    ``end``: int64
    item_name: string
    item_lore: string
    extra: string
    category: string
    tier: string
    starting_bid: int
    claimed: bool
    claimed_bidders: obj[]
    highest_bid_amount: int
    bids: Bid[]
    item_bytes: string
} with
    member x.Start =
        let dto = DateTimeOffset.FromUnixTimeMilliseconds x.start
        dto.ToLocalTime()
    member x.End =
        let dto = DateTimeOffset.FromUnixTimeMilliseconds x.``end``
        dto.ToLocalTime()
type AuctionsResponse = {success:bool; page:int;totalPages:int;totalAuctions:int;lastUpdated:System.Int64; auctions: Auction []}

module Client =
    use client = new HttpClient(BaseAddress=Uri target)
    let getTextFromApi querypath keyPairs title =
        let fullQuery =
            ("key",key) :: keyPairs
            |> List.distinctBy fst
            |> Seq.map(fun (k,v) -> sprintf "%s=%s" k v)
            |> String.concat "&"
            |> sprintf "%s?%s" querypath
        fullQuery |> ddumpt "fullQuery" |> ignore
        async{
            return! Async.AwaitTask <| client.GetStringAsync fullQuery
        }
        |> Async.Catch
        |> Async.RunSynchronously
        |> (function | Choice1Of2 x -> Ok x | Choice2Of2 x -> Error x)
        |> ddumpt title
        
    let getRawFromApi<'t> title querypath keypairs =
        getTextFromApi querypath keypairs title
        |> Result.map Cereal.deserialize<'t>
    
    let inline getFromApi querypath keyPairs title =
        getRawFromApi<_> querypath keyPairs title
        |> Result.map(fun x -> 
            let success = ( ^T : (member Success : bool) x)
            
            if not success then failwithf "Failure" else x
        )
let fetchAuctions pageOpt =
    Client.getRawFromApi<AuctionsResponse> "Auctions" "skyblock/auctions" [ match pageOpt with | None -> () | Some pg -> yield "page" , string pg ]
//let fetchProductInfo productId =
//    Client.getRawFromApi<ProductResponse> "skyblock/bazaar/product" ["productId",upper productId] productId
        
            
//type ArmorPrice = {Boots:decimal;Legs:decimal;Chest:decimal;Helm:decimal}
let sleep (delay:decimal<ms>) x =
    let intDelay = delay |> decimal |> int
    async {
        do! Async.Sleep intDelay
        return x
    }
//https://minecraft-ids.grahamedgecombe.com/items.json
// or https://docs.google.com/spreadsheets/d/1wqNu7vCWbhlnZElwy6Ut43lPtb1IZr7EK8yVotx5HNI/edit#gid=1917100251
fetchAuctions None
|> Dump
|> ignore