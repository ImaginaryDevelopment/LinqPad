<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net.Http</Namespace>
</Query>

// type /api in hypixel
let key = Util.GetPassword("hypixelapikey")
let debug = false

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
    
module Seq =
    let tryAverage title (items:'t seq) : 't option =
        try
            items |> Seq.average |> Some
        with ex ->
            printfn "Error in %s: %s" title ex.Message
            None
            
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

type Response = {Success:bool; ProductIds:string list}
type Order = {Amount:int;PricePerUnit:decimal;Orders:int}
type QuickStatus = {
        BuyPrice:decimal
        BuyVolume:int
        BuyMovingWeek:int
        BuyOrders:int
        SellPrice:decimal
        SellVolume:int
        SellMovingWeek:int
        SellOrders:int
        }
type ProductInfo = { Product_Id:string; Buy_Summary:Order list;Sell_Summary: Order list;Quick_Status:QuickStatus}
type ProductResponse = {Success:bool; Product_Info:ProductInfo}

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
    let getRawFromApi<'t> querypath keypairs title =
        getTextFromApi querypath keypairs title
        |> Result.map Cereal.deserialize<'t>
    
    let inline getFromApi querypath keyPairs title =
        getRawFromApi<_> querypath keyPairs title
        |> Result.map(fun x -> 
            let success = ( ^T : (member Success : bool) x)
            
            if not success then failwithf "Failure" else x
        )
    
let products =
    Util.Cache(fun () ->
        Client.getFromApi "skyblock/bazaar/products" List.empty "Products"
        |> function
            |Ok x -> x.ProductIds
            | Error x -> x.Dump("Products failing"); failwith "Product list fail"
    )
let fetchProductInfo productId =
    Client.getRawFromApi<ProductResponse> "skyblock/bazaar/product" ["productId",upper productId] productId
    
products |> List.sortBy(fun x -> not <| (lower x).StartsWith("enchanted_"),x) |> List.chunkBySize 20 |> horizontal |> setPrompt

type Mode =
    |Display
    |Prompt
type MySummary = {BuyPrice:decimal;SellPrice:decimal;Spread:decimal;For160:decimal;FlipProfit:decimal}
    with member x.ToDump() = 
            sprintf "Spread:%.2f, FlipProfit:%.1f, For160:%.0f, Buy:%.1f,Sell:%.0f" 
                x.Spread x.FlipProfit x.For160 x.BuyPrice x.SellPrice
let getOrdering items =
    match items with
        | _ :: [] -> None
        | h :: tl -> h :: [Seq.last tl] |> Some
        | _ -> None
        
let summarize {Product_Info=product} =
    // we are assuming summarys are sorted in respective ways
    let getHighAvg (prodId,mode) items =
        items
        |> Seq.truncate 10
        |> Seq.map(fun x -> x.PricePerUnit)
        |> Seq.tryAverage (sprintf "%s - %s summ" prodId mode)
     
        
    (
        match getOrdering product.Buy_Summary, getOrdering product.Sell_Summary with
        | Some buy,Some sell when buy.Length > 1 && sell.Length > 1 ->
            assert (buy.[0].PricePerUnit >= buy.[1].PricePerUnit && sell.[0].PricePerUnit <= sell.[1].PricePerUnit)
        | _ -> ()
    )
    (product.Buy_Summary |> getHighAvg (product.Product_Id,"buy"),
     product.Sell_Summary |> getHighAvg (product.Product_Id,"sell"))
    |> function
        | Some buy,Some sell ->
            Ok {BuyPrice=buy;SellPrice=sell; For160=buy*160m; Spread = 1m - (buy/sell);FlipProfit = sell - buy}
        | _ ->
            Error <| Exception "No Price Found"
            
let isPrompt = function | Prompt -> true | _ -> false
let mutable command = "a"
let mutable mode = Prompt
let mutable mySummary = None
type ArmorInfo<'T> = {Boots:'T;Legs:'T;Chest:'T;Helm:'T}
//type ArmorPrice = {Boots:decimal;Legs:decimal;Chest:decimal;Helm:decimal}
let sleep (delay:decimal<ms>) x =
    let intDelay = delay |> decimal |> int
    async {
        do! Async.Sleep intDelay
        return x
    }
//https://minecraft-ids.grahamedgecombe.com/items.json
// or https://docs.google.com/spreadsheets/d/1wqNu7vCWbhlnZElwy6Ut43lPtb1IZr7EK8yVotx5HNI/edit#gid=1917100251
let remaps = [
    "oak","log"
    "spruce","log:1"
    "birch","log:2"
    "dark_oak","log_2:1"
    "acacia","log_2"
    "jungle","log:3"
    "carrot", "carrot_item"
    "potato", "potato_item"
    "cocoa_beans", "ink_sack:3"
    "lapis", "ink_sack:4"
    "salmon", "raw_fish:1"
    "pufferfish", "raw_fish:3"
    "lily_pad", "water_lily"
    "raw_porkchop", "pork"
]
let (|Remapped|_|) (t:string) : string -> unit option =
    lower
    >> fun x ->
        let searchResult =
            remaps |> List.tryFind(snd >> (=) x) |> Option.filter(fun x -> fst x = t)
            |> Option.map ignore
        searchResult
        
let getAllLifted (fProduct:'t -> string) items =
    let ml = sleep delay >> Async.map(fProduct >> fetchProductInfo >> Result.bind summarize) 
    items
    |> Seq.map (mapLift (ml >> Async.RunSynchronously))
    
let getAll items =
    items
    |> getAllLifted id
    |> Seq.choose (function |_,Ok x -> Some x | _ -> None)
    
type ComponentSub = {Name:string;Amount:int;Subtotal:decimal} with 
        member x.ToDump() = sprintf "Name=%s, Amount=%i, Subtotal=%.0f" x.Name x.Amount x.Subtotal
type SubItems = {Subtotal:decimal; AmountSubTotal:int; PricePer:decimal; Components:ComponentSub list} with
        member x.ToDump() = 
            let componentSummary = x.Components |> Seq.map(fun c -> c.ToDump()) |> String.concat("\r\n\t")
            sprintf "Subtotal=%.0f, Count:%i, Per %.2f, \r\n%s" x.Subtotal x.AmountSubTotal x.PricePer componentSummary
type ItemBreakdown = {Material:string;Produces:SubItems}

type ProductKey={Raw:string;ProdId:string}
let breaks = 
    let calculateAnalysis remapProduct x =
        x
        |> List.map Tuple.flip
        |> List.map (Tuple.mapFst (remapProduct))
        |> Map.ofMultikey
        |> fun map ->
            // would be nice to check this list against known good, and include remaps
            Map.keys map
            |> getAllLifted (fun x -> x.ProdId)
            |> Seq.map(fun (prodId, sOpt) ->
                prodId,sOpt |> Result.map(fun s -> s.BuyPrice), map.[prodId]
            )
        |> fun x -> (); x
        |> Seq.fold(fun (total,breakdown:ItemBreakdown list) (name,sOpt,items) ->
            let makeBreakdown s (asub:int) subtotal =
                {   Material=name.Raw
                    Produces=
                    {
                        SubItems.Subtotal= subtotal
                        AmountSubTotal= asub
                        PricePer= s
                        Components= items
                            |> List.map(fun (name,amount) -> {ComponentSub.Name=name;Amount=amount;Subtotal=s * decimal amount})}
                }::breakdown
            
            match sOpt with
            | Ok s ->
                let itemSubtotal = items |> List.map(snd >> decimal >> (*) s) |> List.sum
                let amountSub : int = items |> List.map(snd) |> List.sum
                itemSubtotal + total, makeBreakdown s amountSub itemSubtotal
            | _ -> total, makeBreakdown 0m 0 0m
            //(-1m,name,sOpt |> Option.defaultValue 0m, items)
        ) (0m,List.empty)
        |> fun (x,y) -> 
            let result = y |> List.sortByDescending(fun z -> z.Produces.Subtotal)
            formatCurrency x, result
        |> dumpt "Totals"
        |> ignore
    let getCost items =
        items
        |> getAllLifted fst
        |> Seq.map(
            function
            | (prodId,lifted), Ok s -> prodId,lifted,Some s.BuyPrice
            | (prodId,lifted), Error _ -> prodId,lifted, None
        )
    let getMastiffArmor (remapProduct: _ -> ProductKey) =
        [
            ("Growth Armor", 160*(6 + 8 + 7 + 4) * 64), "dark_oak"
            ("Crown",(16*4 + 32*4) * 160 ), "gold_ingot"
            ("Crown",32*4*4), "wolf_tooth"
            ("Chest",(16*4 + 32*4) * 160 ), "gold_ingot"
            ("Chest",32*4*4), "wolf_tooth"
            ("Legs",(16*4 + 32*4) * 160 ), "diamond"
            ("Legs", 32*4*160), "gold_ingot"
            ("Legs",32*4*4), "wolf_tooth"
            ("Feet",(16*4 + 32*4) * 160 ), "diamond"
            ("Feet", 32*4*160), "gold_ingot"
            ("Feet",32*4*4), "wolf_tooth"
        ]
    let getSwordEnchants (remapProduct: _ -> ProductKey) =
        [
            ("Crit V",16), "enchanted_diamond"
            ("Sharpness V", 16), "Flint"
            // Iron Sword component
            ("Sharpness V", 4), "Iron_Ingot"
            ("Execute V", 80), "Flint"
            ("Execute V", 80), "Diamond"
            ("Giant Killer V", 16), "Ghast_Tear"
            ("First Strike IV", 8), "Enchanted_Flint"
            ("Lethality V", 48), "Obsidian"
            ("Cubism V", 32), "Pumpkin"
            ("Ender Slayer V", 16), "Enchanted_Ender_Pearl"
            // ("Impaling III", 40), "Prismarine_Shard"
            ("Life Steal III", 16*9), "Gold_Ingot"
            ("Vampirism V", 16), "Enchanted_Ghast_Tear"
            ("Luck V", 16), "Rabbit_Hide"
            ("Looting III", 8*9), "Gold_Ingot"
            ("Scavenger III", 4), "Gold_Ingot"
            ("Experience III", 4), "Lapis"
        ]
        |> List.map Tuple.flip
        |> List.map (Tuple.mapFst (remapProduct))
        |> Map.ofMultikey
        |> fun map ->
            // would be nice to check this list against known good, and include remaps
            Map.keys map
            |> getAllLifted (fun x -> x.ProdId)
            |> Seq.map(fun (prodId, sOpt) ->
                prodId,sOpt |> Result.map(fun s -> s.BuyPrice), map.[prodId]
            )
        |> fun x -> (); x
        |> Seq.fold(fun (total,breakdown:ItemBreakdown list) ((name,sOpt,items) as item) ->
            let makeBreakdown s asub subtotal = 
                {
                    Material=name.Raw
                    Produces=
                        {
                            SubItems.Subtotal=subtotal
                            PricePer=s
                            AmountSubTotal= asub
                            Components= items
                                    |> List.map(fun (name,amount) -> 
                                        {Name=name;Amount=amount;Subtotal=s * decimal amount})
                        }
                }::breakdown
            let itemAmount = items |> List.map snd |> List.sum
            match sOpt with
            | Ok s ->
                
                let itemSubtotal = items |> List.map(snd >> decimal >> (*) s) |> List.sum
                
                itemSubtotal + total, makeBreakdown s itemAmount itemSubtotal
            | _ -> total, makeBreakdown 0m itemAmount 0m
            //(-1m,name,sOpt |> Option.defaultValue 0m, items)
        ) (0m,List.empty)
        |> fun (x,y) -> 
            let result = y |> List.sortByDescending(fun z -> z.Produces.Subtotal)
            formatCurrency x, result
        |> dumpt "Totals"
        |> ignore
        
    [
        "SwordEnchants", getSwordEnchants
        "mastiff", fun f -> getMastiffArmor f |> calculateAnalysis f
    ]

let remap cmd =
    let tryRemap x =
        remaps |> List.tryFind(fst >> (=) x) |> Option.map (snd>>upper)
        |> Option.defaultValue x
    Option.ofObj cmd
    |> Option.map(lower >> trim)
    |> Option.defaultValue ""
    |> tryRemap
    
let showProduct cmd s =
    match lower cmd with
    //| "potato" ->
    //    let count =
    | "pork" ->
        let count = 48m * 160m * 160m
        (formatNumber count, s.BuyPrice * count |> formatCurrency) |> dumpt "Pigman Sword" |> ignore
    | "pumpkin" ->
        //let div = 160m
        let count = 40_960m
        let total = count * s.BuyPrice
        (formatNumber count, total |> formatCurrency) |> dumpt "Farmer Boots" |> ignore
    | "enchanted_obsidian" ->
        let eCount = 64m * 8m
        let total = s.BuyPrice * eCount
        
        (formatNumber eCount, total |> formatCurrency) |> dumpt "Treecapitor-obs" |> ignore
    | "obsidian" ->
        let div = 160m
        let eCount = div * (8m * 64m)
        let total = s.BuyPrice * eCount
        
        (formatNumber eCount, total |> formatCurrency) |> dumpt "Treecapitor-obs" |> ignore
    | "enchanted_dark_oak_log" ->
        let div = 64m
        let eCount = div * (6m + 8m + 7m + 4m)
        let total = s.BuyPrice * eCount
        let getCost = (*) div >> (*) s.BuyPrice >> formatCurrency
        let armor = {   Boots= getCost 6m
                        Chest= getCost 8m
                        Legs= getCost 7m
                        Helm= getCost 4m
                    }
        ("Growth Armor",armor, eCount, formatCurrency <|  total)
        |> dumpt "Growth Armor price"
        |> ignore
    | Remapped "dark_oak" _ ->
        let div = 64m * 160m
        let eCount = div * (6m + 8m + 7m + 4m)
        let total = s.BuyPrice * eCount
        let getCost = (*) div >> (*) s.BuyPrice >> formatCurrency
        let armor = {   Boots= getCost 4m
                        Chest= getCost 8m
                        Legs= getCost 7m
                        Helm= getCost 6m
                    }
        ("Growth Armor",armor, eCount, formatCurrency <|  total)
        |> dumpt "Growth Armor price"
        |> ignore
                    
    | EndsWith "_fragment" name ->
        let fragCount = 40m+80m+70m+50m
        let total = s.BuyPrice * fragCount
        let armor =  {  Boots=formatCurrency <| s.BuyPrice * 40m
                        Chest=formatCurrency <| s.BuyPrice*80m
                        Legs=formatCurrency <| s.BuyPrice*70m
                        Helm=formatCurrency <| s.BuyPrice*50m}
        (name,armor, fragCount, formatCurrency <|  total)
        |> dumpt "Armor price"
        |> ignore
    | _ -> ()
while not <| isPrompt mode || not <| String.IsNullOrWhiteSpace command do
    match mode with 
    | Prompt ->
        if not debug then Util.ClearResults()
        dumpPrompt()
        let raw = Util.ReadLine("Product?", defaultValue=null,suggestions= "analyze":: "analyze swordenchants" :: (remaps |> List.map fst) @ products)
        command <- remap <| raw
        
        match lower command with
        | After "analyze " x ->
            match breaks |> List.tryFind(fst >> equalsI x) with
            | Some (_name,f) ->
                f(fun raw -> {Raw=raw;ProdId=remap raw})
            |> ignore
        | "analyze" ->
            products
            |> getAll
            |> Seq.sortByDescending(fun s -> s.Spread)
            |> Seq.truncate 10
            |> Dump
            |> ignore
        | _ ->
            redump()
            products |> Seq.tryFind(fun x -> x = upper command)
            |> function
                | None -> printfn "Not found:%s" command
                | Some _ ->
                    let product = fetchProductInfo command
                    match Result.bind (summarize) product with
                    | Ok summary ->
                        mySummary <- Some summary
                        mySummary.Dump(command)
                    | Error x -> x.Dump("Error")
                    display product
        mode <- Display
    | Display ->
        mode <- Prompt
        mySummary
        |> Option.iter(fun x ->
            x.Dump("summary")
            showProduct (lower command) x
        )
        
            
        Util.ReadLine("Hit enter for next product")
        |> ignore