<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// calculate profitable sales numbers

// should go something like marketSale (2.6 example) selling X, .38 buying X
// profit = purchase 3 for 1, sell .4
// use exact numbers from poe.ninja
// https://poe.ninja/api/data/currencyoverview?league=Heist&type=Fragment
// https://poe.ninja/api/data/currencyoverview?league=Heist&type=Currency
let league = "Heist"
let serialize (x:'t) =
    Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)
let deserialize<'t> (x:string) =
    Newtonsoft.Json.JsonConvert.DeserializeObject<'t>(x)
    
let getProps (x:JObject) =
    x.Properties()
    |> Seq.map(fun p -> p.Name,p.Value)
    
type CurrencyValue = {id:int;league_id:int;pay_currency_id:int;get_currency_id:int;sample_time_utc:DateTime;count:int;value:decimal;data_point_count:int;includes_secondary:bool}
type CurrencyLine = {CurrencyTypeName:string; Pay: CurrencyValue; Receive: CurrencyValue;ChaosEquivalent:float;}
type CurrencyDetail = {id:int;icon:string;name:string;poeTradeId:int;tradeId:string}
type NinjaResponse = {
    lines: CurrencyLine[]
    currencyDetails:CurrencyDetail[]
    //language:obj
}
    
let fetchCurrencies () =
    use hc = new System.Net.Http.HttpClient()
    let uri = sprintf "https://poe.ninja/api/data/currencyoverview?league=%s&type=Currency" league
    hc.GetStringAsync(uri)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    //|> JObject.Parse
    |> deserialize<NinjaResponse>
    //|> getProps
    //|> serialize
    |> fun x -> x.Dump(2) |> ignore
fetchCurrencies()    

let defaults = Map[
    "alteration", (5.0,5.2)
    "fusing",(2.6,2.6)
]

let name,marketSale,marketPurchase =
    let name = Util.ReadLine "currency item name?"
    name,
        Util.ReadLine("sale price?",if defaults.ContainsKey name then fst defaults.[name] else 2.6), Util.ReadLine("purchase price?",if defaults.ContainsKey name then snd defaults.[name] else 5.2)

let purchaseInChaos = 1. / marketPurchase
let apparentMarketMargin = (1. - marketSale * purchaseInChaos)

printfn "Margin is %.3f" apparentMarketMargin


let equivRate = (marketSale + marketPurchase) / 2.



printfn "list chaos as >%.2f %s" marketSale name // to make higher margins a price > marketSale would be used
printfn "list %s as >%.2f chaos" name purchaseInChaos

// doesn't necessarily include both sides if you are doing both
let displayMargin title marketRate yourRate =
    let profitMargin = yourRate / marketRate
    printfn "Your %s %s margin is %.2f" name title profitMargin
let desiredChaosRate = Util.ReadLine<float>(sprintf "purchase %s rate?(>%.2f)" name marketSale)
displayMargin  "purchase" marketSale desiredChaosRate
let desiredXRate = Util.ReadLine<float>(sprintf "sell %s rate? (>%.2f)" name purchaseInChaos)
displayMargin "sale" purchaseInChaos desiredXRate

[
    desiredChaosRate; marketSale; equivRate;desiredXRate; purchaseInChaos
]
    
