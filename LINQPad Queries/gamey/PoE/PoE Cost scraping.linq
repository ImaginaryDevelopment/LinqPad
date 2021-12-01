<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Selenium.WebDriver</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

// convert a currency to reference value (chaos orbs)
open OpenQA.Selenium

open OpenQA.Selenium.Chrome
module Option =
    let getOrDefault y =
        function
        | Some x -> x
        | None -> y
let dumpt t x = x.Dump(description=t); x
let getInnerHtml (x:IWebElement) = x.GetAttribute("innerHTML")
let trim1 (delim:string) (x:string) = x.Trim(delim |> Array.ofSeq)
let trim (x:string) = x.Trim()
let decTryParse = 
    Decimal.TryParse
    >> function
        | true, x -> Some x
        | false, _ -> None
let dResult = 
    let dc = DumpContainer()
    dc.Dump("result")
    fun (o:obj) ->
        dc.Content <- o

// set it for current process only
Environment.SetEnvironmentVariable("path",Environment.CurrentDirectory)
let findAndCopyIfMissing () = 
    let fn = "chromedriver.exe"
    let sourceFile = Path.Combine(Environment.ExpandEnvironmentVariables("%userprofile%"),"downloads","chromedriver_win32", fn)
    let targetFile = Path.Combine(Environment.CurrentDirectory,fn)
    if File.Exists sourceFile && (not <| File.Exists(Path.Combine(Environment.CurrentDirectory,fn)) || FileInfo(sourceFile).LastWriteTimeUtc > FileInfo(targetFile).LastWriteTimeUtc) then
        File.Copy(sourceFile, targetFile, true)
findAndCopyIfMissing()
Environment.ExpandEnvironmentVariables("%path%").Split(';').Dump()

let getPage (uri:string) fDriver = 
    printfn "getting page %s" uri
    try
        let driver = new ChromeDriver()
        driver.Navigate().GoToUrl(uri)
        let result = 
                // letting it use chromedriver directly instead of an interface
                fDriver  driver
        
        printfn "Captured?"

        driver.Quit()
        result
    with 
        | :? DriverServiceNotFoundException as ex ->
            // in case whatever version we are using doesn't include a detailed error message
            printfn "The chromedriver.exe file does not exist in the current directory or in a directory on the PATH environment variable. The driver can be downloaded at http://chromedriver.storage.googleapis.com/index.html."
            reraise()
type TradeData = {Trend:string;Pay:decimal;Get:decimal}
type PoeItem = {Name:String; Buy:TradeData;Sell:TradeData option}

let poeNinja () = 
    let getDecFromDisplay x = 
        x
        |> trim1 "x"
        |> trim
        |> Decimal.Parse
    let getBaseData name trend cb g =
        {Name=name; Buy={Trend= trend; Pay=cb |> getDecFromDisplay; Get=g |> getDecFromDisplay}; Sell=None}
    getPage "http://poe.ninja/challenge/currency" (fun (d:ChromeDriver) -> 
        let css = "table.currency-table"
        // note: not having to wait for page to load, perhaps it is built-in to selenium now?
        let cTable = d.FindElement(By.CssSelector css)
        let rows = 
            cTable.FindElements(By.CssSelector ("tbody tr")) 
            |> Seq.choose (fun e -> 
                e.FindElements(By.CssSelector "td") 
                |> Seq.map(fun e -> e.Text)
                |> List.ofSeq
                |> function
                    | [name;trend;cb;g;_;sTrend;s;sg;_] ->
                        {getBaseData name trend cb g with Sell=Some {Trend=sTrend; Pay=s |> getDecFromDisplay; Get=sg |> getDecFromDisplay}}
                        |> Some
                        //Some {Name=name; BuyTrend=trend; ChaosBuy=cb; ChaosBuyGets=g |> getDecFromDisplay; }
                    | [name;trend;cb;g;_;_;_] ->
                        getBaseData name trend cb g
                        |> Some
                    | x ->
                        x.Dump("bad row")
                        None                    
            )
            |> Seq.map(fun x ->
                {x with Name=x.Name |> trim1 "+" |> trim}
            )
            
            |> List.ofSeq
        rows
    )
let jsonCache<'T> key (x:unit -> 'T) =
    Util.Cache(x >> Newtonsoft.Json.JsonConvert.SerializeObject, key)
    |> fun data -> Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(data)
let data = 
    jsonCache "data" poeNinja 
let getSetByName<'T> key = 
    let key = sprintf "%s:%s" key (typeof<'T>.Name)
    let getter ():'T option =
        AppDomain.CurrentDomain.GetData key
        |> function
            | null -> None
            | :? 'T as result -> Some result
    let setter (x:'T) =
        AppDomain.CurrentDomain.SetData(key, box x)
    getter,setter
    
Util.OnDemand("full price table", fun () -> data)
|> Dump
|> ignore
let fTryDisplay x = 
    if x.Buy.Get = 1m && x.Buy.Pay > 1m then
        printfn "%s is bought at %.1f chaos (better than chaos)" x.Name x.Buy.Pay
        (x.Name,x.Buy)
        |> Some
    elif x.Buy.Pay = 1m && x.Buy.Get > 1m then
        printfn "%s nets %.1f per chaos orb (less than chaos)" x.Name x.Buy.Get
        (x.Name, x.Buy)
        |> Some
    else 
        x.Dump("unknown case")
        None
let picked = Util.ReadLine("Currency?",null, data |> Seq.map(fun d -> d.Name)) //|> Seq.filter((<>) "Chaos Orb")

let (getVH,setVH) = getSetByName<string list> "ValueHistory"
data
|> Seq.tryFind(fun d -> d.Name = picked)
|> Option.bind fTryDisplay
|> Option.iter(fun (name,x) ->
    let chaosValue = x.Pay / x.Get
    Util.ReadLine("times what?")
    |> decTryParse
    |> Option.iter (fun count ->
        let totalValue = count * chaosValue
        let text = sprintf "%.0f %s is worth %.2f chaos" count name totalValue
        printfn "%s" text
        getVH()
        |> Option.getOrDefault List.empty
        |> fun l -> text::l
        |> fun x ->
            dResult x
            setVH x
    )
)
//|> Dump
//|> ignore