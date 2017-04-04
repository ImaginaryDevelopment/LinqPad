<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>FSharp.Charting</NuGetReference>
  <NuGetReference>Google.Apis.Sheets.v4</NuGetReference>
  <Namespace>Google.Apis.Auth.OAuth2</Namespace>
  <Namespace>Google.Apis.Services</Namespace>
  <Namespace>Google.Apis.Sheets.v4</Namespace>
  <Namespace>Google.Apis.Sheets.v4.Data</Namespace>
  <Namespace>Google.Apis.Util.Store</Namespace>
</Query>

// pull in data for the talent calc
// WIP
// tester https://developers.google.com/sheets/reference/rest/v4/spreadsheets/get
let useFile = false
let useCache,_useCacheTags = true,true
let toLetters = 
    let rec c x  = if x<26 then int 'A'+x|>char|>string else (x/26-1|>c)+ c(x%26)
    c
let fromLetters (x:string) =
    let a = int 'A'
    let fPow len i =
        Math.Pow(26., len - 1 - i |> float)
        |> int
    
    let getValue len i c = 
        int c - a + 1 * fPow len i
    let f i = getValue x.Length i x.[i]
    [0 .. x.Length - 1]
    |> Seq.map f
    |> Seq.sum
    |> fun x -> x - 1
    
let dumpt (title:string) x = x.Dump(title) |> ignore; x
let dumpC title items = items |> List.length |> dumpt title |> ignore; items
let dumpReverse :  (obj) -> unit =
    let dc = DumpContainer()
    dc.Dump() |> ignore
    (fun o -> 
        match dc.Content with
        | :? (obj list) as items -> List.Cons(o,items)
        | _ -> [ o ]
        |> fun content -> dc.Content <- content
    )

    
let hoist f x = f x; x
let (|Odd|Even|) x = 
    match x % 2 = 0 with
    | true -> Even
    | false -> Odd
    
module Fs = 
    open Newtonsoft.Json.Linq
    let save path (o:obj) = 
        Newtonsoft.Json.JsonConvert.SerializeObject(o, Newtonsoft.Json.Formatting.Indented)
        |> fun s -> File.WriteAllText(path,s)
    let deserialize<'T> x = Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(x)
    let loadJObject path : JObject = 
        File.ReadAllText path
        |> deserialize
    let load<'T> path : 'T =
        File.ReadAllText path
        |> fun text -> Newtonsoft.Json.JsonConvert.DeserializeObject<_>(text)


type RowId = 
    |RowIdentifier of int
    |RowIndex of int
let RowIdentifier x = 
    if x > 0 then Some (RowIdentifier x) else None

module Google =
    let scopes = [SheetsService.Scope.SpreadsheetsReadonly]
    let applicationName = "CotLI reader"
    
    let getCell (data:string list list) rowIndex columnLetters =   
        let rowIndex = 
            match rowIndex with
            | RowIdentifier x -> x - 1
            | RowIndex x -> x
        if rowIndex >= data.Length then
            failwithf "attempt to read past the end of list for row=%i (length was %i)" rowIndex data.Length
        let row = data.[rowIndex]
        let columnIndex = fromLetters columnLetters
        if columnIndex >= row.Length then
            failwithf "attempt to read past the end of list for column=%i (length was %i) %A" columnIndex row.Length row
        row.[columnIndex]
        
    let getCredential () = 
        let path = Util.GetPassword("client_secret_Path").Trim('"')
        use stream = new FileStream(path, FileMode.Open, FileAccess.Read)
        let credPath = 
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), ".credentials/sheets.googleapis.com-dotnet-quickstart.json")
        
        let credential = 
            GoogleWebAuthorizationBroker.AuthorizeAsync(
                GoogleClientSecrets.Load(stream).Secrets,scopes,"user", CancellationToken.None, FileDataStore(credPath, true)).Result
        printfn "Credential file saved to: %s" credPath
        credential
    
    let credential = getCredential()
    let service = new SheetsService(BaseClientService.Initializer(HttpClientInitializer=credential, ApplicationName=applicationName))
    let getData ssId range = 
        let request = service.Spreadsheets.Values.Get(ssId,range)
        let response = request.Execute()
        response.Values
        
    let spreadSheetId = "1DbvdBWjbMvpfKmCYnOmYDrVM2-Ro_Bgl7xvTftLW5U8" // CotLI - Useful Tools
    let costRange= "Cost Data" // "Cost Data!A1:AQ53"
    let talentRange= "Talent Calculator"
    // works
    //let tagRange= "Tags!A3:Z67"
    module Cost = 
        // assumes we are only using one sheet for this script
        let getSheet range cacheKey = 
            let result = 
                let getSheetData() = 
                    getData spreadSheetId range   
                    |> Seq.map (Seq.map (fun d -> d |> string) >> List.ofSeq)
                    |> List.ofSeq
                    |> dumpC "Rows returned"
                if useCache then
                    Util.Cache(getSheetData,cacheKey)
                else
                    getSheetData()
            result
        let getCostSheetData() = getSheet costRange "CostSheetData"
            
        let getTalentSheetData() = getSheet talentRange "TalentSheetData"
            
        let mapFromCosts (costs:string list list) = 
            let columnLookup = 
                costs 
                |> Seq.head 
                |> Seq.mapi(fun i v ->i,v) 
                |> Seq.filter(snd >> String.IsNullOrWhiteSpace >> not) 
                |> Seq.map(fun (i,v) -> (v,i))
                |> Map.ofSeq
            let assertions =
                costs
                |> Seq.skip 1 
                |> Seq.head
                |> Seq.iteri( fun i v ->
                    match i,v with
                    | 0, "Level"
                    | Odd, "Cost" -> ()
                    | Even, _ -> ()
                    | _ -> failwithf "unexpected value in column %i: %s" i v
                )

            let _lookupData row column = 
                let columnData = costs.[row]
                try
                    columnData.[column]
                with ex ->
                    (row,column,columnData,ex).Dump()
                    failwithf "lookupFailed"
                    
            //let getColumnValue row columnIndex = row.[columnIndex]
            let _getColumnTitle column = 
                costs.[0].[column]

            columnLookup
            |> Map.map (fun talentName column ->
                let talentMap =                
                    costs
                    |> Seq.skip 2
                    |> Seq.filter(fun row ->
                        row.Length > column)
                    |> Seq.map (fun row -> 
                        row.[column])
                    |> List.ofSeq
                    |> Seq.takeWhile(String.IsNullOrWhiteSpace >> not)
                    |> List.ofSeq
                    |> Seq.mapi (fun i v -> i,v)
                    |> List.ofSeq
                talentMap
            )

//let deserialize  
open Google
open Google.Cost
let costMap = 
    getCostSheetData()
//|> Dump
    |> mapFromCosts    
    |> dumpReverse
let talentData = 
    getTalentSheetData()
    |> hoist dumpReverse

type CooldownItems = { Common:int; Uncommon:int; Rare:int; Epic:int} with
    member x.Total = //=(E4*0.5+F4*1+G4*1.5+H4*2)/100
        decimal x.Common * 0.5m + decimal x.Uncommon * 1m + decimal x.Rare * 1.5m + decimal x.Epic * 2m

    
let coolDownTotal = 
    let getCool l = getCell talentData (RowIdentifier 4 |> Option.get) l
    [ 'E' .. 'H']
    
    |> Seq.map (string >> getCool >> float)
    |> Seq.mapi (fun i v ->
        match i with 
        | 0 -> 0.5
        | 1 -> 1.
        | 2 -> 1.5
        | 3 -> 2.
        | _ -> failwithf "unknown input value %i" i
        |> (*) v
    )       
    |> Seq.sum
    
coolDownTotal.Dump()    