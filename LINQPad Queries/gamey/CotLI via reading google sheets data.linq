<Query Kind="FSharpProgram">
  <NuGetReference>Google.Apis.Sheets.v4</NuGetReference>
  <Namespace>Google.Apis.Auth.OAuth2</Namespace>
  <Namespace>Google.Apis.Services</Namespace>
  <Namespace>Google.Apis.Sheets.v4</Namespace>
  <Namespace>Google.Apis.Sheets.v4.Data</Namespace>
  <Namespace>Google.Apis.Util.Store</Namespace>
</Query>

// use google apis to pull in data from the google sheet
// https://developers.google.com/sheets/quickstart/dotnet
let dumpt (title:string) x = x.Dump(title) |> ignore; x
let dumpC title items = items |> List.length |> dumpt title |> ignore; items
let scopes = [SheetsService.Scope.SpreadsheetsReadonly]
let applicationName = "CotLI reader"
let getCredential () = 
    use stream = FileStream(Util.GetPassword("client_secret_Path"), FileMode.Open, FileAccess.Read)
    let credPath = 
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), ".credentials/sheets.googleapis.com-dotnet-quickstart.json")
    
    let credential = GoogleWebAuthorizationBroker.AuthorizeAsync(
        GoogleClientSecrets.Load(stream).Secrets,scopes,"user", CancellationToken.None, FileDataStore(credPath, true)).Result
    printfn "Credential file saved to: %s" credPath
    credential

let credential = getCredential()
let service = SheetsService(BaseClientService.Initializer(HttpClientInitializer=credential, ApplicationName=applicationName))
let getData ssId range = 
    let request = service.Spreadsheets.Values.Get(ssId,range)
    let response = request.Execute()
    response.Values
    
let spreadSheetId = "1n6odBi-lmp-FgozzGl9AXsY9RjxwmVijdvFXMX0q37A" // CotLI - Useful Tools
let crusaderRange = "Gear!A1:E90" // expecting some rows or at least row 90 to be empty as a check for we aren't missing any crusaders in the query
let gearRange= "Gear!C91:G1189" //expecting row 1189 to be completely empty unless new gear has been added


type Crusader = { Name:String; Slot:string; SlotOrder: string (* 0-4? null - d? *); Items:string list; EnchantmentPoints:int; Tags: string list}
type Quality = 
    | Empty
    | Common
    | Uncommon
    | Rare
    | Epic
    | GoldenEpic
type Gear = {Name:string; Value:string; Quality:Quality; Owned:bool}

type GearRow = 
    |Name of string
    |Category of string
    |GearRow of Gear
    
let crusaders = 
    let sheetData = 
        getData spreadSheetId crusaderRange 
        |> List.ofSeq
        |> dumpC "Rows returned"
    
    sheetData
    |> Seq.skip 1
    |> Seq.filter (Seq.exists (fun _ -> true))
    |> Seq.takeWhile (fun data -> not <| String.IsNullOrEmpty( data.[0] |> string))
    |> Seq.map (fun data -> 
        {
            Name=data.[1] |> string
            Slot=Regex.Match(data.[0] |> string,"\d+").Value
            SlotOrder=Regex.Match(data.[0] |> string,"\d+(\w?)").Groups.[1].Value
            Items=data |> Array.ofSeq |> fun x -> x.[2..] |> Seq.map string |> List.ofSeq
            EnchantmentPoints = 0
            Tags = List.empty
        })
    |> List.ofSeq
    //|> Seq.take(30)
    |> Seq.sortBy(fun x -> x.Slot, x.SlotOrder)    
    |> List.ofSeq
    
let crusaderNames = crusaders |> Seq.map(fun c -> c.Name) |> Set.ofSeq
//    let getGear (xs: IList<obj> seq) =
//        // plan:
//        // throw out rows until we find a name row
//        // take gearCategory
//        // take gear within category
//        let rec readCruGear xs =
//            [
//                xs
//                |> Seq.skipWhile
//        

let getGear () = 
    let sheetData = 
        getData spreadSheetId gearRange 
        |> List.ofSeq
        |> dumpC "Gear Rows returned"
    
    let (|GearCruNameRow|CategoryRow|GearRow|Other|) (data:IList<obj>) = 
        try
            match data.Count < 1 || String.IsNullOrEmpty (data.[0] |> string), data.Count <2 || String.IsNullOrEmpty (data.[1] |> string) with
            | false, true -> GearCruNameRow (data.[0] |> string)
            | false, false -> CategoryRow (data.[0] |> string, data |> Seq.skip 1 |> Seq.map string |> List.ofSeq)
            | true,false -> GearRow (data |> Seq.skip 1 |> Seq.map string |> List.ofSeq)
            | _ -> Other
        with ex ->
            data.Dump("is failing")
            reraise()
        
    
    let mapRow (data:IList<obj>) = 
        let mapGearList (gear:string list) = 
            let quality = 
                try
                    match gear.[2] with
                    | "Empty" -> Quality.Empty
                    | "Common" -> Quality.Common
                    | "Uncommon" -> Quality.Uncommon
                    | "Rare" -> Quality.Rare
                    | "Epic" -> Quality.Epic
                    | "Golden Epic" -> Quality.GoldenEpic
                    | x -> failwithf "Quality evaluated as %s" x
                with ex ->
                    gear.Dump("is failing gear")
                    reraise()
            GearRow {Name=gear.[0];Value=gear.[1];Quality=quality; Owned= gear.[3] = "1"}
        [
            match data with
            | GearCruNameRow name -> yield name |> Name |> Some
            | CategoryRow (cat,gear) -> 
                yield Category cat |> Some
                let mappedGear = 
                    try
                        mapGearList gear
                    with ex ->
                        data.Dump("is a failing category row")
                        reraise()
                
                yield  mappedGear |> Some
            | GearRow gear -> yield mapGearList gear |> Some
            | Other -> yield None
        ]
        
    let gearData = 
        sheetData
        |> Seq.mapi (fun i data -> i + 91, data)
//        |> dumpt "sheetRowsRaw"
        |> Seq.skipWhile(fun (i,data) -> 
            
            try
                data.Count < 1 || String.IsNullOrEmpty (data.[0] |> string)
            with ex ->
                data.Dump("failed to read")
                reraise()
        )
        |> Seq.map (fun (i,data) -> i,mapRow data)
        |> Seq.takeWhile (fun (i,xs) -> not <| Seq.exists(Option.isNone) xs)
        |> Seq.map (fun (i,data) -> data |> Seq.map (fun d -> match d with | Some d -> Some(i,d) | None -> None) |> List.ofSeq)
        |> Seq.collect id
        |> Seq.choose id
        // eliminate the rowIndex (unless debug is needed)
        |> Seq.map snd
        |> List.ofSeq
    gearData
let gear = getGear()
(crusaders, gear)
|> Dump
|> ignore




// use http to pull in data from the wiki ?