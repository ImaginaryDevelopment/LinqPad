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

// desired use a different caching mechanism for dev, so you can invalidate individual entries for refresh
// consider using `AppDomain.CurrentDomain.GetData` for caching
let useCache,useCacheTags = true,true

let dumpt (title:string) x = x.Dump(title) |> ignore; x
let dumpC title items = items |> List.length |> dumpt title |> ignore; items
let scopes = [SheetsService.Scope.SpreadsheetsReadonly]
let applicationName = "CotLI reader"
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
    
let spreadSheetId = "1n6odBi-lmp-FgozzGl9AXsY9RjxwmVijdvFXMX0q37A" // CotLI - Useful Tools
let crusaderRange = "Gear!A1:E90" // expecting some rows or at least row 90 to be empty as a check for we aren't missing any crusaders in the query
let gearRange= "Gear!C91:G1189" //expecting row 1189 to be completely empty unless new gear has been added
let tagRange= "Tags!A3:Z67"

type Tag =
    |Female
    |Male
    |Human
    |Animal
    |Robot
    |GoldFinder
    |Royal
    |Event
    |Dps
    |Support
    |Clicker // M
    |Supernatural // N
    |Magic // O
    |Healer // P
    |Tank
    |Alien
    |Angel
    |Demon // Demon?
    |Dwarf // ?
    |Dragon // ?
    |W // Vampire?
    // X
    |Leprechaun 
    /// Orc
    |Orc 
    with
        override x.ToString() = sprintf "%A" x
type Crusader = { Name:String; Slot:string; SlotOrder: string (* 0-4? null - d? *); Items:string list; EnchantmentPoints:int; Tags: Tag list} with
    override x.ToString() = 
        
        sprintf "%A" x

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
    |GearItem of Gear
module CotLI =    
    let getCrusaders() = 
        let crusaders,gear =     
            let crusaders = 
                let sheetData = 
                    let getSheetData () = 
                        getData spreadSheetId crusaderRange 
                        |> Seq.map (Seq.map (fun d -> d |> string) >> List.ofSeq)
                        |> List.ofSeq
                        |> dumpC "Rows returned"
                    if useCache then
                        Util.Cache(getSheetData,"CrusaderSheetData")
                    else
                        getSheetData()
                
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
            //let pickTrueFalse = Util.ReadLine<bool>("is true?")
            //let pickedCrusader = Util.ReadLine("select crusader?", null, crusaderNames)
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
                    let getSheetData() = 
                        getData spreadSheetId gearRange 
                        |> List.ofSeq
                        |> dumpC "Gear Rows returned (not cached)"
                    if useCache then
                        let key = "GearSheetData"
                        printfn "attempting to use Cache for %s" key
                        Util.Cache(getSheetData,key)
                    else getSheetData()
                
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
                        GearItem {Name=gear.[0];Value=gear.[1];Quality=quality; Owned= gear.[3] = "1"}
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
                    
                // based loosely on http://www.fssnip.net/es
                let chunkByRowsAfterHead f (xs: 'T list) =
                    let len = xs.Length
                    let rec loop (xs: 'T list) = 
                        [
                            let chunk = 
                                let headDelimiter = xs.[0]
                                let tail = xs |> Seq.skip 1 |> Seq.takeWhile f |> List.ofSeq
                                headDelimiter::tail
                            yield chunk
                            let remainder = xs |> Seq.skip chunk.Length |> List.ofSeq
                            if Seq.exists (fun  _ -> true) remainder then
                                yield! loop remainder
                        ]
                    loop xs
                    
                let promoteHead (xs: 'T list) = 
                    xs |> function | h::r -> h,r | x -> failwithf "unexpected %A" x
                    
                let toChunkNames (xs: _ list) =
                    xs
                    |> chunkByRowsAfterHead (function | Name _ -> false | _ -> true)
                    |> List.map (promoteHead >> (function | Name n,r -> n,r | x -> failwithf "unexpected %A" x))
                let toChunkCategories (xs: _ list) =
                    xs
                    |> chunkByRowsAfterHead (function |Category _ -> false | Name n -> failwithf "Name in category/gear rows %A" n | _ -> true ) 
                    |> List.map (promoteHead >> (function | Category c, r -> c,r | x -> failwithf "unexpected %A" x))
                let toGearItems (xs: GearRow list) =
                    xs
                    |> List.map (function | GearItem g -> g | x -> failwithf "Unexepected in gearRows %A" x)
                gearData
                |> toChunkNames
                |> List.map (fun (n,r) -> n, r |> toChunkCategories |> List.map (fun (c, gs) -> c, gs |> toGearItems) |> dict)
                |> dict
            
            let gear = getGear()
            // populate tags so we can do things like filter/sort by isDps
            let getTags() =
                let sheetData = 
                    let getSheetData() = 
                        getData spreadSheetId tagRange
                        |> List.ofSeq
                        |> dumpC "Tags rows returned (not cached)"
                        //|> dumpt "Tags rows"
                    if useCacheTags then
                        let key = "TagsSheetData"
                        printfn "attempting to use cache for %s" key
                        Util.Cache(getSheetData, key)
                    else 
                        getSheetData()
                sheetData
                |> Seq.map( 
                    Seq.map string  >> List.ofSeq >>
                                    function
                                        slot::name::r ->
                                            if r.Length < 23 then
                                                r.Dump("insufficient items:" + name)
                                            let tags = 
                                                match r |> Seq.take 23 |> Seq.map ((=) "1") |> List.ofSeq  with
                                                |isFemale::isMale::isAnimal::isRobot::isGoldFinder::isRoyal::isEventObtained::isDps::isSupport::isClicker::isSuper::isMagic::isHealer::isTank::isAlien::isAngel::r ->
                                                    [
                                                        if isFemale then 
                                                            yield Tag.Female
                                                        if isMale then
                                                            yield Tag.Male
                                                        if isAnimal then
                                                            yield Tag.Animal
                                                        if isRobot then
                                                            yield Tag.Robot
                                                        if isGoldFinder then
                                                            yield Tag.GoldFinder
                                                        if isRoyal then
                                                            yield Tag.Royal
                                                        if isEventObtained then
                                                            yield Tag.Event
                                                        if isDps then
                                                            yield Tag.Dps
                                                        if isSupport then 
                                                            yield Tag.Support
                                                        if isClicker then
                                                            yield Tag.Clicker
                                                        if isSuper then
                                                            yield Tag.Supernatural
                                                        if isMagic then
                                                            yield Tag.Magic
                                                        if isTank then
                                                            yield Tag.Tank
                                                        
                                                ]
                                            slot,name,tags
                            
                )
                |> List.ofSeq
            
            let crusaders =
                let tags = getTags()
                let mutable missingTags = false
                let mapped =
                    crusaders
                    |> Seq.map (fun c -> 
                        tags |> Seq.tryFind (fun (_,name,tags) -> name= c.Name )
                        |> Option.map (fun (_,_,tags) -> tags)
                        |> function 
                            | Some t -> {c with Tags = t}
                            | None -> 
                                c.Name.Dump("tags not found")
                                missingTags <- true
                                c
                    )
                    |> List.ofSeq
                if missingTags then
                    tags
                    |> Seq.filter (fun (_,name,tags) -> crusaders|> Seq.map(fun c -> c.Name) |> Seq.contains name |> not)
                    |> dumpt "unmatched tag names"
                    |> ignore
                mapped
            crusaders,gear
        crusaders,gear
//let dpsCruByEp =
//    crusaders
//    |> Seq.sort(fun c -> c.)
let crusaders,gear = CotLI.getCrusaders()
(crusaders,gear)
|> Dump
|> ignore




// use http to pull in data from the wiki ?