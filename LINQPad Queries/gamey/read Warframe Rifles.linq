<Query Kind="FSharpProgram">
  <NuGetReference>Google.Apis.Sheets.v4</NuGetReference>
  <Namespace>Google.Apis.Auth.OAuth2</Namespace>
  <Namespace>Google.Apis.Services</Namespace>
  <Namespace>Google.Apis.Sheets.v4</Namespace>
  <Namespace>Google.Apis.Sheets.v4.Data</Namespace>
  <Namespace>Google.Apis.Util.Store</Namespace>
</Query>

// parse weapons
let dumpt (title:string) x = x.Dump(title) |> ignore; x
let dumpC title items = items |> List.length |> dumpt title |> ignore; items
let after (delimiter:string) (x:string) = 
    match x.IndexOf delimiter with
    | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
    | i -> x.Substring(i + delimiter.Length)
let before (delimiter:string) (x:string) = x.Substring(0,x.IndexOf(delimiter))
let contains (delimiter:string) (x:string) = x.Contains(delimiter)
let beforeOrSelf delimiter x = if x |> contains delimiter then x |> before delimiter else x
let scopes = [SheetsService.Scope.SpreadsheetsReadonly]
let applicationName = "CotLI reader"
let getCredential () = 
    // "client_secret.json" full path?
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
let spreadSheetId = "1qvJYN9hpPMVePewg6zYhFXTIZqHyR1Gi9dDCfnF0wi4"

type PrimaryWeapon =
    |Rifle
    |Shotgun
    |Launcher
    |Bow
    |SniperRifle
    
type WeaponType = 
    |Primary of PrimaryWeapon
    |Secondary
    |Melee

type Weapon = {Name:string; Type:WeaponType; Owned:bool;Mastered:bool}

let getRange range t = 
    getData spreadSheetId range
    |> Seq.collect id 
    |> Seq.skip 1 
    |> Seq.map(string >> beforeOrSelf " (") 
    |> Seq.distinct //|> Seq.filter(String.IsNullOrWhiteSpace>>not)
    |> Seq.sort
    |> Seq.map(fun name -> {Name = name;Type = t;Owned = false; Mastered = false})
    |> List.ofSeq

let getRifles() = 
    let r = "Rifles!A1:A90"
    getRange r (Primary Rifle)
    
let getShotguns () = getRange "Shotguns!A1:A90" (Primary Shotgun)

let getSniperRifles() = getRange "Sniper Rifles!A1:A90" (Primary SniperRifle)
    
let getLaunchers ()  = getRange "Launchers!A1:A90" (Primary Launcher)

let getBows () = getRange "Bows!A1:A90" (Primary Bow)
let primaries = 
    [   getRifles()
        getShotguns()
        getSniperRifles()
        getLaunchers()
        getBows()
    ]
    |> List.collect id
    
let secondaries = getRange "Secondaries!A1:A255" Secondary
let melee = getRange "Melee!A1:A255" Melee
let all = 
    primaries@secondaries@melee@
        [   {Name="Fang"; Type=Melee;Owned=false;Mastered=false}
            {Name="Machete"; Type=Melee;Owned=false;Mastered=false}
            {Name="Ether Reaper"; Type=Melee;Owned=false;Mastered=false}
        ]
    |> Newtonsoft.Json.JsonConvert.SerializeObject

all.Dump("all")

let targetDir =
    let p = Path.GetDirectoryName(Util.CurrentQueryPath)
    let t = Path.Combine(p, "data")
    if not <| Directory.Exists t then
        Directory.CreateDirectory t |> ignore
    t
let targetFile = 
    Path.Combine(targetDir, "weapons.json")
File.WriteAllText(targetFile,all)
Newtonsoft.Json.JsonConvert.DeserializeObject<Weapon list>(all).Dump("deserialized")
