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

// pull in data and chart it
// tester https://developers.google.com/sheets/reference/rest/v4/spreadsheets/get
let useFile = false
let useCache,useCacheTags = true,true

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
        
module Google =
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
        
    let spreadSheetId = "1DbvdBWjbMvpfKmCYnOmYDrVM2-Ro_Bgl7xvTftLW5U8" // CotLI - Useful Tools
    let historyRange= "Idol History!A1:I67"
    // works
    //let tagRange= "Tags!A3:Z67"
    module History = 
        let getSheetData() = 
            let result = 
                let getSheetData() = 
                    getData spreadSheetId historyRange   
                    |> Seq.map (Seq.map (fun d -> d |> string) >> List.ofSeq)
                    |> List.ofSeq
                    |> dumpC "Rows returned"
                if useCache then
                    Util.Cache(getSheetData,"IdolHistorySheetData")
                else
                    getSheetData()
            result
//let deserialize            
type IdolHistory = { Date: DateTime; DaysSinceStart: int}

module Charting = 
    open System.Windows.Forms
    type FSharp.Charting.ChartTypes.GenericChart with 
        member this.CreateForm ()= 
            let frm = new Form(Visible = true, TopMost = true, Width = 700, Height = 500, WindowState=FormWindowState.Maximized)
            let ctl = new FSharp.Charting.ChartTypes.ChartControl(this, Dock = DockStyle.Fill)
            frm.Controls.Add(ctl)
            frm.Show()
            ctl.Focus() |> ignore
            frm
    let showChart chart = 
        use w = new System.Windows.Forms.Form()
        w.Controls.Add chart
        w.ShowDialog()
open Charting
let path = @"\\fs01\Documents\Brandon\cotlihistory.json"
let chartIt() = 
    match useFile with
    | false -> 
        Google.History.getSheetData()
        |> hoist (Fs.save path)
        |> hoist dumpReverse
        |> Seq.skip 1
        |> Seq.filter (Seq.exists (fun _ -> true))
        |> Seq.takeWhile (fun data -> not <| String.IsNullOrEmpty( data.[0]))
        |> Seq.map (fun data ->
            {Date= data.[0] |> DateTime.Parse; DaysSinceStart = int data.[1]}
        )
        
    | true -> 
        Fs.load<IdolHistory seq> path
        
    |> hoist dumpReverse
    |> List.ofSeq
    |> List.map(fun x -> x.Date, x.DaysSinceStart)
    |> FSharp.Charting.Chart.Line
    |> fun x -> x.CreateForm()
    
    
chartIt()
