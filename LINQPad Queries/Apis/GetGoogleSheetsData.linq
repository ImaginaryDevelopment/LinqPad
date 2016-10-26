<Query Kind="FSharpExpression">
  <NuGetReference>Google.Apis.Sheets.v4</NuGetReference>
  <Namespace>Google.Apis.Auth.OAuth2</Namespace>
  <Namespace>Google.Apis.Services</Namespace>
  <Namespace>Google.Apis.Sheets.v4</Namespace>
  <Namespace>Google.Apis.Sheets.v4.Data</Namespace>
  <Namespace>Google.Apis.Util.Store</Namespace>
</Query>

// use google apis to pull in data from the google sheet
// https://developers.google.com/sheets/quickstart/dotnet
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
let range = "Talent Calculator!A1:E"
getData spreadSheetId range




// use http to pull in data from the wiki ?