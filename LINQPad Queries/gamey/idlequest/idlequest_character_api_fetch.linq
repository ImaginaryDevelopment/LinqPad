<Query Kind="FSharpProgram">
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// Fetch a public IdleQuest character snapshot from idlequest.net and save under characters/.
// Endpoint: GET https://idlequest.net/api/character/{characterName}
// Rate limit: 1 request per 5 seconds — this script waits out the cooldown before calling.
// Output file: characters/{Name}_{yyyyMMdd-HHmmss}.json
//   envelope = { pulledAt, characterName, requestUrl, response }
// Throws on non-success HTTP or if the body is not a JSON object.

open System
open System.IO
open System.Net.Http
open System.Text.Json
open System.Threading

module Paths =
  let dataDir =
    let q = Util.CurrentQueryPath
    if String.IsNullOrWhiteSpace q then
      Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
        "LINQPad Queries",
        "idlequest")
    else
      Path.GetDirectoryName q
  let charactersDir = Path.Combine(dataDir, "characters")

/// IdleQuest public API allows at most one request every 5 seconds.
let apiMinInterval = TimeSpan.FromSeconds 5.0
let cooldownPath = Path.Combine(Paths.dataDir, ".idlequest_api_last_request_utc.txt")

let jsonWriteOpts =
  let o = JsonSerializerOptions(WriteIndented = true)
  o

let isNullUnsafe value = Object.Equals(value, null)
let isValueString = String.IsNullOrWhiteSpace >> not
let ensureCharactersDir () =
  if not (Directory.Exists Paths.charactersDir) then
    Directory.CreateDirectory Paths.charactersDir |> ignore
  Paths.charactersDir

let readLastRequestUtc () =
  try
    if File.Exists cooldownPath then
      match DateTimeOffset.TryParse(File.ReadAllText(cooldownPath).Trim()) with
      | true, dto -> Some dto
      | _ -> None
    else None
  with _ -> None

let writeLastRequestUtc (at: DateTimeOffset) =
  try
    if not (Directory.Exists Paths.dataDir) then
      Directory.CreateDirectory Paths.dataDir |> ignore
    File.WriteAllText(cooldownPath, at.UtcDateTime.ToString("o"))
  with _ -> ()

/// Block until the 5s API cooldown has elapsed since the last request from this script folder.
let awaitApiCooldown () =
  match readLastRequestUtc () with
  | None -> ()
  | Some last ->
      let elapsed = DateTimeOffset.UtcNow - last.ToUniversalTime()
      let remaining = apiMinInterval - elapsed
      if remaining > TimeSpan.Zero then
        let ms = int (Math.Ceiling remaining.TotalMilliseconds)
        printfn "API rate limit: waiting %d ms (1 req / 5s)..." ms
        Thread.Sleep ms

let parseJsonObject (body: string) =
  if String.IsNullOrWhiteSpace body then
    invalidOp "Response body was empty (expected a JSON object)."
  let mutable doc = Unchecked.defaultof<JsonDocument>
  try
    doc <- JsonDocument.Parse(body)
  with ex ->
    raise (InvalidOperationException("Response body is not valid JSON.", ex))
  let kind = doc.RootElement.ValueKind
  if kind <> JsonValueKind.Object then
    doc.Dispose()
    invalidOp (sprintf "Response JSON root was %A (expected Object)." kind)
  doc

let fetchCharacter (characterName: string) =
  let name = characterName.Trim()
  if not (isValueString name) then
    invalidOp "characterName is required."
  let url = sprintf "https://idlequest.net/api/character/%s" (Uri.EscapeDataString name)
  awaitApiCooldown ()
  let pulledAt = DateTimeOffset.UtcNow
  writeLastRequestUtc pulledAt
  use client = new HttpClient()
  client.Timeout <- TimeSpan.FromSeconds 30.0
  client.DefaultRequestHeaders.UserAgent.ParseAdd("LinqPad-IdleQuest-CharacterFetch/1.0")
  printfn "GET %s" url
  use response = client.GetAsync(url).GetAwaiter().GetResult()
  let body = response.Content.ReadAsStringAsync().GetAwaiter().GetResult()
  if not response.IsSuccessStatusCode then
    invalidOp (
      sprintf "HTTP %d %s for %s. Body: %s"
        (int response.StatusCode)
        (string response.StatusCode)
        url
        (if isNullUnsafe body then "" elif body.Length > 400 then body.Substring(0, 400) + "..." else body))
  use doc = parseJsonObject body
  // Re-parse into JsonElement clone so we can dispose the document and still serialize.
  let responseElement = doc.RootElement.Clone()
  {|
    PulledAt = pulledAt.ToString("o")
    CharacterName = name
    RequestUrl = url
    Response = responseElement
  |}

let saveEnvelope (characterName: string) (envelope: obj) =
  let dir = ensureCharactersDir ()
  let stamp = DateTimeOffset.UtcNow.ToLocalTime().ToString("yyyyMMdd-HHmmss")
  let safeName =
    let invalid = Path.GetInvalidFileNameChars()
    let cleaned =
      characterName.Trim()
      |> Seq.map (fun c -> if invalid |> Array.contains c then '_' else c)
      |> String.Concat
    if String.IsNullOrWhiteSpace cleaned then "character" else cleaned
  let path = Path.Combine(dir, sprintf "%s_%s.json" safeName stamp)
  let json = JsonSerializer.Serialize(envelope, jsonWriteOpts)
  File.WriteAllText(path, json)
  path

// --- Main ---
let nameKey = "idlequestCharacterApiName"
let defaultName =
  try
    let n = Util.LoadString nameKey
    if isValueString n then n.Trim() else "Bartlebea"
  with _ -> "Bartlebea"


let characterName = 
    let entered = Util.ReadLine("IdleQuest character name", defaultName, [| defaultName |])
    if isNullUnsafe entered then defaultName else entered.Trim()
try Util.SaveString(nameKey, characterName) with _ -> ()

printfn "Saving under: %s" Paths.charactersDir
printfn "Rate limit: 1 request / 5 seconds (cooldown file: %s)" cooldownPath

let envelope = fetchCharacter characterName
let path = saveEnvelope characterName envelope
printfn "Wrote %s" path
{|
  SavedPath = path
  PulledAt = envelope.PulledAt
  CharacterName = envelope.CharacterName
  RequestUrl = envelope.RequestUrl
  Response = envelope.Response
|}
|> Dump
|> ignore
