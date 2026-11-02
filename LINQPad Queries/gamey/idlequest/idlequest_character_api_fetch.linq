<Query Kind="FSharpProgram">
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// Fetch a public IdleQuest character snapshot from idlequest.net and save under characters/.
// Endpoint: GET https://idlequest.net/api/characters/{characterName}
//   (live site uses plural "characters"; singular /api/character/{name} 404s)
// Rate limit: 1 request per 5 seconds — waits out cooldown before calling.
// Output: characters/{Name}_{yyyyMMdd-HHmmss}.json
//   envelope = { pulledAt, characterName, requestUrl, response }
// Throws on non-success HTTP or if the body is not a JSON object.
//
// If the name already exists in eq_characters.json, also updates that sheet + appends
// eq_events.json the same way hand-edit sync does (delta vs folded events, or full
// baseline if no events yet). Inventory is preserved (API has no bag). Source = "idlequest-api".

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization
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
  let charactersPath = Path.Combine(dataDir, "eq_characters.json")
  let eventsPath = Path.Combine(dataDir, "eq_events.json")

/// IdleQuest public API allows at most one request every 5 seconds.
let apiMinInterval = TimeSpan.FromSeconds 5.0
let cooldownPath = Path.Combine(Paths.dataDir, ".idlequest_api_last_request_utc.txt")

let jsonOpts =
  let o = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
  o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
  o.Converters.Add(JsonStringEnumConverter())
  o

let jsonWriteOpts =
  let o = JsonSerializerOptions(WriteIndented = true)
  o

let inline ser value = JsonSerializer.Serialize(value, jsonOpts)
let inline deser<'T> (text: string) = JsonSerializer.Deserialize<'T>(text, jsonOpts)

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

// --- Tracker-compatible types (subset) ---

let slotNames =
  [|
    "primary"; "secondary"; "chest"; "waist"; "legs"; "arms"; "hands"; "head"
    "shoulders"; "face"; "feet"; "wrist1"; "wrist2"; "fingers1"; "fingers2"
    "range"; "ear1"; "ear2"; "neck"; "back"; "ammo"
  |]

/// EQ client inventory slot index → tracker gear key (charm=0 skipped).
let eqSlotToName =
  dict [
    1, "ear1"; 2, "head"; 3, "face"; 4, "ear2"; 5, "neck"; 6, "shoulders"
    7, "arms"; 8, "back"; 9, "wrist1"; 10, "wrist2"; 11, "range"; 12, "hands"
    13, "primary"; 14, "secondary"; 15, "fingers1"; 16, "fingers2"; 17, "chest"
    18, "legs"; 19, "feet"; 20, "waist"; 21, "ammo"
  ]

let raceIdToName =
  dict [
    1, "Human"; 2, "Barbarian"; 3, "Erudite"; 4, "Wood Elf"; 5, "High Elf"
    6, "Dark Elf"; 7, "Half Elf"; 8, "Dwarf"; 9, "Troll"; 10, "Ogre"
    11, "Halfling"; 12, "Gnome"; 128, "Iksar"; 130, "Vah Shir"; 330, "Froglok"
  ]

type Gear = Dictionary<string, Nullable<int>>

[<CLIMutable>]
type InventoryEntry = {
  mutable ItemId: int
  mutable Tag: string
  mutable InBank: bool
  mutable Note: string
  mutable Qty: Nullable<int>
}

[<CLIMutable>]
type Sheet = {
  mutable Level: Nullable<int>
  mutable Hp: Nullable<int>
  mutable Mana: Nullable<int>
  mutable Ac: Nullable<int>
  mutable Atk: Nullable<int>
  mutable Str: Nullable<int>
  mutable Sta: Nullable<int>
  mutable Agi: Nullable<int>
  mutable Dex: Nullable<int>
  mutable Wis: Nullable<int>
  mutable Int: Nullable<int>
  mutable Cha: Nullable<int>
  mutable Mr: Nullable<int>
  mutable Fr: Nullable<int>
  mutable Cr: Nullable<int>
  mutable Pr: Nullable<int>
  mutable Dr: Nullable<int>
  mutable Gear: Gear
  mutable Inventory: ResizeArray<InventoryEntry>
}

[<CLIMutable>]
type Character = {
  mutable Id: string
  mutable Name: string
  mutable Race: string
  [<JsonPropertyName("class")>]
  mutable Class: string
  mutable Current: Sheet
  mutable CurrentAt: string
}

[<CLIMutable>]
type CharactersFile = {
  mutable Characters: ResizeArray<Character>
}

[<CLIMutable>]
type EventRecord = {
  mutable Kind: string
  mutable At: string
  mutable Source: string
  mutable Level: Nullable<int>
  mutable Hp: Nullable<int>
  mutable Mana: Nullable<int>
  mutable Ac: Nullable<int>
  mutable Atk: Nullable<int>
  mutable Str: Nullable<int>
  mutable Sta: Nullable<int>
  mutable Agi: Nullable<int>
  mutable Dex: Nullable<int>
  mutable Wis: Nullable<int>
  mutable Int: Nullable<int>
  mutable Cha: Nullable<int>
  mutable Mr: Nullable<int>
  mutable Fr: Nullable<int>
  mutable Cr: Nullable<int>
  mutable Pr: Nullable<int>
  mutable Dr: Nullable<int>
  mutable Gear: Gear
  mutable InventorySet: ResizeArray<InventoryEntry>
  mutable InventoryAdd: ResizeArray<InventoryEntry>
  mutable InventoryRemove: ResizeArray<int>
}

[<CLIMutable>]
type CharacterEvents = {
  mutable CharacterId: string
  mutable Events: ResizeArray<EventRecord>
}

[<CLIMutable>]
type EventsFile = {
  mutable Characters: ResizeArray<CharacterEvents>
}

[<CLIMutable>]
type ApiItem = {
  mutable id: int
  mutable name: string
}

[<CLIMutable>]
type ApiEquip = {
  mutable slot: int
  mutable item: ApiItem
}

[<CLIMutable>]
type ApiStats = {
  mutable maxHp: int
  mutable maxMana: int
  mutable ac: int
  mutable atk: int
  mutable str: int
  mutable sta: int
  mutable dex: int
  mutable agi: int
  [<JsonPropertyName("int")>]
  mutable intellect: int
  mutable wis: int
  mutable cha: int
  mutable mr: int
  mutable fr: int
  mutable cr: int
  mutable dr: int
  mutable pr: int
}

[<CLIMutable>]
type ApiCharacter = {
  mutable name: string
  mutable level: int
  [<JsonPropertyName("class")>]
  mutable className: string
  mutable raceId: int
  mutable stats: ApiStats
  mutable equipment: ApiEquip[]
}

let utcNow () = DateTimeOffset.UtcNow.ToString("o")
let nInt (v: int) = Nullable<int>(v)

let emptyGear () =
  let g = Gear()
  for name in slotNames do
    g.[name] <- Nullable()
  g

let emptyInventory () = ResizeArray<InventoryEntry>()

let emptySheet () : Sheet =
  {
    Level = Nullable()
    Hp = Nullable(); Mana = Nullable(); Ac = Nullable(); Atk = Nullable()
    Str = Nullable(); Sta = Nullable(); Agi = Nullable(); Dex = Nullable()
    Wis = Nullable(); Int = Nullable(); Cha = Nullable()
    Mr = Nullable(); Fr = Nullable(); Cr = Nullable(); Pr = Nullable(); Dr = Nullable()
    Gear = emptyGear ()
    Inventory = emptyInventory ()
  }

let entryQty (e: InventoryEntry) =
  if isNullUnsafe e then 1
  elif e.Qty.HasValue then max 1 e.Qty.Value
  else 1

let cloneInventoryEntry (e: InventoryEntry) : InventoryEntry =
  {
    ItemId = e.ItemId
    Tag = e.Tag
    InBank = e.InBank
    Note = e.Note
    Qty = e.Qty
  }

let cloneInventory (src: ResizeArray<InventoryEntry>) =
  let a = emptyInventory ()
  if not (isNullUnsafe src) then
    for e in src do a.Add(cloneInventoryEntry e)
  a

let cloneGear (src: Gear) =
  let g = emptyGear ()
  if not (isNullUnsafe src) then
    for KeyValue(k, v) in src do
      if g.ContainsKey k then g.[k] <- v
  g

let ensureInventory (sheet: Sheet) =
  if isNullUnsafe sheet.Inventory then sheet.Inventory <- emptyInventory ()

let inventoryFingerprint (inv: ResizeArray<InventoryEntry>) =
  if isNullUnsafe inv || inv.Count = 0 then ""
  else
    inv
    |> Seq.map (fun e -> sprintf "%d\t%s\t%b\t%s\t%d" e.ItemId e.Tag e.InBank (if isNullUnsafe e.Note then "" else e.Note) (entryQty e))
    |> String.concat "\n"

let nEq (a: Nullable<int>) (b: Nullable<int>) =
  if a.HasValue <> b.HasValue then false
  elif not a.HasValue then true
  else a.Value = b.Value

let nFmt (v: Nullable<int>) = if v.HasValue then string v.Value else "null"

let sheetFieldGetters: (string * (Sheet -> Nullable<int>) * (Sheet -> Nullable<int> -> unit)) list =
  [
    "level", (fun s -> s.Level), (fun s v -> s.Level <- v)
    "hp", (fun s -> s.Hp), (fun s v -> s.Hp <- v)
    "mana", (fun s -> s.Mana), (fun s v -> s.Mana <- v)
    "ac", (fun s -> s.Ac), (fun s v -> s.Ac <- v)
    "atk", (fun s -> s.Atk), (fun s v -> s.Atk <- v)
    "str", (fun s -> s.Str), (fun s v -> s.Str <- v)
    "sta", (fun s -> s.Sta), (fun s v -> s.Sta <- v)
    "agi", (fun s -> s.Agi), (fun s v -> s.Agi <- v)
    "dex", (fun s -> s.Dex), (fun s v -> s.Dex <- v)
    "wis", (fun s -> s.Wis), (fun s v -> s.Wis <- v)
    "int", (fun s -> s.Int), (fun s v -> s.Int <- v)
    "cha", (fun s -> s.Cha), (fun s v -> s.Cha <- v)
    "mr", (fun s -> s.Mr), (fun s v -> s.Mr <- v)
    "fr", (fun s -> s.Fr), (fun s v -> s.Fr <- v)
    "cr", (fun s -> s.Cr), (fun s v -> s.Cr <- v)
    "pr", (fun s -> s.Pr), (fun s v -> s.Pr <- v)
    "dr", (fun s -> s.Dr), (fun s v -> s.Dr <- v)
  ]

let applyEvent (sheet: Sheet) (e: EventRecord) =
  if e.Level.HasValue then sheet.Level <- e.Level
  if e.Hp.HasValue then sheet.Hp <- e.Hp
  if e.Mana.HasValue then sheet.Mana <- e.Mana
  if e.Ac.HasValue then sheet.Ac <- e.Ac
  if e.Atk.HasValue then sheet.Atk <- e.Atk
  if e.Str.HasValue then sheet.Str <- e.Str
  if e.Sta.HasValue then sheet.Sta <- e.Sta
  if e.Agi.HasValue then sheet.Agi <- e.Agi
  if e.Dex.HasValue then sheet.Dex <- e.Dex
  if e.Wis.HasValue then sheet.Wis <- e.Wis
  if e.Int.HasValue then sheet.Int <- e.Int
  if e.Cha.HasValue then sheet.Cha <- e.Cha
  if e.Mr.HasValue then sheet.Mr <- e.Mr
  if e.Fr.HasValue then sheet.Fr <- e.Fr
  if e.Cr.HasValue then sheet.Cr <- e.Cr
  if e.Pr.HasValue then sheet.Pr <- e.Pr
  if e.Dr.HasValue then sheet.Dr <- e.Dr
  if not (isNullUnsafe e.Gear) then
    if isNullUnsafe sheet.Gear then sheet.Gear <- emptyGear ()
    if String.Equals(e.Kind, "full", StringComparison.OrdinalIgnoreCase) then
      for name in slotNames do sheet.Gear.[name] <- Nullable()
    for KeyValue(slot, id) in e.Gear do
      if sheet.Gear.ContainsKey slot then sheet.Gear.[slot] <- id
  if not (isNullUnsafe e.InventorySet) then
    sheet.Inventory <- cloneInventory e.InventorySet
  elif not (isNullUnsafe e.InventoryAdd) then
    ensureInventory sheet
    for add in e.InventoryAdd do
      sheet.Inventory.Add(cloneInventoryEntry add)
  if not (isNullUnsafe e.InventoryRemove) then
    ensureInventory sheet
    for remId in e.InventoryRemove do
      let idx = sheet.Inventory |> Seq.tryFindIndex (fun x -> x.ItemId = remId)
      match idx with
      | Some i ->
          let e0 = sheet.Inventory.[i]
          let q = entryQty e0
          if q <= 1 then sheet.Inventory.RemoveAt i
          else e0.Qty <- nInt (q - 1)
      | None -> ()

let foldEvents (events: seq<EventRecord>) =
  let sheet = emptySheet ()
  for e in events do applyEvent sheet e
  sheet

let eventFromFullSheet (at: string) (source: string) (sheet: Sheet) : EventRecord =
  {
    Kind = "full"
    At = at
    Source = source
    Level = sheet.Level
    Hp = sheet.Hp; Mana = sheet.Mana; Ac = sheet.Ac; Atk = sheet.Atk
    Str = sheet.Str; Sta = sheet.Sta; Agi = sheet.Agi; Dex = sheet.Dex
    Wis = sheet.Wis; Int = sheet.Int; Cha = sheet.Cha
    Mr = sheet.Mr; Fr = sheet.Fr; Cr = sheet.Cr; Pr = sheet.Pr; Dr = sheet.Dr
    Gear = cloneGear sheet.Gear
    InventorySet = cloneInventory sheet.Inventory
    InventoryAdd = null
    InventoryRemove = null
  }

let emptyDeltaEvent (at: string) (source: string) : EventRecord =
  {
    Kind = "delta"
    At = at
    Source = source
    Level = Nullable(); Hp = Nullable(); Mana = Nullable(); Ac = Nullable(); Atk = Nullable()
    Str = Nullable(); Sta = Nullable(); Agi = Nullable(); Dex = Nullable()
    Wis = Nullable(); Int = Nullable(); Cha = Nullable()
    Mr = Nullable(); Fr = Nullable(); Cr = Nullable(); Pr = Nullable(); Dr = Nullable()
    Gear = Gear()
    InventorySet = null
    InventoryAdd = null
    InventoryRemove = null
  }

let diffSheets (expected: Sheet) (actual: Sheet) =
  let changes = ResizeArray<string * string * string>()
  for name, getter, _ in sheetFieldGetters do
    let a = getter expected
    let b = getter actual
    if not (nEq a b) then changes.Add(name, nFmt a, nFmt b)
  for slot in slotNames do
    let a = if expected.Gear.ContainsKey slot then expected.Gear.[slot] else Nullable()
    let b = if actual.Gear.ContainsKey slot then actual.Gear.[slot] else Nullable()
    if not (nEq a b) then changes.Add("gear." + slot, nFmt a, nFmt b)
  ensureInventory expected
  ensureInventory actual
  if inventoryFingerprint expected.Inventory <> inventoryFingerprint actual.Inventory then
    changes.Add("inventory", inventoryFingerprint expected.Inventory, inventoryFingerprint actual.Inventory)
  changes

let buildDeltaFromDiff (at: string) (source: string) (expected: Sheet) (actual: Sheet) =
  let e = emptyDeltaEvent at source
  for name, getter, _ in sheetFieldGetters do
    let a = getter expected
    let b = getter actual
    if not (nEq a b) then
      match name with
      | "level" -> e.Level <- b
      | "hp" -> e.Hp <- b
      | "mana" -> e.Mana <- b
      | "ac" -> e.Ac <- b
      | "atk" -> e.Atk <- b
      | "str" -> e.Str <- b
      | "sta" -> e.Sta <- b
      | "agi" -> e.Agi <- b
      | "dex" -> e.Dex <- b
      | "wis" -> e.Wis <- b
      | "int" -> e.Int <- b
      | "cha" -> e.Cha <- b
      | "mr" -> e.Mr <- b
      | "fr" -> e.Fr <- b
      | "cr" -> e.Cr <- b
      | "pr" -> e.Pr <- b
      | "dr" -> e.Dr <- b
      | _ -> ()
  for slot in slotNames do
    let a = if expected.Gear.ContainsKey slot then expected.Gear.[slot] else Nullable()
    let b = if actual.Gear.ContainsKey slot then actual.Gear.[slot] else Nullable()
    if not (nEq a b) then e.Gear.[slot] <- b
  ensureInventory expected
  ensureInventory actual
  if inventoryFingerprint expected.Inventory <> inventoryFingerprint actual.Inventory then
    e.InventorySet <- cloneInventory actual.Inventory
  e

let emptyCharactersFile () : CharactersFile = { Characters = ResizeArray() }
let emptyEventsFile () : EventsFile = { Characters = ResizeArray() }

let loadOrSeed<'T> path (seed: unit -> 'T) =
  if not (Directory.Exists Paths.dataDir) then Directory.CreateDirectory Paths.dataDir |> ignore
  if not (File.Exists path) then
    let s = seed ()
    File.WriteAllText(path, ser s)
    s
  else
    let text = File.ReadAllText path
    if String.IsNullOrWhiteSpace text then seed ()
    else
      let parsed = deser<'T> text
      if isNullUnsafe parsed then seed () else parsed

let saveEvents (events: EventsFile) = File.WriteAllText(Paths.eventsPath, ser events)
let saveCharacters (chars: CharactersFile) = File.WriteAllText(Paths.charactersPath, ser chars)

let eventsFor (events: EventsFile) (characterId: string) =
  events.Characters
  |> Seq.tryFind (fun c -> c.CharacterId = characterId)
  |> Option.map (fun c ->
      if isNullUnsafe c.Events then c.Events <- ResizeArray()
      c.Events)
  |> Option.defaultWith (fun () ->
      let bucket = { CharacterId = characterId; Events = ResizeArray() }
      events.Characters.Add bucket
      bucket.Events)

let appendEvent (events: EventsFile) (characterId: string) (e: EventRecord) =
  (eventsFor events characterId).Add e

let sheetFromApi (api: ApiCharacter) (preserveInventory: ResizeArray<InventoryEntry>) : Sheet =
  let st = api.stats
  let sheet = emptySheet ()
  sheet.Level <- nInt api.level
  if not (isNullUnsafe st) then
    sheet.Hp <- nInt st.maxHp
    sheet.Mana <- nInt st.maxMana
    sheet.Ac <- nInt st.ac
    sheet.Atk <- nInt st.atk
    sheet.Str <- nInt st.str
    sheet.Sta <- nInt st.sta
    sheet.Agi <- nInt st.agi
    sheet.Dex <- nInt st.dex
    sheet.Wis <- nInt st.wis
    sheet.Int <- nInt st.intellect
    sheet.Cha <- nInt st.cha
    sheet.Mr <- nInt st.mr
    sheet.Fr <- nInt st.fr
    sheet.Cr <- nInt st.cr
    sheet.Pr <- nInt st.pr
    sheet.Dr <- nInt st.dr
  if not (isNullUnsafe api.equipment) then
    for eq in api.equipment do
      if isNullUnsafe eq || isNullUnsafe eq.item then ()
      else
        let mutable slotName = ""
        if eqSlotToName.TryGetValue(eq.slot, &slotName) then
          sheet.Gear.[slotName] <- nInt eq.item.id
  sheet.Inventory <- cloneInventory preserveInventory
  sheet

/// Apply API snapshot onto a tracked character (events + current), if present by name.
let trySyncTrackedCharacter (characterName: string) (api: ApiCharacter) =
  if not (File.Exists Paths.charactersPath) then
    {| Updated = false; Reason = "eq_characters.json missing"; Changes = Array.empty |}
  else
    let chars = loadOrSeed Paths.charactersPath emptyCharactersFile
    let events = loadOrSeed Paths.eventsPath emptyEventsFile
    if isNullUnsafe chars.Characters then chars.Characters <- ResizeArray()
    if isNullUnsafe events.Characters then events.Characters <- ResizeArray()
    match
      chars.Characters
      |> Seq.tryFind (fun c -> String.Equals(c.Name, characterName, StringComparison.OrdinalIgnoreCase))
    with
    | None ->
        {| Updated = false; Reason = sprintf "No eq_characters entry named '%s'" characterName; Changes = Array.empty |}
    | Some ch ->
        if isNullUnsafe ch.Current then ch.Current <- emptySheet ()
        if isNullUnsafe ch.Current.Gear then ch.Current.Gear <- emptyGear ()
        ensureInventory ch.Current
        let mutable raceName = ""
        if raceIdToName.TryGetValue(api.raceId, &raceName) && isValueString raceName then
          ch.Race <- raceName
        if isValueString api.className then ch.Class <- api.className
        let next = sheetFromApi api ch.Current.Inventory
        let evs = eventsFor events ch.Id
        let at = utcNow ()
        if evs.Count = 0 then
          ch.Current <- next
          ch.CurrentAt <- at
          appendEvent events ch.Id (eventFromFullSheet at "idlequest-api" next)
          saveEvents events
          saveCharacters chars
          {|
            Updated = true
            Reason = "No prior events — wrote full baseline from API"
            Changes = [| "full-baseline" |]
          |}
        else
          let expected = foldEvents evs
          // Compare API sheet to event-folded expected (inventory preserved on next).
          let changes = diffSheets expected next
          if changes.Count = 0 then
            {| Updated = false; Reason = "Already in sync with folded events"; Changes = Array.empty |}
          else
            printfn "API sync: %s (%s)" ch.Name ch.Id
            for field, oldV, newV in changes do
              printfn "  %s: %s -> %s" field oldV newV
            let delta = buildDeltaFromDiff at "idlequest-api" expected next
            appendEvent events ch.Id delta
            ch.Current <- next
            ch.CurrentAt <- at
            saveEvents events
            saveCharacters chars
            {|
              Updated = true
              Reason = sprintf "%d field(s) updated" changes.Count
              Changes =
                changes
                |> Seq.map (fun (f, a, b) -> sprintf "%s: %s -> %s" f a b)
                |> Seq.toArray
            |}

let fetchCharacter (characterName: string) =
  let name = characterName.Trim()
  if not (isValueString name) then
    invalidOp "characterName is required."
  let url = sprintf "https://idlequest.net/api/characters/%s" (Uri.EscapeDataString name)
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
  let responseElement = doc.RootElement.Clone()
  let api =
    try JsonSerializer.Deserialize<ApiCharacter>(body, jsonOpts)
    with ex -> raise (InvalidOperationException("Failed to map response to ApiCharacter.", ex))
  if isNullUnsafe api then invalidOp "ApiCharacter deserialize returned null."
  {|
    PulledAt = pulledAt.ToString("o")
    CharacterName = name
    RequestUrl = url
    Response = responseElement
    Api = api
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

let trackedNames =
  try
    if not (File.Exists Paths.charactersPath) then Array.empty
    else
      let chars = loadOrSeed Paths.charactersPath emptyCharactersFile
      if isNullUnsafe chars.Characters then Array.empty
      else
        chars.Characters
        |> Seq.choose (fun c ->
            if isNullUnsafe c || not (isValueString c.Name) then None
            else Some (c.Name.Trim()))
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toArray
  with ex ->
    printfn "WARN: could not read eq_characters names for suggestions: %s" ex.Message
    Array.empty

let rememberedName =
  try
    let n = Util.LoadString nameKey
    if isValueString n then n.Trim() else ""
  with _ -> ""

let defaultName =
  if isValueString rememberedName then rememberedName
  elif trackedNames.Length > 0 then trackedNames.[0]
  else "Bartlebea"

let nameSuggestions =
  if trackedNames.Length = 0 then [| defaultName |]
  elif trackedNames |> Array.exists (fun n -> String.Equals(n, defaultName, StringComparison.OrdinalIgnoreCase)) then
    // Put remembered/default first so ReadLine opens on it, then the rest A–Z.
    Array.append
      [| defaultName |]
      (trackedNames
       |> Array.filter (fun n -> not (String.Equals(n, defaultName, StringComparison.OrdinalIgnoreCase))))
  else
    Array.append [| defaultName |] trackedNames

let characterName =
  let entered = Util.ReadLine("IdleQuest character name", defaultName, nameSuggestions)
  if isNullUnsafe entered then defaultName else entered.Trim()
try Util.SaveString(nameKey, characterName) with _ -> ()

printfn "Saving under: %s" Paths.charactersDir
printfn "Rate limit: 1 request / 5 seconds (cooldown file: %s)" cooldownPath

let envelope = fetchCharacter characterName
let path =
  saveEnvelope characterName
    {|
      PulledAt = envelope.PulledAt
      CharacterName = envelope.CharacterName
      RequestUrl = envelope.RequestUrl
      Response = envelope.Response
    |}
printfn "Wrote %s" path

let sync = trySyncTrackedCharacter characterName envelope.Api
printfn "Tracker sync: %s — %s" (if sync.Updated then "updated" else "skipped") sync.Reason

{|
  SavedPath = path
  PulledAt = envelope.PulledAt
  CharacterName = envelope.CharacterName
  RequestUrl = envelope.RequestUrl
  Response = envelope.Response
  TrackerSync = sync
|}
|> Dump
|> ignore
