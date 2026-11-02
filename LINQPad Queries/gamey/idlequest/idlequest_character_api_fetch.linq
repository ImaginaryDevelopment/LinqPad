<Query Kind="FSharpProgram">
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// Fetch a public IdleQuest character snapshot from idlequest.net and save under characters/.
// Endpoint: GET https://idlequest.net/api/characters/{characterName}
//   (live site uses plural "characters"; singular /api/character/{name} 404s)
// Rate limit: 1 request per 5 seconds — waits out cooldown before calling.
// Output: characters/{Name}.json (overwrites prior snapshot; PulledAt lives in the envelope)
//   envelope = { pulledAt, characterName, requestUrl, response }
// Throws on non-success HTTP or if the body is not a JSON object.
//
// If the name already exists in eq_characters.json, also updates that sheet + appends
// eq_events.json the same way hand-edit sync does (delta vs folded events, or full
// baseline if no events yet). Inventory is preserved (API has no bag). Source = "idlequest-api".
// After save/sync, prompts to compare stats vs another character already saved under
// characters/ (one file per name; independent of eq_characters.json).
// Gear diffs include a "source" row under each "item" (item-cache zoneIds, else local
// brynn idlequest-content drop→zone; prefers idlequestContentRoot / D:\projects\idlequest-content).

open System
open System.Collections.Generic
open System.Data
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
  let itemCachePath = Path.Combine(dataDir, "item-cache.json")

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

/// EQ client inventory slot index → tracker gear key (charm=0 skipped for tracker sync).
let eqSlotToName =
  dict [
    1, "ear1"; 2, "head"; 3, "face"; 4, "ear2"; 5, "neck"; 6, "shoulders"
    7, "arms"; 8, "back"; 9, "wrist1"; 10, "wrist2"; 11, "range"; 12, "hands"
    13, "primary"; 14, "secondary"; 15, "fingers1"; 16, "fingers2"; 17, "chest"
    18, "legs"; 19, "feet"; 20, "waist"; 21, "ammo"
  ]

/// Slot order for gear compare (includes charm).
let eqSlotDisplayOrder =
  [|
    0, "charm"; 1, "ear1"; 2, "head"; 3, "face"; 4, "ear2"; 5, "neck"
    6, "shoulders"; 7, "arms"; 8, "back"; 9, "wrist1"; 10, "wrist2"
    11, "range"; 12, "hands"; 13, "primary"; 14, "secondary"
    15, "fingers1"; 16, "fingers2"; 17, "chest"; 18, "legs"; 19, "feet"
    20, "waist"; 21, "ammo"
  |]

/// Item fields compared slot-by-slot (API JSON names).
let itemStatKeys =
  [|
    "ac"; "hp"; "mana"; "endurance"
    "str"; "sta"; "dex"; "agi"; "int"; "wis"; "cha"
    "mr"; "fr"; "cr"; "dr"; "pr"; "corruptionResist"
    "attack"; "haste"; "hpRegen"; "manaRegen"; "enduranceRegen"
    "damage"; "delay"; "range"
    "skillModType"; "skillModValue"
    "wornSpellId"; "focusSpellId"; "clickSpellId"; "procSpellId"
  |]

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

type ApiItem = {
  id: int
  name: string
  /// Ordered numeric fields from the API item payload (for gear compare).
  stats: (string * int)[]
}

type ApiEquip = {
  slot: int
  item: ApiItem
}

let emptyApiItem () : ApiItem =
  { id = 0; name = ""; stats = Array.empty }

type ApiStats = {
  maxHp: int
  maxMana: int
  ac: int
  atk: int
  str: int
  sta: int
  dex: int
  agi: int
  intellect: int
  wis: int
  cha: int
  mr: int
  fr: int
  cr: int
  dr: int
  pr: int
}

type ApiCharacter = {
  name: string
  level: int
  className: string
  raceId: int
  stats: ApiStats
  equipment: ApiEquip[]
}

/// Coerce JSON number/string/null/missing → int (API sometimes sends floats).
let jsonInt (el: JsonElement) (name: string) (fallback: int) =
  match el.TryGetProperty(name) with
  | false, _ -> fallback
  | true, p ->
      match p.ValueKind with
      | JsonValueKind.Null
      | JsonValueKind.Undefined -> fallback
      | JsonValueKind.Number ->
          let mutable i = 0
          if p.TryGetInt32(&i) then i
          else
            let mutable l = 0L
            if p.TryGetInt64(&l) then int l
            else
              let mutable d = 0.0
              if p.TryGetDouble(&d) then int (Math.Round d) else fallback
      | JsonValueKind.String ->
          match Int32.TryParse(p.GetString()) with
          | true, i -> i
          | _ ->
              match Double.TryParse(p.GetString()) with
              | true, d -> int (Math.Round d)
              | _ -> fallback
      | JsonValueKind.True -> 1
      | JsonValueKind.False -> 0
      | _ -> fallback

let jsonString (el: JsonElement) (name: string) (fallback: string) =
  match el.TryGetProperty(name) with
  | false, _ -> fallback
  | true, p when p.ValueKind = JsonValueKind.String ->
      let s = p.GetString()
      if isNullUnsafe s then fallback else s
  | true, p when p.ValueKind = JsonValueKind.Number -> p.ToString()
  | _ -> fallback

let emptyApiStats () : ApiStats =
  {
    maxHp = 0; maxMana = 0; ac = 0; atk = 0
    str = 0; sta = 0; dex = 0; agi = 0; intellect = 0
    wis = 0; cha = 0; mr = 0; fr = 0; cr = 0; dr = 0; pr = 0
  }

let parseApiStats (el: JsonElement) : ApiStats =
  if el.ValueKind <> JsonValueKind.Object then emptyApiStats ()
  else
    {
      maxHp = jsonInt el "maxHp" 0
      maxMana = jsonInt el "maxMana" 0
      ac = jsonInt el "ac" 0
      atk = jsonInt el "atk" 0
      str = jsonInt el "str" 0
      sta = jsonInt el "sta" 0
      dex = jsonInt el "dex" 0
      agi = jsonInt el "agi" 0
      intellect = jsonInt el "int" 0
      wis = jsonInt el "wis" 0
      cha = jsonInt el "cha" 0
      mr = jsonInt el "mr" 0
      fr = jsonInt el "fr" 0
      cr = jsonInt el "cr" 0
      dr = jsonInt el "dr" 0
      pr = jsonInt el "pr" 0
    }

let parseApiItem (item: JsonElement) : ApiItem =
  if item.ValueKind <> JsonValueKind.Object then emptyApiItem ()
  else
    {
      id = jsonInt item "id" 0
      name = jsonString item "name" ""
      stats =
        itemStatKeys
        |> Array.map (fun k -> k, jsonInt item k 0)
    }

let parseApiEquipment (root: JsonElement) : ApiEquip[] =
  match root.TryGetProperty("equipment") with
  | false, _ -> Array.empty
  | true, arr when arr.ValueKind <> JsonValueKind.Array -> Array.empty
  | true, arr ->
      arr.EnumerateArray()
      |> Seq.choose (fun eq ->
          if eq.ValueKind <> JsonValueKind.Object then None
          else
            match eq.TryGetProperty("item") with
            | false, _ -> None
            | true, item when item.ValueKind <> JsonValueKind.Object -> None
            | true, item ->
                Some
                  {
                    slot = jsonInt eq "slot" -1
                    item = parseApiItem item
                  })
      |> Seq.toArray

let parseApiCharacter (root: JsonElement) : ApiCharacter =
  let stats =
    match root.TryGetProperty("stats") with
    | true, s -> parseApiStats s
    | _ -> emptyApiStats ()
  {
    name = jsonString root "name" ""
    level = jsonInt root "level" 0
    className = jsonString root "class" ""
    raceId = jsonInt root "raceId" 0
    stats = stats
    equipment = parseApiEquipment root
  }

let dumpRawJsonOnError (title: string) (body: string) =
  printfn "%s" title
  try
    use doc = JsonDocument.Parse(body)
    let pretty =
      JsonSerializer.Serialize(doc.RootElement, jsonWriteOpts)
    pretty.Dump("Raw API JSON (parse failed)")
  with _ ->
    body.Dump("Raw API body (not re-formattable)")

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
  for eq in api.equipment do
    let mutable slotName = ""
    if eq.slot >= 0 && eqSlotToName.TryGetValue(eq.slot, &slotName) && eq.item.id <> 0 then
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
    try parseApiCharacter responseElement
    with ex ->
      dumpRawJsonOnError
        (sprintf "Failed to map response to ApiCharacter (%s). Dumping raw JSON." ex.Message)
        body
      raise (InvalidOperationException("Failed to map response to ApiCharacter. See dumped raw JSON.", ex))
  {|
    PulledAt = pulledAt.ToString("o")
    CharacterName = name
    RequestUrl = url
    Response = responseElement
    Api = api
  |}

let safeFileName (characterName: string) =
  let invalid = Path.GetInvalidFileNameChars()
  let cleaned =
    characterName.Trim()
    |> Seq.map (fun c -> if invalid |> Array.contains c then '_' else c)
    |> String.Concat
  if String.IsNullOrWhiteSpace cleaned then "character" else cleaned

let saveEnvelope (characterName: string) (envelope: obj) =
  let dir = ensureCharactersDir ()
  let safeName = safeFileName characterName
  let path = Path.Combine(dir, sprintf "%s.json" safeName)
  // Drop legacy timestamped copies (Name_yyyyMMdd-HHmmss.json) for this character.
  try
    let prefix = safeName + "_"
    for old in Directory.GetFiles(dir, "*.json") do
      let baseName = Path.GetFileNameWithoutExtension old
      if
        not (String.Equals(old, path, StringComparison.OrdinalIgnoreCase))
        && baseName.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)
      then
        File.Delete old
  with _ -> ()
  let json = JsonSerializer.Serialize(envelope, jsonWriteOpts)
  File.WriteAllText(path, json)
  path

type SnapshotRef = {
  CharacterName: string
  PulledAt: string
  Path: string
  Api: ApiCharacter
}

let tryGetPropCI (obj: JsonElement) (name: string) =
  match obj.TryGetProperty(name) with
  | true, p -> Some p
  | false, _ ->
      obj.EnumerateObject()
      |> Seq.tryFind (fun p -> String.Equals(p.Name, name, StringComparison.OrdinalIgnoreCase))
      |> Option.map (fun p -> p.Value)

let tryLoadSnapshot (path: string) : SnapshotRef option =
  try
    use doc = JsonDocument.Parse(File.ReadAllText path)
    let root = doc.RootElement
    if root.ValueKind <> JsonValueKind.Object then None
    else
      match tryGetPropCI root "response" with
      | None -> None
      | Some resp when resp.ValueKind <> JsonValueKind.Object -> None
      | Some resp ->
          let response = resp.Clone()
          let api = parseApiCharacter response
          let envName =
            match tryGetPropCI root "characterName" with
            | Some p when p.ValueKind = JsonValueKind.String ->
                let s = p.GetString()
                if isValueString s then s.Trim() else ""
            | _ -> ""
          let pulledAt =
            match tryGetPropCI root "pulledAt" with
            | Some p when p.ValueKind = JsonValueKind.String ->
                let s = p.GetString()
                if isNullUnsafe s then "" else s
            | _ -> ""
          let name =
            if isValueString envName then envName
            elif isValueString api.name then api.name.Trim()
            else Path.GetFileNameWithoutExtension path
          Some
            {
              CharacterName = name
              PulledAt = pulledAt
              Path = path
              Api = api
            }
  with _ -> None

/// One snapshot per character under characters/ (prefer newest write if duplicates remain).
let listLatestSnapshotsExclude (excludeName: string) =
  let dir = ensureCharactersDir ()
  Directory.GetFiles(dir, "*.json")
  |> Array.choose tryLoadSnapshot
  |> Array.filter (fun s ->
      not (String.Equals(s.CharacterName, excludeName, StringComparison.OrdinalIgnoreCase)))
  |> Array.groupBy (fun s -> s.CharacterName.ToLowerInvariant())
  |> Array.map (fun (_, group) ->
      group |> Array.maxBy (fun s -> File.GetLastWriteTimeUtc s.Path))
  |> Array.sortBy (fun s -> s.CharacterName)

let statsPairs (api: ApiCharacter) =
  let st = api.stats
  if isNullUnsafe st then Array.empty
  else
    [|
      "level", api.level
      "maxHp", st.maxHp
      "maxMana", st.maxMana
      "ac", st.ac
      "atk", st.atk
      "str", st.str
      "sta", st.sta
      "dex", st.dex
      "agi", st.agi
      "int", st.intellect
      "wis", st.wis
      "cha", st.cha
      "mr", st.mr
      "fr", st.fr
      "cr", st.cr
      "dr", st.dr
      "pr", st.pr
    |]

let compareColumnNames (leftName: string) (rightName: string) =
  let leftCol = if isValueString leftName then leftName.Trim() else "Fetched"
  let rightCol = if isValueString rightName then rightName.Trim() else "Other"
  let rightColUnique =
    if String.Equals(leftCol, rightCol, StringComparison.OrdinalIgnoreCase) then
      rightCol + " (other)"
    else rightCol
  leftCol, rightColUnique

let equipmentBySlot (api: ApiCharacter) =
  let map = Dictionary<int, ApiItem>()
  if not (isNullUnsafe api.equipment) then
    for eq in api.equipment do
      if eq.slot >= 0 && not (isNullUnsafe eq.item) && eq.item.id <> 0 then
        map.[eq.slot] <- eq.item
  map

let itemStatValue (item: ApiItem) (stat: string) =
  if isNullUnsafe item || isNullUnsafe item.stats then 0
  else
    match item.stats |> Array.tryFind (fun (k, _) -> k = stat) with
    | Some (_, v) -> v
    | None -> 0

let slotItemId (map: Dictionary<int, ApiItem>) (slot: int) =
  let mutable item = emptyApiItem ()
  if map.TryGetValue(slot, &item) then item.id else 0

/// Paired wear slots that can be swapped without gameplay meaning (ears / wrists / rings).
let pairedCompareSlots =
  [|
    (1, 4)   // ear1, ear2
    (9, 10)  // wrist1, wrist2
    (15, 16) // fingers1, fingers2
  |]

/// Remap the right-hand equipment so paired slots line up with left by item id when a
/// swap improves matches (same pieces worn on opposite ears/wrists/rings).
let alignPairedSlots
  (left: Dictionary<int, ApiItem>)
  (right: Dictionary<int, ApiItem>)
  =
  let aligned = Dictionary<int, ApiItem>(right)
  for s1, s2 in pairedCompareSlots do
    let l1 = slotItemId left s1
    let l2 = slotItemId left s2
    let r1 = slotItemId right s1
    let r2 = slotItemId right s2
    let straight = (if l1 = r1 then 1 else 0) + (if l2 = r2 then 1 else 0)
    let swapped = (if l1 = r2 then 1 else 0) + (if l2 = r1 then 1 else 0)
    if swapped > straight then
      let mutable a = emptyApiItem ()
      let mutable b = emptyApiItem ()
      let hasA = right.TryGetValue(s1, &a)
      let hasB = right.TryGetValue(s2, &b)
      ignore (aligned.Remove s1)
      ignore (aligned.Remove s2)
      if hasB then aligned.[s1] <- b
      if hasA then aligned.[s2] <- a
  aligned

// --- Item source (item-cache ZoneIds, else local brynn idlequest-content drop→zone) ---

let defaultContentRoot = @"D:\projects\idlequest-content"
let contentShards =
  [| '0'..'9' |] |> Array.map string |> Array.append ([| 'a'..'f' |] |> Array.map string)

[<CLIMutable>]
type CachedItemZones = {
  mutable Id: int
  mutable ZoneIds: int[]
}

[<CLIMutable>]
type ItemCacheZonesFile = {
  mutable Items: Dictionary<string, CachedItemZones>
}

let itemCacheReadOpts =
  let o = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
  o

let tryContentRootQuiet () =
  let candidates =
    [
      try
        let pw = Util.GetPassword("idlequestContentRoot")
        if isNullUnsafe pw then "" else pw.Trim()
      with _ -> ""
      defaultContentRoot
    ]
  candidates
  |> List.tryFind (fun root ->
      isValueString root
      && Directory.Exists(Path.Combine(root, "data", "zone"))
      && Directory.Exists(Path.Combine(root, "data", "lootdrop_entries")))

let loadItemCacheZoneIds () =
  let map = Dictionary<int, int[]>()
  try
    if File.Exists Paths.itemCachePath then
      let text = File.ReadAllText Paths.itemCachePath
      let parsed = JsonSerializer.Deserialize<ItemCacheZonesFile>(text, itemCacheReadOpts)
      if not (isNullUnsafe parsed) && not (isNullUnsafe parsed.Items) then
        for KeyValue(key, item) in parsed.Items do
          let id =
            if not (isNullUnsafe item) && item.Id <> 0 then item.Id
            else
              match Int32.TryParse key with
              | true, n -> n
              | _ -> 0
          if id > 0 && not (isNullUnsafe item) && not (isNullUnsafe item.ZoneIds) && item.ZoneIds.Length > 0 then
            map.[id] <- item.ZoneIds
  with ex ->
    printfn "WARN: could not read item-cache.json for sources: %s" ex.Message
  map

let jsonStrEl (el: JsonElement) (name: string) =
  match el.TryGetProperty(name) with
  | true, p when p.ValueKind = JsonValueKind.String ->
      let s = p.GetString()
      if isNullUnsafe s then "" else s
  | _ -> ""

let forEachContentRow (contentRoot: string) (table: string) (action: JsonElement -> unit) =
  for shard in contentShards do
    let path = Path.Combine(contentRoot, "data", table, shard + ".ndjson")
    if File.Exists path then
      try
        for line in File.ReadLines path do
          if line.Length > 0 then
            try
              use doc = JsonDocument.Parse line
              action doc.RootElement
            with _ -> ()
      with ex ->
        printfn "WARN: %s/%s read failed: %s" table shard ex.Message

let loadZoneShortNames (contentRoot: string) =
  let map = Dictionary<int, string>()
  if isValueString contentRoot then
    forEachContentRow contentRoot "zone" (fun row ->
      let id = jsonInt row "id" 0
      let shortName = jsonStrEl row "short_name"
      if id > 0 && isValueString shortName then map.[id] <- shortName)
  map

let addToSetMap (map: Dictionary<'K, HashSet<'V>>) (key: 'K) (value: 'V) =
  let mutable set = Unchecked.defaultof<HashSet<'V>>
  if not (map.TryGetValue(key, &set)) then
    set <- HashSet<'V>()
    map.[key] <- set
  ignore (set.Add value)

/// item_id → zone.id via lootdrop → loottable → npc → spawn → zone (brynn content).
let buildItemDropZones (contentRoot: string) : Dictionary<int, int[]> =
  let zoneByShort = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
  forEachContentRow contentRoot "zone" (fun row ->
    let id = jsonInt row "id" 0
    let shortName = jsonStrEl row "short_name"
    if id > 0 && isValueString shortName then zoneByShort.[shortName] <- id)
  let groupZones = Dictionary<int, HashSet<int>>()
  forEachContentRow contentRoot "spawn2" (fun row ->
    let groupId = jsonInt row "spawngroupID" 0
    let shortName = jsonStrEl row "zone"
    let mutable zoneId = 0
    if groupId > 0 && zoneByShort.TryGetValue(shortName, &zoneId) then
      addToSetMap groupZones groupId zoneId)
  let npcZones = Dictionary<int, HashSet<int>>()
  forEachContentRow contentRoot "spawnentry" (fun row ->
    let npcId = jsonInt row "npcID" 0
    let groupId = jsonInt row "spawngroupID" 0
    let mutable zones = Unchecked.defaultof<HashSet<int>>
    if npcId > 0 && groupZones.TryGetValue(groupId, &zones) then
      for z in zones do addToSetMap npcZones npcId z)
  let tableZones = Dictionary<int, HashSet<int>>()
  forEachContentRow contentRoot "npc_types" (fun row ->
    let npcId = jsonInt row "id" 0
    let loottableId = jsonInt row "loottable_id" 0
    let mutable zones = Unchecked.defaultof<HashSet<int>>
    if loottableId > 0 && npcZones.TryGetValue(npcId, &zones) then
      for z in zones do addToSetMap tableZones loottableId z)
  let dropZones = Dictionary<int, HashSet<int>>()
  forEachContentRow contentRoot "loottable_entries" (fun row ->
    let loottableId = jsonInt row "loottable_id" 0
    let lootdropId = jsonInt row "lootdrop_id" 0
    let mutable zones = Unchecked.defaultof<HashSet<int>>
    if lootdropId > 0 && tableZones.TryGetValue(loottableId, &zones) then
      for z in zones do addToSetMap dropZones lootdropId z)
  let itemZones = Dictionary<int, HashSet<int>>()
  forEachContentRow contentRoot "lootdrop_entries" (fun row ->
    let itemId = jsonInt row "item_id" 0
    let lootdropId = jsonInt row "lootdrop_id" 0
    let mutable zones = Unchecked.defaultof<HashSet<int>>
    if itemId > 0 && dropZones.TryGetValue(lootdropId, &zones) then
      for z in zones do addToSetMap itemZones itemId z)
  let result = Dictionary<int, int[]>()
  for KeyValue(itemId, zones) in itemZones do
    result.[itemId] <- zones |> Seq.sort |> Seq.toArray
  result

let formatZoneSource (zoneIds: int[]) (zoneNames: Dictionary<int, string>) =
  if isNullUnsafe zoneIds || zoneIds.Length = 0 then ""
  else
    zoneIds
    |> Array.map (fun id ->
        let mutable name = ""
        if zoneNames.TryGetValue(id, &name) && isValueString name then name
        else sprintf "zone:%d" id)
    |> Array.distinct
    |> String.concat ", "

/// Resolves drop-zone labels: item-cache ZoneIds first, else local brynn content join.
let makeItemSourceResolver () =
  let cacheZones = loadItemCacheZoneIds ()
  let contentRoot = tryContentRootQuiet () |> Option.defaultValue ""
  let zoneNames =
    if isValueString contentRoot then loadZoneShortNames contentRoot
    else Dictionary<int, string>()
  let contentZones = ref (None: Dictionary<int, int[]> option)
  let memo = Dictionary<int, string>()
  let ensureContentZones () =
    match !contentZones with
    | Some idx -> idx
    | None when not (isValueString contentRoot) ->
        let empty = Dictionary<int, int[]>()
        contentZones := Some empty
        empty
    | None ->
        printfn "Building item→zone drop index from idlequest-content (for gear source rows)..."
        let idx = buildItemDropZones contentRoot
        printfn "Drop-zone index ready (%d items)." idx.Count
        contentZones := Some idx
        idx
  fun (itemId: int) ->
    if itemId <= 0 then ""
    else
      let mutable hit = ""
      if memo.TryGetValue(itemId, &hit) then hit
      else
        let mutable zones = Unchecked.defaultof<int[]>
        let fromCache = cacheZones.TryGetValue(itemId, &zones)
        let zoneIds =
          if fromCache && not (isNullUnsafe zones) && zones.Length > 0 then zones
          else
            let idx = ensureContentZones ()
            let mutable fromContent = Unchecked.defaultof<int[]>
            if idx.TryGetValue(itemId, &fromContent) then fromContent else [||]
        let label = formatZoneSource zoneIds zoneNames
        memo.[itemId] <- label
        label

/// Slot → Stat → left/right/delta. Only rows where the two sides differ.
/// Ear / wrist / ring pairs are aligned by item id before comparing.
/// After each differing "item" row, a "source" row lists drop zones (cache, else brynn).
let compareGearTable (leftCol: string) (rightCol: string) (left: ApiCharacter) (right: ApiCharacter) =
  let leftSlots = equipmentBySlot left
  let rightSlots = alignPairedSlots leftSlots (equipmentBySlot right)
  let resolveSource = makeItemSourceResolver ()
  let table = new DataTable()
  ignore (table.Columns.Add("Slot", typeof<string>))
  ignore (table.Columns.Add("Stat", typeof<string>))
  ignore (table.Columns.Add(leftCol, typeof<obj>))
  ignore (table.Columns.Add(rightCol, typeof<obj>))
  ignore (table.Columns.Add("Delta", typeof<obj>))
  let addRow slot stat (l: obj) (r: obj) (d: obj) =
    table.Rows.Add(slot, stat, l, r, d) |> ignore
  for slotId, slotName in eqSlotDisplayOrder do
    let mutable leftItem = emptyApiItem ()
    let mutable rightItem = emptyApiItem ()
    let hasL = leftSlots.TryGetValue(slotId, &leftItem)
    let hasR = rightSlots.TryGetValue(slotId, &rightItem)
    if not hasL then leftItem <- emptyApiItem ()
    if not hasR then rightItem <- emptyApiItem ()
    let leftName = if hasL && isValueString leftItem.name then leftItem.name else ""
    let rightName = if hasR && isValueString rightItem.name then rightItem.name else ""
    if leftItem.id <> rightItem.id || leftName <> rightName then
      addRow
        slotName
        "item"
        (if hasL then box leftName else box DBNull.Value)
        (if hasR then box rightName else box DBNull.Value)
        (box DBNull.Value)
      let leftSrc = if hasL then resolveSource leftItem.id else ""
      let rightSrc = if hasR then resolveSource rightItem.id else ""
      addRow
        slotName
        "source"
        (if hasL && isValueString leftSrc then box leftSrc else box DBNull.Value)
        (if hasR && isValueString rightSrc then box rightSrc else box DBNull.Value)
        (box DBNull.Value)
    for stat in itemStatKeys do
      let sentinel =
        stat = "skillModType"
        || stat.EndsWith("SpellId", StringComparison.Ordinal)
      let lv = if hasL then itemStatValue leftItem stat else (if sentinel then -1 else 0)
      let rv = if hasR then itemStatValue rightItem stat else (if sentinel then -1 else 0)
      if lv <> rv then
        addRow slotName stat (box lv) (box rv) (box (lv - rv))
  table

let compareStats (leftName: string) (left: ApiCharacter) (right: SnapshotRef) =
  let leftMap = statsPairs left |> dict
  let rightMap = statsPairs right.Api |> dict
  // Prefer gameplay-ish order; fall back to alpha for anything unexpected.
  let preferred =
    [|
      "level"; "maxHp"; "maxMana"; "ac"; "atk"
      "str"; "sta"; "dex"; "agi"; "int"; "wis"; "cha"
      "mr"; "fr"; "cr"; "dr"; "pr"
    |]
  let keys =
    let present =
      Seq.append leftMap.Keys rightMap.Keys
      |> Seq.distinct
      |> Set.ofSeq
    let head = preferred |> Array.filter present.Contains
    let tail =
      present
      |> Seq.filter (fun k -> preferred |> Array.contains k |> not)
      |> Seq.sort
      |> Seq.toArray
    Array.append head tail
  let leftCol, rightColUnique = compareColumnNames leftName right.CharacterName
  let table = new DataTable()
  ignore (table.Columns.Add("Stat", typeof<string>))
  ignore (table.Columns.Add(leftCol, typeof<obj>))
  ignore (table.Columns.Add(rightColUnique, typeof<obj>))
  ignore (table.Columns.Add("Delta", typeof<obj>))
  for k in keys do
    let mutable lv = 0
    let mutable rv = 0
    let hasL = leftMap.TryGetValue(k, &lv)
    let hasR = rightMap.TryGetValue(k, &rv)
    let leftObj: obj = if hasL then box lv else box DBNull.Value
    let rightObj: obj = if hasR then box rv else box DBNull.Value
    let deltaObj: obj =
      if hasL && hasR then box (lv - rv) else box DBNull.Value
    table.Rows.Add(k, leftObj, rightObj, deltaObj) |> ignore
  let gear = compareGearTable leftCol rightColUnique left right.Api
  {|
    Left = leftCol
    Right = rightColUnique
    RightPulledAt = right.PulledAt
    RightPath = right.Path
    Delta = sprintf "%s − %s" leftCol rightColUnique
    Stats = table
    Gear = gear
  |}

/// Prompt to compare this fetch vs another character snapshot already in characters/.
let promptStatComparison (fetchedName: string) (fetched: ApiCharacter) =
  let others = listLatestSnapshotsExclude fetchedName
  if others.Length = 0 then
    printfn "Stat compare: no other character snapshots in %s yet." Paths.charactersDir
    None
  else
    let labels =
      others
      |> Array.map (fun s ->
          let lvl = string s.Api.level
          let cls = if isValueString s.Api.className then s.Api.className else "?"
          sprintf "%s (L%s %s)" s.CharacterName lvl cls)
    let skip = "(skip comparison)"
    let opts = Array.append [| skip |] labels
    let picked = Util.ReadLine("Compare stats vs character in characters/ folder", skip, opts)
    if isNullUnsafe picked || picked = skip || String.IsNullOrWhiteSpace picked then
      printfn "Stat compare: skipped."
      None
    else
      match labels |> Array.tryFindIndex (fun l -> l = picked) with
      | Some i ->
          let other = others.[i]
          printfn "Comparing %s vs %s (%s)" fetchedName other.CharacterName other.Path
          Some (compareStats fetchedName fetched other)
      | None ->
          match
            others
            |> Array.tryFind (fun s ->
                picked.StartsWith(s.CharacterName, StringComparison.OrdinalIgnoreCase))
          with
          | Some other ->
              printfn "Comparing %s vs %s (%s)" fetchedName other.CharacterName other.Path
              Some (compareStats fetchedName fetched other)
          | None ->
              printfn "Stat compare: could not match selection '%s'." picked
              None

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

let comparison = promptStatComparison characterName envelope.Api

{|
  SavedPath = path
  PulledAt = envelope.PulledAt
  CharacterName = envelope.CharacterName
  RequestUrl = envelope.RequestUrl
  Response = envelope.Response
  TrackerSync = sync
  StatComparison = comparison
|}
|> Dump
|> ignore
