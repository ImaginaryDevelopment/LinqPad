<Query Kind="FSharpProgram">
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// IdleQuest character tracker — Classic/Kunark/Velious (no AA).
// Data: eq_characters.json, eq_events.json, item-cache.json (same folder as this script).
// Tracks equipped gear + inventory bag (spare gear / quest / epic items).
// Item lookups use a local idlequest-content clone or GitHub raw shards.
// Content-root preference: Util.Cache + password; GitHub cancel cooldown ~1 day.
// UI: one DumpContainer (PromptUi) for current prompt/options + last result; Util.ReadLine suggestions (no WinForms).

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

module Paths =
  let dataDir =
    let q = Util.CurrentQueryPath
    if String.IsNullOrWhiteSpace q then Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "LINQPad Queries", "idlequest")
    else Path.GetDirectoryName q
  let charactersPath = Path.Combine(dataDir, "eq_characters.json")
  let eventsPath = Path.Combine(dataDir, "eq_events.json")
  let itemCachePath = Path.Combine(dataDir, "item-cache.json")

let jsonOpts =
  let o = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
  o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
  o.Converters.Add(JsonStringEnumConverter())
  o

// Item-cache writes omit default zeros/nulls (slim JSON).
let itemCacheJsonOpts =
  let o = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
  o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingDefault
  o.Converters.Add(JsonStringEnumConverter())
  o

let inline ser value = JsonSerializer.Serialize(value, jsonOpts)
let inline serItemCache value = JsonSerializer.Serialize(value, itemCacheJsonOpts)
let inline deser<'T> (text: string) = JsonSerializer.Deserialize<'T>(text, jsonOpts)

/// Null check without requiring 'T : null (F# isNull constraint workaround).
let isNullUnsafe value = Object.Equals(value, null)

/// Single live panel: last result + current options (prompt text lives on Util.ReadLine).
module PromptUi =
  let dc = DumpContainer(Content = "(ready)")
  let mutable options: string[] = Array.empty
  let mutable resultTitle = "(none yet)"
  let mutable resultData: obj = null

  let refresh () =
    let optionsPanel: obj =
      let rows =
        if options.Length = 0 then [| {| N = 0; Option = "(none)" |} |]
        else options |> Array.mapi (fun i o -> {| N = i + 1; Option = o |})
      box {| Options = rows |}
    let resultPanel: obj = box {| LastResult = (resultTitle, resultData) |}
    // Options | LastResult as siblings in one horizontal run (not nested fields).
    dc.Content <- Util.HorizontalRun(true, [ optionsPanel; resultPanel ])

  /// title is only for the ReadLine prompt (caller); not shown here — avoids duplicating Util.ReadLine's label.
  let showOptions (_title: string) (opts: string[]) =
    options <- if isNullUnsafe opts then Array.empty else opts
    refresh ()

  let showResult (title: string) (data: obj) =
    resultTitle <- title
    resultData <- data
    refresh ()

let showOptions title options = PromptUi.showOptions title options
let showResult title data = PromptUi.showResult title data

// --- EQEmu slot bitmasks ---
// Display / entry order (no charm on this server). Bit values are EQEmu masks.
// Unlisted keepers (ears/neck/back) sit after range; ammo always last.
let slotBits: (string * int) list =
  [
    "primary", 8192
    "secondary", 16384
    "chest", 131072
    "waist", 1048576
    "legs", 262144
    "arms", 128
    "hands", 4096 // gauntlets
    "head", 4
    "shoulders", 64
    "face", 8
    "feet", 524288
    "wrist1", 512
    "wrist2", 1024
    "fingers1", 32768
    "fingers2", 65536
    "range", 2048
    "ear1", 2
    "ear2", 16
    "neck", 32
    "back", 256
    "ammo", 2097152
  ]

let slotNames = slotBits |> List.map fst |> Array.ofList
let slotBitMap = dict slotBits

let raceBits =
  dict [
    "Human", 1; "Barbarian", 2; "Erudite", 4; "Wood Elf", 8; "High Elf", 16
    "Dark Elf", 32; "Half Elf", 64; "Dwarf", 128; "Troll", 256; "Ogre", 512
    "Halfling", 1024; "Gnome", 2048; "Iksar", 4096; "Vah Shir", 8192; "Froglok", 16384
  ]

let classBits =
  dict [
    "Warrior", 1; "Cleric", 2; "Paladin", 4; "Ranger", 8; "Shadow Knight", 16
    "Druid", 32; "Monk", 64; "Bard", 128; "Rogue", 256; "Shaman", 512
    "Necromancer", 1024; "Wizard", 2048; "Magician", 4096; "Enchanter", 8192
    "Beastlord", 16384; "Berserker", 32768
  ]

let raceNames = raceBits.Keys |> Seq.sort |> Array.ofSeq
let classNames = classBits.Keys |> Seq.sort |> Array.ofSeq

type Gear = Dictionary<string, Nullable<int>>

/// Bag / bank extras — spare gear, quest turn-ins, epic pieces, etc.
[<CLIMutable>]
type InventoryEntry = {
  mutable ItemId: int
  /// gear | quest | epic | other
  mutable Tag: string
  mutable Note: string
  /// Stack size; null/absent means 1.
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
  /// When set, replaces the entire inventory bag (full baselines + hand-edit sync).
  mutable InventorySet: ResizeArray<InventoryEntry>
  /// Delta: append / stack these entries.
  mutable InventoryAdd: ResizeArray<InventoryEntry>
  /// Delta: remove one stack unit per item id (first match).
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
type CachedItem = {
  mutable Id: int
  mutable Name: string
  mutable Ac: int
  mutable Hp: int
  mutable Mana: int
  mutable Damage: int
  mutable Delay: int
  mutable Astr: int
  mutable Asta: int
  mutable Aagi: int
  mutable Adex: int
  mutable Awis: int
  mutable Aint: int
  mutable Acha: int
  mutable Attack: int
  mutable Mr: int
  mutable Fr: int
  mutable Cr: int
  mutable Pr: int
  mutable Dr: int
  mutable Slots: int
  mutable Itemtype: int
  mutable Classes: int
  mutable Races: int
  mutable Reqlevel: int
  /// zone.id values from idlequest-content (item → loot → npc → spawn → zone).
  mutable ZoneIds: int[]
}

[<CLIMutable>]
type ItemCacheFile = {
  mutable Items: Dictionary<string, CachedItem>
}

let inventoryTags = [| "gear"; "quest"; "epic"; "other" |]

let emptyGear () =
  let g = Gear()
  for name in slotNames do
    g.[name] <- Nullable()
  g

let emptyInventory () = ResizeArray<InventoryEntry>()

let entryQty (e: InventoryEntry) =
  if isNullUnsafe e then 1
  elif e.Qty.HasValue then max 1 e.Qty.Value
  else 1

let cloneInventoryEntry (e: InventoryEntry) : InventoryEntry =
  {
    ItemId = e.ItemId
    Tag = if isNullUnsafe e.Tag then "other" else e.Tag
    Note = if isNullUnsafe e.Note then "" else e.Note
    Qty = if e.Qty.HasValue then Nullable<int>(entryQty e) else Nullable()
  }

let cloneInventory (src: ResizeArray<InventoryEntry>) =
  let dst = emptyInventory ()
  if not (isNullUnsafe src) then
    for e in src do dst.Add(cloneInventoryEntry e)
  dst

let ensureInventory (sheet: Sheet) =
  if isNullUnsafe sheet.Inventory then sheet.Inventory <- emptyInventory ()

let inventoryFingerprint (inv: ResizeArray<InventoryEntry>) =
  if isNullUnsafe inv || inv.Count = 0 then ""
  else
    inv
    |> Seq.map (fun e -> $"{e.ItemId}\t{e.Tag}\t{e.Note}\t{entryQty e}")
    |> String.concat "\n"

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

let cloneGear (src: Gear) =
  let g = emptyGear ()
  if not (isNullUnsafe src) then
    for KeyValue(k, v) in src do
      if g.ContainsKey k then g.[k] <- v
  g

let cloneSheet (s: Sheet) : Sheet =
  {
    Level = s.Level
    Hp = s.Hp; Mana = s.Mana; Ac = s.Ac; Atk = s.Atk
    Str = s.Str; Sta = s.Sta; Agi = s.Agi; Dex = s.Dex
    Wis = s.Wis; Int = s.Int; Cha = s.Cha
    Mr = s.Mr; Fr = s.Fr; Cr = s.Cr; Pr = s.Pr; Dr = s.Dr
    Gear = cloneGear s.Gear
    Inventory = cloneInventory s.Inventory
  }

let utcNow () = DateTimeOffset.UtcNow.ToString("o")

let nInt (v: int) = Nullable<int>(v)
let hasN (v: Nullable<'T>) = v.HasValue
let getN (v: Nullable<'T>) = v.Value

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

let removeInventoryItemId (inv: ResizeArray<InventoryEntry>) (itemId: int) =
  let idx =
    inv
    |> Seq.tryFindIndex (fun e -> e.ItemId = itemId)
  match idx with
  | None -> false
  | Some i ->
      let e = inv.[i]
      let q = entryQty e
      if q <= 1 then inv.RemoveAt i
      else e.Qty <- Nullable<int>(q - 1)
      true

let addInventoryEntry (inv: ResizeArray<InventoryEntry>) (entry: InventoryEntry) =
  let tag = if isNullUnsafe entry.Tag || String.IsNullOrWhiteSpace entry.Tag then "other" else entry.Tag
  let note = if isNullUnsafe entry.Note then "" else entry.Note
  let addQty = entryQty entry
  match
    inv
    |> Seq.tryFindIndex (fun e ->
        e.ItemId = entry.ItemId
        && String.Equals(e.Tag, tag, StringComparison.OrdinalIgnoreCase)
        && String.Equals((if isNullUnsafe e.Note then "" else e.Note), note, StringComparison.Ordinal))
  with
  | Some i ->
      let e = inv.[i]
      e.Qty <- Nullable<int>(entryQty e + addQty)
  | None ->
      inv.Add
        {
          ItemId = entry.ItemId
          Tag = tag
          Note = note
          Qty = if addQty = 1 then Nullable() else Nullable<int>(addQty)
        }

let applyEvent (sheet: Sheet) (e: EventRecord) =
  let applyInventory (src: EventRecord) =
    ensureInventory sheet
    if not (isNullUnsafe src.InventorySet) then
      sheet.Inventory <- cloneInventory src.InventorySet
    else
      if not (isNullUnsafe src.InventoryRemove) then
        for itemId in src.InventoryRemove do
          removeInventoryItemId sheet.Inventory itemId |> ignore
      if not (isNullUnsafe src.InventoryAdd) then
        for entry in src.InventoryAdd do
          addInventoryEntry sheet.Inventory entry

  let applyFields (src: EventRecord) =
    if hasN src.Level then sheet.Level <- src.Level
    if hasN src.Hp then sheet.Hp <- src.Hp
    if hasN src.Mana then sheet.Mana <- src.Mana
    if hasN src.Ac then sheet.Ac <- src.Ac
    if hasN src.Atk then sheet.Atk <- src.Atk
    if hasN src.Str then sheet.Str <- src.Str
    if hasN src.Sta then sheet.Sta <- src.Sta
    if hasN src.Agi then sheet.Agi <- src.Agi
    if hasN src.Dex then sheet.Dex <- src.Dex
    if hasN src.Wis then sheet.Wis <- src.Wis
    if hasN src.Int then sheet.Int <- src.Int
    if hasN src.Cha then sheet.Cha <- src.Cha
    if hasN src.Mr then sheet.Mr <- src.Mr
    if hasN src.Fr then sheet.Fr <- src.Fr
    if hasN src.Cr then sheet.Cr <- src.Cr
    if hasN src.Pr then sheet.Pr <- src.Pr
    if hasN src.Dr then sheet.Dr <- src.Dr
    if not (isNullUnsafe src.Gear) then
      for KeyValue(slot, itemId) in src.Gear do
        if sheet.Gear.ContainsKey slot then
          sheet.Gear.[slot] <- itemId
    applyInventory src

  match (if isNullUnsafe e.Kind then "delta" else e.Kind).ToLowerInvariant() with
  | "full" ->
      let cleared = emptySheet ()
      sheet.Level <- cleared.Level
      sheet.Hp <- cleared.Hp; sheet.Mana <- cleared.Mana; sheet.Ac <- cleared.Ac; sheet.Atk <- cleared.Atk
      sheet.Str <- cleared.Str; sheet.Sta <- cleared.Sta; sheet.Agi <- cleared.Agi; sheet.Dex <- cleared.Dex
      sheet.Wis <- cleared.Wis; sheet.Int <- cleared.Int; sheet.Cha <- cleared.Cha
      sheet.Mr <- cleared.Mr; sheet.Fr <- cleared.Fr; sheet.Cr <- cleared.Cr; sheet.Pr <- cleared.Pr; sheet.Dr <- cleared.Dr
      if isNullUnsafe sheet.Gear then sheet.Gear <- emptyGear ()
      for name in slotNames do sheet.Gear.[name] <- Nullable()
      sheet.Inventory <- emptyInventory ()
      applyFields e
  | _ ->
      applyFields e

let foldEvents (events: seq<EventRecord>) =
  let sheet = emptySheet ()
  for e in events do applyEvent sheet e
  sheet

let foldThrough (events: EventRecord seq) (take: int) =
  foldEvents (events |> Seq.truncate take)

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

let emptyCharactersFile () : CharactersFile = { Characters = ResizeArray() }
let emptyEventsFile () : EventsFile = { Characters = ResizeArray() }
let emptyItemCacheFile () : ItemCacheFile = { Items = Dictionary<string, CachedItem>() }

let nEq (a: Nullable<int>) (b: Nullable<int>) =
  if a.HasValue <> b.HasValue then false
  elif not a.HasValue then true
  else a.Value = b.Value

let nFmt (v: Nullable<int>) = if v.HasValue then string v.Value else "null"

let diffSheets (expected: Sheet) (actual: Sheet) =
  let changes = ResizeArray<string * string * string>()
  for name, getter, _ in sheetFieldGetters do
    let a = getter expected
    let b = getter actual
    if not (nEq a b) then
      changes.Add(name, nFmt a, nFmt b)
  for slot in slotNames do
    let a = if expected.Gear.ContainsKey slot then expected.Gear.[slot] else Nullable()
    let b = if actual.Gear.ContainsKey slot then actual.Gear.[slot] else Nullable()
    if not (nEq a b) then
      changes.Add("gear." + slot, nFmt a, nFmt b)
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
    if not (nEq a b) then
      e.Gear.[slot] <- b
  ensureInventory expected
  ensureInventory actual
  if inventoryFingerprint expected.Inventory <> inventoryFingerprint actual.Inventory then
    e.InventorySet <- cloneInventory actual.Inventory
  e

let ensureDir () =
  if not (Directory.Exists Paths.dataDir) then
    Directory.CreateDirectory Paths.dataDir |> ignore

let loadOrSeed<'T> path (seed: unit -> 'T) =
  ensureDir ()
  if not (File.Exists path) then
    let s = seed ()
    File.WriteAllText(path, ser s)
    s
  else
    let text = File.ReadAllText path
    if String.IsNullOrWhiteSpace text then seed ()
    else
      // isNullUnsafe avoids requiring 'T : null.
      let parsed = deser<'T> text
      if isNullUnsafe parsed then seed () else parsed

let saveEvents (events: EventsFile) =
  File.WriteAllText(Paths.eventsPath, ser events)

let saveCharacters (chars: CharactersFile) =
  File.WriteAllText(Paths.charactersPath, ser chars)

let saveItemCache (cache: ItemCacheFile) =
  File.WriteAllText(Paths.itemCachePath, serItemCache cache)

let loadAll () =
  let chars = loadOrSeed Paths.charactersPath emptyCharactersFile
  let events = loadOrSeed Paths.eventsPath emptyEventsFile
  let cache = loadOrSeed Paths.itemCachePath emptyItemCacheFile
  if isNullUnsafe chars.Characters then chars.Characters <- ResizeArray()
  if isNullUnsafe events.Characters then events.Characters <- ResizeArray()
  if isNullUnsafe cache.Items then cache.Items <- Dictionary<string, CachedItem>()
  chars, events, cache

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

let refreshCurrent (ch: Character) (events: EventsFile) =
  let evs = eventsFor events ch.Id
  ch.Current <- foldEvents evs
  if evs.Count > 0 then
    ch.CurrentAt <- evs.[evs.Count - 1].At
  else
    ch.CurrentAt <- utcNow ()

let appendEvent (events: EventsFile) (characterId: string) (e: EventRecord) =
  let evs = eventsFor events characterId
  evs.Add e

let persistAfterMutation (chars: CharactersFile) (events: EventsFile) =
  saveEvents events
  saveCharacters chars

// --- Item cache / idlequest-content ---
// Items export has no PRIMARY key, so shards are hashed from the full row.
// Lookup scans shards 0-f (local is fine; GitHub is slower — prefer a local clone).

// Content-root Util.Cache pref (shared shape with item-lookup.linq when present).
// Remember local path in Util.Cache + password; GitHub cancel is cache-only for ~1 day.
// Do NOT reintroduce Util.SetPassword(..., "__github__") as a permanent lockout.
let githubSentinel = "__github__"
let contentRootPrefKey = "idlequest-content-root-pref"
let githubPrefCooldown = TimeSpan.FromDays 1.0
let mutable contentRootSlot : string * DateTime = "", DateTime.MinValue

type ContentRootDecision =
  | UseLocal of string
  | UseGithub
  | Ask

let isValidContentRoot (root: string) =
  not (String.IsNullOrWhiteSpace root)
  && root <> githubSentinel
  && Directory.Exists(Path.Combine(root, "data", "items"))

let clearLegacyGithubPassword () =
  try
    if Util.GetPassword("idlequestContentRoot") = githubSentinel then
      Util.SetPassword("idlequestContentRoot", "")
      printfn "Cleared permanent GitHub-only password so a local clone can be chosen again."
  with _ -> ()

let readContentRootPref () =
  contentRootSlot <- "", DateTime.MinValue
  Util.Cache<string * DateTime>((fun () -> contentRootSlot), key = contentRootPrefKey, forceRefresh = false)

let writeContentRootPref (root: string) =
  contentRootSlot <- root, DateTime.UtcNow
  Util.Cache<string * DateTime>((fun () -> contentRootSlot), key = contentRootPrefKey, forceRefresh = true) |> ignore

let passwordContentRoot () =
  clearLegacyGithubPassword ()
  let cached = Util.GetPassword("idlequestContentRoot")
  if isValidContentRoot cached then cached else null

let rememberLocalRoot (root: string) =
  writeContentRootPref root
  Util.SetPassword("idlequestContentRoot", root)

let rememberGithubRoot () =
  writeContentRootPref githubSentinel
  clearLegacyGithubPassword ()

let promptContentRoot (initial: string) =
  let defaultPath = if isNullUnsafe initial then "" else initial
  let suggestions =
    if isValidContentRoot defaultPath then [| defaultPath |]
    else Array.empty
  showOptions "idlequest-content root (blank = GitHub raw for now)" suggestions
  let entered =
    if suggestions.Length = 0 then Util.ReadLine("Path to idlequest-content (blank = GitHub)", defaultPath)
    else Util.ReadLine("Path to idlequest-content (blank = GitHub)", defaultPath, suggestions)
  let path = if isNullUnsafe entered then "" else entered.Trim()
  if isValidContentRoot path then
    rememberLocalRoot path
    UseLocal path
  else
    rememberGithubRoot ()
    printfn "Using GitHub raw for now. Will ask again after a day."
    UseGithub

let decideContentRoot () =
  let fromPassword = passwordContentRoot ()
  if not (isNullUnsafe fromPassword) then
    let prefRoot, _ = readContentRootPref ()
    if prefRoot <> fromPassword then writeContentRootPref fromPassword
    UseLocal fromPassword
  else
    let prefRoot, decidedAt = readContentRootPref ()
    if isValidContentRoot prefRoot then
      Util.SetPassword("idlequestContentRoot", prefRoot)
      UseLocal prefRoot
    elif prefRoot = githubSentinel && decidedAt > DateTime.MinValue
         && DateTime.UtcNow - decidedAt.ToUniversalTime() < githubPrefCooldown then
      UseGithub
    else
      Ask

let getContentRoot () =
  match decideContentRoot () with
  | UseLocal root -> root
  | UseGithub -> null
  | Ask ->
      let hint =
        let prefRoot, _ = readContentRootPref ()
        if isValidContentRoot prefRoot then prefRoot
        else
          let pw = Util.GetPassword("idlequestContentRoot")
          if not (String.IsNullOrWhiteSpace pw) && pw <> githubSentinel then pw else ""
      match promptContentRoot hint with
      | UseLocal root -> root
      | UseGithub | Ask -> null

let readShardLines (contentRoot: string) (shard: string) =
  if not (String.IsNullOrWhiteSpace contentRoot) then
    let path = Path.Combine(contentRoot, "data", "items", shard + ".ndjson")
    if File.Exists path then File.ReadLines path
    else Seq.empty
  else
    let url = $"https://raw.githubusercontent.com/brynnb/idlequest-content/main/data/items/{shard}.ndjson"
    use client = new HttpClient()
    client.Timeout <- TimeSpan.FromMinutes 2.0
    let text = client.GetStringAsync(url).GetAwaiter().GetResult()
    text.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toSeq

let tryGetProperty (el: JsonElement) (name: string) =
  // Avoid JsonElement.TryGetProperty overload ambiguity in F#.
  el.EnumerateObject()
  |> Seq.tryPick (fun p -> if p.NameEquals name then Some p.Value else None)

let projectItem (doc: JsonElement) : CachedItem option =
  try
    let getInt name =
      match tryGetProperty doc name with
      | Some p when p.ValueKind = JsonValueKind.Number -> p.GetInt32()
      | _ -> 0
    let getStr name =
      match tryGetProperty doc name with
      | Some p when p.ValueKind = JsonValueKind.String -> p.GetString()
      | _ -> ""
    let id = getInt "id"
    Some {
      Id = id
      Name = getStr "Name"
      Ac = getInt "ac"; Hp = getInt "hp"; Mana = getInt "mana"
      Damage = getInt "damage"; Delay = getInt "delay"
      Astr = getInt "astr"; Asta = getInt "asta"; Aagi = getInt "aagi"; Adex = getInt "adex"
      Awis = getInt "awis"; Aint = getInt "aint"; Acha = getInt "acha"; Attack = getInt "attack"
      Mr = getInt "mr"; Fr = getInt "fr"; Cr = getInt "cr"; Pr = getInt "pr"; Dr = getInt "dr"
      Slots = getInt "slots"; Itemtype = getInt "itemtype"
      Classes = getInt "classes"; Races = getInt "races"; Reqlevel = getInt "reqlevel"
      ZoneIds = null
    }
  with _ -> None

let findItemInContent (contentRoot: string) (itemId: int) =
  let needle = $"\"id\":{itemId}"
  let shards = [| '0'..'9' |] |> Array.map string |> Array.append ([| 'a'..'f' |] |> Array.map string)
  shards
  |> Seq.tryPick (fun shard ->
      try
        readShardLines contentRoot shard
        |> Seq.tryPick (fun line ->
            if line.Contains needle then
              try
                use doc = JsonDocument.Parse line
                let root = doc.RootElement
                match tryGetProperty root "id" with
                | Some idEl when idEl.ValueKind = JsonValueKind.Number && idEl.GetInt32() = itemId ->
                    projectItem root
                | _ -> None
              with _ -> None
            else None)
      with ex ->
        printfn "WARN: shard %s read failed: %s" shard ex.Message
        None)

let resolveItem (cache: ItemCacheFile) (contentRoot: string) (itemId: int) (expectedName: string option) =
  let key = string itemId
  let mutable existing = Unchecked.defaultof<CachedItem>
  if cache.Items.TryGetValue(key, &existing) then
    match expectedName with
    | Some n when not (String.Equals(n, existing.Name, StringComparison.OrdinalIgnoreCase)) ->
        printfn "WARN: item %d cache name '%s' != expected '%s' — refreshing" itemId existing.Name n
        match findItemInContent contentRoot itemId with
        | Some item ->
            cache.Items.[key] <- item
            saveItemCache cache
            if not (String.Equals(item.Name, n, StringComparison.OrdinalIgnoreCase)) then
              printfn "WARN: content name '%s' still differs from expected '%s'" item.Name n
            item
        | None ->
            printfn "WARN: could not refresh item %d from content; using cache" itemId
            existing
    | _ -> existing
  else
    if String.IsNullOrWhiteSpace contentRoot then
      printfn "Looking up item %d via GitHub shards (slow). Prefer selecting a local idlequest-content clone." itemId
    match findItemInContent contentRoot itemId with
    | Some item ->
        cache.Items.[key] <- item
        saveItemCache cache
        match expectedName with
        | Some n when not (String.Equals(item.Name, n, StringComparison.OrdinalIgnoreCase)) ->
            printfn "WARN: item %d name checksum '%s' != '%s'" itemId item.Name n
        | _ -> ()
        item
    | None ->
        invalidOp $"Item id {itemId} not found in idlequest-content items data"

let itemLabel (item: CachedItem) = $"{item.Id} — {item.Name}"

let tryParseItemSelection (text: string) =
  if String.IsNullOrWhiteSpace text then None
  else
    let t = text.Trim()
    let dash = t.IndexOf("—")
    let head = if dash > 0 then t.Substring(0, dash).Trim() else t
    match Int32.TryParse head with
    | true, id -> Some id
    | _ ->
        match Int32.TryParse t with
        | true, id -> Some id
        | _ -> None

// --- LINQPad prompts (Util.ReadLine + DumpContainers) ---
let promptText (title: string) (label: string) (suggestions: string[]) (defaultText: string) =
  let opts = if isNullUnsafe suggestions then Array.empty else suggestions
  let prompt =
    if String.IsNullOrWhiteSpace label then title
    else $"{title} — {label} (type cancel to abort)"
  showOptions title opts
  let raw =
    if opts.Length = 0 then Util.ReadLine(prompt, defaultText)
    else Util.ReadLine(prompt, defaultText, opts)
  let text = if isNullUnsafe raw then "" else raw.Trim()
  if String.Equals(text, "cancel", StringComparison.OrdinalIgnoreCase) then None
  else Some text

let promptChoice (title: string) (options: string[]) =
  if isNullUnsafe options || options.Length = 0 then None
  else
    showOptions title options
    match promptText title "number, text, or suggestion" options "" with
    | None -> None
    | Some t when String.IsNullOrWhiteSpace t -> None
    | Some t ->
        match Int32.TryParse t with
        | true, n when n >= 1 && n <= options.Length -> Some options.[n - 1]
        | _ ->
            options
            |> Array.tryFind (fun o -> String.Equals(o, t, StringComparison.OrdinalIgnoreCase))
            |> Option.orElseWith (fun () ->
                options
                |> Array.tryFind (fun o -> o.IndexOf(t, StringComparison.OrdinalIgnoreCase) >= 0))

let promptMenu (title: string) (options: (string * string) list) =
  let labels = options |> List.map (fun (k, v) -> $"{k} — {v}") |> Array.ofList
  match promptChoice title labels with
  | None -> None
  | Some picked ->
      options
      |> List.tryFind (fun (k, _) -> picked.StartsWith(k + " —") || picked = k)
      |> Option.map fst

// --- Hand-edit sync ---
let syncHandEdits (chars: CharactersFile) (events: EventsFile) =
  let mutable any = false
  for ch in chars.Characters do
    if isNullUnsafe ch.Current then ch.Current <- emptySheet ()
    if isNullUnsafe ch.Current.Gear then ch.Current.Gear <- emptyGear ()
    if ch.Current.Gear.ContainsKey "charm" then ch.Current.Gear.Remove "charm" |> ignore
    for slot in slotNames do
      if not (ch.Current.Gear.ContainsKey slot) then ch.Current.Gear.[slot] <- Nullable()
    ensureInventory ch.Current

    let evs = eventsFor events ch.Id
    if evs.Count = 0 then
      if not (isNullUnsafe ch.Current) then
        printfn "Hand-edit sync: %s has current sheet but no events — writing full baseline" ch.Name
        let at = utcNow ()
        appendEvent events ch.Id (eventFromFullSheet at "hand-edit" ch.Current)
        ch.CurrentAt <- at
        any <- true
    else
      let expected = foldEvents evs
      let changes = diffSheets expected ch.Current
      if changes.Count > 0 then
        any <- true
        printfn "Hand-edit sync: %s (%s)" ch.Name ch.Id
        for field, oldV, newV in changes do
          printfn "  %s: %s -> %s" field oldV newV
        let at = utcNow ()
        let delta = buildDeltaFromDiff at "hand-edit" expected ch.Current
        appendEvent events ch.Id delta
        ch.CurrentAt <- at
  if any then
    persistAfterMutation chars events
    printfn "Hand-edit sync: events updated."
  else
    printfn "Hand-edit sync: no hand-edits detected."

// --- Display helpers ---
let formatNullable (v: Nullable<int>) = if v.HasValue then string v.Value else "-"

let formatAt (at: string) =
  if String.IsNullOrWhiteSpace at then "-"
  else
    match DateTimeOffset.TryParse(at) with
    | true, dto -> dto.ToLocalTime().ToString("yyyy-MM-dd HH:mm")
    | _ -> at

/// Display form used across status/progress: Name(id) or (empty).
let formatItemRef (cache: ItemCacheFile) (itemId: Nullable<int>) =
  if not itemId.HasValue then "(empty)"
  else
    let id = itemId.Value
    let mutable item = Unchecked.defaultof<CachedItem>
    if cache.Items.TryGetValue(string id, &item) then $"{item.Name}({id})"
    else $"?({id})"

let inventoryStatusRows (cache: ItemCacheFile) (ch: Character) =
  ensureInventory ch.Current
  ch.Current.Inventory
  |> Seq.map (fun e ->
      let qty = entryQty e
      let item = formatItemRef cache (nInt e.ItemId)
      let itemTxt = if qty > 1 then $"{item} x{qty}" else item
      {|
        Slot = $"bag:{e.Tag}"
        Item = itemTxt
        Note = if isNullUnsafe e.Note then "" else e.Note
      |})
  |> Seq.toArray

let dumpSheet (cache: ItemCacheFile) (ch: Character) =
  let s = ch.Current
  let gearRows =
    slotNames
    |> Array.map (fun slot ->
        let id = if not (isNullUnsafe s.Gear) && s.Gear.ContainsKey slot then s.Gear.[slot] else Nullable()
        {| Slot = slot; Item = formatItemRef cache id; Note = "" |})
  let bagRows = inventoryStatusRows cache ch
  let rows = Array.append gearRows bagRows
  showResult $"{ch.Name} — {ch.Race} {ch.Class} — updated {formatAt ch.CurrentAt}" rows

let dumpAllStatus (cache: ItemCacheFile) (chars: CharactersFile) =
  let rows =
    chars.Characters
    |> Seq.collect (fun ch ->
        let s = ch.Current
        let gear =
          slotNames
          |> Seq.choose (fun slot ->
              let id = if not (isNullUnsafe s.Gear) && s.Gear.ContainsKey slot then s.Gear.[slot] else Nullable()
              if not id.HasValue then None
              else
                Some
                  {|
                    Character = ch.Name
                    Race = ch.Race
                    Class = ch.Class
                    At = formatAt ch.CurrentAt
                    Slot = slot
                    Item = formatItemRef cache id
                    Note = ""
                  |})
        let bag =
          inventoryStatusRows cache ch
          |> Seq.map (fun r ->
              {|
                Character = ch.Name
                Race = ch.Race
                Class = ch.Class
                At = formatAt ch.CurrentAt
                Slot = r.Slot
                Item = r.Item
                Note = r.Note
              |})
        Seq.append gear bag)
    |> Seq.toArray
  showResult $"Status — ALL ({rows.Length} gear/bag rows)" rows

let pickCharacter (chars: CharactersFile) =
  if chars.Characters.Count = 0 then
    showResult "Select character" "No characters yet."
    None
  else
    let opts = chars.Characters |> Seq.map (fun c -> $"{c.Name} ({c.Race} {c.Class})") |> Array.ofSeq
    match promptChoice "Select character" opts with
    | None -> None
    | Some picked ->
        chars.Characters
        |> Seq.tryFind (fun c -> picked.StartsWith(c.Name))

let filterCachedItems (cache: ItemCacheFile) (race: string) (cls: string) (slot: string option) =
  let mutable raceBit = 0
  let mutable classBit = 0
  let hasRace = raceBits.TryGetValue(race, &raceBit)
  let hasClass = classBits.TryGetValue(cls, &classBit)
  let mutable slotBit = 0
  let hasSlot =
    match slot with
    | Some s -> slotBitMap.TryGetValue(s, &slotBit)
    | None -> false
  cache.Items.Values
  |> Seq.filter (fun it ->
      let raceOk = (not hasRace) || it.Races = 0 || it.Races = 65535 || (it.Races &&& raceBit) <> 0
      let classOk = (not hasClass) || it.Classes = 0 || it.Classes = 65535 || (it.Classes &&& classBit) <> 0
      let slotOk = (not hasSlot) || (it.Slots &&& slotBit) <> 0
      raceOk && classOk && slotOk)
  |> Seq.map itemLabel
  |> Seq.sort
  |> Array.ofSeq

let ensureContentRoot (lazyRoot: string option ref) =
  match lazyRoot.Value with
  | Some r -> r
  | None ->
      let r = getContentRoot ()
      lazyRoot.Value <- Some r
      r

// --- Modes ---
let modeStatus (chars: CharactersFile) (cache: ItemCacheFile) =
  let opts = Array.append [| "ALL characters" |] (chars.Characters |> Seq.map (fun c -> c.Name) |> Array.ofSeq)
  match promptChoice "Status — select target" opts with
  | None -> ()
  | Some "ALL characters" -> dumpAllStatus cache chars
  | Some name ->
      match chars.Characters |> Seq.tryFind (fun c -> c.Name = name) with
      | Some ch -> dumpSheet cache ch
      | None -> showResult "Status" $"Not found: {name}"

let modeAddCharacter (chars: CharactersFile) (events: EventsFile) : Character option =
  match promptText "Add character" "Name" Array.empty "" with
  | None -> None
  | Some name when String.IsNullOrWhiteSpace name ->
      printfn "Cancelled."
      None
  | Some name ->
      match promptChoice "Race" raceNames with
      | None -> None
      | Some race ->
          match promptChoice "Class" classNames with
          | None -> None
          | Some cls ->
              let id = Guid.NewGuid().ToString("N")
              let sheet = emptySheet ()
              sheet.Level <- nInt 1
              match promptText "Starting level" "Level" Array.empty "1" with
              | Some lvlText ->
                  match Int32.TryParse lvlText with
                  | true, lvl -> sheet.Level <- nInt lvl
                  | _ -> ()
              | None -> ()
              let at = utcNow ()
              let ch =
                {
                  Id = id
                  Name = name
                  Race = race
                  Class = cls
                  Current = sheet
                  CurrentAt = at
                }
              chars.Characters.Add ch
              appendEvent events id (eventFromFullSheet at "script" sheet)
              persistAfterMutation chars events
              showResult "Add character" $"Added {name} ({race} {cls})."
              match promptChoice "After create" [| "Enter gear now"; "Back to main menu" |] with
              | Some s when s.StartsWith("Enter gear", StringComparison.OrdinalIgnoreCase) -> Some ch
              | _ -> None

let applyGearChange (ch: Character) (events: EventsFile) (chars: CharactersFile) (cache: ItemCacheFile) (contentRoot: string option ref) (slot: string) (text: string) =
  if String.IsNullOrWhiteSpace text then
    let at = utcNow ()
    let delta = emptyDeltaEvent at "script"
    delta.Gear.[slot] <- Nullable()
    appendEvent events ch.Id delta
    refreshCurrent ch events
    persistAfterMutation chars events
    showResult $"Gear — {ch.Name}" $"Unequipped {slot}."
  else
    match tryParseItemSelection text with
    | None ->
        let hits =
          cache.Items.Values
          |> Seq.filter (fun it -> it.Name.IndexOf(text, StringComparison.OrdinalIgnoreCase) >= 0)
          |> Seq.map itemLabel
          |> Array.ofSeq
        match promptChoice "Matching items" hits with
        | None -> ()
        | Some pick ->
            match tryParseItemSelection pick with
            | Some itemId ->
                let root = ensureContentRoot contentRoot
                let expectedName =
                  let parts = pick.Split('—')
                  if parts.Length > 1 then Some (parts.[1].Trim()) else None
                let item = resolveItem cache root itemId expectedName
                let at = utcNow ()
                let delta = emptyDeltaEvent at "script"
                delta.Gear.[slot] <- nInt item.Id
                appendEvent events ch.Id delta
                refreshCurrent ch events
                persistAfterMutation chars events
                showResult $"Gear — {ch.Name}" $"{slot} -> {formatItemRef cache (nInt item.Id)}"
            | None -> showResult $"Gear — {ch.Name}" "Could not parse item."
    | Some itemId ->
        let root = ensureContentRoot contentRoot
        let item = resolveItem cache root itemId None
        let at = utcNow ()
        let delta = emptyDeltaEvent at "script"
        delta.Gear.[slot] <- nInt item.Id
        appendEvent events ch.Id delta
        refreshCurrent ch events
        persistAfterMutation chars events
        showResult $"Gear — {ch.Name}" $"{slot} -> {formatItemRef cache (nInt item.Id)}"

let promptOneGearSlot (ch: Character) (events: EventsFile) (chars: CharactersFile) (cache: ItemCacheFile) (contentRoot: string option ref) =
  match promptChoice $"Gear for {ch.Name} — slot" slotNames with
  | None -> ()
  | Some slot ->
      let suggestions = filterCachedItems cache ch.Race ch.Class (Some slot)
      match promptText $"Gear — {slot}" "Item id or 'id — name' (blank = unequip)" suggestions "" with
      | None -> ()
      | Some text -> applyGearChange ch events chars cache contentRoot slot text

let modeEnterGear (ch: Character) (events: EventsFile) (chars: CharactersFile) (cache: ItemCacheFile) (contentRoot: string option ref) =
  let rec loop () =
    match promptChoice $"Gear entry — {ch.Name}" [| "equip / change a slot"; "done — back to main menu" |] with
    | Some s when s.StartsWith("equip", StringComparison.OrdinalIgnoreCase) ->
        promptOneGearSlot ch events chars cache contentRoot
        loop ()
    | _ -> ()
  loop ()

let formatInventoryPick (cache: ItemCacheFile) (e: InventoryEntry) =
  let qty = entryQty e
  let note = if isNullUnsafe e.Note || String.IsNullOrWhiteSpace e.Note then "" else $" — {e.Note}"
  let qtyTxt = if qty > 1 then $" x{qty}" else ""
  $"[{e.Tag}] {formatItemRef cache (nInt e.ItemId)}{qtyTxt}{note}"

let showInventoryResult (cache: ItemCacheFile) (ch: Character) (title: string) =
  ensureInventory ch.Current
  showResult title (inventoryStatusRows cache ch)

let modeInventory (ch: Character) (events: EventsFile) (chars: CharactersFile) (cache: ItemCacheFile) (contentRoot: string option ref) =
  ensureInventory ch.Current
  let rec loop () =
    match
      promptChoice
        $"Inventory — {ch.Name}"
        [| "list bag"; "add item"; "remove item"; "done — back" |]
    with
    | None -> ()
    | Some s when s.StartsWith("done", StringComparison.OrdinalIgnoreCase) -> ()
    | Some s when s.StartsWith("list", StringComparison.OrdinalIgnoreCase) ->
        showInventoryResult cache ch $"Inventory — {ch.Name} ({ch.Current.Inventory.Count} stacks)"
        loop ()
    | Some s when s.StartsWith("add", StringComparison.OrdinalIgnoreCase) ->
        match promptChoice "Inventory tag" inventoryTags with
        | None -> loop ()
        | Some tag ->
            let suggestions =
              cache.Items.Values
              |> Seq.map itemLabel
              |> Seq.sort
              |> Array.ofSeq
            match promptText "Inventory add" "Item id or 'id — name'" suggestions "" with
            | None -> loop ()
            | Some text when String.IsNullOrWhiteSpace text -> loop ()
            | Some text ->
                let resolveId () =
                  match tryParseItemSelection text with
                  | Some id -> Some id
                  | None ->
                      let hits =
                        cache.Items.Values
                        |> Seq.filter (fun it -> it.Name.IndexOf(text, StringComparison.OrdinalIgnoreCase) >= 0)
                        |> Seq.map itemLabel
                        |> Array.ofSeq
                      match promptChoice "Matching items" hits with
                      | None -> None
                      | Some pick -> tryParseItemSelection pick
                match resolveId () with
                | None ->
                    showResult $"Inventory — {ch.Name}" "Could not resolve item."
                    loop ()
                | Some itemId ->
                    let root = ensureContentRoot contentRoot
                    try resolveItem cache root itemId None |> ignore with _ -> ()
                    let note =
                      match promptText "Note (optional)" "e.g. epic turn-in / spare BP" Array.empty "" with
                      | None -> ""
                      | Some n -> n.Trim()
                    let qty =
                      match promptText "Qty" "Stack size" Array.empty "1" with
                      | Some t ->
                          match Int32.TryParse t with
                          | true, q when q > 0 -> q
                          | _ -> 1
                      | None -> 1
                    let entry =
                      {
                        ItemId = itemId
                        Tag = tag
                        Note = note
                        Qty = if qty = 1 then Nullable() else nInt qty
                      }
                    let at = utcNow ()
                    let delta = emptyDeltaEvent at "script"
                    delta.InventoryAdd <- ResizeArray([ entry ])
                    appendEvent events ch.Id delta
                    refreshCurrent ch events
                    persistAfterMutation chars events
                    showInventoryResult cache ch $"Inventory — added {formatItemRef cache (nInt itemId)} [{tag}]"
                    loop ()
    | Some s when s.StartsWith("remove", StringComparison.OrdinalIgnoreCase) ->
        if ch.Current.Inventory.Count = 0 then
          showResult $"Inventory — {ch.Name}" "Bag is empty."
          loop ()
        else
          let picks =
            ch.Current.Inventory
            |> Seq.map (formatInventoryPick cache)
            |> Array.ofSeq
          match promptChoice "Remove which stack? (removes 1 qty)" picks with
          | None -> loop ()
          | Some pick ->
              let next = cloneInventory ch.Current.Inventory
              match next |> Seq.tryFindIndex (fun e -> formatInventoryPick cache e = pick) with
              | None -> loop ()
              | Some idx ->
                  let entry = next.[idx]
                  let removedId = entry.ItemId
                  let q = entryQty entry
                  if q <= 1 then next.RemoveAt idx
                  else entry.Qty <- Nullable<int>(q - 1)
                  let at = utcNow ()
                  let delta = emptyDeltaEvent at "script"
                  // Set (not Remove-by-id) so duplicate itemIds with different tags stay correct.
                  delta.InventorySet <- next
                  appendEvent events ch.Id delta
                  refreshCurrent ch events
                  persistAfterMutation chars events
                  showInventoryResult cache ch $"Inventory — removed 1× {formatItemRef cache (nInt removedId)}"
                  loop ()
    | _ -> loop ()
  loop ()

let modeUpdate (chars: CharactersFile) (events: EventsFile) (cache: ItemCacheFile) (contentRoot: string option ref) =
  match pickCharacter chars with
  | None -> ()
  | Some ch ->
      let kinds = [
        "gear", "Change one gear slot"
        "inventory", "Bag: spare gear / quest / epic items"
        "level", "Set level"
        "stats", "Set primary stats / AC / HP / Mana / ATK"
        "resists", "Set resists"
        "field", "Set one named field"
      ]
      match promptMenu "Update type" kinds with
      | None -> ()
      | Some "gear" -> promptOneGearSlot ch events chars cache contentRoot
      | Some "inventory" -> modeInventory ch events chars cache contentRoot
      | Some "level" ->
          match promptText "Level" "New level" Array.empty (formatNullable ch.Current.Level) with
          | Some t ->
              match Int32.TryParse t with
              | true, lvl ->
                  let at = utcNow ()
                  let delta = emptyDeltaEvent at "script"
                  delta.Level <- nInt lvl
                  appendEvent events ch.Id delta
                  refreshCurrent ch events
                  persistAfterMutation chars events
                  showResult $"Update — {ch.Name}" $"level -> {lvl}"
              | _ -> showResult $"Update — {ch.Name}" "Invalid number."
          | None -> ()
      | Some "stats" ->
          let fields = [| "hp"; "mana"; "ac"; "atk"; "str"; "sta"; "agi"; "dex"; "wis"; "int"; "cha" |]
          match promptChoice "Which stat?" fields with
          | Some field ->
              match promptText field $"New {field}" Array.empty "" with
              | Some t ->
                  match Int32.TryParse t with
                  | true, v ->
                      let at = utcNow ()
                      let delta = emptyDeltaEvent at "script"
                      match field with
                      | "hp" -> delta.Hp <- nInt v
                      | "mana" -> delta.Mana <- nInt v
                      | "ac" -> delta.Ac <- nInt v
                      | "atk" -> delta.Atk <- nInt v
                      | "str" -> delta.Str <- nInt v
                      | "sta" -> delta.Sta <- nInt v
                      | "agi" -> delta.Agi <- nInt v
                      | "dex" -> delta.Dex <- nInt v
                      | "wis" -> delta.Wis <- nInt v
                      | "int" -> delta.Int <- nInt v
                      | "cha" -> delta.Cha <- nInt v
                      | _ -> ()
                      appendEvent events ch.Id delta
                      refreshCurrent ch events
                      persistAfterMutation chars events
                      showResult $"Update — {ch.Name}" $"{field} -> {v}"
                  | _ -> showResult $"Update — {ch.Name}" "Invalid number."
              | None -> ()
          | None -> ()
      | Some "resists" ->
          let fields = [| "mr"; "fr"; "cr"; "pr"; "dr" |]
          match promptChoice "Which resist?" fields with
          | Some field ->
              match promptText field $"New {field}" Array.empty "" with
              | Some t ->
                  match Int32.TryParse t with
                  | true, v ->
                      let at = utcNow ()
                      let delta = emptyDeltaEvent at "script"
                      match field with
                      | "mr" -> delta.Mr <- nInt v
                      | "fr" -> delta.Fr <- nInt v
                      | "cr" -> delta.Cr <- nInt v
                      | "pr" -> delta.Pr <- nInt v
                      | "dr" -> delta.Dr <- nInt v
                      | _ -> ()
                      appendEvent events ch.Id delta
                      refreshCurrent ch events
                      persistAfterMutation chars events
                      showResult $"Update — {ch.Name}" $"{field} -> {v}"
                  | _ -> showResult $"Update — {ch.Name}" "Invalid number."
              | None -> ()
          | None -> ()
      | Some "field" ->
          let fields = sheetFieldGetters |> List.map (fun (name, _, _) -> name) |> Array.ofList
          match promptChoice "Field" fields with
          | Some field ->
              match promptText field $"New {field}" Array.empty "" with
              | Some t ->
                  match Int32.TryParse t with
                  | true, v ->
                      let at = utcNow ()
                      let expected = cloneSheet ch.Current
                      let actual = cloneSheet ch.Current
                      match sheetFieldGetters |> List.tryFind (fun (n, _, _) -> n = field) with
                      | Some (_, _, setter) -> setter actual (nInt v)
                      | None -> ()
                      let delta = buildDeltaFromDiff at "script" expected actual
                      appendEvent events ch.Id delta
                      refreshCurrent ch events
                      persistAfterMutation chars events
                      showResult $"Update — {ch.Name}" $"{field} -> {v}"
                  | _ -> showResult $"Update — {ch.Name}" "Invalid number."
              | None -> ()
          | None -> ()
      | _ -> ()

let modeRaceClass (chars: CharactersFile) =
  let rows =
    chars.Characters
    |> Seq.groupBy (fun c -> c.Race, c.Class)
    |> Seq.sortBy fst
    |> Seq.map (fun ((race, cls), group) ->
        {| Race = race; Class = cls; Count = Seq.length group; Names = group |> Seq.map (fun c -> c.Name) |> Array.ofSeq |})
    |> Seq.toArray
  showResult $"Race/class matrix ({rows.Length} groups)" rows

let modeFullBaseline (chars: CharactersFile) (events: EventsFile) =
  match pickCharacter chars with
  | None -> ()
  | Some ch ->
      refreshCurrent ch events
      let at = utcNow ()
      appendEvent events ch.Id (eventFromFullSheet at "baseline" ch.Current)
      ch.CurrentAt <- at
      persistAfterMutation chars events
      showResult "Baseline" $"Full baseline recorded for {ch.Name} at {formatAt at}"

let modeProgress (chars: CharactersFile) (events: EventsFile) (cache: ItemCacheFile) =
  let targets =
    match promptChoice "Progress — select target" (Array.append [| "ALL" |] (chars.Characters |> Seq.map (fun c -> c.Name) |> Array.ofSeq)) with
    | None -> Array.empty
    | Some "ALL" -> chars.Characters |> Seq.toArray
    | Some name -> chars.Characters |> Seq.filter (fun c -> c.Name = name) |> Seq.toArray
  // Stat fields worth showing as progress rows (not ac/atk/hp).
  let progressStatFields =
    [
      "level", fun (e: EventRecord) -> e.Level
      "mana", fun (e: EventRecord) -> e.Mana
      "str", fun (e: EventRecord) -> e.Str
      "sta", fun (e: EventRecord) -> e.Sta
      "agi", fun (e: EventRecord) -> e.Agi
      "dex", fun (e: EventRecord) -> e.Dex
      "wis", fun (e: EventRecord) -> e.Wis
      "int", fun (e: EventRecord) -> e.Int
      "cha", fun (e: EventRecord) -> e.Cha
      "mr", fun (e: EventRecord) -> e.Mr
      "fr", fun (e: EventRecord) -> e.Fr
      "cr", fun (e: EventRecord) -> e.Cr
      "pr", fun (e: EventRecord) -> e.Pr
      "dr", fun (e: EventRecord) -> e.Dr
    ]
  let rows = ResizeArray<_>()
  for ch in targets do
    let evs = eventsFor events ch.Id
    for e in evs do
      let at = formatAt e.At
      let kind = if isNullUnsafe e.Kind then "delta" else e.Kind
      let source = if isNullUnsafe e.Source then "" else e.Source
      for field, getter in progressStatFields do
        let v = getter e
        if v.HasValue then
          rows.Add
            {|
              Character = ch.Name
              At = at
              Kind = kind
              Source = source
              Slot = field
              Item = string v.Value
            |}
      if not (isNullUnsafe e.Gear) then
        let isFull = kind.Equals("full", StringComparison.OrdinalIgnoreCase)
        // Stable slot order. Deltas include clears (empty); full baselines omit empty slots.
        for slot in slotNames do
          if e.Gear.ContainsKey slot then
            let id = e.Gear.[slot]
            if (not isFull) || id.HasValue then
              rows.Add
                {|
                  Character = ch.Name
                  At = at
                  Kind = kind
                  Source = source
                  Slot = slot
                  Item = formatItemRef cache id
                |}
      if not (isNullUnsafe e.InventorySet) then
        for inv in e.InventorySet do
          let qty = entryQty inv
          let item = formatItemRef cache (nInt inv.ItemId)
          rows.Add
            {|
              Character = ch.Name
              At = at
              Kind = kind
              Source = source
              Slot = $"bag={inv.Tag}"
              Item = if qty > 1 then $"{item} x{qty}" else item
            |}
      else
        if not (isNullUnsafe e.InventoryAdd) then
          for inv in e.InventoryAdd do
            let qty = entryQty inv
            let item = formatItemRef cache (nInt inv.ItemId)
            rows.Add
              {|
                Character = ch.Name
                At = at
                Kind = kind
                Source = source
                Slot = $"bag+{inv.Tag}"
                Item = if qty > 1 then $"{item} x{qty}" else item
              |}
        if not (isNullUnsafe e.InventoryRemove) then
          for itemId in e.InventoryRemove do
            rows.Add
              {|
                Character = ch.Name
                At = at
                Kind = kind
                Source = source
                Slot = "bag-"
                Item = formatItemRef cache (nInt itemId)
              |}
  let label =
    match targets.Length with
    | 0 -> "Progress (cancelled)"
    | 1 -> $"Progress — {targets.[0].Name} ({rows.Count} rows)"
    | n -> $"Progress — {n} characters ({rows.Count} rows)"
  showResult label (rows.ToArray())

let modeResolveCache (chars: CharactersFile) (events: EventsFile) (cache: ItemCacheFile) (contentRoot: string option ref) =
  let ids = HashSet<int>()
  let addInv (inv: ResizeArray<InventoryEntry>) =
    if not (isNullUnsafe inv) then
      for e in inv do ids.Add e.ItemId |> ignore
  for ch in chars.Characters do
    for KeyValue(_, v) in ch.Current.Gear do
      if v.HasValue then ids.Add v.Value |> ignore
    ensureInventory ch.Current
    addInv ch.Current.Inventory
    for e in eventsFor events ch.Id do
      if not (isNullUnsafe e.Gear) then
        for KeyValue(_, v) in e.Gear do
          if v.HasValue then ids.Add v.Value |> ignore
      addInv e.InventorySet
      addInv e.InventoryAdd
      if not (isNullUnsafe e.InventoryRemove) then
        for itemId in e.InventoryRemove do ids.Add itemId |> ignore
  let root = ensureContentRoot contentRoot
  let lines = ResizeArray<string>()
  for id in ids do
    try
      let item = resolveItem cache root id None
      lines.Add($"Cached {itemLabel item}")
    with ex ->
      lines.Add($"Failed {id}: {ex.Message}")
  showResult $"Resolve cache ({lines.Count} ids)" (lines.ToArray())

let modeLookupItem (cache: ItemCacheFile) (contentRoot: string option ref) =
  match promptText "Lookup item" "Item id" Array.empty "" with
  | Some t ->
      match Int32.TryParse t with
      | true, id ->
          let root = ensureContentRoot contentRoot
          try
            let item = resolveItem cache root id None
            showResult $"Lookup — {itemLabel item}" item
          with ex -> showResult $"Lookup — {id}" ex.Message
      | _ -> showResult "Lookup item" "Invalid id."
  | None -> ()

// --- Main ---
printfn "IdleQuest tracker"
printfn "Data: %s" Paths.dataDir

let chars, events, cache = loadAll ()
syncHandEdits chars events

// One panel for prompt + options + last result (stays visible while ReadLine waits).
PromptUi.dc.Dump("Tracker")
showResult "Startup" $"Data: {Paths.dataDir}"

let contentRoot: string option ref = ref None

let menu = [
  "status", "Dump character status"
  "add", "Add character"
  "update", "Interactive update (gear/inventory/stats/level)"
  "raceclass", "Race/class matrix"
  "baseline", "Append full baseline snapshot"
  "progress", "Progress history"
  "resolve", "Resolve all equipped item ids into cache"
  "lookup", "Lookup one item id"
  "quit", "Exit"
]

let mutable running = true
while running do
  match promptMenu "Mode" menu with
  | Some "status" -> modeStatus chars cache
  | Some "add" ->
      match modeAddCharacter chars events with
      | Some ch -> modeEnterGear ch events chars cache contentRoot
      | None -> ()
  | Some "update" -> modeUpdate chars events cache contentRoot
  | Some "raceclass" -> modeRaceClass chars
  | Some "baseline" -> modeFullBaseline chars events
  | Some "progress" -> modeProgress chars events cache
  | Some "resolve" -> modeResolveCache chars events cache contentRoot
  | Some "lookup" -> modeLookupItem cache contentRoot
  | Some "quit" | None ->
      running <- false
      showResult "Done" "Exited."
  | Some other -> showResult "Mode" $"Unknown mode: {other}"
