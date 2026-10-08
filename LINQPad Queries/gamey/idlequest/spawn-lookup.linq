<Query Kind="FSharpProgram">
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// Spawn / PH / disable lookup against a local idlequest-content clone (brynn).
// Enter a mob name (substring ok) or numeric npc id → npc_types, spawnentry (incl. PH
// siblings), spawn2, spawn2_disabled, and loottable item ids/names when present.
// Prefer the shared idlequestContentRoot password from item-lookup / tracker.

open System
open System.Collections.Generic
open System.IO
open System.Text.Json

let defaultContentRoot = @"D:\projects\idlequest-content"

let jsonOpts =
  let o = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
  o

let isNullUnsafe value = Object.Equals(value, null)

let readLines (path: string) =
  if not (File.Exists path) then Seq.empty
  else File.ReadLines(path)

let shardFiles (dir: string) =
  if not (Directory.Exists dir) then Array.empty
  else Directory.GetFiles(dir, "*.ndjson")

let parseLine<'T> (line: string) : 'T option =
  try Some (JsonSerializer.Deserialize<'T>(line, jsonOpts))
  with _ -> None

let ensureContentRoot () =
  let mutable root = ""
  try
    let pw = Util.GetPassword("idlequestContentRoot")
    if not (isNullUnsafe pw) && Directory.Exists(Path.Combine(pw, "data", "npc_types")) then
      root <- pw
  with _ -> ()
  if String.IsNullOrWhiteSpace root then
    let entered =
      Util.ReadLine(
        "Path to idlequest-content (needs data/npc_types)",
        defaultContentRoot,
        [| defaultContentRoot |])
    root <- if isNullUnsafe entered then "" else entered.Trim()
  if not (Directory.Exists(Path.Combine(root, "data", "npc_types"))) then
    failwithf "Not a valid idlequest-content root: %s" root
  try Util.SetPassword("idlequestContentRoot", root) with _ -> ()
  root

type NpcRow = {
  id: int
  name: string
  level: int
  loottable_id: int
  raid_target: int
  isquest: int
  unique_spawn_by_name: int
  spawn_limit: int
}

type SpawnEntryRow = {
  spawngroupID: int
  npcID: int
  chance: int
  condition_value_filter: int
  min_expansion: int
  max_expansion: int
}

type Spawn2Row = {
  id: int
  spawngroupID: int
  zone: string
  version: int
  x: float
  y: float
  z: float
  heading: float
  respawntime: int
  variance: int
  _condition: int
  cond_value: int
}

type Spawn2DisabledRow = {
  id: string
  spawn2_id: int
  instance_id: int
  disabled: int
}

type SpawnGroupRow = {
  id: int
  name: string
  spawn_limit: int
  mindelay: int
  delay: int
}

type LoottableEntryRow = {
  loottable_id: int
  lootdrop_id: int
  multiplier: int
  droplimit: int
  mindrop: int
  probability: float
}

type LootdropEntryRow = {
  lootdrop_id: int
  item_id: int
  chance: float
  disabled_chance: float
  equip_item: int
}

type ItemRow = {
  id: int
  Name: string
}

let loadAll<'T> (dataRoot: string) (table: string) =
  shardFiles (Path.Combine(dataRoot, table))
  |> Seq.collect readLines
  |> Seq.choose parseLine<'T>
  |> Seq.toArray

let normName (s: string) =
  if isNullUnsafe s then ""
  else s.Replace("#", "").Replace("_", " ").Trim().ToLowerInvariant()

let matchesQuery (query: string) (npc: NpcRow) =
  if String.IsNullOrWhiteSpace query then false
  elif Char.IsDigit query.[0] then
    match Int32.TryParse(query.Trim()) with
    | true, id -> npc.id = id
    | _ -> false
  else
    let q = normName query
    let n = normName npc.name
    n.Contains q || npc.name.IndexOf(query, StringComparison.OrdinalIgnoreCase) >= 0

let formatRespawn (seconds: int) =
  if seconds <= 0 then "-"
  elif seconds % 86400 = 0 then sprintf "%d day(s)" (seconds / 86400)
  elif seconds % 3600 = 0 then sprintf "%d hour(s)" (seconds / 3600)
  elif seconds % 60 = 0 then sprintf "%d min" (seconds / 60)
  else sprintf "%d sec" seconds

let contentRoot = ensureContentRoot ()
let dataRoot = Path.Combine(contentRoot, "data")
printfn "Content: %s" contentRoot

printfn "Loading shards (npc_types, spawnentry, spawn2, spawn2_disabled, spawngroup)..."
let npcs = loadAll<NpcRow> dataRoot "npc_types"
let entries = loadAll<SpawnEntryRow> dataRoot "spawnentry"
let spawn2 = loadAll<Spawn2Row> dataRoot "spawn2"
let disabled = loadAll<Spawn2DisabledRow> dataRoot "spawn2_disabled"
let groups = loadAll<SpawnGroupRow> dataRoot "spawngroup"
printfn "Loaded %d npcs, %d spawnentries, %d spawn2, %d disabled, %d spawngroups."
  npcs.Length entries.Length spawn2.Length disabled.Length groups.Length

let npcById =
  npcs
  |> Seq.groupBy (fun n -> n.id)
  |> Seq.map (fun (id, xs) -> id, Seq.head xs)
  |> dict

let groupById =
  groups
  |> Seq.groupBy (fun g -> g.id)
  |> Seq.map (fun (id, xs) -> id, Seq.head xs)
  |> dict

let disabledBySpawn2 =
  disabled
  |> Seq.groupBy (fun d -> d.spawn2_id)
  |> Seq.map (fun (id, xs) -> id, xs |> Seq.toArray)
  |> dict

let query =
  let entered = Util.ReadLine("Mob name or npc id (e.g. Venril Sathir / 102126)", "Venril Sathir")
  if isNullUnsafe entered then "" else entered.Trim()

let hits =
  npcs
  |> Array.filter (matchesQuery query)
  |> Array.sortBy (fun n -> n.name, n.id)

if hits.Length = 0 then
  {| Query = query; Hits = 0; Note = "No npc_types match." |} |> Dump |> ignore
else
  let pick =
    if hits.Length = 1 then hits.[0]
    else
      let labels =
        hits
        |> Array.map (fun n -> sprintf "%s (%d) L%d loot=%d" n.name n.id n.level n.loottable_id)
      let chosen =
        let s = Util.ReadLine("Pick npc", labels.[0], labels)
        if isNullUnsafe s then labels.[0] else s
      match labels |> Array.tryFindIndex (fun l -> l = chosen) with
      | Some i -> hits.[i]
      | None ->
          match hits |> Array.tryFind (fun n -> chosen.Contains(string n.id)) with
          | Some n -> n
          | None -> hits.[0]

  let myEntries = entries |> Array.filter (fun e -> e.npcID = pick.id)
  let myGroupIds = myEntries |> Array.map (fun e -> e.spawngroupID) |> Array.distinct

  let phRows =
    myGroupIds
    |> Array.collect (fun sg ->
        entries
        |> Array.filter (fun e -> e.spawngroupID = sg)
        |> Array.map (fun e ->
            let name =
              let mutable n = Unchecked.defaultof<NpcRow>
              if npcById.TryGetValue(e.npcID, &n) then n.name else "?"
            let gname =
              let mutable g = Unchecked.defaultof<SpawnGroupRow>
              if groupById.TryGetValue(sg, &g) then g.name else ""
            {|
              SpawngroupId = sg
              SpawngroupName = gname
              NpcId = e.npcID
              Name = name
              Chance = e.chance
              IsSelf = e.npcID = pick.id
              Spawnable = e.chance > 0
            |}))
    |> Array.sortBy (fun r -> r.SpawngroupId, -r.Chance, r.Name)

  let spawnRows =
    myGroupIds
    |> Array.collect (fun sg ->
        spawn2
        |> Array.filter (fun s -> s.spawngroupID = sg)
        |> Array.map (fun s ->
            let dis =
              let mutable arr = Unchecked.defaultof<Spawn2DisabledRow[]>
              if disabledBySpawn2.TryGetValue(s.id, &arr) then arr else Array.empty
            let disabledFlag = dis |> Array.exists (fun d -> d.disabled <> 0 && d.instance_id = 0)
            {|
              Spawn2Id = s.id
              SpawngroupId = sg
              Zone = s.zone
              X = Math.Round(s.x, 2)
              Y = Math.Round(s.y, 2)
              Z = Math.Round(s.z, 2)
              Respawn = formatRespawn s.respawntime
              RespawnSec = s.respawntime
              VarianceSec = s.variance
              Condition = s._condition
              CondValue = s.cond_value
              Disabled = disabledFlag
              DisableRows = dis
            |}))
    |> Array.sortBy (fun r -> r.Zone, r.Spawn2Id)

  /// Stream-filter shards (avoid loading every loot/item row into memory).
  let scanTable<'T> (table: string) (want: 'T -> bool) =
    let acc = ResizeArray<'T>()
    for file in shardFiles (Path.Combine(dataRoot, table)) do
      for line in readLines file do
        match parseLine<'T> line with
        | Some row when want row -> acc.Add row
        | _ -> ()
    acc.ToArray()

  // Loot (useful for epic drop checks); streamed so it stays tolerable.
  let lootSummary =
    if pick.loottable_id <= 0 then
      box {| LoottableId = 0; Note = "No loottable_id on npc." |}
    else
      printfn "Scanning loottable %d..." pick.loottable_id
      let ltEntries =
        scanTable<LoottableEntryRow> "loottable_entries" (fun e -> e.loottable_id = pick.loottable_id)
      let dropIds = ltEntries |> Array.map (fun e -> e.lootdrop_id) |> set
      let dropEntries =
        scanTable<LootdropEntryRow> "lootdrop_entries" (fun e -> dropIds.Contains e.lootdrop_id)
      let itemIds = dropEntries |> Array.map (fun e -> e.item_id) |> set
      let itemNames =
        scanTable<ItemRow> "items" (fun it -> itemIds.Contains it.id)
        |> Array.map (fun it -> it.id, it.Name)
        |> dict
      let rows =
        dropEntries
        |> Array.map (fun e ->
            let mutable name = ""
            let nm = if itemNames.TryGetValue(e.item_id, &name) then name else "?"
            let prob =
              ltEntries
              |> Array.tryFind (fun t -> t.lootdrop_id = e.lootdrop_id)
              |> Option.map (fun t -> t.probability)
              |> Option.defaultValue 0.0
            {|
              LootdropId = e.lootdrop_id
              ItemId = e.item_id
              Item = nm
              DropChance = e.chance
              TableProbability = prob
              Equip = e.equip_item <> 0
            |})
        |> Array.sortBy (fun r -> r.LootdropId, -r.DropChance, r.Item)
      box {| LoottableId = pick.loottable_id; Drops = rows |}

  let anyChance = myEntries |> Array.exists (fun e -> e.chance > 0)
  let anyLiveSpawn =
    spawnRows |> Array.exists (fun s -> not s.Disabled)
  let verdict =
    if myEntries.Length = 0 then
      "No spawnentry rows — not a static spawn (quest/script spawn only, or missing data)."
    elif not anyChance then
      "Has spawnentry but all chance=0 — cannot roll from its spawngroup(s)."
    elif spawnRows.Length = 0 then
      "Has spawnentry chance>0 but no spawn2 rows for those spawngroups."
    elif not anyLiveSpawn then
      "Spawn2 exists but all matching points are spawn2_disabled."
    else
      "Statically spawnable (chance>0 and at least one non-disabled spawn2)."

  {|
    Query = query
    Npc =
      {|
        Id = pick.id
        Name = pick.name
        Level = pick.level
        LoottableId = pick.loottable_id
        RaidTarget = pick.raid_target <> 0
        IsQuest = pick.isquest <> 0
        UniqueByName = pick.unique_spawn_by_name <> 0
        SpawnLimit = pick.spawn_limit
      |}
    Verdict = verdict
    SpawnEntries = myEntries
    PlaceholderTable = phRows
    SpawnPoints = spawnRows
    Loot = lootSummary
    OtherNameMatches =
      hits
      |> Array.filter (fun n -> n.id <> pick.id)
      |> Array.map (fun n -> {| Id = n.id; Name = n.name; Level = n.level; LoottableId = n.loottable_id |})
  |}
  |> Dump
  |> ignore
