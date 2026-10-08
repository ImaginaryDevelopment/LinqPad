<Query Kind="FSharpProgram">
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// Spawn / PH / disable lookup against a local idlequest-content clone (brynn).
// Enter a mob name (substring ok) or numeric npc id → npc_types, spawnentry (incl. PH
// siblings), spawn2, spawn2_disabled, and loottable item ids/names when present.
// Each match reports SourceFile + SourceLine (paths relative to content root).
// Prefer the shared idlequestContentRoot password from item-lookup / tracker.
//
// TODO: add git blame / line change-date capture for these SourceFile:Line hits
// and include LastChanged (and maybe author/commit) in the Dump results — useful when
// investigating chance=0 / spawn2_disabled provenance (e.g. Karnor Venril).

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

let relPath (contentRoot: string) (full: string) =
  let root =
    Path.GetFullPath(contentRoot).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
    + string Path.DirectorySeparatorChar
  let file = Path.GetFullPath(full)
  if file.StartsWith(root, StringComparison.OrdinalIgnoreCase) then
    file.Substring(root.Length).Replace('\\', '/')
  else
    file.Replace('\\', '/')

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

/// One deserialized row plus the shard file / 1-based line it came from.
type Sourced<'T> = {
  File: string
  Line: int
  Row: 'T
}

let loadAllSourced<'T> (contentRoot: string) (dataRoot: string) (table: string) =
  let acc = ResizeArray<Sourced<'T>>()
  for file in shardFiles (Path.Combine(dataRoot, table)) do
    let mutable lineNo = 0
    for line in readLines file do
      lineNo <- lineNo + 1
      match parseLine<'T> line with
      | Some row ->
          acc.Add({ File = relPath contentRoot file; Line = lineNo; Row = row })
      | None -> ()
  acc.ToArray()

let scanTableSourced<'T> (contentRoot: string) (dataRoot: string) (table: string) (want: 'T -> bool) =
  let acc = ResizeArray<Sourced<'T>>()
  for file in shardFiles (Path.Combine(dataRoot, table)) do
    let mutable lineNo = 0
    for line in readLines file do
      lineNo <- lineNo + 1
      match parseLine<'T> line with
      | Some row when want row ->
          acc.Add({ File = relPath contentRoot file; Line = lineNo; Row = row })
      | _ -> ()
  acc.ToArray()

let sourceRef (s: Sourced<_>) = sprintf "%s:%d" s.File s.Line

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
let npcs = loadAllSourced<NpcRow> contentRoot dataRoot "npc_types"
let entries = loadAllSourced<SpawnEntryRow> contentRoot dataRoot "spawnentry"
let spawn2 = loadAllSourced<Spawn2Row> contentRoot dataRoot "spawn2"
let disabled = loadAllSourced<Spawn2DisabledRow> contentRoot dataRoot "spawn2_disabled"
let groups = loadAllSourced<SpawnGroupRow> contentRoot dataRoot "spawngroup"
printfn "Loaded %d npcs, %d spawnentries, %d spawn2, %d disabled, %d spawngroups."
  npcs.Length entries.Length spawn2.Length disabled.Length groups.Length

let npcById =
  npcs
  |> Seq.groupBy (fun s -> s.Row.id)
  |> Seq.map (fun (id, xs) -> id, Seq.head xs)
  |> dict

let groupById =
  groups
  |> Seq.groupBy (fun s -> s.Row.id)
  |> Seq.map (fun (id, xs) -> id, Seq.head xs)
  |> dict

let disabledBySpawn2 =
  disabled
  |> Seq.groupBy (fun s -> s.Row.spawn2_id)
  |> Seq.map (fun (id, xs) -> id, xs |> Seq.toArray)
  |> dict

let query =
  let entered = Util.ReadLine("Mob name or npc id (e.g. Venril Sathir / 102126)", "Venril Sathir")
  if isNullUnsafe entered then "" else entered.Trim()

let hits =
  npcs
  |> Array.filter (fun s -> matchesQuery query s.Row)
  |> Array.sortBy (fun s -> s.Row.name, s.Row.id)

if hits.Length = 0 then
  {| Query = query; Hits = 0; Note = "No npc_types match." |} |> Dump |> ignore
else
  let pickS =
    if hits.Length = 1 then hits.[0]
    else
      let labels =
        hits
        |> Array.map (fun s ->
            let n = s.Row
            sprintf "%s (%d) L%d loot=%d [%s]" n.name n.id n.level n.loottable_id (sourceRef s))
      let chosen =
        let s = Util.ReadLine("Pick npc", labels.[0], labels)
        if isNullUnsafe s then labels.[0] else s
      match labels |> Array.tryFindIndex (fun l -> l = chosen) with
      | Some i -> hits.[i]
      | None ->
          match hits |> Array.tryFind (fun s -> chosen.Contains(string s.Row.id)) with
          | Some s -> s
          | None -> hits.[0]

  let pick = pickS.Row
  let myEntries = entries |> Array.filter (fun s -> s.Row.npcID = pick.id)
  let myGroupIds = myEntries |> Array.map (fun s -> s.Row.spawngroupID) |> Array.distinct

  let phRows =
    myGroupIds
    |> Array.collect (fun sg ->
        entries
        |> Array.filter (fun s -> s.Row.spawngroupID = sg)
        |> Array.map (fun s ->
            let e = s.Row
            let name =
              let mutable n = Unchecked.defaultof<Sourced<NpcRow>>
              if npcById.TryGetValue(e.npcID, &n) then n.Row.name else "?"
            let gname =
              let mutable g = Unchecked.defaultof<Sourced<SpawnGroupRow>>
              if groupById.TryGetValue(sg, &g) then g.Row.name else ""
            {|
              SpawngroupId = sg
              SpawngroupName = gname
              NpcId = e.npcID
              Name = name
              Chance = e.chance
              IsSelf = e.npcID = pick.id
              Spawnable = e.chance > 0
              SourceFile = s.File
              SourceLine = s.Line
            |}))
    |> Array.sortBy (fun r -> r.SpawngroupId, -r.Chance, r.Name)

  let groupSourceRows =
    myGroupIds
    |> Array.choose (fun sg ->
        let mutable g = Unchecked.defaultof<Sourced<SpawnGroupRow>>
        if groupById.TryGetValue(sg, &g) then
          Some
            {|
              SpawngroupId = sg
              Name = g.Row.name
              SpawnLimit = g.Row.spawn_limit
              SourceFile = g.File
              SourceLine = g.Line
            |}
        else None)

  let spawnRows =
    myGroupIds
    |> Array.collect (fun sg ->
        spawn2
        |> Array.filter (fun s -> s.Row.spawngroupID = sg)
        |> Array.map (fun s ->
            let sp = s.Row
            let dis =
              let mutable arr = Unchecked.defaultof<Sourced<Spawn2DisabledRow>[]>
              if disabledBySpawn2.TryGetValue(sp.id, &arr) then arr else Array.empty
            let disabledFlag =
              dis |> Array.exists (fun d -> d.Row.disabled <> 0 && d.Row.instance_id = 0)
            {|
              Spawn2Id = sp.id
              SpawngroupId = sg
              Zone = sp.zone
              X = Math.Round(sp.x, 2)
              Y = Math.Round(sp.y, 2)
              Z = Math.Round(sp.z, 2)
              Respawn = formatRespawn sp.respawntime
              RespawnSec = sp.respawntime
              VarianceSec = sp.variance
              Condition = sp._condition
              CondValue = sp.cond_value
              Disabled = disabledFlag
              SourceFile = s.File
              SourceLine = s.Line
              DisableRows =
                dis
                |> Array.map (fun d ->
                    {|
                      Id = d.Row.id
                      Spawn2Id = d.Row.spawn2_id
                      InstanceId = d.Row.instance_id
                      Disabled = d.Row.disabled
                      SourceFile = d.File
                      SourceLine = d.Line
                    |})
            |}))
    |> Array.sortBy (fun r -> r.Zone, r.Spawn2Id)

  // Loot (useful for epic drop checks); streamed so it stays tolerable.
  let lootSummary =
    if pick.loottable_id <= 0 then
      box {| LoottableId = 0; Note = "No loottable_id on npc."; SourceFiles = Array.empty<string> |}
    else
      printfn "Scanning loottable %d..." pick.loottable_id
      let ltEntries =
        scanTableSourced<LoottableEntryRow> contentRoot dataRoot "loottable_entries"
          (fun e -> e.loottable_id = pick.loottable_id)
      let dropIds = ltEntries |> Array.map (fun s -> s.Row.lootdrop_id) |> set
      let dropEntries =
        scanTableSourced<LootdropEntryRow> contentRoot dataRoot "lootdrop_entries"
          (fun e -> dropIds.Contains e.lootdrop_id)
      let itemIds = dropEntries |> Array.map (fun s -> s.Row.item_id) |> set
      let itemNames =
        scanTableSourced<ItemRow> contentRoot dataRoot "items" (fun it -> itemIds.Contains it.id)
        |> Array.map (fun s -> s.Row.id, (s.Row.Name, s.File, s.Line))
        |> dict
      let rows =
        dropEntries
        |> Array.map (fun s ->
            let e = s.Row
            let mutable tip = ("?", "", 0)
            let nm, itemFile, itemLine =
              if itemNames.TryGetValue(e.item_id, &tip) then tip else ("?", "", 0)
            let prob =
              ltEntries
              |> Array.tryFind (fun t -> t.Row.lootdrop_id = e.lootdrop_id)
              |> Option.map (fun t -> t.Row.probability)
              |> Option.defaultValue 0.0
            let ltSrc =
              ltEntries
              |> Array.tryFind (fun t -> t.Row.lootdrop_id = e.lootdrop_id)
            {|
              LootdropId = e.lootdrop_id
              ItemId = e.item_id
              Item = nm
              DropChance = e.chance
              TableProbability = prob
              Equip = e.equip_item <> 0
              LootdropSourceFile = s.File
              LootdropSourceLine = s.Line
              LoottableEntrySourceFile = ltSrc |> Option.map (fun t -> t.File) |> Option.defaultValue ""
              LoottableEntrySourceLine = ltSrc |> Option.map (fun t -> t.Line) |> Option.defaultValue 0
              ItemSourceFile = itemFile
              ItemSourceLine = itemLine
            |})
        |> Array.sortBy (fun r -> r.LootdropId, -r.DropChance, r.Item)
      let lootFiles =
        seq {
          yield! ltEntries |> Seq.map (fun s -> s.File)
          yield! dropEntries |> Seq.map (fun s -> s.File)
          yield! itemNames.Values |> Seq.map (fun (_, f, _) -> f) |> Seq.filter (fun f -> f <> "")
        }
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toArray
      box {| LoottableId = pick.loottable_id; Drops = rows; SourceFiles = lootFiles |}

  let anyChance = myEntries |> Array.exists (fun s -> s.Row.chance > 0)
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

  // Policy file often documents intentional disables (e.g. Karnor Venril migration 069).
  let policyHits =
    let policyPath = Path.Combine(contentRoot, "tools", "export-policy.mjs")
    if not (File.Exists policyPath) then Array.empty
    else
      let needles =
        [|
          string pick.id
          pick.name
          pick.name.TrimStart('#')
        |]
        |> Array.distinct
        |> Array.filter (fun n -> not (String.IsNullOrWhiteSpace n))
      File.ReadAllLines(policyPath)
      |> Array.mapi (fun i line -> i + 1, line)
      |> Array.choose (fun (lineNo, line) ->
          if needles |> Array.exists (fun n -> line.IndexOf(n, StringComparison.OrdinalIgnoreCase) >= 0) then
            Some
              {|
                SourceFile = relPath contentRoot policyPath
                SourceLine = lineNo
                Line = line.Trim()
              |}
          else None)

  let dataFiles =
    seq {
      yield pickS.File
      yield! myEntries |> Seq.map (fun s -> s.File)
      yield! phRows |> Seq.map (fun r -> r.SourceFile)
      yield! groupSourceRows |> Seq.map (fun r -> r.SourceFile)
      yield! spawnRows |> Seq.map (fun r -> r.SourceFile)
      yield! spawnRows |> Seq.collect (fun r -> r.DisableRows |> Seq.map (fun d -> d.SourceFile))
      yield! policyHits |> Seq.map (fun p -> p.SourceFile)
    }
    |> Seq.distinct
    |> Seq.sort
    |> Seq.toArray

  // TODO: for each DataFiles / SourceFile:SourceLine below, run git blame (or
  // git log -L) under contentRoot and attach LastChanged / Author / Commit to results.

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
        SourceFile = pickS.File
        SourceLine = pickS.Line
      |}
    Verdict = verdict
    DataFiles = dataFiles
    SpawnEntries =
      myEntries
      |> Array.map (fun s ->
          {|
            SpawngroupId = s.Row.spawngroupID
            NpcId = s.Row.npcID
            Chance = s.Row.chance
            SourceFile = s.File
            SourceLine = s.Line
          |})
    Spawngroups = groupSourceRows
    PlaceholderTable = phRows
    SpawnPoints = spawnRows
    ExportPolicyHits = policyHits
    Loot = lootSummary
    OtherNameMatches =
      hits
      |> Array.filter (fun s -> s.Row.id <> pick.id)
      |> Array.map (fun s ->
          {|
            Id = s.Row.id
            Name = s.Row.name
            Level = s.Row.level
            LoottableId = s.Row.loottable_id
            SourceFile = s.File
            SourceLine = s.Line
          |})
  |}
  |> Dump
  |> ignore
