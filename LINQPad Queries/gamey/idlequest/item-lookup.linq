<Query Kind="FSharpProgram">
  <Namespace>System.Windows.Forms</Namespace>
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// IdleQuest item lookup by name (or numeric id), with optional class/slot filters.
// Shares item-cache.json and the idlequestContentRoot LINQPad password with eq-character-tracker.linq.
// Prompting uses Util.ReadLine with suggestions (LINQPad's interactive prompt API).
// Cache hit returns immediately. Otherwise scans a local idlequest-content clone, or GitHub raw shards, and writes matches into the cache.
// A configured local clone is git-pulled when due. Util.Cache stores (last attempt, success). Failures wait 1 day; successes wait 3 days.
// Content-root choice is also Util.Cache: local path sticks; GitHub cancel only skips the prompt for 1 day (never a permanent password lockout).

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization
open System.Windows.Forms

// Leave blank to be prompted. Set rescan to ignore a cache hit and read content again.
// Set repromptContentRoot to force the idlequest-content folder browser again.
let itemName = ""
let rescan = false
let repromptContentRoot = false

module Paths =
  let dataDir =
    let q = Util.CurrentQueryPath
    if String.IsNullOrWhiteSpace q then Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "LINQPad Queries", "idlequest")
    else Path.GetDirectoryName q
  let itemCachePath = Path.Combine(dataDir, "item-cache.json")

let jsonOpts =
  let o = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
  o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
  o.Converters.Add(JsonStringEnumConverter())
  o

let inline ser value = JsonSerializer.Serialize(value, jsonOpts)
let inline deser<'T> (text: string) = JsonSerializer.Deserialize<'T>(text, jsonOpts)
let isNullUnsafe value = Object.Equals(value, null)

let slotBits: (string * int) list =
  [
    "charm", 1; "ear1", 2; "head", 4; "face", 8; "ear2", 16; "neck", 32
    "shoulders", 64; "arms", 128; "back", 256; "wrist1", 512; "wrist2", 1024
    "range", 2048; "hands", 4096; "primary", 8192; "secondary", 16384
    "fingers1", 32768; "fingers2", 65536; "chest", 131072; "legs", 262144
    "feet", 524288; "waist", 1048576; "ammo", 2097152
  ]

let raceBits =
  [
    "Human", 1; "Barbarian", 2; "Erudite", 4; "Wood Elf", 8; "High Elf", 16
    "Dark Elf", 32; "Half Elf", 64; "Dwarf", 128; "Troll", 256; "Ogre", 512
    "Halfling", 1024; "Gnome", 2048; "Iksar", 4096; "Vah Shir", 8192; "Froglok", 16384
  ]

let classBits =
  [
    "Warrior", 1; "Cleric", 2; "Paladin", 4; "Ranger", 8; "Shadow Knight", 16
    "Druid", 32; "Monk", 64; "Bard", 128; "Rogue", 256; "Shaman", 512
    "Necromancer", 1024; "Wizard", 2048; "Magician", 4096; "Enchanter", 8192
    "Beastlord", 16384; "Berserker", 32768
  ]

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
}

[<CLIMutable>]
type ItemCacheFile = {
  mutable Items: Dictionary<string, CachedItem>
}

let emptyItemCacheFile () : ItemCacheFile = { Items = Dictionary<string, CachedItem>() }

let ensureDir () =
  if not (Directory.Exists Paths.dataDir) then
    Directory.CreateDirectory Paths.dataDir |> ignore

let loadCache () =
  ensureDir ()
  if not (File.Exists Paths.itemCachePath) then
    let s = emptyItemCacheFile ()
    File.WriteAllText(Paths.itemCachePath, ser s)
    s
  else
    let text = File.ReadAllText Paths.itemCachePath
    if String.IsNullOrWhiteSpace text then emptyItemCacheFile ()
    else
      let parsed = deser<ItemCacheFile> text
      if isNullUnsafe parsed then emptyItemCacheFile ()
      else
        if isNullUnsafe parsed.Items then parsed.Items <- Dictionary<string, CachedItem>()
        parsed

let saveItemCache (cache: ItemCacheFile) =
  File.WriteAllText(Paths.itemCachePath, ser cache)

// Content-root preference: local path stays in Util.Cache (+ password for the tracker).
// Choosing GitHub is also cached, but only for a day — never a permanent "never ask again".
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
  // Do not write a permanent password sentinel. Leave any real path alone for later.
  clearLegacyGithubPassword ()

let promptContentRoot (initial: string) =
  use browser = new FolderBrowserDialog(Description = "Select idlequest-content repo root (Cancel = use GitHub raw for now)")
  if isValidContentRoot initial then
    browser.SelectedPath <- initial
  elif not (String.IsNullOrWhiteSpace initial) && Directory.Exists initial then
    browser.SelectedPath <- initial
  use form = new Form(TopMost = true, TopLevel = true)
  let result = browser.ShowDialog form
  if result = DialogResult.OK && isValidContentRoot browser.SelectedPath then
    rememberLocalRoot browser.SelectedPath
    UseLocal browser.SelectedPath
  else
    rememberGithubRoot ()
    printfn "Using GitHub raw for now. Will ask again after a day (or set repromptContentRoot = true)."
    UseGithub

let decideContentRoot () =
  if repromptContentRoot then Ask
  else
    let fromPassword = passwordContentRoot ()
    if not (isNull fromPassword) then
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

let configuredContentRoot () =
  match decideContentRoot () with
  | UseLocal root -> root
  | UseGithub | Ask -> null

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

// Util.Cache value is (last attempt, succeeded). Failure waits a day; success waits a few days.
let pullFailureCooldown = TimeSpan.FromDays 1.0
let pullSuccessCooldown = TimeSpan.FromDays 3.0
let noPullYet : DateTime * bool = DateTime.MinValue, false
let mutable pullSlot : DateTime * bool = noPullYet

let pullStateKey (root: string) =
  "idlequest-content-git-pull:" + Path.GetFullPath(root).TrimEnd([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |])

let readPullState (root: string) =
  pullSlot <- noPullYet
  Util.Cache<DateTime * bool>((fun () -> pullSlot), key = pullStateKey root, forceRefresh = false)

let writePullState (root: string) (state: DateTime * bool) =
  pullSlot <- state
  Util.Cache<DateTime * bool>((fun () -> pullSlot), key = pullStateKey root, forceRefresh = true) |> ignore

let pullIsDue (attemptedAt: DateTime, succeeded: bool) =
  if attemptedAt = DateTime.MinValue then true
  else
    let age = DateTime.UtcNow - attemptedAt.ToUniversalTime()
    age >= (if succeeded then pullSuccessCooldown else pullFailureCooldown)

let formatAttempt (attemptedAt: DateTime) =
  attemptedAt.ToLocalTime().ToString("yyyy-MM-dd HH:mm")

let runGitPull (root: string) =
  let psi = ProcessStartInfo()
  psi.FileName <- "git"
  psi.ArgumentList.Add "-C"
  psi.ArgumentList.Add root
  psi.ArgumentList.Add "pull"
  psi.ArgumentList.Add "--ff-only"
  psi.UseShellExecute <- false
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  psi.CreateNoWindow <- true
  use proc = new Process()
  proc.StartInfo <- psi
  try
    if not (proc.Start()) then false, "could not start git"
    else
      let outTask = proc.StandardOutput.ReadToEndAsync()
      let errTask = proc.StandardError.ReadToEndAsync()
      if not (proc.WaitForExit(120000)) then
        try proc.Kill() with _ -> ()
        false, "timed out after 2 minutes"
      else
        let output = (outTask.GetAwaiter().GetResult() + "\n" + errTask.GetAwaiter().GetResult()).Trim()
        if proc.ExitCode = 0 then true, output else false, output
  with ex ->
    false, ex.Message

let ensureContentFresh (root: string) =
  if String.IsNullOrWhiteSpace root || not (Directory.Exists root) then ()
  elif not (Directory.Exists(Path.Combine(root, ".git"))) then
    printfn "Content root has no .git folder; skipping pull."
  else
    let state = readPullState root
    if not (pullIsDue state) then
      let attemptedAt, succeeded = state
      if succeeded then
        printfn "Last git pull succeeded %s; skipping until a few days have passed." (formatAttempt attemptedAt)
      else
        printfn "Last git pull failed %s; skipping until a day has passed." (formatAttempt attemptedAt)
    else
      printfn "git pull in %s" root
      let succeeded, detail = runGitPull root
      writePullState root (DateTime.UtcNow, succeeded)
      if succeeded then
        let note = if String.IsNullOrWhiteSpace detail then "ok" else detail.Replace("\r", " ").Replace("\n", " ")
        printfn "git pull succeeded: %s" note
      else
        printfn "git pull failed: %s" (if String.IsNullOrWhiteSpace detail then "git exited non-zero" else detail)

let shards =
  [| '0'..'9' |] |> Array.map string |> Array.append ([| 'a'..'f' |] |> Array.map string)

let http =
  lazy
    let client = new HttpClient()
    client.Timeout <- TimeSpan.FromMinutes 5.0
    client.DefaultRequestHeaders.UserAgent.ParseAdd "LinqPad-IdleQuest"
    client

let readShardLines (contentRoot: string) (shard: string) =
  if not (String.IsNullOrWhiteSpace contentRoot) then
    let path = Path.Combine(contentRoot, "data", "items", shard + ".ndjson")
    if File.Exists path then File.ReadLines path
    else Seq.empty
  else
    let url = $"https://raw.githubusercontent.com/brynnb/idlequest-content/main/data/items/{shard}.ndjson"
    seq {
      use stream = http.Value.GetStreamAsync(url).GetAwaiter().GetResult()
      use reader = new StreamReader(stream)
      let mutable line = reader.ReadLine()
      while not (isNull line) do
        if line.Length > 0 then yield line
        line <- reader.ReadLine()
    }

let tryGetProperty (el: JsonElement) (name: string) =
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
    }
  with _ -> None

let tryProjectLine (line: string) =
  try
    use doc = JsonDocument.Parse line
    projectItem doc.RootElement
  with _ -> None

let sameName (query: string) (name: string) =
  not (String.IsNullOrEmpty name) && String.Equals(name, query, StringComparison.OrdinalIgnoreCase)

let containsName (query: string) (name: string) =
  not (String.IsNullOrEmpty name) && name.IndexOf(query, StringComparison.OrdinalIgnoreCase) >= 0

let classNames = classBits |> List.map fst |> List.sort
let slotNames = slotBits |> List.map fst

let tryMatchClass (text: string) =
  classBits |> List.tryFind (fun (n, _) -> String.Equals(n, text, StringComparison.OrdinalIgnoreCase))

let tryMatchSlot (text: string) =
  slotBits |> List.tryFind (fun (n, _) -> String.Equals(n, text, StringComparison.OrdinalIgnoreCase))

let itemMatchesFilters (item: CachedItem) (classBit: int option) (slotBit: int option) =
  let classOk =
    match classBit with
    | None -> true
    | Some bit -> item.Classes = 0 || item.Classes = 65535 || (item.Classes &&& bit) <> 0
  let slotOk =
    match slotBit with
    | None -> true
    | Some bit -> (item.Slots &&& bit) <> 0
  classOk && slotOk

let cacheByName (cache: ItemCacheFile) (query: string) (pred: string -> string -> bool) =
  cache.Items.Values
  |> Seq.filter (fun item -> pred query item.Name)
  |> Seq.sortBy (fun item -> item.Id)
  |> Seq.toList

let scanContent (contentRoot: string) (query: string) (byId: int option) (classBit: int option) (slotBit: int option) =
  let exact = ResizeArray<CachedItem>()
  let partial = ResizeArray<CachedItem>()
  let seen = HashSet<int>()
  let wantAll = String.IsNullOrWhiteSpace query && byId.IsNone
  for shard in shards do
    if String.IsNullOrWhiteSpace contentRoot then
      printfn "Scanning GitHub shard %s ..." shard
    try
      for line in readShardLines contentRoot shard do
        let worthParsing =
          match byId with
          | Some id -> line.Contains($"\"id\":{id}")
          | None when wantAll -> true
          | None -> line.IndexOf(query, StringComparison.OrdinalIgnoreCase) >= 0
        if worthParsing then
          match tryProjectLine line with
          | Some item when seen.Add item.Id && itemMatchesFilters item classBit slotBit ->
              if wantAll then exact.Add item
              else
                match byId with
                | Some id when item.Id = id -> exact.Add item
                | Some _ -> ()
                | None when sameName query item.Name -> exact.Add item
                | None when containsName query item.Name -> partial.Add item
                | None -> ()
          | _ -> ()
    with ex ->
      printfn "WARN: shard %s read failed: %s" shard ex.Message
  let exactList = exact |> Seq.sortBy (fun i -> i.Id) |> Seq.toList
  let partialList = partial |> Seq.sortBy (fun i -> i.Id) |> Seq.toList
  if exactList.IsEmpty then partialList, false else exactList, true

/// Load every local item matching the current class/slot filters (for autocomplete).
let loadItemsMatchingFilters (contentRoot: string) (classBit: int option) (slotBit: int option) =
  let items = ResizeArray<CachedItem>()
  let seen = HashSet<int>()
  printfn "Loading matching item names from local repo (may take a bit)..."
  for shard in shards do
    try
      for line in readShardLines contentRoot shard do
        match tryProjectLine line with
        | Some item when seen.Add item.Id && itemMatchesFilters item classBit slotBit ->
            items.Add item
        | _ -> ()
    with ex ->
      printfn "WARN: shard %s read failed: %s" shard ex.Message
  let list = items |> Seq.sortBy (fun i -> i.Name, i.Id) |> Seq.toList
  printfn "Loaded %d matching item(s) for autocomplete." list.Length
  list

let mergeIntoCache (cache: ItemCacheFile) (items: CachedItem list) =
  let mutable added = 0
  for item in items do
    let key = string item.Id
    if not (cache.Items.ContainsKey key) then added <- added + 1
    cache.Items.[key] <- item
  if items.Length > 0 then saveItemCache cache
  added

let decodeMask (bits: (string * int) list) (mask: int) =
  if mask = 0 then "NONE"
  elif mask = 65535 then "ALL"
  else
    bits
    |> List.filter (fun (_, bit) -> mask &&& bit <> 0)
    |> List.map fst
    |> function
      | [] -> string mask
      | names -> String.Join(", ", names)

let decodeSlots (mask: int) =
  if mask = 0 then "NONE"
  else
    slotBits
    |> List.filter (fun (_, bit) -> mask &&& bit <> 0)
    |> List.map fst
    |> function
      | [] -> string mask
      | names -> String.Join(", ", names)

let present (source: string) (exact: bool) (items: CachedItem list) =
  let kind = if exact then "exact" else "partial"
  printfn "%d %s match(es) from %s" items.Length kind source
  items
  |> List.map (fun item ->
      {|
        Id = item.Id
        Name = item.Name
        Ac = item.Ac
        Hp = item.Hp
        Mana = item.Mana
        Damage = item.Damage
        Delay = item.Delay
        Attack = item.Attack
        Str = item.Astr
        Sta = item.Asta
        Agi = item.Aagi
        Dex = item.Adex
        Wis = item.Awis
        Int = item.Aint
        Cha = item.Acha
        Mr = item.Mr
        Fr = item.Fr
        Cr = item.Cr
        Pr = item.Pr
        Dr = item.Dr
        ReqLevel = item.Reqlevel
        ItemType = item.Itemtype
        Slots = decodeSlots item.Slots
        Classes = decodeMask classBits item.Classes
        Races = decodeMask raceBits item.Races
        Source = source
      |})
  |> Dump
  |> ignore

// LINQPad interactive prompt with autocomplete suggestions (API name: Util.ReadLine).
let prompt (message: string) (suggestions: string seq) =
  let arr =
    suggestions
    |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace s))
    |> Seq.distinct
    |> Seq.toArray
  let raw = Util.ReadLine(message, "", arr)
  if isNullUnsafe raw then None
  else Some ((string raw).Trim())

type LookupRequest = {
  Query: string
  ClassFilter: (string * int) option
  SlotFilter: (string * int) option
  PrefetchedItems: CachedItem list option
}

/// Until a class or gear slot is chosen, suggestions are only those filters.
/// After one is set (and a local repo exists), suggestions add matching item names,
/// while still offering the other unused filter type.
let promptLookup (localRoot: string) : LookupRequest option =
  let mutable classFilter: (string * int) option = None
  let mutable slotFilter: (string * int) option = None
  let mutable prefetched: CachedItem list option = None

  let refreshPrefetch () =
    prefetched <- None
    match classFilter, slotFilter with
    | None, None -> ()
    | _ when String.IsNullOrWhiteSpace localRoot ->
        printfn "No local idlequest-content clone; item-name autocomplete unavailable until a local root is selected."
    | _ ->
        let classBit = classFilter |> Option.map snd
        let slotBit = slotFilter |> Option.map snd
        prefetched <- Some (loadItemsMatchingFilters localRoot classBit slotBit)

  let rec loop () =
    let classSuggestions =
      match classFilter with
      | Some _ -> Seq.empty
      | None -> classNames |> Seq.ofList
    let slotSuggestions =
      match slotFilter with
      | Some _ -> Seq.empty
      | None -> slotNames |> Seq.ofList
    let itemSuggestions =
      match prefetched with
      | Some items -> items |> Seq.map (fun i -> i.Name)
      | None -> Seq.empty

    let suggestions =
      match classFilter, slotFilter with
      | None, None ->
          // First step: only classes and gear slots (or free-type a name/id to skip filters).
          Seq.append classSuggestions slotSuggestions
      | _ ->
          Seq.concat [ classSuggestions; slotSuggestions; itemSuggestions ]

    let statusParts = [
      yield!
        match classFilter with
        | Some (n, _) -> [ $"class={n}" ]
        | None -> []
      yield!
        match slotFilter with
        | Some (n, _) -> [ $"slot={n}" ]
        | None -> []
    ]
    let status = String.Join(", ", statusParts)
    let message =
      match classFilter, slotFilter with
      | None, None ->
          "Class, gear slot, or item name/id (blank cancels). Until a class/slot is chosen, suggestions are only those filters."
      | _ ->
          $"Filters [{status}]. Pick the other filter, or an item name/id (blank = list all filtered)."

    match prompt message suggestions with
    | None -> None
    | Some text when String.IsNullOrWhiteSpace text ->
        match classFilter, slotFilter with
        | None, None -> None
        | _ ->
            Some {
              Query = ""
              ClassFilter = classFilter
              SlotFilter = slotFilter
              PrefetchedItems = prefetched
            }
    | Some text ->
        match classFilter, tryMatchClass text with
        | None, Some pair ->
            classFilter <- Some pair
            printfn "Filter: class %s" (fst pair)
            refreshPrefetch ()
            loop ()
        | _ ->
            match slotFilter, tryMatchSlot text with
            | None, Some pair ->
                slotFilter <- Some pair
                printfn "Filter: slot %s" (fst pair)
                refreshPrefetch ()
                loop ()
            | _ ->
                Some {
                  Query = text
                  ClassFilter = classFilter
                  SlotFilter = slotFilter
                  PrefetchedItems = prefetched
                }
  loop ()

let resolveLocalRootForPrompt () =
  match decideContentRoot () with
  | UseLocal root ->
      ensureContentFresh root
      root
  | UseGithub ->
      printfn "Content preference is GitHub for now; class/slot filters still work, but item-name autocomplete needs a local clone."
      null
  | Ask ->
      // Ask early so autocomplete can use the local repo when the user has one.
      let root = getContentRoot ()
      if not (String.IsNullOrWhiteSpace root) then ensureContentFresh root
      root

printfn "Item cache: %s" Paths.itemCachePath

let request =
  if not (String.IsNullOrWhiteSpace itemName) then
    Some {
      Query = itemName.Trim()
      ClassFilter = None
      SlotFilter = None
      PrefetchedItems = None
    }
  else
    let localRoot = resolveLocalRootForPrompt ()
    promptLookup localRoot

match request with
| None -> printfn "No lookup entered."
| Some req ->
  let classBit = req.ClassFilter |> Option.map snd
  let slotBit = req.SlotFilter |> Option.map snd
  let query = req.Query
  let cache = loadCache ()
  let asId =
    match Int32.TryParse query with
    | true, id when not (String.IsNullOrWhiteSpace query) -> Some id
    | _ -> None

  let filterList (items: CachedItem list) =
    items |> List.filter (fun i -> itemMatchesFilters i classBit slotBit)

  let cachedHits =
    if rescan then []
    elif String.IsNullOrWhiteSpace query && asId.IsNone then []
    else
      match asId with
      | Some id ->
          let mutable existing = Unchecked.defaultof<CachedItem>
          if cache.Items.TryGetValue(string id, &existing) && itemMatchesFilters existing classBit slotBit then
            [ existing ]
          else []
      | None -> cacheByName cache query sameName |> filterList

  if not cachedHits.IsEmpty then
    present "item-cache.json" true cachedHits
  else
    match req.PrefetchedItems with
    | Some items when String.IsNullOrWhiteSpace query && asId.IsNone ->
        let hits = filterList items
        let toStore = if hits.Length <= 200 then hits else hits |> List.truncate 200
        let added = mergeIntoCache cache toStore
        printfn "Wrote %d new item(s) into item-cache.json (%d already present)." added (toStore.Length - added)
        present "local clone (filtered)" true hits
    | Some items when asId.IsNone && not (String.IsNullOrWhiteSpace query) ->
        let exact = items |> List.filter (fun i -> sameName query i.Name) |> filterList
        let hits, exactFlag =
          if not exact.IsEmpty then exact, true
          else items |> List.filter (fun i -> containsName query i.Name) |> filterList, false
        if hits.IsEmpty then
          printfn "No item matched '%s' with the current filters." query
        else
          let toStore =
            if exactFlag then hits
            elif hits.Length <= 25 then hits
            else
              printfn "Partial matches: %d. Caching the first 25." hits.Length
              hits |> List.truncate 25
          let added = mergeIntoCache cache toStore
          printfn "Wrote %d new item(s) into item-cache.json (%d already present)." added (toStore.Length - added)
          present "local clone (filtered)" exactFlag hits
    | _ ->
        let prefetchedRoot = configuredContentRoot ()
        let contentRoot = getContentRoot ()
        if not (String.IsNullOrWhiteSpace contentRoot) && not (String.Equals(contentRoot, prefetchedRoot, StringComparison.OrdinalIgnoreCase)) then
          ensureContentFresh contentRoot
        let source =
          if String.IsNullOrWhiteSpace contentRoot then
            printfn "No local idlequest-content clone selected. Fetching GitHub shards (slow)."
            "github"
          else
            printfn "Scanning local content: %s" contentRoot
            "local clone"
        let found, exact = scanContent contentRoot query asId classBit slotBit
        if found.IsEmpty then
          if String.IsNullOrWhiteSpace query then printfn "No items matched the current filters."
          else printfn "No item matched '%s'." query
        else
          let toStore =
            if exact then found
            elif found.Length <= 25 then found
            else
              printfn "Partial matches: %d. Caching the first 25. Use a more specific name for the rest." found.Length
              found |> List.truncate 25
          let added = mergeIntoCache cache toStore
          printfn "Wrote %d new item(s) into item-cache.json (%d already present)." added (toStore.Length - added)
          present source exact found
