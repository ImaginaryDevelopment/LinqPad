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
  // TODO: remove this — temporary equip helpers (until tracker merge)
  let charactersPath = Path.Combine(dataDir, "eq_characters.json")
  // end TODO: remove this

let jsonOpts =
  let o = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
  o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
  o.Converters.Add(JsonStringEnumConverter())
  o

// Item cache only: omit nulls and numeric zeros (and other defaults) to keep the file small.
let itemCacheJsonOpts =
  let o = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
  o.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingDefault
  o.Converters.Add(JsonStringEnumConverter())
  o

let inline ser value = JsonSerializer.Serialize(value, jsonOpts)
let inline serItemCache value = JsonSerializer.Serialize(value, itemCacheJsonOpts)
let inline deser<'T> (text: string) = JsonSerializer.Deserialize<'T>(text, jsonOpts)
let isNullUnsafe value = Object.Equals(value, null)

/// After one-time setup (git pull, content root), looping I/O goes here instead of flooding results.
module SessionUi =
  let dc = DumpContainer()
  let logLines = ResizeArray<string>()
  let mutable panels: (string * obj) list = []
  let mutable active = false

  let refresh () =
    if active then
      dc.Content <-
        {|
          Log = logLines |> Seq.toArray
          Results =
            panels
            |> List.mapi (fun i (title, data) ->
                {| Order = i + 1; Title = title; Data = data |})
            |> Array.ofList
        |}

  let beginSession () =
    active <- true
    logLines.Clear()
    panels <- []
    dc.Dump("lookup session") |> ignore
    refresh ()

  let beginLookup () =
    if active then
      logLines.Clear()
      panels <- []
      refresh ()

  let write (msg: string) =
    if not active then
      printfn "%s" msg
    else
      logLines.Add(sprintf "%s  %s" (DateTime.Now.ToString("HH:mm:ss")) msg)
      while logLines.Count > 50 do
        logLines.RemoveAt(0)
      refresh ()

  let show (title: string) (data: obj) =
    if not active then
      data.Dump(description = title) |> ignore
    else
      panels <- (title, data) :: panels |> List.truncate 4
      refresh ()

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

// TODO: remove this — temporary equip-into-character helpers until eq-character-tracker merge.
type Gear = Dictionary<string, Nullable<int>>

[<CLIMutable>]
type Sheet = {
  mutable Level: Nullable<int>
  mutable Gear: Gear
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
// end TODO: remove this

let emptyItemCacheFile () : ItemCacheFile = { Items = Dictionary<string, CachedItem>() }

let ensureDir () =
  if not (Directory.Exists Paths.dataDir) then
    Directory.CreateDirectory Paths.dataDir |> ignore

let loadCache () =
  ensureDir ()
  if not (File.Exists Paths.itemCachePath) then
    let s = emptyItemCacheFile ()
    File.WriteAllText(Paths.itemCachePath, serItemCache s)
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
  File.WriteAllText(Paths.itemCachePath, serItemCache cache)

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
      SessionUi.write $"Scanning GitHub shard {shard} ..."
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
  SessionUi.write "Loading matching item names from local repo (may take a bit)..."
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
  SessionUi.write $"Loaded {list.Length} matching item(s) for autocomplete."
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
  SessionUi.write $"{items.Length} {kind} match(es) from {source}"
  let rows =
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
  SessionUi.show $"{items.Length} {kind} — {source}" rows

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

let confirmYesNo (message: string) =
  match prompt message [ "y"; "n"; "yes"; "no" ] with
  | Some t when String.Equals(t, "y", StringComparison.OrdinalIgnoreCase)
             || String.Equals(t, "yes", StringComparison.OrdinalIgnoreCase) -> true
  | _ -> false

let itemLabel (item: CachedItem) = $"{item.Id} — {item.Name}"

let pickWhichItem (message: string) (hits: CachedItem list) : CachedItem option =
  match hits with
  | [] -> None
  | many ->
      // Keep cancel first so Enter / first suggestion does not auto-accept an item.
      let cancelLabel = "(abort)"
      let labels =
        cancelLabel :: (many |> List.map itemLabel)
        |> Array.ofList
      let msg =
        if message.IndexOf("Enter to abort", StringComparison.OrdinalIgnoreCase) >= 0 then message
        else message.TrimEnd() + " — Enter to abort"
      match prompt msg labels with
      | None -> None
      | Some t when String.IsNullOrWhiteSpace t -> None
      | Some picked when String.Equals(picked, cancelLabel, StringComparison.OrdinalIgnoreCase)
                      || String.Equals(picked, "abort", StringComparison.OrdinalIgnoreCase)
                      || String.Equals(picked, "cancel", StringComparison.OrdinalIgnoreCase) -> None
      | Some picked ->
          many
          |> List.tryFind (fun i -> itemLabel i = picked || sameName picked i.Name || string i.Id = picked)
          |> Option.orElseWith (fun () ->
              match Int32.TryParse (picked.Split('—').[0].Trim()) with
              | true, id -> many |> List.tryFind (fun i -> i.Id = id)
              | _ -> None)

// TODO: remove this — temporary equip prompt after adding or looking up an item (until tracker merge).
let slotsForItem (item: CachedItem) =
  slotBits
  |> List.choose (fun (name, bit) -> if item.Slots &&& bit <> 0 then Some name else None)

let loadCharacters () =
  if not (File.Exists Paths.charactersPath) then
    SessionUi.write $"No characters file at {Paths.charactersPath}"
    None
  else
    try
      let parsed = deser<CharactersFile> (File.ReadAllText Paths.charactersPath)
      if isNullUnsafe parsed || isNullUnsafe parsed.Characters then None
      else Some parsed
    with ex ->
      SessionUi.write $"Failed to read characters: {ex.Message}"
      None

let saveCharacters (chars: CharactersFile) =
  File.WriteAllText(Paths.charactersPath, ser chars)

let ensureCharacterGear (ch: Character) =
  if isNullUnsafe ch.Current then
    ch.Current <- { Level = Nullable(); Gear = Dictionary<string, Nullable<int>>() }
  if isNullUnsafe ch.Current.Gear then
    ch.Current.Gear <- Dictionary<string, Nullable<int>>()
  for name in slotNames do
    if not (ch.Current.Gear.ContainsKey name) then
      ch.Current.Gear.[name] <- Nullable()

let formatGearItem (cache: ItemCacheFile) (itemId: int) =
  let mutable existing = Unchecked.defaultof<CachedItem>
  if cache.Items.TryGetValue(string itemId, &existing) && not (String.IsNullOrWhiteSpace existing.Name) then
    $"{existing.Name}({itemId})"
  else
    string itemId

let showCharacterGear (ch: Character) =
  ensureCharacterGear ch
  let cache = loadCache ()
  let priorityFront = [ "primary"; "secondary"; "chest"; "legs"; "back"; "waist" ]
  let frontSet = priorityFront |> Set.ofList
  let middle =
    slotNames
    |> List.filter (fun s ->
        not (frontSet.Contains s)
        && not (String.Equals(s, "charm", StringComparison.OrdinalIgnoreCase)))
  let displaySlots = priorityFront @ middle @ [ "charm" ]
  let rows =
    displaySlots
    |> List.map (fun slot ->
        let filled = ch.Current.Gear.ContainsKey slot && ch.Current.Gear.[slot].HasValue
        {|
          Slot = slot
          Item = if filled then formatGearItem cache ch.Current.Gear.[slot].Value else ""
        |})
  SessionUi.show $"{ch.Name} gear ({ch.Class})" rows

let tryEquipItemOnCharacter (classFilter: (string * int) option) (equipTargetName: string option) (item: CachedItem) =
  match loadCharacters () with
  | None -> ()
  | Some chars when chars.Characters.Count = 0 ->
      SessionUi.write "No characters to equip on."
  | Some chars ->
      let proceedWith (ch: Character) =
        ensureCharacterGear ch
        let possible = slotsForItem item
        let slotOpt =
          match possible with
          | [] ->
              SessionUi.write $"Item has no equip slots (slots={item.Slots}); skipping."
              None
          | [ one ] -> Some one
          | many ->
              match prompt $"Slot for {item.Name} (blank skips)" many with
              | None -> None
              | Some t when String.IsNullOrWhiteSpace t -> None
              | Some s when many |> List.exists (fun m -> String.Equals(m, s, StringComparison.OrdinalIgnoreCase)) ->
                  many |> List.find (fun m -> String.Equals(m, s, StringComparison.OrdinalIgnoreCase)) |> Some
              | Some s ->
                  SessionUi.write $"Unknown slot '{s}'; skipping."
                  None
        match slotOpt with
        | None ->
            SessionUi.write "Equip skipped."
            showCharacterGear ch
        | Some slot ->
            let already =
              ch.Current.Gear.ContainsKey slot
              && ch.Current.Gear.[slot].HasValue
              && ch.Current.Gear.[slot].Value = item.Id
            if already then
              SessionUi.write (sprintf "%s already has %s in %s; no write." ch.Name (itemLabel item) slot)
            else
              let prev =
                if ch.Current.Gear.ContainsKey slot && ch.Current.Gear.[slot].HasValue then
                  string ch.Current.Gear.[slot].Value
                else "empty"
              ch.Current.Gear.[slot] <- Nullable(item.Id)
              ch.CurrentAt <- DateTime.UtcNow.ToString("o")
              saveCharacters chars
              SessionUi.write $"Equipped {item.Name} on {ch.Name} ({ch.Race} / {ch.Class}) in {slot} (was {prev}). Hand-edit sync in the tracker will record this."
            showCharacterGear ch
      let showTargetGear () =
        match equipTargetName with
        | None -> ()
        | Some targetName ->
            match chars.Characters |> Seq.tryFind (fun c -> String.Equals(c.Name, targetName, StringComparison.OrdinalIgnoreCase)) with
            | Some ch -> showCharacterGear ch
            | None -> ()
      match equipTargetName with
      | Some targetName ->
          match chars.Characters |> Seq.tryFind (fun c -> String.Equals(c.Name, targetName, StringComparison.OrdinalIgnoreCase)) with
          | None ->
              SessionUi.write $"Equip target '{targetName}' not found in characters file."
          | Some ch ->
              SessionUi.write $"Auto-equipping {itemLabel item} on {ch.Name} ({ch.Class})."
              proceedWith ch
      | None ->
          let matching =
            chars.Characters
            |> Seq.filter (fun c ->
                match classFilter with
                | None -> true
                | Some (className, _) ->
                    String.Equals(c.Class, className, StringComparison.OrdinalIgnoreCase))
            |> Seq.filter (fun c -> not (String.IsNullOrWhiteSpace c.Name))
            |> Seq.toList
          match classFilter, matching with
          | Some (className, _), [ only ] ->
              if confirmYesNo (sprintf "Equip %s to the only %s, %s? (y/n)" (itemLabel item) className only.Name) then
                proceedWith only
              else
                SessionUi.write "Equip skipped."
                showCharacterGear only
          | _ ->
              if matching.IsEmpty then
                match classFilter with
                | Some (n, _) -> SessionUi.write $"No characters match class filter '{n}'."
                | None -> SessionUi.write "No characters to equip on."
              elif not (confirmYesNo (sprintf "Equip %s on a character? (y/n)" (itemLabel item))) then
                showTargetGear ()
              else
                let names =
                  matching
                  |> List.map (fun c -> c.Name)
                  |> List.distinct
                  |> List.sort
                  |> Array.ofList
                let promptLabel =
                  match classFilter with
                  | Some (n, _) -> $"Character to equip ({n} only; blank skips)"
                  | None -> "Character to equip (blank skips)"
                match prompt promptLabel names with
                | None ->
                    SessionUi.write "Equip skipped."
                    showTargetGear ()
                | Some t when String.IsNullOrWhiteSpace t ->
                    SessionUi.write "Equip skipped."
                    showTargetGear ()
                | Some picked ->
                    match matching |> List.tryFind (fun c -> String.Equals(c.Name, picked, StringComparison.OrdinalIgnoreCase)) with
                    | None -> SessionUi.write $"Character '{picked}' not found."
                    | Some ch -> proceedWith ch

let maybePromptEquip (cache: ItemCacheFile) (classFilter: (string * int) option) (equipTargetName: string option) (hits: CachedItem list) =
  // Runs after adding an item or looking up one that already exists in item-cache.json.
  let ensureCached (item: CachedItem) =
    let added = mergeIntoCache cache [ item ]
    if added > 0 then
      SessionUi.write (sprintf "Wrote %s into item-cache.json." (itemLabel item))
  match hits with
  | [] -> ()
  | [ item ] ->
      ensureCached item
      tryEquipItemOnCharacter classFilter equipTargetName item
  | many ->
      match pickWhichItem "Equip which item? (only the chosen item is written to item-cache.json)" many with
      | Some item ->
          ensureCached item
          tryEquipItemOnCharacter classFilter equipTargetName item
      | None -> SessionUi.write "Equip aborted."
// end TODO: remove this

let storeAndPresent (cache: ItemCacheFile) (source: string) (exact: bool) (hits: CachedItem list) (classFilter: (string * int) option) (equipTargetName: string option) =
  present source exact hits
  let chosen =
    match hits with
    | [] -> None
    | [ one ] when exact -> Some one
    | [ one ] ->
        pickWhichItem "Fuzzy match — did you mean this?" [ one ]
    | many when exact ->
        pickWhichItem $"Multiple exact matches ({many.Length}); which one?" many
    | many ->
        pickWhichItem $"Fuzzy matches ({many.Length}); which item did you mean?" many
  match chosen with
  | None ->
      if not hits.IsEmpty then
        SessionUi.write "Aborted; nothing written to item-cache.json."
  | Some item ->
      let added = mergeIntoCache cache [ item ]
      SessionUi.write (sprintf "Wrote %d new item(s) into item-cache.json (%d already present)." added (1 - added))
      // TODO: remove this — temporary equip prompt after add/lookup (until tracker merge).
      maybePromptEquip cache classFilter equipTargetName [ item ]
      // end TODO: remove this

/// Show unfiltered candidates and only cache/accept if the user picks one.
let offerUnfilteredMatches (cache: ItemCacheFile) (source: string) (exact: bool) (hits: CachedItem list) (filtersText: string) (classFilter: (string * int) option) (equipTargetName: string option) =
  if hits.IsEmpty then false
  else
    let kind = if exact then "exact" else "partial"
    SessionUi.write (sprintf "No match with current filters (%s). Candidates without filters (%d %s)." filtersText hits.Length kind)
    SessionUi.show $"Unfiltered candidates ({hits.Length})" (hits |> List.map itemLabel)
    let msg =
      if exact && hits.Length = 1 then
        $"Accept this unfiltered match? Filters were: {filtersText}"
      else
        $"No filtered match. Which unfiltered item did you mean? Filters were: {filtersText}"
    match pickWhichItem msg hits with
    | None ->
        SessionUi.write "Aborted; unfiltered candidates rejected."
        false
    | Some item ->
        storeAndPresent cache source true [ item ] classFilter equipTargetName
        true

type LookupRequest = {
  Query: string
  ClassFilter: (string * int) option
  SlotFilter: (string * int) option
  PrefetchedItems: CachedItem list option
  EquipTargetName: string option
}

type PromptResult =
  | Exit
  | Lookup of LookupRequest

let formatClassSlot (classFilter: (string * int) option) (slotFilter: (string * int) option) =
  let classText =
    match classFilter with
    | Some (n, _) -> n
    | None -> "(none)"
  let slotText =
    match slotFilter with
    | Some (n, _) -> n
    | None -> "(none)"
  $"class={classText}; slot={slotText}"

let formatFilters (classFilter: (string * int) option) (slotFilter: (string * int) option) (equipTargetName: string option) =
  let equipText =
    match equipTargetName with
    | Some n -> n
    | None -> "(none)"
  $"{formatClassSlot classFilter slotFilter}; equip={equipText}"

// Blank "list all filtered" and filter-scan autocomplete live in Util.Cache, not item-cache.json.
let mutable filterListSlot : string[] = [||]

let filterListCacheKey (classFilter: (string * int) option) (slotFilter: (string * int) option) =
  "idlequest-filter-items:" + formatClassSlot classFilter slotFilter

let writeFilterListCache (classFilter: (string * int) option) (slotFilter: (string * int) option) (items: CachedItem list) =
  let names =
    items
    |> Seq.map (fun i -> i.Name)
    |> Seq.distinct
    |> Seq.sort
    |> Array.ofSeq
  filterListSlot <- names
  Util.Cache<string[]>((fun () -> filterListSlot), key = filterListCacheKey classFilter slotFilter, forceRefresh = true)
  |> ignore
  names

let readFilterListCache (classFilter: (string * int) option) (slotFilter: (string * int) option) =
  filterListSlot <- [||]
  Util.Cache<string[]>((fun () -> filterListSlot), key = filterListCacheKey classFilter slotFilter, forceRefresh = false)

let dumpCachedNamesForFilters (classFilter: (string * int) option) (slotFilter: (string * int) option) =
  match classFilter, slotFilter with
  | Some (_, classBit), Some (_, slotBit) ->
      let filtersText = formatClassSlot classFilter slotFilter
      let fromLinqPad = readFilterListCache classFilter slotFilter
      if fromLinqPad.Length > 0 then
        SessionUi.show $"Filter scan / Util.Cache ({fromLinqPad.Length}): {filtersText}" fromLinqPad
      let fromJson =
        loadCache().Items.Values
        |> Seq.filter (fun item -> itemMatchesFilters item (Some classBit) (Some slotBit))
        |> Seq.map (fun item -> item.Name)
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toList
      SessionUi.show $"item-cache.json only ({fromJson.Length}): {filtersText}" fromJson
  | _ -> ()

let isClearCommand (text: string) =
  String.Equals(text.Trim(), "clear", StringComparison.OrdinalIgnoreCase)

let listCharacterNames () =
  match loadCharacters () with
  | None -> [||]
  | Some chars ->
      chars.Characters
      |> Seq.map (fun c -> c.Name)
      |> Seq.filter (fun n -> not (String.IsNullOrWhiteSpace n))
      |> Seq.distinct
      |> Seq.sort
      |> Seq.toArray

let tryMatchCharacter (text: string) =
  match loadCharacters () with
  | None -> None
  | Some chars ->
      chars.Characters
      |> Seq.tryFind (fun c ->
          not (String.IsNullOrWhiteSpace c.Name)
          && String.Equals(c.Name, text, StringComparison.OrdinalIgnoreCase))

/// Until a class or gear slot is chosen, suggestions are classes/slots/character names.
/// After one is set (and a local repo exists), suggestions add matching item names,
/// while still offering the other unused filter type.
/// Type a character name to set class filter + auto-equip target for lookups.
/// Filters and the matching content list persist across lookups; type "clear" to remove them.
let promptLookup
    (localRoot: string)
    (initialClass: (string * int) option)
    (initialSlot: (string * int) option)
    (initialPrefetched: CachedItem list option)
    (initialEquipTarget: string option)
    : PromptResult =
  let mutable classFilter = initialClass
  let mutable slotFilter = initialSlot
  let mutable prefetched = initialPrefetched
  let mutable equipTarget = initialEquipTarget
  let characterNames = listCharacterNames ()

  let ensurePrefetch (forceReload: bool) =
    match classFilter, slotFilter with
    | None, None ->
        prefetched <- None
    | _ when String.IsNullOrWhiteSpace localRoot ->
        prefetched <- None
        SessionUi.write "No local idlequest-content clone; item-name autocomplete unavailable until a local root is selected."
    | _ ->
        let classBit = classFilter |> Option.map snd
        let slotBit = slotFilter |> Option.map snd
        match prefetched with
        | Some items when not forceReload ->
            let narrowed =
              items
              |> List.filter (fun item -> itemMatchesFilters item classBit slotBit)
            prefetched <- Some narrowed
            writeFilterListCache classFilter slotFilter narrowed |> ignore
            SessionUi.write $"Using cached filter list ({narrowed.Length} item(s) after narrow)."
        | _ ->
            let loaded = loadItemsMatchingFilters localRoot classBit slotBit
            prefetched <- Some loaded
            writeFilterListCache classFilter slotFilter loaded |> ignore
    dumpCachedNamesForFilters classFilter slotFilter

  // Keep the existing content list when filters are unchanged; only dump cache names.
  match classFilter, slotFilter, prefetched with
  | None, None, _ -> prefetched <- None
  | _, _, Some _ ->
      dumpCachedNamesForFilters classFilter slotFilter
  | _ ->
      ensurePrefetch true

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
    let clearSuggestion =
      match classFilter, slotFilter, equipTarget with
      | None, None, None -> Seq.empty
      | _ -> seq { "clear" }
    let characterSuggestions = characterNames |> Seq.ofArray

    let suggestions =
      match classFilter, slotFilter with
      | None, None ->
          // First step: classes, slots, and character names (or free-type an item name/id).
          Seq.concat [ classSuggestions; slotSuggestions; characterSuggestions ]
      | _ ->
          Seq.concat [ clearSuggestion; classSuggestions; slotSuggestions; characterSuggestions; itemSuggestions ]

    let filtersText = formatFilters classFilter slotFilter equipTarget
    let message =
      match classFilter, slotFilter, equipTarget with
      | None, None, None ->
          $"Current filters: {filtersText}. Enter a class, gear slot, character name, or item name/id (blank exits)."
      | _, _, Some name ->
          $"Current filters: {filtersText}. Lookups auto-equip on {name} (multi-slot items still ask). Enter item name/id, slot, another character, or 'clear' (blank = list all in Util.Cache)."
      | Some _, None, _ ->
          $"Current filters: {filtersText}. Enter a gear slot, character name, item name/id, or 'clear' (blank = list all in Util.Cache, not item-cache.json)."
      | None, Some _, _ ->
          $"Current filters: {filtersText}. Enter a class, character name, item name/id, or 'clear' (blank = list all in Util.Cache, not item-cache.json)."
      | Some _, Some _, _ ->
          $"Current filters: {filtersText}. Enter an item name/id, character name, or 'clear' (blank = list all in Util.Cache, not item-cache.json)."

    SessionUi.write $"Current filters: {filtersText}"
    match prompt message suggestions with
    | None -> Exit
    | Some text when String.IsNullOrWhiteSpace text ->
        match classFilter, slotFilter with
        | None, None -> Exit
        | _ ->
            Lookup {
              Query = ""
              ClassFilter = classFilter
              SlotFilter = slotFilter
              PrefetchedItems = prefetched
              EquipTargetName = equipTarget
            }
    | Some text when isClearCommand text ->
        classFilter <- None
        slotFilter <- None
        prefetched <- None
        equipTarget <- None
        SessionUi.write (sprintf "Cleared filters (now %s)." (formatFilters classFilter slotFilter equipTarget))
        loop ()
    | Some text ->
        match tryMatchCharacter text with
        | Some ch ->
            equipTarget <- Some ch.Name
            showCharacterGear ch
            match tryMatchClass ch.Class with
            | Some pair ->
                let switching = classFilter |> Option.exists (fun (n, _) -> not (String.Equals(n, fst pair, StringComparison.OrdinalIgnoreCase)))
                classFilter <- Some pair
                if switching then prefetched <- None
                SessionUi.write (sprintf "Equip target %s (%s); class filter set. Lookups will auto-equip (now %s)." ch.Name ch.Class (formatFilters classFilter slotFilter equipTarget))
                ensurePrefetch true
            | None ->
                SessionUi.write (sprintf "Equip target %s, but class '%s' is not in classBits; set a class filter manually. (now %s)" ch.Name ch.Class (formatFilters classFilter slotFilter equipTarget))
            loop ()
        | None ->
            match tryMatchClass text with
            | Some pair ->
                // Allow switching class even when one is already set (otherwise "Wizard" is treated as an item name).
                let switching = classFilter |> Option.exists (fun (n, _) -> not (String.Equals(n, fst pair, StringComparison.OrdinalIgnoreCase)))
                classFilter <- Some pair
                // Manual class change drops character auto-equip unless the target already has this class.
                match equipTarget with
                | Some name ->
                    match tryMatchCharacter name with
                    | Some ch when String.Equals(ch.Class, fst pair, StringComparison.OrdinalIgnoreCase) -> ()
                    | _ ->
                        equipTarget <- None
                        SessionUi.write "Cleared equip target (class filter no longer matches that character)."
                | None -> ()
                if switching then
                  prefetched <- None
                  SessionUi.write (sprintf "Switched class filter to %s (now %s)." (fst pair) (formatFilters classFilter slotFilter equipTarget))
                else
                  SessionUi.write (sprintf "Added class filter: %s (now %s)." (fst pair) (formatFilters classFilter slotFilter equipTarget))
                ensurePrefetch true
                loop ()
            | None ->
                match tryMatchSlot text with
                | Some pair ->
                    let switching = slotFilter |> Option.exists (fun (n, _) -> not (String.Equals(n, fst pair, StringComparison.OrdinalIgnoreCase)))
                    slotFilter <- Some pair
                    if switching then
                      prefetched <- None
                      SessionUi.write (sprintf "Switched slot filter to %s (now %s)." (fst pair) (formatFilters classFilter slotFilter equipTarget))
                    else
                      SessionUi.write (sprintf "Added slot filter: %s (now %s)." (fst pair) (formatFilters classFilter slotFilter equipTarget))
                    ensurePrefetch true
                    loop ()
                | None ->
                    Lookup {
                      Query = text
                      ClassFilter = classFilter
                      SlotFilter = slotFilter
                      PrefetchedItems = prefetched
                      EquipTargetName = equipTarget
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

let runLookup (req: LookupRequest) =
  let classBit = req.ClassFilter |> Option.map snd
  let slotBit = req.SlotFilter |> Option.map snd
  let hasFilters = classBit.IsSome || slotBit.IsSome
  let filtersText = formatFilters req.ClassFilter req.SlotFilter req.EquipTargetName
  let query = req.Query
  let cache = loadCache ()
  let qText = if String.IsNullOrWhiteSpace query then "(all filtered)" else query
  SessionUi.write $"Lookup with filters: {filtersText}; query={qText}"
  let asId =
    match Int32.TryParse query with
    | true, id when not (String.IsNullOrWhiteSpace query) -> Some id
    | _ -> None
  let hasNameQuery = asId.IsSome || not (String.IsNullOrWhiteSpace query)

  let filterList (items: CachedItem list) =
    items |> List.filter (fun i -> itemMatchesFilters i classBit slotBit)

  let nameHitsFrom (items: CachedItem list) =
    let exact = items |> List.filter (fun i -> sameName query i.Name)
    if not exact.IsEmpty then exact, true
    else items |> List.filter (fun i -> containsName query i.Name), false

  let cachedFiltered =
    if rescan then []
    elif not hasNameQuery then []
    else
      match asId with
      | Some id ->
          let mutable existing = Unchecked.defaultof<CachedItem>
          if cache.Items.TryGetValue(string id, &existing) && itemMatchesFilters existing classBit slotBit then
            [ existing ]
          else []
      | None -> cacheByName cache query sameName |> filterList

  let cachedUnfiltered () =
    if rescan || not hasNameQuery then []
    else
      match asId with
      | Some id ->
          let mutable existing = Unchecked.defaultof<CachedItem>
          if cache.Items.TryGetValue(string id, &existing) then [ existing ] else []
      | None ->
          let exact = cacheByName cache query sameName
          if not exact.IsEmpty then exact
          else cacheByName cache query containsName

  let tryUnfilteredFallback (alreadyTried: CachedItem list) =
    if not (hasFilters && hasNameQuery) then
      false
    else
      SessionUi.write $"No item matched '{query}' with current filters ({filtersText}); retrying without filters."
      let fromCache =
        let hits = cachedUnfiltered ()
        let exact = hits |> List.forall (fun i -> asId.IsSome || sameName query i.Name)
        hits, exact
      let cacheHits, cacheExact = fromCache
      if not cacheHits.IsEmpty then
        offerUnfilteredMatches cache "item-cache.json (unfiltered)" cacheExact cacheHits filtersText req.ClassFilter req.EquipTargetName
      else
        match req.PrefetchedItems with
        | Some filtered when not (String.IsNullOrWhiteSpace query) ->
            // Prefetch was filter-scoped; scan content without filters.
            let contentRoot =
              let r = configuredContentRoot ()
              if String.IsNullOrWhiteSpace r then getContentRoot () else r
            if String.IsNullOrWhiteSpace contentRoot then
              SessionUi.write "No local content root available for an unfiltered retry."
              false
            else
              let found, exact = scanContent contentRoot query asId None None
              let novel = found |> List.filter (fun i -> alreadyTried |> List.forall (fun t -> t.Id <> i.Id))
              offerUnfilteredMatches cache "local clone (unfiltered)" exact (if novel.IsEmpty then found else novel) filtersText req.ClassFilter req.EquipTargetName
        | _ ->
            let prefetchedRoot = configuredContentRoot ()
            let contentRoot = getContentRoot ()
            if not (String.IsNullOrWhiteSpace contentRoot) && not (String.Equals(contentRoot, prefetchedRoot, StringComparison.OrdinalIgnoreCase)) then
              ensureContentFresh contentRoot
            if String.IsNullOrWhiteSpace contentRoot then
              SessionUi.write "No content source available for an unfiltered retry."
              false
            else
              let source = if String.IsNullOrWhiteSpace (configuredContentRoot ()) then "github (unfiltered)" else "local clone (unfiltered)"
              let found, exact = scanContent contentRoot query asId None None
              offerUnfilteredMatches cache source exact found filtersText req.ClassFilter req.EquipTargetName

  if not cachedFiltered.IsEmpty then
    present "item-cache.json" true cachedFiltered
    // TODO: remove this — temporary equip prompt after lookup of existing cache item (until tracker merge).
    maybePromptEquip cache req.ClassFilter req.EquipTargetName cachedFiltered
    // end TODO: remove this
  else
    let unfilteredCacheHits =
      if hasFilters && hasNameQuery then cachedUnfiltered () else []
    if not unfilteredCacheHits.IsEmpty then
      let exact = unfilteredCacheHits |> List.forall (fun i -> asId.IsSome || sameName query i.Name)
      offerUnfilteredMatches cache "item-cache.json (unfiltered)" exact unfilteredCacheHits filtersText req.ClassFilter req.EquipTargetName |> ignore
    else
      match req.PrefetchedItems with
      | Some items when String.IsNullOrWhiteSpace query && asId.IsNone ->
          let hits = filterList items
          let names = writeFilterListCache req.ClassFilter req.SlotFilter hits
          SessionUi.write $"Listed {names.Length} filtered item(s) into Util.Cache only (not written to item-cache.json)."
          SessionUi.show $"Filter scan / Util.Cache ({names.Length}): {filtersText}" names
          present "filter list (Util.Cache only)" true hits
      | Some items when hasNameQuery && asId.IsNone ->
          let named, exactFlag = nameHitsFrom items
          let hits = filterList named
          if not hits.IsEmpty then
            storeAndPresent cache "local clone (filtered)" exactFlag hits req.ClassFilter req.EquipTargetName
          elif not (tryUnfilteredFallback named) then
            SessionUi.write $"No item matched '{query}'."
      | _ ->
          let prefetchedRoot = configuredContentRoot ()
          let contentRoot = getContentRoot ()
          if not (String.IsNullOrWhiteSpace contentRoot) && not (String.Equals(contentRoot, prefetchedRoot, StringComparison.OrdinalIgnoreCase)) then
            ensureContentFresh contentRoot
          let source =
            if String.IsNullOrWhiteSpace contentRoot then
              SessionUi.write "No local idlequest-content clone selected. Fetching GitHub shards (slow)."
              "github"
            else
              SessionUi.write $"Scanning local content: {contentRoot}"
              "local clone"
          let found, exact = scanContent contentRoot query asId classBit slotBit
          if not found.IsEmpty then
            storeAndPresent cache source exact found req.ClassFilter req.EquipTargetName
          elif String.IsNullOrWhiteSpace query && asId.IsNone then
            SessionUi.write "No items matched the current filters."
          elif not (tryUnfilteredFallback found) then
            SessionUi.write $"No item matched '{query}'."

if not (String.IsNullOrWhiteSpace itemName) then
  SessionUi.beginSession ()
  SessionUi.beginLookup ()
  runLookup {
    Query = itemName.Trim()
    ClassFilter = None
    SlotFilter = None
    PrefetchedItems = None
    EquipTargetName = None
  }
else
  // One-time setup (content root / git pull) prints above; looping output uses the DumpContainer.
  let localRoot = resolveLocalRootForPrompt ()
  SessionUi.beginSession ()
  let mutable sessionClass: (string * int) option = None
  let mutable sessionSlot: (string * int) option = None
  let mutable sessionPrefetched: CachedItem list option = None
  let mutable sessionEquip: string option = None
  let rec session () =
    match promptLookup localRoot sessionClass sessionSlot sessionPrefetched sessionEquip with
    | Exit ->
        SessionUi.write "Done."
    | Lookup req ->
        sessionClass <- req.ClassFilter
        sessionSlot <- req.SlotFilter
        sessionPrefetched <- req.PrefetchedItems
        sessionEquip <- req.EquipTargetName
        SessionUi.beginLookup ()
        runLookup req
        session ()
  session ()
