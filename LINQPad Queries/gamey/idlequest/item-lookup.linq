<Query Kind="FSharpProgram">
  <Namespace>System.Windows.Forms</Namespace>
  <IncludeUncapsulator>false</IncludeUncapsulator>
</Query>

// IdleQuest item lookup by name (or numeric id).
// Shares item-cache.json and the idlequestContentRoot LINQPad password with eq-character-tracker.linq.
// Cache hit returns immediately. Otherwise scans a local idlequest-content clone, or GitHub raw shards, and writes matches into the cache.
// A configured local clone is git-pulled when due. Util.Cache stores (last attempt, success). Failures wait 1 day; successes wait 3 days.

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization
open System.Windows.Forms

// Leave blank to be prompted. Set rescan to ignore a cache hit and read content again.
let itemName = ""
let rescan = false

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

// Same password entry as eq-character-tracker.linq. "__github__" means skip the local clone.
let githubSentinel = "__github__"

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

let configuredContentRoot () =
  let cached = Util.GetPassword("idlequestContentRoot")
  if cached = githubSentinel || String.IsNullOrWhiteSpace cached then null
  elif Directory.Exists(Path.Combine(cached, "data", "items")) then cached
  else null

let getContentRoot () =
  let cached = Util.GetPassword("idlequestContentRoot")
  if cached = githubSentinel then
    null
  elif not (String.IsNullOrWhiteSpace cached) && Directory.Exists(Path.Combine(cached, "data", "items")) then
    cached
  else
    use browser = new FolderBrowserDialog(Description = "Select idlequest-content repo root (Cancel = use GitHub raw)")
    if not (String.IsNullOrWhiteSpace cached) && Directory.Exists cached then
      browser.InitialDirectory <- cached
    use form = new Form(TopMost = true, TopLevel = true)
    let result = browser.ShowDialog form
    if result = DialogResult.OK && Directory.Exists(Path.Combine(browser.SelectedPath, "data", "items")) then
      Util.SetPassword("idlequestContentRoot", browser.SelectedPath)
      browser.SelectedPath
    else
      Util.SetPassword("idlequestContentRoot", githubSentinel)
      null

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

let cacheByName (cache: ItemCacheFile) (query: string) (pred: string -> string -> bool) =
  cache.Items.Values
  |> Seq.filter (fun item -> pred query item.Name)
  |> Seq.sortBy (fun item -> item.Id)
  |> Seq.toList

let scanContent (contentRoot: string) (query: string) (byId: int option) =
  let exact = ResizeArray<CachedItem>()
  let partial = ResizeArray<CachedItem>()
  let seen = HashSet<int>()
  for shard in shards do
    if String.IsNullOrWhiteSpace contentRoot then
      printfn "Scanning GitHub shard %s ..." shard
    try
      for line in readShardLines contentRoot shard do
        let worthParsing =
          match byId with
          | Some id -> line.Contains($"\"id\":{id}")
          | None -> line.IndexOf(query, StringComparison.OrdinalIgnoreCase) >= 0
        if worthParsing then
          match tryProjectLine line with
          | Some item when seen.Add item.Id ->
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

let promptName () =
  use form = new Form(Text = "Lookup item", Width = 520, Height = 160, StartPosition = FormStartPosition.CenterScreen, TopMost = true)
  let lbl = new Label(Text = "Item name or id", Left = 12, Top = 12, Width = 480)
  let inputBox = new TextBox(Left = 12, Top = 40, Width = 480)
  let ok = new Button(Text = "OK", DialogResult = DialogResult.OK, Left = 320, Top = 80, Width = 80)
  let cancel = new Button(Text = "Cancel", DialogResult = DialogResult.Cancel, Left = 412, Top = 80, Width = 80)
  form.AcceptButton <- ok
  form.CancelButton <- cancel
  form.Controls.AddRange [| lbl :> Control; inputBox; ok; cancel |]
  if form.ShowDialog() = DialogResult.OK then inputBox.Text.Trim() else ""

let query =
  if not (String.IsNullOrWhiteSpace itemName) then itemName.Trim()
  else promptName ()

if String.IsNullOrWhiteSpace query then
  printfn "No name entered."
else
  printfn "Item cache: %s" Paths.itemCachePath
  let prefetchedRoot = configuredContentRoot ()
  if not (String.IsNullOrWhiteSpace prefetchedRoot) then
    ensureContentFresh prefetchedRoot
  let cache = loadCache ()
  let asId =
    match Int32.TryParse query with
    | true, id -> Some id
    | _ -> None

  let cachedHits =
    if rescan then []
    else
      match asId with
      | Some id ->
          let mutable existing = Unchecked.defaultof<CachedItem>
          if cache.Items.TryGetValue(string id, &existing) then [ existing ] else []
      | None -> cacheByName cache query sameName

  if not cachedHits.IsEmpty then
    present "item-cache.json" true cachedHits
  else
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
    let found, exact = scanContent contentRoot query asId
    if found.IsEmpty then
      printfn "No item matched '%s'." query
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
