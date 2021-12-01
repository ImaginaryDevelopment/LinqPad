<Query Kind="FSharpProgram" />

// hosts file lock of certain sites, and unlocking, while not interfering with other entries
// WIP

let winpath =
    //"C:\\windows"
     Util.GetPassword("MaxWindowsPath")

if not <| Directory.Exists winpath then failwithf "Unable to locate path '%s'" winpath
let sites = [
    "discord.com"
    "twitch.tv"
    "twitter.com"
]
type RunType =
    | Explore
    | Block
    | Unblock
    
module Helpers =
    let trim (x:string) = x.Trim()
    let tuple x y = (x,y)
    let startsWith (delim:string) (x:string) = x.StartsWith(delim,StringComparison.InvariantCultureIgnoreCase)
    let split (delim:char) (value:string) = value.Split(Array.singleton delim)
    let flip f (x:'t) (y:'t2) = f y x
    let getValueString x =
        if String.IsNullOrWhiteSpace x then None else Some x
    let (|Trim|) = trim
    let (|StartsWith|_|) delim x = if startsWith delim x then Some () else None
    let (|NonValueString|ValueString|) x =
        match getValueString x with
        | None -> 
            NonValueString
        | Some v -> ValueString v
    module Map =
        let addListItem key x m =
            let child =
                match m |> Map.tryFind key with
                | Some items -> x::items
                | None -> [x]
            m |> Map.add key child
        let addListItems (key:'tkey) (items: 't list) (m:Map<'tkey,'t list>) =
            (m,items)
            //||> List.fold(flip addListItem key)
            ||> List.fold(fun m item -> addListItem key item m)
open Helpers

module HostLogic =
    let (|Comment|Empty|Entry|) =
        function
        | NonValueString -> Empty
        | Trim(StartsWith "#") -> Comment
        | ValueString line ->
            match split ' ' line |> List.ofSeq with
            | ip::names -> Entry(Ok (ip,names))
            | _ -> Entry (Error ())
open HostLogic

type FilePath = FilePath of string
let hosts = FilePath <| Path.Combine(winpath, "System32","drivers","etc","hosts")
type HostData = {Addr:string;LineIndex:int option;NameIndex:int}
type HostState = {  Entries: Map<string,HostData>
                    FirstGap: int option
                    FirstEntry: int option} with
        static member None = {Entries=Map.empty; FirstGap=None; FirstEntry=None}
        static member ToLineMap x =
            (Map.empty,x.Entries)
            ||> Map.fold(fun m site hd ->
                m |> Map.addListItem hd.LineIndex site
            )
            |> Map.map(fun _ v ->
                v |> List.sortBy(fun site ->
                    x.Entries.[site].NameIndex
                )
            )
            
            
let readHosts (FilePath hostpath) =
    File.ReadAllLines hostpath
   
let buildState lines =
    (HostState.None,lines |> List.indexed)
    ||> List.fold(fun state (i,line) ->
        match line with
        | Comment _ -> state
        | Empty ->
            match state.FirstGap with
            // ignore a gap if it is in the first 2 lines
            | None when i > 1 -> {state with FirstGap = Some i}
            | None
            | Some _ -> state
        | Entry (Ok (ip,names)) ->
            let m =
                (state.Entries,names |> List.indexed)
                ||> List.fold(fun entries (j,name) ->
                    match entries |> Map.tryFind name with
                    | Some v ->
                        if v.Addr <> ip then failwithf "Found 2 entries for same host '%s' ('%s','%s')" name v.Addr ip
                        entries
                    | None ->  entries |> Map.add name {Addr=ip;LineIndex=Some i;NameIndex=j}
                )
            {state with Entries = m}
        | Entry (Error ()) ->
            let context = lines.[max 0 (i - 2) .. min (lines.Length - 1) (i+2)]
            context.Dump()
            failwithf "Error reading line %i" i
    )
let addBlocking (hostState:HostState) =
    // add the new site entries after the first blank line or opening comments end
    (hostState,sites)
    ||> List.fold(fun hostState site ->
        
        match hostState.Entries |> Map.tryFind site with
        // entry already exists
        | Some v ->
            if v.Addr <> "127.0.0.1" then failwithf "Site to block '%s' is already mapped to %s" site v.Addr
            hostState
        | None ->
            let entries = hostState.Entries |> Map.add site { Addr="127.0.0.1";LineIndex=None;NameIndex=0 }
            { hostState with Entries = entries }
    )
let lines = 
    readHosts hosts
    |> List.ofSeq
lines
|> buildState
|> addBlocking
|> fun e ->
    let result = ResizeArray lines
    let lm = HostState.ToLineMap e
    let formatter = sprintf "127.0.0.1 %s"
    match lm |> Map.tryFind None, e.FirstGap, e.FirstEntry with
    | Some toAdd, Some i, _ ->
        Some (toAdd,i+1)
    | Some toAdd, _, Some i ->
        Some (toAdd, i) // probably could use a blank line before items
    | _ -> None
    |> Option.iter (fun (items,i) ->
            result.InsertRange(i,""::(items |> List.map formatter) |> List.rev)
    )
    let (FilePath fp) = hosts
    File.WriteAllLines(fp,Array.ofSeq result)
    result
|> Dump
|> ignore