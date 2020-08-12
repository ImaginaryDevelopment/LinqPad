<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Paket.Core</NuGetReference>
  <Namespace>System.Net.Http</Namespace>
</Query>

// paket lock file diffing
// https://github.com/fsprojects/Paket/blob/master/src/Paket.Core/PaketConfigFiles/LockFile.fs

let debug = false

let getKeys (m:Map<_,_>) = m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

type DiffResult<'diff> =
    |Equal
    |Unequal of 'diff
//type JoinDiff<'tkey, 't, 'diff> =
//    | Left of 'tkey * 't
//    | Diff of 'tkey * DiffResult<'diff>
//    | Right of 'tkey * 't
    
type Joined<'key, 't when 'key : comparison> = {
    Left: Map<'key,'t>
    Right: Map<'key,'t>
    Both: Map<'key,('t * 't)>
}
let getKV (m:Map<_,_>) k = k,m.[k]
let joinMaps(l:Map<_,_>,r):Joined<_,_>=
    let l',r' = getKeys l, getKeys r
    let getKV (m:Map<_,_>) k = k, m.[k]
    let both = Set.intersect l' r'
    let leftOnly = l' - both
    let rightOnly = r' - both
    let lx = leftOnly |> Seq.map(getKV l) |> Map.ofSeq
    {
        Left= lx
        Right= rightOnly |> Seq.map(getKV r) |> Map.ofSeq
        Both= both |> Seq.map(fun k -> k, (l.[k],r.[k])) |> Map.ofSeq
    }
type DiffMap<'key,'t,'diff when 'key : comparison> = {
    Left: Map<'key,'t>
    Right: Map<'key,'t>
    Both: Map<'key, 'diff>
}
    
let joinDiff fDiff (x:Joined<_,_>):DiffMap<_,_,_>=
    {
        Left = x.Left
        Right= x.Right
        Both = x.Both |> Map.map(fun _ (l,r) ->
            fDiff (l,r)
        )
    }

type GroupName = GroupName of string
type PkgName = PkgName of string
type PkgVersion = PkgVersion of string
type Package = PkgName * PkgVersion
//type PackageDiff = JoinDiff<PkgName,Package,PkgVersion*PkgVersion>
//type GroupDiff = JoinDiff<GroupName,Package list, PackageDiff list>
type PackageMap = Map<PkgName,PkgVersion>
    
let packageJoin (l:Map<PkgName, PkgVersion>, r):Joined<PkgName,PkgVersion>=
    joinMaps(l,r)
    
let comparePackage (PkgName x) (PkgVersion lv,PkgVersion rv) =
    if lv = rv then Equal else Unequal (x,(lv,rv))
    
let diffPackages (l,r) =
    packageJoin(l,r)
    |> joinDiff(fun (l,r) ->
        if l = r then
            Equal
        else
            Unequal(l,r)
    )
    
let compareGroup (l:PackageMap,r:PackageMap) =
    diffPackages(l,r)
    
let compareLocks (l: Map<GroupName,PackageMap>, r) =
    joinMaps (l,r)
    |> joinDiff(fun (l,r) ->
        compareGroup (l,r)
    )
let l,r =
    let package n v = PkgName n, PkgVersion v
    let gn x = GroupName x
    let argu = package "argu" "1.0"
    let fsharp v = package "fsharp.core" v
    Map[
        gn "main", Map [ argu; fsharp "4.7"; package "missing1" "2.0"]
        gn "build", Map [ package "missing2" "1.0"]
    ],Map[
        gn "main", Map[ argu; package "onlyr" "1.2"; fsharp "4.6"]
    ]
    
compareLocks (l,r)
//|> Dump
|> ignore
module Helpers =
    /// Calls ToString on the given object, passing in a format-string value.
    let inline stringf format (x : ^a) = 
        (^a : (member ToString : string -> string) (x, format))
    let splitLines (x:string) =
        let value = x.Split("\n",StringSplitOptions.None)
        if debug then (value.Length, value.[0..100]).Dump("split")
        value
    let inline getCacheOrF name f = 
        Util.Cache(Func<_> f, name)
    let tryDump f x =
        try
            f x
        with ex ->
            (ex.Message, x).Dump()
            reraise()
            
    type [<Measure>] bytes
    module Map =
        let mapBoth f x = x |> Map.toSeq |> Seq.map(fun (k,v) ->  f k v) |> Map.ofSeq
    
    module Memory = // https://cseducators.stackexchange.com/questions/4425/should-i-teach-that-1-kb-1024-bytes-or-1000-bytes/4426
        let inline memoryFormat x = stringf "N1" x
        let getUsedMemory () = GC.GetTotalMemory(false)
        let suffix = [ "B";"KB";"MB";"GB";"TB";"PB";"EB"]
        let formatBytes multiplier (x:int64<bytes>) =
        
            let formatValue i v = sprintf "%s%s" (memoryFormat v) suffix.[i]
            let x' = double x
            [0..suffix.Length - 1]
            |> Seq.choose(fun i -> 
                let p = pown multiplier i 
                let v = double x' / double p
                let result =
                    if v >= 1.0 then
                        sprintf "%s%s" (memoryFormat v) suffix.[i]
                        |> Some
                    elif i = 0 then
                        formatValue i x'
                        |> Some
                    else None
                result
            )
            |> Seq.last
    let systemMult =
        let _kilobyte = 1000L
        let _kibibyte = 1024L
        _kilobyte // 1024 or 1000 for the purposes of this usage?

open Helpers


module LockFiles =
//let parseLock lines = Paket.LockFileParser.Parse lines
    let parseLock lines = Paket.LockFile.Parse(null,lines)
    let getLockHttp =
        let hc = new HttpClient()
        let fetch (url:string) = hc.GetStringAsync url |> Async.AwaitTask |> Async.RunSynchronously
        fun (url:string) ->
            let value = getCacheOrF url (fun () -> fetch url)
            if debug then (value.Length,value.[0..200]).Dump("Sample")
            value
            
let inline displayMap fkey fvalue m = 
    m |> Map.mapBoth (fun k v -> fkey k, fvalue v)
    
let displayDiffMap f (x:DiffMap<_,_,_>) =
    [
        if Map.exists (fun _ _ -> true) x.Left then
            yield "left", x.Left |> displayMap string string
        yield "Both", f x.Both //  |> Map.filter(fun _ -> function | Equal -> false | _ -> true ) |> Map.mapBoth (fun k v -> string k, string v)
        if Map.exists (fun _ _ -> true) x.Right then
            yield "right",  x.Right |> displayMap string string
    ]
    
    
let displayDiffResultMap (x:Map<_,DiffResult<_>>) =
    x |> Map.filter(fun _ -> function | Equal -> false | _ -> true ) |> Map.mapBoth (fun k v -> string k, string v)
    
let inline displayInequalities (x:DiffMap<_,_,DiffResult<_>>) =
    
    [
        if Map.exists (fun _ _ -> true) x.Left then
            yield "left", x.Left |> displayMap string string
        yield "Both", displayDiffResultMap x.Both //  |> Map.filter(fun _ -> function | Equal -> false | _ -> true ) |> Map.mapBoth (fun k v -> string k, string v)
        if Map.exists (fun _ _ -> true) x.Right then
            yield "right",  x.Right |> displayMap string string
    ]
    
let inline displayRecMap (fkey:'key -> _) f (x:DiffMap<'key,_,DiffMap<_,_,_>>) =
    [
        if Map.exists (fun _ _ -> true) x.Left then
            yield "left", x.Left |> displayMap fkey string
        yield "Both", x.Both |> Map.mapBoth (fun k v -> fkey k, f v) |> displayMap string string//  |> Map.filter(fun _ -> function | Equal -> false | _ -> true ) |> Map.mapBoth (fun k v -> string k, string v)
        if Map.exists (fun _ _ -> true) x.Right then
            yield "right",  x.Right |> displayMap fkey string
    ]
    
let displayComparison (x:DiffMap<GroupName,PackageMap,DiffMap<PkgName,_,_>>) = 
    x |> displayRecMap (function | GroupName gn -> gn) (fun x ->
        displayInequalities x
    )
    
let sampleLock1 = "https://github.com/fsprojects/Paket/raw/master/paket.lock"
// hope it is different!
let sampleLock2 =
    //"https://github.com/fsprojects/Paket/raw/bugfixmerge/paket.lock"
    "https://github.com/fable-compiler/ts2fable/raw/master/paket.lock"
let parseState1 =
    sampleLock1    
    |> LockFiles.getLockHttp
    |> splitLines
    |> tryDump LockFiles.parseLock
let parseState2 =
    sampleLock2
    |> LockFiles.getLockHttp
    |> splitLines
    |> tryDump LockFiles.parseLock
    
let prepareLock (l:Paket.LockFile) =
    l.Groups |> Map.mapBoth (fun gn _ -> GroupName gn.CompareString, l.GetTopLevelDependencies gn |> Map.mapBoth(fun pn pv -> PkgName pn.CompareString, PkgVersion pv.Version.AsString))
let lockDiff = compareLocks(prepareLock parseState1, prepareLock parseState2)    
lockDiff
|> displayComparison
|> Dump
|> ignore

let mem = GC.GetTotalMemory(false)
mem * 1L<bytes>
|> Memory.formatBytes systemMult
|> Dump
|> ignore
