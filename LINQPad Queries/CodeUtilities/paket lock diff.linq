<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Paket.Core</NuGetReference>
  <Namespace>System.Net.Http</Namespace>
</Query>

// paket lock file diffing
// https://github.com/fsprojects/Paket/blob/master/src/Paket.Core/PaketConfigFiles/LockFile.fs

let debug = false

let dump<'t> (x:'t) =
    x
    #if !true // #if LINQPAD doesn't work // also #if true doesn't appear to work!
    |> Dump
    |> ignore
    #else
    |> printfn "%A"
    #endif

let getKeys (m:Map<_,_>) = m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

type DiffResult<'t,'diff> =
    |Equal of 't
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
    Both: Map<'key, 'diff>
    Right: Map<'key,'t>
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
let unwrapGN = function | GroupName x -> x
type PkgName = PkgName of string
let unwrapPN = function | PkgName x -> x
type PkgVersion = PkgVersion of string
let unwrapPV = function | PkgVersion x -> x
type Package = PkgName * PkgVersion
//type PackageDiff = JoinDiff<PkgName,Package,PkgVersion*PkgVersion>
//type GroupDiff = JoinDiff<GroupName,Package list, PackageDiff list>
type PackageMap = Map<PkgName,PkgVersion>
    
let packageJoin (l:Map<PkgName, PkgVersion>, r):Joined<PkgName,PkgVersion>=
    joinMaps(l,r)
    
let comparePackage (PkgName x) (PkgVersion lv,PkgVersion rv) =
    if lv = rv then Equal lv else Unequal (x,(lv,rv))
    
let diffPackages (l,r) =
    packageJoin(l,r)
    |> joinDiff(fun (l,r) ->
        if l = r then
            Equal l
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
|> (fun x -> if debug then dump x else ())

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
        let mapkv f x = x |> Map.toSeq |> Seq.map(fun (k,v) ->  f k v) |> Map.ofSeq
        let mapBoth fk fv x = x |> Map.toSeq |> Seq.map(fun (k,v) -> fk k, fv v) |> Map.ofSeq
    
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
            
    
let displayDiffMap fkey fvalue fdiff (x:DiffMap<_,_,_>) =
    let inline displayMap fvalue m = 
        m |> Map.mapBoth fkey fvalue
    [
        if Map.exists (fun _ _ -> true) x.Left then
            yield "left", x.Left |> displayMap fvalue
        yield "Both", x.Both |> displayMap fdiff
        if Map.exists (fun _ _ -> true) x.Right then
            yield "right",  x.Right |> displayMap fvalue
    ]
    
type PackageDiff = DiffMap<PkgName,PkgVersion,DiffResult<PkgVersion,PkgVersion*PkgVersion>>

let displayDiffResult fValue (x:DiffResult<_,_>) =
    match x with
    | Equal x -> sprintf "%A" <| fValue x
    | Unequal (l,r) -> sprintf "%s <> %s" (fValue l) (fValue r)

let displayComparison (x:DiffMap<GroupName,PackageMap,PackageDiff>) = // DiffMap<PkgName,PkgVersion,DiffResult<PkgVersion*PkgVersion>>>) = 
    let fValue x = x |> Map.mapBoth unwrapPN unwrapPV
    x
    |> displayDiffMap unwrapGN fValue (
        fun y ->
            y |> displayDiffMap unwrapPN unwrapPV (displayDiffResult unwrapPV) |> Map.ofSeq |> Map.map (fun _ v -> sprintf "%A" v)
    )
    
let sampleLock1 = "https://github.com/fsprojects/Paket/raw/master/paket.lock"
// hope it is different!
let sampleLock2 =
    //"https://github.com/fsprojects/Paket/raw/bugfixmerge/paket.lock"
    //"https://github.com/fable-compiler/ts2fable/raw/master/paket.lock"
    "https://github.com/fsprojects/FSharp.Formatting/raw/master/paket.lock"
    
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
    l.Groups |> Map.mapkv (fun gn _ -> GroupName gn.CompareString, l.GetTopLevelDependencies gn |> Map.mapkv (fun pn pv -> PkgName pn.CompareString, PkgVersion pv.Version.AsString))
    
let lockDiff = compareLocks(prepareLock parseState1, prepareLock parseState2)    

lockDiff
|> displayComparison
|> dump

let mem = GC.GetTotalMemory(false)
mem * 1L<bytes>
|> Memory.formatBytes systemMult
|> dump
