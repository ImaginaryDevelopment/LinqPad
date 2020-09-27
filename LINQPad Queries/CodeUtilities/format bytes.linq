<Query Kind="FSharpProgram" />

// format a value of bytes in MB, GB, KB, etc..
// https://stackoverflow.com/questions/1242266/converting-bytes-to-gb-in-c
let debug = false
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
        // return a Tuple where (A, B) (both present if they have a match)
    module List =
        let forceJoin b a =
            let b = Set.ofList b
            let a = Set.ofList a
            let x = Set.intersect a b
            let diffa = a - b
            let diffb = b - a
            diffa - x
            |> Seq.map (fun a' -> Some a', None)
            |> Seq.append (x |> Seq.map (fun x' -> (Some x', Some x')))
            |> Seq.append (diffb - x |> Seq.map (fun b' -> None, Some b'))
            |> List.ofSeq 
    module Map =
        let forceJoin b a =
            let b' = Map.toList b
            let a' = Map.toList a
            List.forceJoin b' a'
            |> Map.ofList
            
    type [<Measure>] bytes
    
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
            
open Helpers
let systemMult =
    let _kilobyte = 1000L
    let _kibibyte = 1024L
    _kilobyte // 1024 or 1000 for the purposes of this usage?
    
type [<Measure>] KB // 10xx bytes
type [<Measure>] MB // 10xx KB
let kb x = x * 1L<bytes/KB> * systemMult

let mb (x:int64<MB>):int64<bytes>= x * (pown systemMult 2)* 1L<bytes/MB>
(
    let kb (x:int64) = x, if x < 1024L then 1 else 2 
    [  
        1L,0
        kb 1L
        kb 5L
        kb 1024L
        kb 1100L
        kb 9999L
        
        //1L<bytes>, Memory.memoryFormat 1 |> sprintf "%sB"
        //1L<bytes>*systemMult, Memory.memoryFormat 1 |> sprintf "%sKB"
        //kb 100L<KB>, Memory.memoryFormat 100 |> sprintf "%sKB"
        //mb 1L<MB>, Memory.memoryFormat 1 |> sprintf "%sMB"
        //System.Int64.MaxValue * 1L<bytes>, Memory.memoryFormat |> sprintf "%sPB"
    ]
)
|> List.map(fun (value,i) ->
    let expected =
        let i,v =
            if value >= systemMult && i + 1 < Memory.suffix.Length then i + 1, double value / double systemMult else i,double value
        let v = Memory.memoryFormat v
        sprintf "%s%s" v Memory.suffix.[i]
    let bytes = (if i = 0 then value else value * pown systemMult i) * 1L<bytes>
    let values = Memory.formatBytes systemMult bytes
    let actual = values |> Seq.last
    Util.HighlightIf((value,expected,values),(fun _ -> expected <> actual))
)
|> Dump
|> ignore

Memory.formatBytes systemMult 76430026L<bytes>
|> Dump