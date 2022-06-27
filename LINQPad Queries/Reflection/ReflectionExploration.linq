<Query Kind="FSharpProgram" />

module ResizeArray =
    let init i f =
        Seq.init i f
        |> ResizeArray
    
let (|IsSeq|_|) (t:Type) = 
    t.GetInterfaces() |> Seq.choose(fun t ->
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<seq<_>> then Some("iseq",t.GenericTypeArguments) else None
    )
    |> Seq.tryHead

let asSeq (x:obj) =
    match x with
    | :? seq<obj> as items -> Some items
    | :? seq<int> as items -> items |> Seq.cast<obj> |> Some
    | :? seq<char> as items -> items |> Seq.cast<obj> |> Some
    | _ -> None
    
let rnd = Random()
type FInit<'t> = int -> (int -> 't) -> 't seq

let gen0to2<'t> fName (fInit:FInit<'t>) (f:unit-> 't) =
    [0..2]
    |> List.map(fun cnt ->
        fName cnt, fInit cnt (fun _ -> f()) |> box
    )
    
let inline genPrim<'t> (f:unit -> 't) (fs: (string*FInit<'t>) list) =
    let name = typeof<'t>.Name

    [
        [name, f() |> box]
        fs
        |> List.collect(fun (colName,fInit) ->
            gen0to2<'t> (sprintf "%s %s(%i)" name colName) fInit f
        )
    ]
let genList<'t> name (f:_ -> 't) =
    [0..2]
    |> List.map(fun i ->
        sprintf "%s list(%i)" name i, List.init i f :> seq<'t>
    )
let inline asSeqInit<'t,'tCol when 'tCol :> seq<'t>> (fInit:int -> (int -> 't) -> 'tCol) i (f:int -> 't) =
    fInit i f :> seq<'t>
    
let genned = 
    let genInt() = rnd.Next(0,10)
    let genString () =  match rnd.Next(0,9) with | 0 -> null | 1 -> "" | x -> Seq.replicate x (string x) |> String.concat ""
    let genFloat () = rnd.Next(0,10) |> float
    let genBool () = rnd.Next(0,1) = 1
    [
        genPrim<int> genInt [ "seq", Seq.init; "list", asSeqInit List.init; "array", asSeqInit Array.init]
        genPrim<string> genString ["seq", Seq.init; "list", asSeqInit List.init; "array", asSeqInit Array.init ]
        genPrim<float>  genFloat ["seq", Seq.init; "list", asSeqInit List.init ]
        genPrim<bool> genBool ["seq", Seq.init; "list", asSeqInit List.init ]
    ]
    |> List.concat
    |> List.concat
genned
|> List.map(fun (n,o) -> n, o.GetType().IsPrimitive,  o.GetType() |> (|IsSeq|_|), o |> asSeq) // , o |> (|EnumerableT|_|))
|> Dump
|> ignore