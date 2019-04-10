<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>Microsoft.FSharp.Core</Namespace>
</Query>

// poe calc pseudo resist total

let getText () = System.Windows.Forms.Clipboard.GetText()
let flip f x y = f y x
let maybeCache useCache keyOpt f = 
    match useCache,keyOpt with
    |true, None -> Util.Cache(Func<_>(f))
    |true, Some k -> Util.Cache(Func<_>(f),k)
    | false, _ -> f()
let (|RMatch|_|) (p:string) (x:string) = 
    let r = Regex.Match(x,p)
    if r.Success then
        Some r
    else None
let (|RMatches|_|) (p:string) (x:string) = 
    let r = Regex.Matches(x,p)
    if r.Count > 0 then
        Some r
    else None
let text = getText()

let (|Elemental|_|) =
    function
    |"Cold"
    |"Lightning"
    |"Fire" as x -> Some x
    | _ -> None

let expandCaps (amount:int) caps = 
    caps
    |> List.unfold(
        function
        | [] -> None
        | "all Elemental"::tl -> 
            let item = (amount,"Fire")
            let tl = ["Cold";"Lightning"]@tl
            (item,tl) |> Some
        | x::tl -> ((amount,x),tl) |> Some
    ) 
let foldElemental= 
    List.fold(fun elTotal (t,x) ->
            match t with
            | Elemental _ -> elTotal + x
            | _ -> elTotal
        ) 0
let foldTotal items = 
    let x = 
        items
        |> Seq.map snd
        |> Seq.sum
    let elem = foldElemental items
    items@["Elemental",elem;"Total",x]
text
|> function
    | RMatches @"([+-]\d+)% to (?:(all Elemental|Fire|Cold|Chaos|Lightning)(?: and )?)+ Resistances?" r ->
        r
        |> Seq.cast<Match>
        |> Seq.map (fun m -> (m.Groups.[1].Value |> int), m.Groups.[2].Captures |> Seq.cast<Capture> |> Seq.map(fun c -> c.Value) |> List.ofSeq)
        |> Seq.collect(fun (amount,caps) -> expandCaps amount caps)
        |> Seq.groupBy snd
        |> Seq.map(fun (t,x) -> t, x |> Seq.map fst |> Seq.sum)
        |> List.ofSeq
        |> List.sortBy (fst >> (=) "Chaos")
        |> foldTotal
        |> Dump
        |> ignore
    | txt -> printfn "Could not match"


[text].Dump("item")