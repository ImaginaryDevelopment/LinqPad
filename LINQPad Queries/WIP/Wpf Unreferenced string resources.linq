<Query Kind="FSharpProgram" />

// find unreferenced language translations
// works, but doesn't account for xaml comments as falsely in use, nor translations accessed from code-behind, or elsewhere in the project

let searchRoot = @"C:\TFS\PracticeManagement\dev\PracticeManagement"
let getName (xe:XElement) = xe.Name
let flip f y x = f x y
let uncurry f x y = f (x,y)
let dumpt (t:string) x = x.Dump(t); x
let dumpLengthT t items = items |> Seq.length |> dumpt t |> ignore; items
let getAttributeNames (xe:XElement) = 
    xe.Attributes() |> Seq.map (fun a -> a.Name.LocalName)
    
let dumpReverse :  (obj) -> unit =
    let dc = DumpContainer()
    dc.Dump() |> ignore
    (fun o -> 
        match dc.Content with
        | :? (obj list) as items -> List.Cons(o,items)
        | _ -> [ o ]
        |> fun content -> dc.Content <- content
    )

let getDescendants (xe:XElement) = 
    xe.Descendants()
type StringResource = { Key:string; Value:string; NamespaceName:string;LocalName:string}    
let getStringResources (xe:XElement) = 
    let srNs = XNamespace.Get(@"http://schemas.microsoft.com/winfx/2006/xaml")
    let sNs = XNamespace.Get(@"clr-namespace:System;assembly=mscorlib")
    getDescendants xe
    |> Seq.filter(fun x -> x.Name.LocalName = "String" && x.Name.Namespace = sNs)
    |> Seq.map (fun x -> {Key=x.GetAttribValOrNull(srNs + "Key"); Value=x.Value; NamespaceName= x.Name.NamespaceName;LocalName= x.Name.LocalName})
//    |> dumpt "descendants"
    
let getSrsFromDoc (doc:XDocument) = 
    // after testing come back and make this return an option?
    if doc.Root.Name.LocalName <> "ResourceDictionary" then
        failwith "not a resource dictionary"
    getStringResources doc.Root |> List.ofSeq
    
let getSRsFromPath (path:string) = 
    let doc = XDocument.Load path
    path, doc, getSrsFromDoc doc |> List.ofSeq
type RDStrategy = 
    | OnlyRdRoots
    | AnyRd
let getAllXamlPathsUnder searchRoot = 
    Directory.GetFiles(searchRoot, "*.xaml", SearchOption.AllDirectories)
let getAllRdStrings rds searchRoot = 
    getAllXamlPathsUnder searchRoot
    |> Seq.choose (fun f ->
        let doc = XDocument.Load f
        let isRd (x:XElement) = x.Name.LocalName = "ResourceDictionary"
        match rds with
        | OnlyRdRoots -> 
            if isRd doc.Root then
                Some [f,getSrsFromDoc doc,doc]
            else None
        | AnyRd -> 
            [             
                if isRd doc.Root then
                    let srs = getSrsFromDoc doc
                    yield! srs
                let childRdSrs = 
                    doc.Root.Descendants()
                    |> Seq.filter isRd
                    |> Seq.map getStringResources
                    |> Seq.concat
                yield! childRdSrs

            ]
            |> function
                | [] -> None
                | x -> Some([f,x,doc])
    )
    |> List.ofSeq
    
    
let samplePath = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement.UI.Localization\Cultures\DefaultResources.xaml"    
let _tryGetSRsFromSample () = 
    getSRsFromPath samplePath
    |> fun (path,doc, sr) -> (sr, if List.isEmpty sr then doc else null)
    |> Dump
    |> ignore
let tryGetAllRDStrings() = 
    getAllRdStrings AnyRd searchRoot 
    |> Seq.concat
    |> List.ofSeq
//    |> Dump
//    |> ignore
let rdStrings = Util.Cache(fun () -> 
    tryGetAllRDStrings()
    |> Seq.map (fun (_,sr,_) -> sr)
    |> Seq.concat
)
// this doesn't account for commented out blocks =(
// walk the xaml files that aren't resource dictionaries, and pull in all string refs
// example: <TextBlock Text="{lang:Translate PracticeManagement_MainWindow_Schedule}" VerticalAlignment="Center" HorizontalAlignment="Center" />
let getAllKeyRefs () =
    getAllXamlPathsUnder searchRoot
    |> Seq.map (fun p -> p,File.ReadAllText p)
    |> Seq.choose(fun (p,text) -> 
        Regex.Matches(text,"{\s*lang\s*:\s*Translate\s*(\w+)\s*}")
        |> Seq.cast<Match>
        |> List.ofSeq
        |> function
            | [] -> None
            | x -> Some (p,x |> Seq.map (fun m -> m.Groups.[1].Value))
    )

//    |> Map.ofSeq

let usedKeys = Util.Cache(fun () -> getAllKeyRefs())

//|> Seq.filter(fun (_,text) -> rdStrings |> Seq.exists(fun sr -> text.Contains(sr.Key)))
rdStrings
|> Seq.filter (fun sr -> usedKeys |> Seq.map (snd) |> Seq.concat |> Seq.exists(fun k -> sr.Key = k) |> not)
|> Dump
|> ignore