<Query Kind="FSharpProgram" />

// find all xaml tags of x type, for searching out attribute presence/values, bindings etc.
// not going to account for contents at this time?

type Attr = Attr of string
    with 
        member x.Value = match x with | Attr y -> y
        override x.ToString() = x.Value
        member x.ToDump() = x.Value
type AttrValue = AttrValue of string
    with
        member x.Value = match x with |AttrValue  y -> y
        override x.ToString() = x.Value
        member x.ToDump() = x.Value
        
type FilePath = FilePath of string
    with
        member x.Value = match x with |FilePath y -> y
        override x.ToString() = x.Value
        member x.ToDump() = x.Value
//let getValue x = (^a:(member Value:_) x)
    

let targetDir = @"C:\tfs\practicemanagement\trunk"
let ignoreNs = "http://www.w3.org/2000/xmlns/"
let getNamespaceName (xa:XAttribute) = xa.Name.NamespaceName
let invertMaplist (x:('parent * Map<'key,'value>) list) : Map<'key,Map<'value,'parent list>> =
    x
    |> Seq.fold(fun invertedMap (parent,pMap) ->
        pMap
        |> Map.fold(fun invertedMap (k:'key) (v:'value) ->
            // take in the new valueMap and current state of the inverted map that is building
            let addInvert (valueMap:Map<'value, 'parent list>) invertedMap =
                Map.add k valueMap invertedMap
            let addParent (valueMap:Map<'value, 'parent list>) =
                let parents =
                    valueMap
                    |> Map.tryFind v
                    |> fun parents -> defaultArg parents List.empty
                Map.add v (parent::parents) valueMap
                
            match Map.tryFind k invertedMap with
            | None ->
                let valueMap = Map[v,[parent]]
                addInvert valueMap invertedMap
            | Some valueMap ->
                let valueMap = addParent valueMap
                addInvert valueMap invertedMap
        ) invertedMap
    ) Map.empty
let rec getAttributesByTag (xe:XElement) = 
    let getTagWithAttrib (xe:XElement) = (Attr xe.Name.LocalName, xe.Attributes() |> Seq.map(fun a -> AttrValue a.Name.LocalName) |> Seq.distinct |> List.ofSeq)//, a.Name.NamespaceName))
    let itemsSeq = 
        seq {
            yield getTagWithAttrib xe
            for xe in xe.Elements() do
                yield! getAttributesByTag xe
        }
    itemsSeq
 
let xamls = Directory.EnumerateFiles(targetDir, "*.xaml", SearchOption.AllDirectories)
let p = Process.GetCurrentProcess()
p.WorkingSet64.Dump("working set")

let filesWithAttrMaps =
    xamls
    |> Seq.map(fun x -> 
        FilePath x, XElement.Load x |> getAttributesByTag |> Map.ofSeq
    )
    |> List.ofSeq
    |> fun x ->
        p.WorkingSet64.Dump("working set now")
        x
let invertedMap = invertMaplist filesWithAttrMaps

invertedMap.Dump("inverted")
filesWithAttrMaps.Dump()

