<Query Kind="FSharpExpression" />

// find all xaml tags of x type, for searching out attribute presence/values, bindings etc.
// not going to account for contents at this time?

let targetDir = @"C:\tfs\practicemanagement\trunk"
let ignoreNs = "http://www.w3.org/2000/xmlns/"
let getNamespaceName (xa:XAttribute) = xa.Name.NamespaceName

let rec getAttributesByTag (xe:XElement) = 
    let getTagWithAttrib (xe:XElement) = (xe.Name.LocalName, xe.Attributes() |> Seq.map(fun a -> a.Name.LocalName) |> Seq.distinct)//, a.Name.NamespaceName))
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

xamls
|> Seq.map(fun x -> 
    x, XElement.Load x |> getAttributesByTag |> Map.ofSeq
)
|> List.ofSeq
|> fun x ->
    p.WorkingSet64.Dump("working set now")
    x