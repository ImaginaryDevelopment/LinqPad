<Query Kind="FSharpProgram" />

// find unreferenced language translations

// open any .xaml where the root element is ResourceDictionary

//let samplePath = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement.UI.Localization\Cultures\DefaultResources.xaml"

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

let getElementWithoutChildren (xe:XElement) = 
    let x = XElement(xe.Name)
    x.Add(xe.Attributes() |> Array.ofSeq)
    x
asdfasfdasdfasdfasdfsfd
is not including all the namespaces, missing xmlns:system currently    
let getDocNamespaces (xd:XDocument) = 
    //xd.Root |> getElementWithoutChildren |> dumpt "root"
    //xd.Root |> getAttributeNames |> dumpt "AttribNames" |> ignore
    seq{
        yield! xd.Root.Attributes() |> Seq.filter(fun a -> a.Name.NamespaceName = "http://www.w3.org/2000/xmlns/") (* |> dumpt "xmlns" *) |> Seq.map (fun a -> XNamespace.Get(a.Value))
    }
    |> List.ofSeq 
    |> uncurry List.Cons xd.Root.Name.Namespace
    |> Seq.distinct
    |> List.ofSeq
    //|> dumpt "namespaces"
let getDescendants (ns:XNamespace) name (xe:XElement) = 
    xe.Descendants(ns + name)
    
let getStringResources (namespaces:XNamespace list) (xe:XElement) = 
    getDescendants namespaces.[1] "String" xe
    
let raw =
    Directory.GetFiles(searchRoot, "*.xaml", SearchOption.AllDirectories)
    |> dumpLengthT "Files"
    |> Seq.map (fun f-> f, XDocument.Load f)
    |> Seq.map (fun (f,doc) -> f,doc, getDocNamespaces doc)
    |> List.ofSeq
    //|> Seq.map (fun (f, doc, namespaces) -> f,doc,namespaces, getComboBoxes namespaces doc.Root |> Seq.append (getRadCombos namespaces doc.Root) |> List.ofSeq) // doc.Descendants(namespaces.[0] + "ComboBox") 
    //|> Seq.map (fun (f,doc,namespaces) -> elements, namespaces, f, (if elements.Length < 1 then doc.ToString() else null))
    //|> Seq.filter (fun (es,_,_,_) -> es.Length > 0)
let rds = 
    raw
    |> Seq.filter (fun (f,doc,namespaces) -> doc.Root.Name.LocalName = "ResourceDictionary")
    |> Seq.map (fun (f, doc, namespaces) -> f,doc,namespaces, getStringResources namespaces doc.Root |> List.ofSeq)
    |> Seq.map (fun (f,doc,namespaces,elements) -> elements, namespaces, f, (if elements.Length < 1 then doc.ToString() else null))
    
rds |> dumpt "raw"
|> ignore