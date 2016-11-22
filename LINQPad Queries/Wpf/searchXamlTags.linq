<Query Kind="FSharpProgram" />

// search all xaml files for all unique attributes (search for a previously used tag or attribute, that we don't recall the name of

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
    
let getComboBoxes (namespaces:XNamespace list) (xe:XElement) = 
    getDescendants namespaces.[0] "ComboBox" xe
    
let getRadCombos (namespaces:XNamespace list) (xe:XElement) = 
    if namespaces.Length > 1 then
        //namespaces.Dump("namespaces")
        namespaces 
        |> Seq.filter (fun ns -> ns.ToString() |> containsI "telerik")
        |> Seq.map (flip getDescendants "RadComboBox")
        |> Seq.map (fun f -> f xe)
        |> Seq.collect id
        //getDescendants namespaces.[1] "RadComboBox" xe
    else Seq.empty

let raw =
    Directory.GetFiles(searchRoot, "*.xaml", SearchOption.AllDirectories)
    |> dumpLengthT "Files"
    |> Seq.map (fun f-> f, XDocument.Load f)
    |> Seq.map (fun (f,doc) -> f,doc, getDocNamespaces doc)
    |> Seq.map (fun (f, doc, namespaces) -> f,doc,namespaces, getComboBoxes namespaces doc.Root |> Seq.append (getRadCombos namespaces doc.Root) |> List.ofSeq) // doc.Descendants(namespaces.[0] + "ComboBox") 
    |> Seq.map (fun (f,doc,namespaces,elements) -> elements, namespaces, f, (if elements.Length < 1 then doc.ToString() else null))
    |> Seq.filter (fun (es,_,_,_) -> es.Length > 0)
let mapped =
    raw
    |> Seq.map (fun (es, ns, f, docText) -> es, Util.OnDemand("Details", fun () -> ns,f,docText))
    |> Seq.map (fun (es,d) -> es |> Seq.map (fun c -> c,d))
    |> Seq.collect id
    |> Seq.groupBy (fun (c,_) -> c.Name.LocalName)
    |> Seq.map (fun (k, items) -> k, items |> Seq.map (fun (c,d) -> c.Attributes(), c,d))
mapped
|> Dump
|> ignore