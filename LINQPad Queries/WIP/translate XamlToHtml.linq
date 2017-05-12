<Query Kind="FSharpProgram" />

// WIP: translate xaml to html (loosely obviously)


[<AutoOpen>]        
        
module ReverseDumper2 = 
    // reverse dumper with raw html support
    // linqpad helper (for when you can't .Dump(description), but still want the html)
    // also of note: there is an extra open div tag, because the ToHtmlString closes one that it didn't open
    let titleize t (x:obj) = 
        let objHtml = Util.ToHtmlString(enableExpansions=true, noHeader=true, objectsToDump= ([ x ] |> Array.ofList))
        let result = sprintf """<table class="headingpresenter">
        <tr>
        <th class="">%s</th>
        </tr>
        <tr>
        <td class="headingpresenter"><div>%s</td>
        </tr>
        </table>"""                 t objHtml
        Util.RawHtml result
    type DumpType = 
        | Raw of string
        | DumpObj of obj
    // consider taking in obj, downcast to string, check for "<" starting the string to autodetect a raw html string? nah.    
    let dumpReverse :  DumpType -> unit =
        let dc = DumpContainer()
        dc.Dump() |> ignore
        (fun o -> 
            let o = 
                match o with
                | Raw s -> Util.RawHtml s
                | DumpObj o -> o
                
            match dc.Content with
            | :? (obj list) as items -> List.Cons(o,items)
            | _ -> [ o ]
            |> fun content -> dc.Content <- content
        )
    let dumptRev t x = 
        titleize t x |> DumpObj |> dumpReverse
        x
    // override/overwrite existing dump methods, no direct dumpReverse calls required
    type System.Object with
        member x.Dump() = printfn "override hit!";  x |> DumpObj |> dumpReverse |> ignore
        member x.Dump description = printfn "override2 hit! %s" description; x |> titleize description |> DumpObj |> dumpReverse |> ignore
        
        
module Xml = 
    open System.Xml.Linq
    let nsNone = XNamespace.None
    let toXName (ns:XNamespace) name =
        ns + name
    let getAttributeNames (xe:XElement) = xe.Attributes() |> Seq.map (fun a -> a.Name.LocalName)
    let getElement1 n (e:XElement) =
        e.Element n
        |> Option.ofObj
    // leaving out a 'getElement' as it will likely be (samples in code comments below):
    //    let getElement = toXName nsNone >> getElement1
    //    let getElement = toXName doc.Root.Name.Namespace >> getElement1
    let getElements1 n (e:XElement) = e.Elements n
    // point-free Seq.filter argument
    let isNamed n (e:XElement) = e.Name = n
    let getElementsAfter n (e:XElement) = 
        e
        |> getElements1 n
        |> Seq.skipWhile (isNamed n >> not)
        |> Seq.skip 1

    let getAttribVal name (e:XElement) = 
        nsNone + name
        |> e.Attribute
        |> Option.ofObj
        |> Option.map (fun a -> a.Value)
    let setAttribVal name value (e:XElement) = 
        e.SetAttributeValue(nsNone + name, value)

    let getDescendants n (e:XElement) = e.Descendants n

    let attribValueIs name value e =
        e
        |> getAttribVal name
        |> Option.toObj
        |> (=) value
    let isElement (e:XNode) = 
        match e with
        | :? XElement -> true
        | _ -> false
        
    // when you |> string an XElement, normally it writes appends the namespace as an attribute, but this is normally covered by the root element
    let stripNamespaces (e:XElement):XElement=
        // if the node is not XElement, pass through
        let rec stripNamespaces (n:XNode): XNode =
            match n with
            | :? XElement as x -> 
                let contents = 
                    x.Attributes() 
                    // strips default namespace, but not other declared namespaces
                    |> Seq.filter(fun xa -> xa.Name.LocalName <> "xmlns")
                    |> Seq.cast<obj> 
                    |> List.ofSeq 
                    |> (@) (
                        x.Nodes() 
                        |> Seq.map stripNamespaces 
                        |> Seq.cast<obj> 
                        |> List.ofSeq
                    )
                XElement(nsNone + x.Name.LocalName, contents |> Array.ofList) :> XNode
            | x -> x
        stripNamespaces e :?> XElement

        
//        e.nodes
//        XElement(nsNone + e.Name.LocalName, content = e.Nodes)
    ()


let target = @"c:\tfs\practicemanagement\dev\practicemanagement\practicemanagement\patientdatagrid\appointmentaddeditpopup.xaml"
let makeTable fElement (ns:XNamespace) (e:XElement) = 
    
    let columns = e.Elements(ns + "Grid.ColumnDefinitions")
    let rows = 
        e.Elements()
        |> Seq.map fElement
        |> delimit Environment.NewLine
    let table = sprintf "<table><thead>%s</thead><tbody>%s</tbody>" (sprintf "<tr>%s</tr>" (columns |> Seq.map (fun _c ->"<td></td>") |> delimit String.Empty) )
    
    table
    
    
    
let rec mapElement (e:XElement) : string = 
    match e.Name.LocalName with
    | "Grid" ->
        printfn "mapping grid"
        makeTable mapElement e.Name.Namespace e
    | n -> failwithf "unknown element type %s" n
let doc = XDocument.Load target
doc.Root.Elements()
|> dumptRev "root"
|> Seq.skipWhile(fun e -> e.Name.LocalName.EndsWith ".Resources")
|> Seq.head
|> mapElement
|> Dump
