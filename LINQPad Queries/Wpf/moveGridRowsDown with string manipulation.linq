<Query Kind="FSharpProgram" />

// move grid column up for appointmentaddedit, using mutation instead of creating new element for copy/paste on this one (vs. moveGridColumns.linq)

let is<'T> x = match box x with | :? 'T -> true | _ -> false
module Seq = 
    let ofType<'T> (items: _ seq) : 'T seq = items |> Seq.cast<obj> |> Seq.filter is<'T> |> Seq.cast<_>
module ReverseDumper2 = 
    // reverse dumper with raw html support
    // linqpad helper (for when you can't .Dump(description), but still want the html)
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
        result
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
    let Dump = tee (DumpObj >> dumpReverse)
    let dumpt t x = x |> tee (fun x -> titleize t x |> Raw |> dumpReverse)
    type System.Object with
        member x.Dump() =  x |> Dump |> ignore
        member x.Dump description = x |> dumpt description |> ignore
        
open ReverseDumper2

let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\AppointmentAddEditPopup.xaml"

let doc = XDocument.Load(path, LoadOptions.PreserveWhitespace ||| LoadOptions.SetLineInfo)

let ns = 
    // root element isn't in default namespace
    doc.Root.CreateNavigator().GetNamespacesInScope(XmlNamespaceScope.ExcludeXml)
    |> Seq.map (|KeyValue|)
    |> Seq.find(fun (ns,v) -> String.IsNullOrWhiteSpace ns)
    |> snd
    |> XNamespace.Get
    |> dumpt "default namespace"
ns.Dump()
[<AutoOpen>]
module XmlHelper =
    let nsNone = XNamespace.None
    let getElement name (e:XElement) = 
        ns + name
        |> e.Element
        |> Option.ofNull
    let getElements (e:XElement) = e.Elements()
    let getElements1 name (e:XElement) =     
        e.Elements (ns + name)
    let isNamed name (e:XElement) = 
        ns + name
        |> (=) e.Name 
    let getElementsAfter name (e:XElement) = 
        e
        |> getElements
        |> Seq.skipWhile (isNamed name >> not)
        |> Seq.skip 1
        
    let getAttribVal name (e:XElement) = 
        XNamespace.None + name
        |> e.Attribute
        |> Option.ofObj
        |> Option.map (fun a -> a.Value)
    let setAttribVal name value (e:XElement) = 
        e.SetAttributeValue(nsNone + name, value)
        
    let getDescendants name (e:XElement) = 
        ns + name
        |> e.Descendants
        
    let attribValueIs name value e =
        e
        |> getAttribVal name
        |> Option.getOrDefault null
        |> (=) value
let moveDownStart, moveDownAmount = 5, 3
let getLeadingComments (xe:XElement) = 
    xe.NodesBeforeSelf()
    |> List.ofSeq
    |> List.rev
    |> Seq.takeWhile (is<XElement> >> not)
    |> Seq.ofType<XComment>
    |> List.ofSeq
    
let getGrid() = 
    let shouldUpdate = flip (>=) moveDownStart
    doc.Root
    |> getElement "Grid"
    |> Option.bind (getElement "Grid")
    |> Option.bind (getElement "Grid")
    |> Option.map (getElementsAfter "Grid.RowDefinitions")
    //|> dumpt "about to filter"
    |> Option.map (Seq.filter(getAttribVal "Grid.Row" >> Option.getOrDefault "0" >> int >> shouldUpdate))
    |> Option.get
    |> Seq.iter (fun rowItem -> 
        let oldRowNumber = rowItem |> getAttribVal "Grid.Row" |> Option.getOrDefault "0" |> int
        let newRowNumber = oldRowNumber |> (+) moveDownAmount 
        rowItem |> setAttribVal "Grid.Row" (newRowNumber |> string)
        rowItem
        |> getLeadingComments
        |> Seq.filter(fun xc -> xc.Value.Contains " row")
        |> Seq.iter (fun (rowComment:XComment) -> 
            let commentRowNumber = rowComment.Value |> dumpt "operating on commentRowNumber" |> after "row " |> int
            if shouldUpdate oldRowNumber && commentRowNumber = oldRowNumber  then
                rowComment.Value <- rowComment.Value |> replace (commentRowNumber |> string) (newRowNumber |> string)
        )
    )
//    |> Seq.skip 1
//    |> Seq.head
//    |> fun e -> e.Name.Namespace, e.Name.LocalName
    
getGrid()
doc.ToString(SaveOptions.OmitDuplicateNamespaces)
|> after "Grid x:Name=\"Details\""
|> after "Grid.RowDefinitions"
|> after "Grid.RowDefinitions"

|> before "</Grid>"
|> replace "SelectedValue=\"" "\r\n    SelectedValue=\""
|> replace "ItemsSource=\"" "\r\n    ItemsSource=\""
|> Dump
|> ignore
ns.Dump();