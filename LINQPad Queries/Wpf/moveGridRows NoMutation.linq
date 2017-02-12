<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
  <Namespace>System.Xml.Linq</Namespace>
</Query>

// issue: it adds namespaces to the entire output
// in this use case the new row definition (new row 6) was created, the rowdefinition comments were added/updated, but none of the row items in a grid were moved down yet.
let is<'T> x = match box x with | :? 'T -> true | _ -> false
module Seq = 
    let ofType<'T> (items: _ seq) : 'T seq = items |> Seq.cast<obj> |> Seq.filter is<'T> |> Seq.cast<_>
    //let mapForType<'T,'TItem when 'T :> 'TItem > (f: 'T -> _) (items: 'TItem seq) : 'TItem seq = items |> Seq.cast<obj> |> Seq.map (function | :? 'T as t ->  f t :> obj | x -> x) |> Seq.cast<'TItem>
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



module Tuple2 = // idea and most code taken from https://gist.github.com/ploeh/6d8050e121a5175fabb1d08ef5266cd7
    let replicate x = x,x
    // useful for Seq.mapi
    let fromCurry x y = (x,y)
    let curry f x y = f (x, y)
    // calling already defined function from outer namespace, instead of duplicating the functionality for consistency with gist
    let uncurry f (x, y) = uncurry f (x, y)
    let swap (x, y) = (y, x)
    let mapFst f (x, y) = f x, y
    let mapSnd f (x, y) = x, f y
    let extendFst f (x,y) = f (x,y), y
    let extendSnd f (x,y) = x, f(x,y)
    let optionOfFst f (x,y) =
        match f x with
        | Some x -> Some (x, y)
        | None -> None
    let optionOfSnd f (x,y) =
        match f y with
        | Some y -> Some (x,y)
        | None -> None
    // start Brandon additions
    let mapBoth f (x,y) = f x, f y

let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\Dialogs\AppointmentAddEditTelerik.xaml"
let doc = XDocument.Load( path, LoadOptions.PreserveWhitespace ||| LoadOptions.SetLineInfo)
let ns = doc.Root.Name.Namespace
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

// when you |> string an XElement, normally it writes appends the namespace as an attribute, but this is normally covered by the root element
let namespaces = doc.Root.Elements() |> Seq.head |> fun xe -> xe.CreateNavigator().GetNamespacesInScope(XmlNamespaceScope.ExcludeXml) |> Seq.map (|KeyValue|) |> Map.ofSeq

let moveRowsDown startIndex =
    doc.Root
    |> getElement "Style"
    |> Option.get 
    |> getElements1 "Setter"
    |> Seq.find (attribValueIs "Property" "Template")
    |> getElement "Setter.Value"
    |> Option.get
    |> getDescendants "Grid"
    |> Seq.head
    |> getElement "Grid"
    |> Option.get
    |> Tuple2.replicate
    |> Tuple2.mapFst (getElement "Grid.RowDefinitions" >> Option.get)
    |> Tuple2.mapFst (getElements1 "RowDefinition")
    |> Tuple2.mapSnd (getElementsAfter "Grid.RowDefinitions")
    |> fun (rds, items) -> 
        rds 
        |> Seq.mapi (fun i rd -> i, rd, 
                                    items 
                                    |> Seq.filter (fun rowItem -> 
                                        
                                        let rowItemValue = rowItem |> getAttribVal "Grid.Row" |> Option.getOrDefault "0" |> int
                                        if i < startIndex then
                                            i = rowItemValue
                                        // rowdefinition 6 is already created                                        
                                        elif i = startIndex then
                                            false
                                        else 
                                            i = rowItemValue + 1
                                    ) |> List.ofSeq
        )
    |> List.ofSeq
        
    |> dumpt "joined, not inserted"
    //|> dict
    |> List.map (fun (i, rd, items) -> 
        let formatRowComment index = sprintf " Add/edit Appointment dialog row %i " index
        let lastComment : XComment option Lazy = lazy( items |> Seq.last |> fun x -> x.NodesAfterSelf() |> Seq.ofType<XComment> |> Seq.tryHead)
        if i > startIndex then 
            items |> Seq.iter (i |> string |> setAttribVal "Grid.Row" )
        //update the comment node after the last rowItem here
        if i = startIndex - 1 then
            lastComment.Value
            |> Option.get
            |> fun xc -> xc.AddAfterSelf (XComment(formatRowComment 7))
        // not implemented, add comment before any items
        elif startIndex = 0 then
            ()
        if i > startIndex - 1 then
            match items with
            | [] -> ()
            | items -> 
                lastComment.Value 
                |> Option.iter (fun xc -> 
                    xc.Value <- i + 1 |> formatRowComment 
                )
        i,rd,items        
    
    )
// starting with rowItems Grid.Row = ?
let ``Grid.Row=`` = 8
moveRowsDown ``Grid.Row=``
|> Seq.head
|> tee (Dump >> ignore)
|> fun (_,rd,rows) -> 
    
    let allRows = 
        rows 
        |> Seq.head 
        |> fun row -> 
            let tail = row.NodesAfterSelf() |> List.ofSeq
            (row :> XNode) ::tail
        
    let parent = allRows  |> Seq.ofType<XElement> |> Seq.head |> fun xe -> xe.Parent
    // add declarations
    namespaces
    |> Map.toSeq
    |> Seq.filter (fst >> String.IsNullOrEmpty>> not)
    |> Seq.iter(fun (k,v) -> 
            parent.Add(XAttribute(XNamespace.Xmlns + k, v))
    )
    try
        allRows |> Seq.map(fun r -> r.GetType().Name) |> Seq.distinct |> dumpt "node types" |> ignore
        allRows 
        |> Seq.filter (function | :? XText -> false | _ -> true) 
        |> dumpt "allRows without whitespaces" 
        |> ignore
        rd.Parent.Dump("abc123")
    with _ -> ()
    parent.ToString(SaveOptions.OmitDuplicateNamespaces)
    |> string 
    |> fun x -> x.Dump("final result stripped?") 
    |> ignore