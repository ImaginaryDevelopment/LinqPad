<Query Kind="FSharpProgram" />

let dumpt t x = x.Dump(description=t); x

module BasicReverseDumper = 
    let dumpReverse :  (obj) -> unit =
        let dc = DumpContainer()
        dc.Dump() |> ignore
        (fun o -> 
            match dc.Content with
            | :? (obj list) as items -> List.Cons(o,items)
            | _ -> [ o ]
            |> fun content -> dc.Content <- content
        )
        
        
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
        
        

// also worth noting:

let dc = DumpContainer()


dc.Dump();

let makeDiv t = sprintf "<div>%s</div>" t

// works
dc.Content<- Util.RawHtml( makeDiv "Hello world")
// also works
dc.Content <- [ Util.RawHtml("<div>Hello world</div>"); Util.RawHtml("<div>Hello world</div>")]

let makeXEl t = XElement(XNamespace.None + t)

open ReverseDumper2
dumpReverse (Raw "<div>Hello world</div>")
dumpReverse (DumpObj (XElement(XNamespace.None + "HelloXml")))
dumptRev "Hello tRev" (makeXEl "trev") |> ignore
makeDiv "yay html!" |> dumptRev "hello actual html" |> ignore
dumptRev "Hello list" [ makeXEl "listitem1"; makeXEl "listitem2"] |> ignore
dumpReverse (Raw "<div>Hello world2</div>")

