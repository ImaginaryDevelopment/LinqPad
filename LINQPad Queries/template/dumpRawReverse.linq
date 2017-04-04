<Query Kind="FSharpProgram" />

let dumpt t x = x.Dump(description=t); x
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

let dumpReverse,endDump :  (DumpType -> unit)*(unit->unit) =
    let toDump = ResizeArray<DumpType>()
    
    
    toDump.Add, 
        (fun () -> 
            toDump
            |> List.ofSeq
            |> List.rev
            |> Seq.iter (
                function 
                | Raw s -> Util.RawHtml s
                | DumpObj o -> o 
                >> Dump 
                >> ignore
            )
        )

"abc"
|> box
|> DumpType.DumpObj |> dumpReverse

let testElement = XElement(XNamespace.None + "Test")
testElement |> box |> DumpType.DumpObj
|> dumpReverse

testElement
|> titleize "Hello dumpTest"
|> DumpType.Raw
|> dumpReverse

endDump()