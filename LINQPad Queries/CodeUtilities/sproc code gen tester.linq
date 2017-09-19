<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll">C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll</Reference>
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\MacroRunner.exe</Reference>
</Query>

let dc = new TypedDataContext()

//let cgsm = {CodeGeneration.DataModelToF.CodeGenSettingMap.TargetProject }
printfn "starting"
// put the result at the top of the page.
let dumpResult =
    let dc = DumpContainer()
    dc.Dump()
    // linqpad helper (for when you can't .Dump(description), but still want the html)
    // also of note: there is an extra open div tag, because the ToHtmlString closes one that it didn't open
    let titleize t (x:obj) :obj = 
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
    fun t (x:obj) ->
        dc.Content <- titleize t x
printfn "dc created?"
let ssm : CodeGeneration.DataModelToF.CodeGenSprocSettingMap= 
    {
        SprocBlacklist = [  "sp_alterdiagram"
                            "sp_creatediagram"
                            "sp_dropdiagram"
                            "sp_helpdiagramdefinition"
                            "sp_helpdiagrams"
                            "sp_renamediagram"
                            "sp_upgraddiagrams"
                        ]
        GenerateSprocInputRecords = true
    }
let sb = StringBuilder()
let appendLine x = Enumerable.Repeat("    ", x) |> delimit String.Empty |> fun spc text -> (sprintf "%s%s" spc text |> sb.AppendLine |> ignore)
CodeGeneration.DataModelToF.generateSprocComponent (dc.Connection.ConnectionString |> MacroRunner.AdoHelper.Connector.CreateCString) "TestNamespace" appendLine ssm
sb.AppendLine(" ------------------------------------------------------------------------") |> ignore
printfn "finished!"
dumpResult "text" <| sb.ToString()
sb.ToString().Dump()
