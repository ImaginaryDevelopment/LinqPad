<Query Kind="FSharpExpression" />

// read/replace all :r's in a pre or post deploy step, merge them together, see if the line number lines up with the error message
// line 34


let (|StartsWithI|_|) s1 (toMatch:string) = if toMatch <> null && toMatch.StartsWith(s1, StringComparison.InvariantCultureIgnoreCase) then Some () else None
let maybeLimitIt limitOpt items = 
    match limitOpt with
    | Some x -> items |> Seq.take x
    | None -> items
    
let getSubScripts limitOpt path = 
    let result =
        seq{
            let text = 
                File.ReadLines path
                |> maybeLimitIt limitOpt 
                |> Array.ofSeq // @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql"
            yield (path,text |> Seq.mapi(fun lineNumber line -> lineNumber,line)|> Seq.groupWhen(snd >> stringEqualsI "GO")|> Seq.map (Seq.filter (fun (_,l) -> not <| stringEqualsI "GO" l && not <| String.IsNullOrWhiteSpace l)) |> Seq.concat |> List.ofSeq)
        }
        //|> dumpt "subScripts"
        |> List.ofSeq
    result
let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\Scripts\Post-Deployment\Script.PostDeployment.sql"
let relativeToPath fp = Path.GetDirectoryName path |> fun x -> Path.Combine(x,fp)
getSubScripts (Some 50) path
|> Seq.map snd
|> Seq.head // currently there are no "GO" statements in the postdeployment
|> Seq.map (fun (_,line) ->
    seq{
    match line with
    | StartsWithI ":r " ->
        yield! 
            try
                line 
                |> after ":r "
                |> relativeToPath
                |> getSubScripts None
                |> Seq.map snd 
                |> Seq.concat 
                |> Seq.map snd
            with ex ->
                ex.Data.Add("line", line)
                reraise()
    | _ -> yield line
    }
)
|> Seq.concat
|> delimit Environment.NewLine
