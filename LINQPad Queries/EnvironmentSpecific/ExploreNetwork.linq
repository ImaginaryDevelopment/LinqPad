<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Namespace>System.DirectoryServices</Namespace>
</Query>

// walk local computers 
// based on https://stackoverflow.com/a/21377649/57883
let asDE = Seq.cast<DirectoryEntry>

let walkComputer (x:DirectoryEntry) = 
    let dc = DumpContainer()
    // returns nothing or the list of users and groups
    let getChildren =
        async{ 
            let v = x.Children 
            // this being done as a let instead of in the setter below, enables it to be async properly
            let items = asDE v |> Seq.map (fun d -> d.Name,d.SchemaClassName) |> List.ofSeq
            dc.Content <- items
        }
    let t = Async.StartAsTask(getChildren)
    x.Name,x.SchemaClassName, dc, t
let walkComputers (x:DirectoryEntry seq) = 
    x
    |> Seq.filter(fun x -> x.Name <> "Schema") 
    |> Seq.map walkComputer
let walk () =
    let root = new DirectoryEntry("WinNT:")
    root.Children
    |> asDE // workgroups
    |> Seq.map(fun de -> de.Name, de.Children |> asDE |> walkComputers ) // computers
    |> Dump
    |> ignore
    
walk()