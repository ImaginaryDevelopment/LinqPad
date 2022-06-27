<Query Kind="FSharpProgram" />

let target = @"c:\tfs\practicemanagement\trunk\PracticeManagement\Dialogs\AppointmentAddEditTelerik.xaml"
let rreplace p r x =
    Regex.Replace(x,pattern=p,replacement=r)
type LineNumber = LineNumber of int
type RowDef = {RowDefStart:LineNumber}
type ChangeTarget = {Index:int;OldRow:int;NewRow:int;LineStart:int;LineStop:int;Lines:string[]}

module String =
    let contains d (x:string) = 
        x.Contains(d)
        
let lines = File.ReadAllLines target
let rdDict = 
    let rowStarts =

        (lines |> Seq.mapi(fun i l -> i,l),List.empty)
        ||> Seq.foldBack(fun (i,line:string) found -> 
            let trimmed = line.TrimStart()
            if trimmed.StartsWith("<!-- Add/edit Appointment row ") || trimmed.StartsWith("<!-- Add/edit Appointment dialog row") then
                {RowDefStart=LineNumber i}::found
            else found
        )
        |> List.ofSeq
    rowStarts
    |> Seq.unfold(fun rows ->
        match rows with
        | [] -> None
        | {RowDefStart=LineNumber i}::({RowDefStart=LineNumber j} as x)::rest ->
            // we have another item following
            Some((i,j - 1),x::rest)
        | {RowDefStart=LineNumber i}::rest ->
            // find the grid end
            let j =
                lines 
                |> Seq.skip i
                |> Seq.takeWhile (String.contains "</Grid>">>not)
                |> Seq.length
            Some ((i,i+j - 1),rest)
    )
    |> Seq.mapi(fun i (x,y) -> i, x+1,y+1,lines.[x..y])
    |> Seq.map(fun (i,x,y,lines) ->
        let oldRow = int <| Regex.Match(lines.[0],"\d+").Value
        let newRow = if oldRow = 18 then 6 elif oldRow > 5 then oldRow + 1 else oldRow
        let repl =
            lines
            |> Array.mapi(fun j x ->
                if j = 0 then
                    rreplace "row \d+" (sprintf "row %i" newRow) x
                else rreplace "Grid.Row=\"\d+\"" (sprintf "Grid.Row=\"%i\"" newRow) x
            )
            
        {   Index=i;LineStart=x;LineStop=y
            NewRow= newRow
            OldRow= oldRow;Lines=repl}
    )
    |> Seq.skipWhile(fun x -> x.OldRow = x.NewRow)
    |> Seq.sortBy(fun x -> x.NewRow)
    |> List.ofSeq
let startReplace = (rdDict |> Seq.map(fun x -> x.LineStart) |> Seq.min) - 1
let stopReplace = rdDict |> Seq.map(fun x ->x.LineStop) |> Seq.max
let middle = rdDict |> Seq.collect(fun x -> x.Lines) |> Array.ofSeq
startReplace.Dump()
//[| lines.[0..startReplace - 2];middle;lines.[stopReplace..] |]
//|> Array.collect id
middle
//|> fun lines -> File.WriteAllLines(target,lines)
|> Dump
|> ignore