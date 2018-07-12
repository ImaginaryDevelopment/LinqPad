<Query Kind="FSharpProgram" />

// file metrics (for honing a single file instead of finding files that need work)

type FileSummary = {
    Filename:string
    LineCount:int
    Nonspaces:int
    DoubleQuotes:int
    MaxLineLength:int
    MaxLineIndex:int
    LongLines:int
} with
    // how do we deal with multiple enumeration allowances/guarantees vs. making a copy of the data for no good reason 
    // signature cannot specify
    static member Create fullPath (reader: _ -> string IReadOnlyList) =
        let lines = reader fullPath
        let text = lines |> String.concat String.Empty
        let lengthMetrics = lines |> Seq.mapi(fun i x -> i, String.length x) |> List.ofSeq
        let longestLineIndex = lengthMetrics |> Seq.maxBy snd |> fst
        lines.[longestLineIndex].Dump()
        {   
            Filename= fullPath
            LineCount= lines |> Seq.length
            Nonspaces= text |> Seq.filter (fun x-> Char.IsWhiteSpace x <>true) |> Seq.length
            DoubleQuotes= text |> Seq.filter ((=) '"') |> Seq.length
            MaxLineLength= lengthMetrics |> Seq.map snd |> Seq.max
            MaxLineIndex= longestLineIndex
            LongLines= lengthMetrics |> Seq.filter(snd >> (>) 80) |> Seq.length
        }


FileSummary.Create @"c:\tfs\practicemanagement\trunk\Pm.Dal\DataAccess.fs" (fun x -> upcast File.ReadAllLines x)
|> Dump
|> ignore