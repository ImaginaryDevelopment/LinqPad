<Query Kind="FSharpExpression" />

// find all bound properties in xaml

let reg = "Path=\"(.*?)\"|{Binding ([\w.]+)[, }]"
let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\PayerProfileAddEditPopup.xaml"

let beforeOrSelf (delimiter:string) (s:string) = if s.Contains(delimiter) then s.Substring(0, s.IndexOf(delimiter)) else s
let afterOrSelf (delimiter:string) (s:string) = if s.Contains(delimiter) then s.Substring(s.IndexOf(delimiter) + delimiter.Length) else s
let regMatches pattern (s:string) = Regex.Matches(s,pattern)

File.ReadAllText path
|> regMatches reg
|> Seq.cast<Match>
//|> LINQPad.Extensions.Dump
|> Seq.map (fun m -> if m.Groups.[1].Success then m.Groups.[1].Value else m.Groups.[2].Value)
|> Seq.distinct
|> Seq.sort
//|> Seq.groupBy (fun s -> if s.Contains(".") then s.Substring(0,s.IndexOf(".")) else s)
|> Seq.groupBy (beforeOrSelf ".")
|> Seq.map (fun (k, values) -> 
                k, 
                    values 
                    |> Seq.filter(fun v -> v <> k) 
                    |> Seq.map (fun s -> (if s.Contains(".") then s.Substring(s.IndexOf(".") + 1) else s),s
    )
)
