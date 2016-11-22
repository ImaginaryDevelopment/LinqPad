<Query Kind="FSharpExpression" />

// find all bound properties in xaml
// join code-behind looking for props that can be moved to the view model without danger of anything breaking at runtime, and no xaml refactoring needed

let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\PayerProfileAddEditPopup.xaml"
let codeBehind = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\PayerProfileAddEditPopup.xaml.cs"

let xamlReg = "Path=\"(.*?)\"|{Binding ([\w.]+)[, }]"
let codeReg = "public\s+([\w<>]+)\s+(?:partial)?\s*(?:[\w.]+\.)?(\w+)(?<!\()" 
let beforeOrSelf (delimiter:string) (s:string) = if s.Contains(delimiter) then s.Substring(0, s.IndexOf(delimiter)) else s
let afterOrSelf (delimiter:string) (s:string) = if s.Contains(delimiter) then s.Substring(s.IndexOf(delimiter) + delimiter.Length) else s
let regMatches pattern (s:string) = Regex.Matches(s,pattern, RegexOptions.Multiline)

let boundProps =
    File.ReadAllText path
    |> regMatches xamlReg
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
                        |> Seq.map (fun s -> (if s.Contains "." then s.Substring(s.IndexOf "." + 1) else s),s
        )
    )

let notAPropKeywords = ["class"; "void"; "override"; "sealed"]
let publicMembers = 
    File.ReadAllText codeBehind
    |> regMatches codeReg
    |> Seq.cast<Match>
    |> Seq.filter (fun m -> notAPropKeywords |> Seq.exists (fun np -> np = m.Groups.[1].Value) |> not)
    |> Seq.map (fun m -> m.Groups.[2].Value, m.Groups.[1].Value, m.Groups.[0].Value)
    |> Seq.sortBy (fun (n,_t,_m) -> n)

let membersInBoth = 
    query {
        for (name,memberType, matchText) in publicMembers do
        where (ExtraTopLevelOperators.query
                    {
                        for (pName,_) in boundProps do
                            exists(name = pName)
                    })
        select (name, memberType, matchText)
    }
    
let missingMembers =
    publicMembers
    |> Seq.filter (fun m -> membersInBoth |> Seq.contains m |> not)
missingMembers, publicMembers, boundProps