<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

let text = System.Windows.Forms.Clipboard.GetText()


System.Text.RegularExpressions.Regex.Replace(text, "Grid.Row=\"(\d+)\"", (fun r ->
    let nextValue = 
        r.Groups.[1].Value
        |> int
        |> (+) 1
    r.Value.Replace(r.Groups.[1].Value, nextValue |> string)
    
))