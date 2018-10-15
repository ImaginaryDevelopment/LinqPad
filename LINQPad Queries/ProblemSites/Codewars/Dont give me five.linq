<Query Kind="FSharpExpression" />

let dontGiveMeFive startValue endValue = 
    [startValue..endValue]
    |> Seq.filter(string>> (fun x -> x.Contains"5") >>not)
    |> Seq.length

dontGiveMeFive 1 9