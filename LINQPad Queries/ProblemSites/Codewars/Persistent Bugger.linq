<Query Kind="FSharpExpression" />

let persistence (x:int) =
    let partInt (t:string) =
        t
        |> Seq.map (string >> System.Int32.Parse)
        |> Seq.reduce(*)
        
    let rec f (i,x) =
        let t = string x
        if t.Length = 1 then
            i,x
        else
            let y = partInt t
            f(i+1,y)
    f (0,x)
    |> fst
 
getPersistence 999
