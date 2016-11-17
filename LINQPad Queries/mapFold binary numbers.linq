<Query Kind="FSharpExpression" />

let makeTuple (x:int) = 
    x,Convert.ToString(x,2)

0
|> Seq.unfold(
    function
    | 0 as x -> Some (makeTuple x, 1)
    | x -> 
        let next = x * 2
        if next > x then
            Some(makeTuple x, next)
        else None
)
|> List.ofSeq
|> List.rev
//no idea how to use mapFoldBack here perhaps a flip is needed
|> List.mapFold(fun (maxLen:int) (x:int,s:string) -> 
        let len = Math.Max(s.Length,maxLen)
        ((x,s.PadLeft(len,'0')), len)
    ) 0
|> fst
|> List.rev
|> fun x -> x
    
