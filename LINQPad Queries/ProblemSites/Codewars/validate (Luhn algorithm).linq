<Query Kind="FSharpExpression" />

let validate (str:string) =
    let reduceDigits x = if x > 9 then x - 9 else x
    let toNum (c:char) = string c |> System.Int32.Parse
    
    let doubleIt (c:char) = toNum c |> (*) 2
        
    let chars = str.Replace(" ","").ToCharArray()
    chars
    |> Array.mapi(fun i c -> if chars.Length % 2 = 0 && i % 2 = 0 then doubleIt c elif chars.Length % 2 = 1 && i % 2 = 1 then doubleIt c else toNum c)
    |> Array.map reduceDigits
    |> Array.reduce(+)
    |> fun x -> x % 10 = 0
        
//validate "18 40 10 18"
[   "1714"
    "12345"
    "891"
    "18 40 10 18"
]
|> List.map validate 