<Query Kind="FSharpExpression" />


// alternate version
let rec _toBase26Letters = 
    function 
    | x when x < 26 -> 
        a + x |> char |> string
    | x ->
        toBase26Letters (x % 26)
        |> string
        |> (+) (toBase26Letters (x / 26 - 1) )
        
// translated, and refactored from http://stackoverflow.com/a/26063181/57883            
let toBase26Letters x = 
    let rec c x  = if x<26 then int 'A'+x|>char|>string else (x/26-1|>c)+ c(x%26)
    c x
let trim (s:string) = s.Trim()
// 127
let codeLength = 
    """
    let rec c x  = if x<26 then int 'A'+x|>char|>string else (x/26-1|>c)+ c(x%26)
    """ 
    |> trim
    |> Seq.length
codeLength.Dump()
let a = int 'A'      

let fB26 (x:string) =
    let fPow len i =
        Math.Pow(26., len - 1 - i |> float)
        |> int
    
    let getValue len i c = 
        int c - a + 1 * fPow len i
    let f i = getValue x.Length i x.[i]
    [0 .. x.Length - 1]
    |> Seq.map f
    |> Seq.sum
    |> fun x -> x - 1
        
[0..30]
|> Seq.map toBase26Letters 
|> Seq.mapi (fun i x -> i,x, fB26 x)
   