<Query Kind="FSharpExpression" />

let factorial i =
    let rec fact n x =
        match n with
        | 0 -> 1
        | 1 -> x
        | _ -> fact (n-1) (x*n)
    fact i 1
let swap (arr:'a array) i j = [| for k in 0..(arr.Length-1) -> if k = i then arr.[j] elif k = j then arr.[i] else arr.[k] |]

let rec permutation (k:int,j:int) (r:'a array) =
    if j = (r.Length + 1) then r
    else permutation (k/j+1, j+1) (swap r (j-1) (k%j))

let permutations (source:'a array) = seq { for k = 0 to (source |> Array.length |> factorial) - 1 do yield permutation (k,2) source }
let inline isFactor x i = float x % (float i) = 0.
let factors x = 
    seq { 
        for i=2 to x - 1 do 
            if isFactor x i then
                yield i
        }
let inline gcf (x:int) (y:int) = 
    seq{ 
        let upper = int (Math.Round(Math.Sqrt(float(Math.Max(x,y))) + 1.))
        upper.Dump("upper")
        for i=2 to upper do
            if isFactor x i && isFactor y i then yield i
    }
    |> Seq.fold(fun state i -> if i > state then i else state) 1
    
let reduce a b = 
    let gcf = gcf a b
    sprintf "%i / %i" (a/gcf) (b/gcf)
// 26*26*26*26*10*10    

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
    
let digitArranges = permutations [| 8;6;6;6;3;3;3|] |> Seq.distinct |> Seq.length

let childrenDrive = comb 6 [1..17] |> Seq.length

let showReductionWork a b = 
    let gcf = gcf a b
    sprintf "%i / %i" a b,float a/ float b, reduce a b, float(a/gcf) / float(b/gcf)
    
//let ownerpeople = // did not finish reading q
//    let a = 11600+2700+30
//    let b = 1880+4500+100
//    showReductionWork a (b+a)
//ownerpeople
let men = 420+320+300+170
let women = 10+60+70+80
let y = men + women

showReductionWork women y

let theSeq = [ 1;1;1;0;0;0;0;0;0;0]
let possible = 
    comb 3 theSeq
    |> Seq.length
    
let totalSuccesses= 
    comb 3 theSeq 
    |> Seq.filter (fun l -> match l with |[0;0;0] -> true |_ -> false)
    |> Seq.length

    
theSeq |> Seq.length,possible,showReductionWork (totalSuccesses ) possible

