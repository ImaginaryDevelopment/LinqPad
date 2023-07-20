<Query Kind="FSharpExpression" />

// swoks spawn https://www.reddit.com/r/avorion/comments/65gxtu/swoks/
// 415 and 380 from center
let inbounds distance = distance >= 380.0 && distance <= 415.0
let getDistance x y = Math.Pow(float x, 2.0) + Math.Pow(float y, 2.0) |> Math.Sqrt

let x = 222
let y = 344


let getLimit title value =
    // guess about limit
    // hack: there are 2 bands, just concern ourselves with NE quadrant of current game
    let values = [0..500] |> List.map(fun v -> v, getDistance v value) |> List.filter (snd >> inbounds)
    match List.length values with
    | 0 -> "*"
    | 1 -> string values.[0]
    | _ -> 
        let v1,d1 = values.[0]
        let v2,d2 = values.[values.Length - 1]
        sprintf "%s - [%i..%i] (distances %.0f..%.0f)" title v1 v2 d1 d2
    
    
let distance = getDistance x y    
distance, inbounds distance, getLimit "y" x, getLimit "x" y