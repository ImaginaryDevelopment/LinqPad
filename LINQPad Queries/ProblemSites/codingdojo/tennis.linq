<Query Kind="FSharpExpression" />

// http://codingdojo.org/kata/Tennis/
let dump x = x.Dump(); x
let scores = [0;15;30;40]
let getPoint =
    // for testing that deuce works
    let getPointDeuce () =
        let x = ref 0
        let points = [true;false;true;false;true;false;true;false;true;true]
        fun () ->
            x := !x+1
            printfn "Getting point from %i" (!x - 1)
            points.[!x - 1]
    let getPointRandom() =
        let r = Random()
        r.NextBool
    //    getPointRandom()
    getPointDeuce()

let makeResult score =
    match score with
    | x when x < 4 -> scores.[x]
    | _ -> 40
let getWinner p1 p2 =
    if p1 > p2 + 1 && p1 > 3 then
        Some "p1"
    elif p2 > p1 + 1 && p2 > 3 then
        Some "p2"
    else None
    
(None,0,0)
|> Seq.unfold(fun (winner,p1,p2) ->
    match winner with
    | Some _ ->
        printfn "Found finish!"
        None
    | _ ->
        let pt = getPoint()
        let p1,p2 =
            if pt then
                p1+1, p2
            else p1, p2+1
        let state = getWinner p1 p2,p1,p2
        Some(state,state)
)
|> List.ofSeq
|> Seq.map(fun (w,p1,p2) ->
    w,p1,p2,makeResult p1, makeResult p2
)
|> dump
|> Seq.last
