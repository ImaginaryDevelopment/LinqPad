<Query Kind="FSharpProgram" />

[<Measure>] type m
[<Measure>] type hr
[<Measure>] type min
[<Measure>] type km
[<Measure>] type s
[<Measure>] type dc
type Capture = {Height:float<m>; Velocity:float<m/dc>}
let g:float<m/dc^2> = 9.81<m/s^2> * 0.1<s/dc> * 0.1<s/dc>
let t' = 1.0<dc>
let getMeasures =
    fun (v:float<km/hr>) -> 
        let mV:float<m/dc>=  v * 1000.0<m/km>  / 60.0<min/hr> / 60.0<s/min> / 10.0<dc/s>
        printfn "%f -> %f (g=%f)" v mV g
        mV
    >> fun mV ->
        let initial = {Height=0.0<m>;Velocity=mV} 
        initial
        |> Seq.unfold(fun x ->
            let acc = g * t'
            let h1 = x.Height + x.Velocity * t' - 0.5 * acc * t'
            if h1 > 0.0<_> then
                let v1 = x.Velocity - acc
                let r = {Height=h1;Velocity = v1}
                Some (r,r)
            else
                None
        )
        |> Seq.takeWhile(fun x -> x.Height > 0.<_>)
        |> Seq.append [ initial]
        |> Seq.mapi (fun i x -> i,x)
        |> Seq.maxBy snd
        |> fst

let maxBall(v:int):int =
    getMeasures (float v * 1.<km/hr>)
//|> List.head
maxBall 15
|> Dump
|> ignore