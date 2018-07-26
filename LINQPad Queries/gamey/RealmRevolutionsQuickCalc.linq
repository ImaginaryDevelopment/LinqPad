<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day

let rate = 4.25e14<Bp/s>
let owned = 1.98e8<Bp>
let target = 2.59e8<Bp>


let sToMin (x:float<s>):float<min> = x / 60.0<s/min>
let minToHr (x:float<min>):float<hr> = x / 60.0<min/hr>
let hrToDay (x:float<hr>):float<day> = x / 24.0<hr/day>
let sci = float >> fun x -> x.ToString("0.##e-0")
let inline timeToTarget rate owned target = 
    (target - owned) / rate
let rec formatTime (x:float<s>) = TimeSpan(0,0,int x).ToString()



let ttt = timeToTarget rate owned target
(formatTime ttt, DateTime.Now.AddSeconds(float ttt).ToShortTimeString())
|> Dump
|> ignore

Math.Pow(2.,9.).Dump("Pow")