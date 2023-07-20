<Query Kind="FSharpExpression" />

let times = [60,"second";60,"minute"; 24,"hour";7,"day"]
let timeDisplay (seconds:float) =
    ((0,seconds, 1), times)
    
    // doesn't stop keeps going into hours
    ||> Seq.fold(fun (i,time,divisorTotal) (next,_unitName) ->
        if divisorTotal > 0 && int time >= next && times.Length > i + 1 then
            let dtNext = divisorTotal * next
            let nextState = (i+1, time / float next, dtNext)
                //(i,time,float next,dtNext,nextState).Dump(_unitName)
            nextState
        else (i,time,0)
    )
    |> fun (i,v,_) -> 
        v, snd times.[i],sprintf "%.2f %s" v (snd times.[i])
[
    59
    60
    61
    3599
    3600
    3601
]
|> Seq.map(fun seconds ->
    seconds, timeDisplay (float seconds)
)
//        if divisorTotal > 0 then
//            let dtNext = divisorTotal * next
//            //(int time,next, int time > next).Dump()
//            if int time > next && times.Length > i + 1 then
//                let nextState = (i+1, time / float next, dtNext)
//                //(i,time,float next,dtNext,nextState).Dump(_unitName)
//                nextState
//            else (i,time,0)
//        else (i,time,0)
//