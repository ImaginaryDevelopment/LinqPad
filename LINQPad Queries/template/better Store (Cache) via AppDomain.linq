<Query Kind="FSharpProgram" />


// https://forum.linqpad.net/discussion/13/caching-things-between-execution-runs-but-same-linqpad-process-same-open-tab
let getData,setData,clearData =
    let getKey keyBase = sprintf "Cache.%s" keyBase
    let getD (key:string) f =
        match getKey key |> AppDomain.CurrentDomain.GetData with
        | null -> None
        | x -> x :?> 't |> Some
    let setF key value = 
        AppDomain.CurrentDomain.SetData(getKey key,value)
    getD, setF, fun key -> setF key null
    
let getSetByName<'T> key = 
    let key = sprintf "%s:%s" key (typeof<'T>.Name)
    let getter ():'T option =
        AppDomain.CurrentDomain.GetData key
        |> function
            | null -> None
            | :? option<'T> as x -> x
            | :? 'T as x -> Some x
            | x ->
                let t = x.GetType().Name
            
                sprintf "cached value (%s) was not of the expected type %s option" t typeof<'T>.Name
                |> invalidOp
    let setter (x:'T option) =
        AppDomain.CurrentDomain.SetData(key, box x)
    getter,setter
            
    
let getElapsed,setElapsed : ((unit -> TimeSpan)*(TimeSpan option -> unit)) =
    let getE,setE = getSetByName<TimeSpan> "Elapsed"
    (fun () -> getE() |> Option.defaultValue Util.ElapsedTime), fun (ts:TimeSpan option) -> setE ts
()
    
getElapsed().Dump("Elapsed")

setElapsed (TimeSpan(9000L) |> Some)
    