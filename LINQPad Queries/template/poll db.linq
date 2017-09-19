<Query Kind="FSharpProgram" />

// poll a db operation for any change dump the new values
open System.Threading
open System.Threading.Tasks
let dc = new TypedDataContext()



// polling for x

let pollIt (title:string) fSleep f = 
    let mutable result = f() // dc can be closed over outside here
    let dc = DumpContainer(result)
    dc.Dump(title)
    let mutable hash = (box result).GetHashCode()
    let cts = new CancellationTokenSource()
    let t = new Task(fun () -> 
        fSleep()
        while not cts.IsCancellationRequested do
            let nextResult = f()
            let nextHash = (box nextResult).GetHashCode()
            if nextHash <> hash then
                printfn "Hash Changed"
                result <- nextResult
                hash <- nextHash
            // even if the hash didn't change, update the ui, just in case the hashcode calc is bad
            dc.Content <- nextResult
            fSleep()
    )
    t.Start()
    t,cts
    
let fSleep () =     Thread.Sleep 500
let t1,cts1 = 
    pollIt "Count of PayerProfileInfo Rows" fSleep (fun () -> 
        dc.PayerProfileInfo.Count()
    )


Util.ReadLine()
|> ignore
cts1.Cancel()
t1.Wait()
cts1.Dispose()
t1.Dispose()

