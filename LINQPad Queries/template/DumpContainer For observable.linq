<Query Kind="FSharpProgram" />

open System.Collections.ObjectModel
open System.Collections.Specialized
let dumpt (t:string) x = x.Dump(t); x
module Observables =
    

    let bindObsTToObsObj (obsCollection:ObservableCollection<'t>) = 
        let obsObj = ObservableCollection<obj>()
        obsCollection.CollectionChanged.Add (fun e ->
            match e.Action with
            |NotifyCollectionChangedAction.Add ->
                e.NewItems
                |> Seq.cast<obj>
                |> Seq.iter obsObj.Add
            |NotifyCollectionChangedAction.Move ->
                let oldIndex = e.OldStartingIndex
                let newIndex = e.NewStartingIndex
                obsObj.Move(oldIndex,newIndex)

            |NotifyCollectionChangedAction.Remove ->
                e.OldItems
                |> Seq.cast<obj>
                |> Seq.iter (obsObj.Remove>> ignore<bool>)
            |NotifyCollectionChangedAction.Replace ->
                e.NewItems
                |> Seq.cast<obj>
                |> Seq.zip (e.OldItems |> Seq.cast<obj>)
                |> Seq.iteri(fun i (oldItem,newItem) ->
                    
                    let obsObjItem = obsObj.[e.OldStartingIndex + i]
                    printfn "Replacing obsObjItem %A, with %A" obsObjItem newItem
                    (i,e, obsObj.ToList()).Dump()
                    assert (obsObjItem = oldItem)
                    obsObj.[e.OldStartingIndex + i] <- newItem
                )

            | NotifyCollectionChangedAction.Reset ->
                obsObj.Clear()
                if not <| isNull e.NewItems then
                    e.NewItems
                    |> Seq.cast<obj>
                    |> Seq.iter obsObj.Add
            | x -> failwithf "Case %A is unimplmented" x

        )
        obsObj

let (obs,obsObj,fUpdate) = 
    let createDc content = 
        let dc = DumpContainer()
        dc.Dump()
        dc.Content <- content
        dc
        
    let obs = ObservableCollection<string>()
    obs.CollectionChanged.Add (fun e -> printfn "%A" e.Action)
    let obsObj = Observables.bindObsTToObsObj obs
    let dc1 = createDc obs
    let dc2 = createDc obsObj
    obs,obsObj, 
        fun doAssert -> 
            dc1.Content <- null
            dc1.Content <- obs
            dc2.Content <- null
            dc2.Content <- obsObj
            let areEqual = 
                obs
                |> Seq.cast<obj>
                |> Seq.zip (obsObj |> Seq.cast<obj>)
                |> Seq.forall (fun (x,y) -> x = y)
            if doAssert && not areEqual then failwithf "Drifted!"
            Thread.Sleep 1000

let runTest() = 
    
    obs.Add "Hello world"
    printfn "updated 1"
    fUpdate true
    obs.Add "Hello 2"
    fUpdate true
    
    obs.Remove "Hello 2" |> ignore<bool>
    fUpdate true
    obs.Add "Hello 2"
    fUpdate true
    obsObj.[1] <- "Hello indirection"
    fUpdate false
    obs.[1] <- "Hello setter"
    fUpdate true
    obs.Clear()
    fUpdate true
    obs.Add "Hello verld"
    fUpdate true

runTest()