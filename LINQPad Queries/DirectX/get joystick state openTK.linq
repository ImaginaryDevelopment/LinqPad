<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>OpenTK</NuGetReference>
  <Namespace>OpenTK</Namespace>
  <Namespace>OpenTK.Input</Namespace>
</Query>

//watch joystick looking for stuck key
// TODO: hook up RX to help watch for the problem
let merge f x = f x, x

let getFirstConnected() = 
    [0..3]
    |> Seq.map (merge Joystick.GetState)
    |> Seq.choose (fun ((state,i) as x) -> if state.IsConnected then Some x else None)
    |> Seq.head
let getGamePad() = 
    [0..3]
    |> Seq.map (merge GamePad.GetState)
    |> Seq.choose (fun ((state,i) as x) -> if state.IsConnected then Some x else None)
    |> Dump
    |> ignore
let _,identifier = getFirstConnected()

// works
// getGamePad()
let doWhilePrompt f = 
    let mutable stop = false
    let worker _ = 
        Util.ReadLine() |> ignore
    use bw = new System.ComponentModel.BackgroundWorker()
    bw.DoWork.Add worker
    bw.RunWorkerAsync()
    while not stop do
        f()
    

doWhilePrompt (fun () ->
    let state = Joystick.GetState(identifier)
    // didn't work
    //OpenTK.Input.GamePad.SetVibration(0, 0.5f, 0.5f).Dump("setvibration") |> ignore
    
    System.Threading.Thread.Sleep(500)
    if state.ToString().Contains("Buttons: 0000000000000000") |> not then
        state.ToString().Dump() |> ignore
    )
