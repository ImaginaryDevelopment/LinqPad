<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>SlimDX</NuGetReference>
  <Namespace>SlimDX.DirectInput</Namespace>
</Query>

let dinput = SlimDX.DirectInput.DirectInput()
let tee f x = f x; x
let merge f x = f x, x

let available() =     
    dinput.GetDevices(DeviceClass.GameController, DeviceEnumerationFlags.AttachedOnly)
    |> Seq.map(fun d -> d.InstanceGuid,d.InstanceName)
let acquire guid = 
    let pad = new Joystick(dinput,guid)
    try
        pad.SetCooperativeLevel(null, (CooperativeLevel.Nonexclusive ||| CooperativeLevel.Background)) |> Dump |> ignore
        
    with ex -> ex.Dump("setting coop level")
    pad.Acquire() |> Dump |> ignore
    pad
let getState (joystick:Joystick) = 
    joystick.GetCurrentState()
available()
|> Seq.head
|> Dump
//|> merge (fst >> acquire)
|> (fst >> acquire)
|> getState
|> Dump