<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>SlimDX</NuGetReference>
  <Namespace>SlimDX.RawInput</Namespace>
</Query>

// not working, not sure where to go from here
let tee f x = f x; x
let merge f x = f x, x

SlimDX.RawInput.Device.RegisterDevice(SlimDX.Multimedia.UsagePage.Game, SlimDX.Multimedia.UsageId.Joystick, DeviceFlags.None)
SlimDX.RawInput.Device.RawInput.Add (printfn "Found input event:%A")
//currently looking for device dfb66110-f7bf-11df-8001-444553540000
let devices = SlimDX.RawInput.Device.GetDevices()
devices 
|> Seq.filter(fun d -> d.DeviceType <> DeviceType.Mouse && d.DeviceType <> DeviceType.Keyboard)


|> Dump

Util.ReadLine()
|> ignore