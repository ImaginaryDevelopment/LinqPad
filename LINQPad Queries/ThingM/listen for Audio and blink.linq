<Query Kind="FSharpProgram">
  <NuGetReference>hidlibrary</NuGetReference>
  <NuGetReference>NAudio</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Sleddog.Blink1</NuGetReference>
  <Namespace>NAudio</Namespace>
  <Namespace>System.Reactive.Linq</Namespace>
</Query>

// by imaginarydevelopment.blogspot.com
// twitter @maslowjax

//history 
//  http://share.linqpad.net/6vvje5.linq
let getBlink() = 
    Sleddog.Blink1.Blink1Connector.Scan()
    |> Seq.head
    
let listen fUntil = 
    let captureCount = ref 0
    use waveIn = new Wave.WasapiLoopbackCapture()
    use blink = getBlink()
    let sampleThrottle = TimeSpan.FromSeconds 5.5
    let onAudio (evArgs:Wave.WaveInEventArgs) = 
        incr captureCount
        blink.Blink( Drawing.Color.Red, TimeSpan.FromSeconds 1. , 2us) |> ignore
        evArgs.Dump()
    
    use _subscription = waveIn.DataAvailable.Where(fun evArgs -> evArgs.Buffer |> Seq.exists(fun b -> b <> 0uy)).Sample(sampleThrottle).Subscribe( onAudio )

    waveIn.WaveFormat.Dump()
    waveIn.StartRecording()
    fUntil() |> ignore
    waveIn.StopRecording()
    !captureCount |> Dump

(fun () -> Util.ReadLine())
|> listen