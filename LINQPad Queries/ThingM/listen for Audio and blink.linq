<Query Kind="FSharpProgram">
  <NuGetReference>hidlibrary</NuGetReference>
  <NuGetReference>NAudio</NuGetReference>
  <NuGetReference>Sleddog.Blink1</NuGetReference>
  <Namespace>NAudio</Namespace>
  <Namespace>System.Reactive.Linq</Namespace>
</Query>

// by imaginarydevelopment.blogspot.com
// twitter @maslowjax

// needs https://www.nuget.org/packages/Rx-Main/ which is now unlisted

// since getting this via nuget in linqpad, let's try to paket it

// IBlink has dispose method but doesn't implement IDisposable
type IDisposalWrapper<'T> =
    abstract member Item:'T with get
    abstract member IsDisposed:bool with get
    inherit IDisposable
let inline dispose x = (^a:(member Dispose:unit -> unit) x)
let wrapDisposableIsh x =
    let mutable isDisposed = false
    let dispose()= dispose x; isDisposed <- true
    let getItem () = if isDisposed then invalidOp "Object is disposed" else x
    {new IDisposalWrapper<_> with
        member __.IsDisposed with get() = isDisposed
        member __.Item with get() = getItem()
        member __.Dispose() = dispose()
    }
//history 
//  http://share.linqpad.net/6vvje5.linq
let getBlink() = 
    Sleddog.Blink1.Blink1Connector.Scan()
    |> Seq.head
    
let listen fUntil = 
    let captureCount = ref 0
    use waveIn = new Wave.WasapiLoopbackCapture()
    use blink = wrapDisposableIsh <| getBlink()
    let sampleThrottle = TimeSpan.FromSeconds 5.5
    let onAudio (evArgs:Wave.WaveInEventArgs) = 
        incr captureCount
        blink.Item.Blink( Drawing.Color.Red, TimeSpan.FromSeconds 1. , 2us) |> ignore
        (DateTime.Now,evArgs).Dump()
    
    use _subscription = waveIn.DataAvailable.Where(fun evArgs -> evArgs.Buffer |> Seq.exists(fun b -> b <> 0uy)).Sample(sampleThrottle).Subscribe( onAudio )

    waveIn.WaveFormat.Dump()
    waveIn.StartRecording()
    fUntil() |> ignore
    waveIn.StopRecording()
    !captureCount |> Dump

(fun () -> Util.ReadLine())
|> listen