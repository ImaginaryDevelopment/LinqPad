<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\Accessibility.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Deployment.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Serialization.Formatters.Soap.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>InputSimulator</NuGetReference>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// click browser stuffs

//desired feature activate/make sure is active, a specific chrome window or tab


// desired feature: allow opting for send message in case the app supports that without actually using the mouse (which switches OS focus)
// sendMessage sample at http://stackoverflow.com/questions/10355286/programmatically-mouse-click-in-another-window
// was done on resolution 1680x1050 would need adjusting for others, I bet
let point x y = System.Drawing.Point(x,y)
let toTuple x y = (x,y)
type MyPoint ={ X:int; Y:int;}
let curry f (x,y) = f x y
let sleep (x:int) = System.Threading.Thread.Sleep x
type RelativePoints={Absolute:System.Drawing.Point; Relative:System.Drawing.Point}

let mapAbsPoint (relativity:System.Drawing.Point) abs = 
    {Absolute = abs; Relative=point (abs.X - relativity.X) (abs.Y - relativity.Y) }
module WinForm =
    let getMousePosition() = 
        Cursor.Position
    let setMousePosition x y = 
        //printfn "Changing mouse to %i,%i" x y
        Cursor.Position <- System.Drawing.Point(x,y)    
        
let timer = System.Diagnostics.Stopwatch.StartNew()
timer.Reset()
module Simulator =

    let is = WindowsInput.InputSimulator()
    let click() = 
//        WinForm.getMousePosition() |> fun x -> printfn "Clicking at %i,%i" x.X x.Y
        is.Mouse.LeftButtonClick()
    
    //is.Mouse.MoveMouseBy(100,100)
    //is.Mouse
    //is.Mouse.RightButtonClick()
    let move pt =
        //is.Mouse.MoveMouseTo(2687. , 504. )
        WinForm.setMousePosition pt.X pt.Y
    
    let keepFing delay f = 
        let mutable clickEnabled = true
        
        while not <| is.InputDeviceState.IsKeyDown WindowsInput.Native.VirtualKeyCode.CONTROL do
            // enable click pausing, instead of stopping
            if is.InputDeviceState.IsKeyDown WindowsInput.Native.VirtualKeyCode.MENU then // menu is alt key
                if clickEnabled then
                    timer.Stop()
                    clickEnabled <- false
                    sleep <| min delay 250
                else
                    clickEnabled <- true
                    sleep <| min delay 100
                    timer.Start()
            if clickEnabled then
                f()
                sleep delay
            else sleep <| min delay 100
            
    // keep clicking center, but move mouse around some to pick things up?
    let run watchPosition delay (funs: (unit -> unit) list) =
        let toDo = 
            let mutable i = 0
            let fillDc = 
                let dc = DumpContainer()
                let fillDc() = 
                    dc.Content <- 
                        WinForm.getMousePosition() 
                        |> box
                fillDc()
                //dc.Dump
                fillDc

            let options = funs
            (fun () ->
                if watchPosition then
                    fillDc()
                options.[i]()
                i <- (i + 1) % options.Length
            )
        keepFing delay toDo

let target = 
    Util.ReadLine("Position mouse over click target") |> ignore
    let pt = WinForm.getMousePosition()
    {X=pt.X; Y=pt.Y}
    
let mutable count = 0
let funs = 
    [
        fun () -> 
//            Simulator.move target
            Simulator.click () |> ignore
            count <- count + 1
            ()
            
    ]

let watchPosition = false
Simulator.run watchPosition 55 funs
let seconds = timer.Elapsed.TotalSeconds
let avg = float count / seconds
printfn "Clicked %i times in %A(%f seconds) (%f/second)" count timer seconds avg