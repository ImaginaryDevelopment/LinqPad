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

//
//module PInvoke = 
//    // https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/delegates
//    type delegateEnumWindowsProc = delegate of hWnd:IntPtr*lParam:IntPtr -> bool
//    //public delegate bool EnumWindowsProc(IntPtr hWnd, IntPtr lParam);
//    [<DllImport("user32.dll")>]
//    extern bool private SetForegroundWindow(IntPtr hWnd)
//    [<DllImport("user32.dll")>]
//    extern bool private EnumWindows(delegateEnumWindowsProc enumProc, IntPtr lParam)
//    
//    let setForegroundWindow hWnd = 
//        SetForegroundWindow hWnd
//    let enumWindows () = 
//        let windows = ResizeArray()
//        let f = new delegateEnumWindowsProc(fun (hWnd,lParam) -> windows.Add hWnd )
//        EnumWindows(f, IntPtr.Zero)
//PInvoke.enumWindows().Dump()
let mapAbsPoint (relativity:System.Drawing.Point) abs = 
    {Absolute = abs; Relative=point (abs.X - relativity.X) (abs.Y - relativity.Y) }
module WinForm =
    let getMousePosition() = 
        Cursor.Position
    let setMousePosition x y = 
        //printfn "Changing mouse to %i,%i" x y
        Cursor.Position <- System.Drawing.Point(x,y)    
        
module Simulator =

    let is = WindowsInput.InputSimulator()
    let click() = 
        WinForm.getMousePosition() |> fun x -> printfn "Clicking at %i,%i" x.X x.Y
        is.Mouse.LeftButtonClick()
    
    //is.Mouse.MoveMouseBy(100,100)
    //is.Mouse
    //is.Mouse.RightButtonClick()
    let move pt =
        //is.Mouse.MoveMouseTo(2687. , 504. )
        WinForm.setMousePosition pt.X pt.Y
    
    let keepFing delay f = 
        while not <| is.InputDeviceState.IsKeyDown WindowsInput.Native.VirtualKeyCode.CONTROL do
            f()
            sleep delay
            
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
    Util.ReadLine("Position mouse over preferred formation area") |> ignore
    let pt = WinForm.getMousePosition()
    {X=pt.X; Y=pt.Y}
    
//CotLi.ClickLevelUp 3
////Process.GetProcesses()
////|> Seq.filter(fun p -> p.ProcessName = "chrome")
//Process.GetProcessesByName "chrome"
//|> Seq.map (fun p -> 
//    p.MainWindowHandle,p.MainWindowTitle)
//|> Dump
//|> ignore
let funs = 
    [
        //fun() -> ()
        fun () -> 
            Simulator.move target
            Simulator.click ()
            ()
            
        //fun () -> moveMouseToCenterRightOfRightScreen()
    ]
let delay = 10
let watchPosition = false
Simulator.run watchPosition 500 funs
//CotLi.clickCrusadersTab()