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
    let clickCenterOfRightScreen () =
        //is.Mouse.MoveMouseTo(2687. , 504. )
        WinForm.setMousePosition 2687 504 
        is.Mouse.LeftButtonClick()
    let moveMouseToCenterRightOfRightScreen() =
        WinForm.setMousePosition 3124 504
    let keepFing delay f = 
        while not <| is.InputDeviceState.IsKeyDown WindowsInput.Native.VirtualKeyCode.CONTROL do
            f()
            sleep delay
            
    let keepClicking delay =
        keepFing delay ( clickCenterOfRightScreen >> ignore)
    // keep clicking center, but move mouse around some to pick things up?
    let run watchPosition delay relativity (funs: (unit -> unit) list) =
        let toDo = 
            let mutable i = 0
            let fillDc = 
                let dc = DumpContainer()
                let fillDc() = 
                    dc.Content <- 
                        WinForm.getMousePosition() 
                        |> mapAbsPoint relativity
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
        
module CotLi = 
    let topLeftCorner = point 1990 125
    let setMouseRelative x y = WinForm.setMousePosition (x + topLeftCorner.X) (y + topLeftCorner.Y)
        
    let clickCrusadersTab () = 
        
        setMouseRelative 940 640
        
        Simulator.click() |> ignore
        ()
    let sweepPickupArea() = ()
        //WinForm.setMousePosition 
    // 1    2   3
    // 4    5   6
    let ClickLevelUp currentViewSlot =
        let centerToCenterX = 400
        let centerToCenterY = 120
        let x1 = 380
        let y1 = 690
        
        sleep 400
        let calcPos slotX slotY = 
            (x1 + centerToCenterX * slotX), (y1 + centerToCenterY * slotY)
        match currentViewSlot with
        | 1 -> calcPos 0 0 |> Some
        | 2 -> calcPos 1 0 |> Some
        | 3 -> calcPos 2 0 |> Some
        | 4 -> calcPos 0 1 |> Some
        | 5 -> calcPos 1 1 |> Some
        | 6 -> calcPos 2 1 |> Some
        | _ -> None
        |> Option.iter (curry setMouseRelative)
        Simulator.is.Mouse.LeftButtonClick() |> ignore
        ()

WinForm.getMousePosition()
|> mapAbsPoint CotLi.topLeftCorner
|> Dump
|> ignore
//CotLi.ClickLevelUp 3
////Process.GetProcesses()
////|> Seq.filter(fun p -> p.ProcessName = "chrome")
//Process.GetProcessesByName "chrome"
//|> Seq.map (fun p -> 
//    p.MainWindowHandle,p.MainWindowTitle)
//|> Dump
//|> ignore
let funs = 
    CotLi.clickCrusadersTab ()
    [
        //fun() -> ()
        fun () -> Simulator.clickCenterOfRightScreen() |> ignore
        fun () -> Simulator.is.Mouse.LeftButtonClick() |> ignore
        fun () -> CotLi.ClickLevelUp 3
        //fun () -> moveMouseToCenterRightOfRightScreen()
    ]
let delay = 10
let watchPosition = false
Simulator.run watchPosition 500 CotLi.topLeftCorner funs
//CotLi.clickCrusadersTab()