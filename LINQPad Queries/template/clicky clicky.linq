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

// desired feature: allow opting for send message in case the app supports that without actually using the mouse (which switches OS focus)
// was done on resolution 1680x1050 would need adjusting for others, I bet
let point x y = System.Drawing.Point(x,y)
let toTuple x y = (x,y)
type MyPoint ={ X:int; Y:int;}
let curry f (x,y) = f x y
let sleep (x:int) = System.Threading.Thread.Sleep x
module WinForm =
    let getMousePosition() = 
        Cursor.Position
    let setMousePosition x y = 
        printfn "Changing mouse to %i,%i" x y
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
    
    let keepClicking() =
        while not <| is.InputDeviceState.IsKeyDown WindowsInput.Native.VirtualKeyCode.CONTROL do
            clickCenterOfRightScreen() |> ignore
            sleep 800
        
module CotLi = 
    let topLeftCorner = point 1990 125
    let setMouseRelative x y = WinForm.setMousePosition (x + topLeftCorner.X) (y + topLeftCorner.Y)
        
    let clickCrusadersTab () = 
        
        setMouseRelative 940 640
        
        Simulator.click() |> ignore
        ()
        
    // 1    2   3
    // 4    5   6
    let ClickLevelUp currentViewSlot =
        let centerToCenterX = 400
        let centerToCenterY = 120
        let x1 = 380
        let y1 = 690
        clickCrusadersTab ()
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
        
//        match currentViewSlot with
//        | 1 -> Some (x1, y1)
//        | 2 -> toTuple (x1 + centerToCenterX) y1 |> Some
//        | 3 -> toTuple (x1 + centerToCenterX * 2) y1 |> Some
//        | 4 -> toTuple x1 (y1 + centerToCenterY) |> Some
//        | 5 -> toTuple (x1 + centerToCenterX) (y1 + centerToCenterY) |> Some
//        | _ -> None
//        |> Option.iter (curry setMouseRelative)
        ()
type StartSample={Absolute:System.Drawing.Point; Relative:System.Drawing.Point}
WinForm.getMousePosition()
|> fun x -> {Absolute=x; Relative=point (x.X - CotLi.topLeftCorner.X) (x.Y - CotLi.topLeftCorner.Y)}
|> Dump
|> ignore
CotLi.ClickLevelUp 3
//keepClicking()
//CotLi.clickCrusadersTab()