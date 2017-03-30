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
let curry f x y = f (x, y)
let uncurry f (x,y) = f x y
let sleep (x:int) = System.Threading.Thread.Sleep x
type RelativePoints={Absolute:System.Drawing.Point; Relative:System.Drawing.Point}



module WinForm =
    let getMousePosition() = 
        Cursor.Position
    let setMousePosition x y = 
        //printfn "Changing mouse to %i,%i" x y
        Cursor.Position <- System.Drawing.Point(x,y)    
        
module Simulator =

    let is = WindowsInput.InputSimulator()
    let click() = 
        WinForm.getMousePosition() 
        //|> fun x -> printfn "Clicking at %i,%i" x.X x.Y
        |> ignore
        is.Mouse.LeftButtonClick()
    let shiftClick() = 
        is.Keyboard.KeyDown( WindowsInput.Native.VirtualKeyCode.SHIFT)
        sleep 200
        let r = click()
        sleep 200
        is.Keyboard.KeyUp ( WindowsInput.Native.VirtualKeyCode.SHIFT)

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

// record 4 points?
type P = Drawing.Point
type PoIMap = { CrusaderTabSelector: P
                DpsLevelUpPos: P
//                MoveClickTopLeft: P
                MoveClickTopRight: P
//                MoveClickBottomRight:P
                MoveClickBottomLeft:P
                FormationButton :P
                StormFormationButton:P
                MagnifyButton:P
                StormButton:P
                }

let populatePoints () = 
    {   CrusaderTabSelector =
            Util.ReadLine("Position mouse over crusader tab selector") |> ignore
            WinForm.getMousePosition()
        DpsLevelUpPos = 
            Util.ReadLine("Position mouse over dps crusader level up") |> ignore
            WinForm.getMousePosition()
//        MoveClickTopLeft =
//            Util.ReadLine("Position mouse over top left of click/hover area") |> ignore
//            WinForm.getMousePosition()
        MoveClickTopRight =
            Util.ReadLine("Position mouse over top right of click/hover area") |> ignore
            WinForm.getMousePosition()
//        MoveClickBottomRight = 
//            Util.ReadLine("Position mouse over bottom right of click/hover area") |> ignore
//            WinForm.getMousePosition()
        MoveClickBottomLeft =
            Util.ReadLine("Position mouse over bottom left of click/hover area") |> ignore
            WinForm.getMousePosition()
        FormationButton = 
            Util.ReadLine("Position mouse over preferred formation area") |> ignore
            WinForm.getMousePosition()
        StormFormationButton = 
            Util.ReadLine("Position mouse over storm formation area") |> ignore
            WinForm.getMousePosition()
        MagnifyButton = 
            Util.ReadLine("Position mouse over magnify button") |> ignore
            WinForm.getMousePosition()
        StormButton = 
            Util.ReadLine("Position mouse over storm rider button") |> ignore
            WinForm.getMousePosition()
    }
let p = Util.Cache(populatePoints, "points")
let getRandomInsideBox=
    let getNext = 
        let rnd = Random()
        fun (x:int) (y:int) ->
            rnd.Next(Math.Min(x,y), Math.Max(x,y))
    
    fun () ->
        getNext p.MoveClickBottomLeft.X p.MoveClickTopRight.X, getNext p.MoveClickBottomLeft.Y p.MoveClickTopRight.Y
let getRandomizedClickSweepPoint () = 
    getRandomInsideBox()
let funs = 
    let mutable lastCast = DateTime.Now
    let mutable lastClicks = List.empty
    let dumpStatus =
        
        let dc = DumpContainer()
        dc.Dump()
        fun () ->
            dc.Content <- [box lastCast] @ (lastClicks |> List.map box)
    let setMouse (px:P) =
        WinForm.setMousePosition px.X px.Y
        px
    let click px = 
        Simulator.click() |> ignore
        lastClicks <- px :: (lastClicks |> Seq.take (Math.Min(lastClicks.Length,5)) |> List.ofSeq)
    let moveClick px () = 
        setMouse px |> click
    let doRandomSweepClick = 
        getRandomizedClickSweepPoint
        >> uncurry WinForm.setMousePosition
    [
        doRandomSweepClick
        dumpStatus
        fun () ->
            moveClick p.CrusaderTabSelector
            sleep 400
            WinForm.setMousePosition p.DpsLevelUpPos.X p.DpsLevelUpPos.Y
            Simulator.shiftClick () |> ignore
        doRandomSweepClick
        doRandomSweepClick
        moveClick p.FormationButton
        doRandomSweepClick
        doRandomSweepClick
        moveClick p.FormationButton
        doRandomSweepClick
        // in case auto advance was turned off at some point, click where the advance arrow would be
        moveClick p.MoveClickTopRight
        fun () ->
            if DateTime.Now - lastCast > TimeSpan.FromHours 1. then
                moveClick p.StormFormationButton ()
                sleep 400
                moveClick p.MagnifyButton ()
                sleep 400
                moveClick p.StormButton ()
                sleep 400
                lastCast <- DateTime.Now ()
    ]
let delay = 800
let watchPosition = false
Simulator.run watchPosition delay funs
//CotLi.clickCrusadersTab()