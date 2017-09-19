<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

// purpose click without using the mouse, so perhaps we can be free to use the computer while it clicks a background/other monitor app
// https://stackoverflow.com/questions/7315196/performing-a-mouse-click-without-moving-cursor
// works in a windows form it seems, but not to flash
// perhaps this can help? https://stackoverflow.com/questions/10183146/c-sharp-how-to-postmessage-to-a-flash-window-embedded-in-a-webbrowser
open System.Drawing
// stuff that worked, but we don't need, but might show something that will help figure out something else down the road

let hoist f x = 
    f x |> ignore     
    x
let dumpt t x = x.Dump(description=t); x
type RailWay<'TSuccess> = 
    | Failed of obj
    // don't want to clash with anything else already called Success
    | Railed of 'TSuccess
module Rails = 
    let bind f x = 
        match x with
        | Railed s -> f s
        | Failed x -> Failed x
    let map f x = 
        match x with
        | Railed s -> f s |> Railed
        | Failed x -> Failed x
        
module PInvoked = 
    open System.Runtime.InteropServices
    // flags enum compiles, but didn't get as far as trying to use it
    [<Flags>]
    type SendMessageTimeoutFlags  = 
        | SMTO_NORMAL             = 0x0
        | SMTO_BLOCK              = 0x1
        | SMTO_ABORTIFHUNG        = 0x2
        | SMTO_NOTIMEOUTIFNOTHUNG = 0x8
        | SMTO_ERRORONEXIT = 0x20
    [<Struct>]
    type PointFX =
        val mutable x:int
        val mutable y:int
    [<DllImport("user32.dll", EntryPoint="GetCursorPos")>]
    extern bool private GetCursorPos2(PointFX& lpPoint);
    [<DllImport("User32", EntryPoint="WindowFromPoint", ExactSpelling = true)>]
    extern nativeint private WindowFromPoint (PointFX& point)
    
    let getCursorPos() = 
        let mutable ptx = PointFX()
        match GetCursorPos2(&ptx) with
        | true -> Some ptx
        | false -> None
    let windowFromPoint(pt:PointFX) = 
        let mutable pt = pt
        WindowFromPoint(&pt)
        
module PInvoke = 
    type WindowHandle = nativeint
    module Native = 
        open System.Runtime.InteropServices
        
        // isn't working
        let bm_click = 0x00F5
        
        
        //http://pinvoke.net/default.aspx/user32.SendMessage
        // IntPtr would be fine here, nativeint is more idiomatic
        [<DllImport("User32", SetLastError=true)>]
        extern nativeint private SendMessage(WindowHandle hWnd, int Msg, nativeint wParam, nativeint lParam)
        
        [<DllImport("User32", EntryPoint="WindowFromPoint", ExactSpelling = true)>]
        extern WindowHandle private WindowFromPoint (Point point)
    
        // https://stackoverflow.com/questions/18184654/find-process-id-by-windows-handle
        //https://stackoverflow.com/a/18184700/57883
        [<DllImport("User32", SetLastError=true)>]
        extern int private GetWindowThreadProcessId(WindowHandle hWnd, int& processId)
        // https://stackoverflow.com/questions/1316681/getting-mouse-position-in-c-sharp
        [<DllImport("User32", SetLastError=true)>]
        extern bool private GetCursorPos(Point& lpPoint);
        //https://stackoverflow.com/questions/647236/moving-mouse-cursor-programmatically
        // claims sendInput is better than send messages for clicking
        [<DllImport("User32", SetLastError=true)>]
        extern System.Int64 SetCursorPos(int x, int y);
    open Native
    
    type Message = 
        | Click of WindowHandle
        | ClickPt of WindowHandle * Point
        | [<Obsolete("Use only for testing, don't leave things unmapped")>]
            Raw of (nativeint * int * nativeint * nativeint)
    let ptToParam (pt:Point) = nativeint (pt.Y <<< 16 ||| pt.X)
    let sendMessage message = 
        let sendMessage a b c d = SendMessage(a,b,c,d)
        match message with
        | Click hwnd -> sendMessage hwnd bm_click IntPtr.Zero IntPtr.Zero
        | ClickPt (hwnd,pt) -> 
            let lParam = ptToParam pt
            let wParam = IntPtr.Zero
            printfn "clicking at %A" pt
            sendMessage hwnd bm_click wParam lParam
        | Raw x -> 
            
            SendMessage(x) //(a,b,c,d)
        
    let click hwnd = sendMessage (Click hwnd)
    
    let windowFromPoint(pt:Point) = 
        let mutable pt = pt
        let hwnd = WindowFromPoint(pt)
        if hwnd <> IntPtr.Zero then
            Some hwnd
        else None
    let getCursorPos() = 
        let mutable pt = Point()
        match GetCursorPos(&pt) with
        | true -> Some pt
        | false -> None
    let getWindowThreadProcessId hwnd = 
        let mutable pId = 0
        let result = GetWindowThreadProcessId(hwnd, &pId)
        if pId <> 0 then
            Some pId
        else None
    let setCursorPos x y = 
        SetCursorPos(x,y) |> ignore

let capturePointInfo hwnd = 
    hwnd
    |> PInvoke.getWindowThreadProcessId
    |> Option.map (Process.GetProcessById)
    |> Option.map (fun p ->
        hwnd,p.Id, p.ProcessName, p.MainWindowHandle
    )
    |> Dump
module TForms = 
    let getDomainCache k = 
        (fun () -> AppDomain.CurrentDomain.GetData k), (fun (o:obj) -> AppDomain.CurrentDomain.SetData(k, o))
    open System.Windows.Forms
    let getCursorPos() = 
        Cursor.Position
    let findScreenLocation (c:Control) = 
        let p = c.TopLevelControl
        Screen.AllScreens
        |> Seq.find(fun s -> s.Bounds.Contains p.Location)
        |> fun s -> Point(p.Location.X - s.Bounds.Left, p.Location.Y - s.Bounds.Top)
        
    let ctrl,pt = 
        printfn "Creating button and form"
        let b = new Button()
        let f = new Form()
        let (lastPosGet,lastPosSet) = getDomainCache "LastFormPosition"
        
        
        
        f.Controls.Add b
        
        b.Text <- "Hello world"
        b.Click.Add(fun _ -> printfn "clicked")
        
        f.Dump()
        //f.Show()
        // set position after show, since windows will decide where to put it regardless of location on first showing (or linqpad, whichever)
        match lastPosGet() with
        | x  when isNull <| box x -> 
            printfn "Null? %A" x
            None
        | :? Point as pt -> Some pt
        | :? Option<Point> as pt -> pt
        
        | x -> 
            printfn "Unexpected type %A" x //x :?> Option<Point>
            None
        |> Option.iter (fun pt -> 
            printfn "Setting position to %A" pt
            f.Location <- pt
        )
        
        //let c = b.TopLevelControl
        let interrogatePosition (c:Control) = 
            (findScreenLocation c, c.Location, c.PointToScreen(c.Location), c.PointToClient(c.Location), c.Top, c.Left)
        //Cursor.Position <- b.PointToScreen(b.Location)
        //(c.Location,interrogatePosition b).Dump("parent,button")
        let getButtonPosition() = f.PointToScreen(b.Location)
        let disp = Util.KeepRunning()
        System.Threading.Thread.Sleep(1000)
        f.Closing.Add(fun _ -> 
            let lastPt = 
                //f.Location 
                f |> findScreenLocation
            printfn "Setting LastPosition to %A" lastPt
            let bPos = getButtonPosition()
            printfn "Setting mouse to button position %A from %A" bPos Cursor.Position
            Cursor.Position <- bPos
            lastPosSet (Some lastPt)
            f.Dispose()
            disp.Dispose()
            
        )
        let buttonCenter = getButtonPosition() |> fun bp -> Point(bp.X + (b.Width / 2), bp.Y + (b.Height / 2))
        f,buttonCenter
let currentPt = PInvoke.getCursorPos()
printfn "Current Pos is %A" currentPt
let clicking = 
    [   TForms.pt //Point(3491, 1496) // level 3852 Ilsa
        Point(3450, 1174)
        Point(3492,Y=1486)
        Point(3450, 1174)
        Point(3492,Y=1486)
        Point(3450, 1174)
        TForms.pt
        // try clicking something we don't own
        Point(21,42)
    ]
let cashey = Dictionary<_,_>()
let clickIt x = 
    let clicky ((hwnd,pId:int,procName:string,handle:PInvoke.WindowHandle) as y) = 
        cashey.[x] <- y
        let result = PInvoke.sendMessage (PInvoke.Message.ClickPt (handle, x))
//        System.Threading.Thread.Sleep(1000)
//        let result2 = PInvoke.sendMessage(PInvoke.Message.ClickPt (hwnd,x))
        PInvoke.setCursorPos x.X x.Y
        System.Threading.Thread.Sleep(1000)
        y
    if cashey.ContainsKey x then
        clicky cashey.[x]
        |> Some
    else
    
        PInvoke.windowFromPoint x
    //|> hoist capturePointInfo
        |> Option.bind capturePointInfo
        |> Option.map (fun (hwnd, pId,pName, handle) -> 
            clicky(hwnd, pId, pName, handle)
        )
    |> Option.map (dumpt "result,hwnd,pId,procName,handle")
    |> ignore
clicking
|> Seq.iter clickIt
//
//PInvoke.getCursorPos()
//|> Dump
//|> ignore