<Query Kind="FSharpProgram" />

// close an app's foremost window - will try to just close whatever window the mouse is over first, see if that does the trick
// close works when linqpad runs as administrator. Used to close a modal in linqpad that was stuck

open System.Drawing
        
module PInvoke = 
    type WindowHandle = nativeint
    module Native = 
        open System.Drawing
        open System.Runtime.InteropServices
        
        
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
//        let dll = DllImportAttribute("")
        
        // But, if you need to get text from a control in another process, GetWindowText() won't work. Use WM_GETTEXT instead.
        // another mapping for StringBuilder out
        // you might need to get the string length from another call before calling this: https://www.pinvoke.net/default.aspx/user32.getwindowtext
        [<DllImport("User32",CharSet=CharSet.Auto,EntryPoint="SendMessage")>]
        extern nativeint SendWMText(WindowHandle hWnd, int msg, nativeint wParam, StringBuilder sb);
        
    open Native
    
    type SendMessageRaw = {hWnd:nativeint; msg:int; wParam:nativeint; lParam:nativeint}
    type Message<'t> = 
        | Close of windowHandle:nativeint
        | GetText of windowHandle:nativeint * withReturn :(string -> unit)
        | [<Obsolete("Use only for testing, don't leave things unmapped")>]
            Raw of SendMessageRaw
            
    let ptToParam (pt:System.Drawing.Point) = nativeint (pt.Y <<< 16 ||| pt.X)
    let sendMessage message = 
        let sendMessage a b c d = SendMessage(a,b,c,d)
        match message with
        | Close hwnd ->
            let WM_CLOSE = 0x0010
            printfn "Attempting close"
            sendMessage hwnd WM_CLOSE IntPtr.Zero IntPtr.Zero
        | GetText (hWnd,f) ->
            
            let WM_GETTEXTLENGTH = 0x000E
            printfn "Getting text length"
            let length: int =int<|SendMessage(hWnd,WM_GETTEXTLENGTH, IntPtr.Zero, IntPtr.Zero)
            printfn "Got text length: %i " length
            let mutable sb = StringBuilder(length + 1)

            let WM_GETTEXT = 0x000D

            let result = SendWMText(hWnd,WM_GETTEXT,nativeint (length + 1),sb)
            printfn "Text returned is length %i" sb.Length
            sb.ToString()
            |> f
            result
        | Raw x -> SendMessage(x.hWnd, x.msg, x.wParam, x.lParam)
    
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
        
type WindowInfo = {PId:int;MainWindowHandle:nativeint; ProcessName:string}
let getWindowInfo hwnd = 
    hwnd
    |> PInvoke.getWindowThreadProcessId
    |> Option.map (Process.GetProcessById)
    |> Option.map (fun p ->
        {PId=p.Id; MainWindowHandle=p.MainWindowHandle; ProcessName=p.ProcessName}
    )
        
Util.ReadLine("Put the cursor of the desired window") |> ignore
let currentPt = PInvoke.getCursorPos()
printfn "Current Pos is %A" currentPt
currentPt
|> Option.bind(fun pt ->
    PInvoke.windowFromPoint pt
)
|> Option.map(fun hWnd ->
    printfn "Current hWnd is %A" hWnd
    let wi = getWindowInfo hWnd
    printfn "CurrentWindowInfo is %A" wi
    wi.Dump()
    let pid = PInvoke.getWindowThreadProcessId hWnd
    printfn "With pId = %A" pid
    hWnd
)
|> Option.iter(fun hWnd ->
    let text =
        let mutable text:string = null
        let r = PInvoke.sendMessage <| PInvoke.GetText(hWnd,
                    (fun s -> 
                        printfn " got text?"
                        text <- s))
        text
    printfn "Window text:%s" text
    if Util.ReadLine<bool>("Attempt close?") then
        PInvoke.sendMessage (PInvoke.Message.Close( hWnd))
        |> ignore<nativeint>
)