<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

// intercept a specific key, and send it to a browser tab
// help from http://stackoverflow.com/questions/604410/global-keyboard-capture-in-c-sharp-application
module Intercept = 
    type LowLevelKeyboardProc = delegate of nCode:int*wParam:IntPtr*lParam:IntPtr -> IntPtr
    [<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern IntPtr private SetWindowsHookEx(int idHook, LowLevelKeyboardProc lpfn, IntPtr hMod, System.UInt32 dwThreadId);

    [<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private UnhookWindowsHookEx(IntPtr hhk);

    [<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern IntPtr private CallNextHookEx(IntPtr hhk, int nCode, IntPtr wParam, IntPtr lParam);

    [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern IntPtr private GetModuleHandle(string lpModuleName);
    let private WH_KEYBOARD_LL = 13
    let private WM_KEYDOWN = 0x0100
    let mutable hookId = IntPtr.Zero
    let inline private hookCallback nCode (wParam:IntPtr) (lParam:IntPtr):IntPtr =
        if nCode >=0 && wParam = IntPtr WM_KEYDOWN then
            let vkCode = Marshal.ReadInt32 lParam
            printfn "Key! %i %A" vkCode (Enum.GetName(typeof<System.Windows.Forms.Keys>, vkCode))
        // callnexthookex if you don't want the key eaten
        CallNextHookEx(hookId,nCode,wParam, lParam)
    let private proc:LowLevelKeyboardProc = LowLevelKeyboardProc hookCallback
    let setHook proc =
        use curProcess = Process.GetCurrentProcess()
        use curModule = curProcess.MainModule
        hookId <- SetWindowsHookEx(WH_KEYBOARD_LL, proc, GetModuleHandle curModule.ModuleName, 0u)
        hookId
    
    let run() = 
        let hookId = setHook proc
        
        {new IDisposable with
            member x.Dispose() = 
                UnhookWindowsHookEx hookId |> ignore<bool>
        }
        
        
        
        
let unHooker = Intercept.run()
let keepAlive = Util.KeepRunning()

let disposal = 
    { new IDisposable with
        member x.Dispose() = 
            try
                unHooker.Dispose()
            with ex -> ex.Dump()
            keepAlive.Dispose()
    }        
    
Util.OnDemand("Done", fun () -> disposal.Dispose()).Dump("click when done")