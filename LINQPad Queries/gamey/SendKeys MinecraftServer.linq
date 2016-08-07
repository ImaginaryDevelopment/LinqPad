<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

module Externs = 
    [<DllImport("User32.dll")>]
    extern int FindWindow(String className, string WindowName);
    [<DllImport("User32.dll")>]
    extern int SetForegroundWindow(int hWnd)

open Externs

let sendToServer text = 
    //let hWnd = FindWindow(null, "Minecraft")
    let hWnd = FindWindow(null, "Minecraft server")
    if hWnd > 0 then
        SetForegroundWindow(hWnd)
        
        // may be needed for minecraft, but doesn't appear to be for minecraft server
        //Thread.Sleep(100)
        // workd on server
        SendKeys.SendWait( sprintf "%s\r" text) //"/op ImaginaryDev\r")
        
        //SendKeys.Send("/op ImaginaryDev")
let sendToClient text = 
    let hWnd = FindWindow(null, "Minecraft 1.8.9")
    if hWnd > 0 then
        SetForegroundWindow(hWnd)
        SendKeys.SendWait("{Esc}")
        Thread.Sleep(100)
        SendKeys.SendWait(text)
    else failwithf "Could not locate client"
        
sendToClient("t/gamemode maxinerd 1{enter}")


