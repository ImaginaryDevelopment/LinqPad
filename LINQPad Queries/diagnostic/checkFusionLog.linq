<Query Kind="FSharpProgram" />

// purpose: turn on and off fusion logging properly


//https://stackoverflow.com/questions/255669/how-to-enable-assembly-bind-failure-logging-fusion-in-net
let getFusionKey32Bit () = 
    // 32bit linqpad reaches into the 'wrong' place for this? it seems.
    // regedit shows this is using the WOW6432Node of Local machine, Assembly Binding Viewer uses the proper one
    Microsoft.Win32.Registry.LocalMachine.OpenSubKey("SOFTWARE\\Microsoft\\Fusion")
let fusionKey = 
    use lmKey = Microsoft.Win32.RegistryKey.OpenBaseKey(Microsoft.Win32.RegistryHive.LocalMachine, Microsoft.Win32.RegistryView.Registry64)
    lmKey.OpenSubKey("SOFTWARE\\Microsoft\\Fusion")
    
    // 
// fusion log viewer may be installed at "C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Bin\NETFX 4.0 Tools\x64\FUSLOGVW.exe"
let valNames = fusionKey.GetValueNames()
fusionKey.GetSubKeyNames()
|> Dump
|> ignore
// none of the keys desired existed on my machine
valNames 
|> Seq.map (fun vn -> vn, fusionKey.GetValue(vn))
|> Dump
|> ignore
