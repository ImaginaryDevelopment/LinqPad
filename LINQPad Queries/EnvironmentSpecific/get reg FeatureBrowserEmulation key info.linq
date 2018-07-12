<Query Kind="FSharpExpression" />

// check what fbe flags are set - https://weblog.west-wind.com/posts/2011/May/21/Web-Browser-Control-Specifying-the-IE-Version

let openSubKey n (k:Microsoft.Win32.RegistryKey) = 
    k.OpenSubKey(n)
//let tryGetValue
let containsI (d:string) (x:string) = x.IndexOf(d, StringComparison.InvariantCultureIgnoreCase) >= 0

let v = 
    [ 
        Microsoft.Win32.Registry.CurrentUser.OpenSubKey @"SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION"
        Microsoft.Win32.Registry.LocalMachine.OpenSubKey @"SOFTWARE\Microsoft\Internet Explorer\MAIN\FeatureControl\FEATURE_BROWSER_EMULATION"
        Microsoft.Win32.Registry.LocalMachine.OpenSubKey @"SOFTWARE\WOW6432Node\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION"
    ]
    |> Seq.map (fun x -> x.Name, x.GetValueNames() |> Seq.map (fun vn -> vn, x.GetValue vn ))
    |> List.ofSeq
let asms =     
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.map(fun asm -> asm.CodeBase)
//    |> Seq.filter(fun asm -> asm.CodeBase.Contains "/Temp/) 
AppDomain.CurrentDomain.Load("Microsoft.Build.Utilities.Core, Version=14.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a") |> ignore
asms.Dump()

v