<Query Kind="FSharpExpression" />

let openSubKey n (k:Microsoft.Win32.RegistryKey) = 
    k.OpenSubKey(n)
    
let uninstallKey = Microsoft.Win32.Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall")
uninstallKey.GetSubKeyNames()
|> Seq.map (fun n -> n, uninstallKey.OpenSubKey n)
|> Seq.map (fun (name, sk) -> name, sk.GetValueNames() |> Seq.map (fun vn -> vn,sk.GetValue vn))
|> Seq.sortBy( fun (name, _) -> name.IndexOf("scanner", StringComparison.CurrentCultureIgnoreCase) <0 )
