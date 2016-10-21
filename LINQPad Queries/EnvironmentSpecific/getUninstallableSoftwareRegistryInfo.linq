<Query Kind="FSharpExpression" />

let openSubKey n (k:Microsoft.Win32.RegistryKey) = 
    k.OpenSubKey(n)
let containsI (d:string) (x:string) = x.IndexOf(d, StringComparison.InvariantCultureIgnoreCase) >= 0
let uninstallKey = Microsoft.Win32.Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall")

uninstallKey.GetSubKeyNames()
|> Seq.map (fun n -> n, uninstallKey.OpenSubKey n)
|> Seq.map (fun (name, sk) -> name, sk.GetValueNames() |> Seq.map (fun vn -> vn,sk.GetValue vn))
|> Seq.sortBy( fun (name, _) -> name |> containsI "scanner"|> not && name |> containsI "cssn" |> not)