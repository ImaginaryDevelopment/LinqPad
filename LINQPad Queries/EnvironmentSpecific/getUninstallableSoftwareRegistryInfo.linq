<Query Kind="FSharpExpression" />

let openSubKey n (k:Microsoft.Win32.RegistryKey) = 
    k.OpenSubKey(n)
let containsI (d:string) (x:string) = x.IndexOf(d, StringComparison.InvariantCultureIgnoreCase) >= 0
let uninstallKey = Microsoft.Win32.Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall")

uninstallKey.GetSubKeyNames()
|> Seq.map (fun n -> n, uninstallKey.OpenSubKey n)
|> Seq.map (fun (name, sk) -> 
    let vNames = sk.GetValueNames() |> List.ofSeq
    let pairs = vNames |> Seq.map (fun vn -> vn,sk.GetValue vn)
    match name.StartsWith"{", pairs |> Seq.tryFind(fun (k,v) -> k = "DisplayName") with
    | true, Some (_,v) -> 
        v |> string,name, pairs
    | _, _ -> name, null, pairs
)
|> Seq.sortBy( fun (name, _, _) -> 
    name |> containsI "scanner"|> not && name |> containsI "cssn" |> not, name.StartsWith("{"))