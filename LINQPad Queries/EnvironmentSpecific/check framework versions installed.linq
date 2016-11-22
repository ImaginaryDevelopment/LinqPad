<Query Kind="FSharpExpression" />

// check .net versions installed
// using http://stackoverflow.com/questions/199080/how-to-detect-what-net-framework-versions-and-service-packs-are-installed
let split (d:string) (text:string) = text.Split( [d] |> Array.ofList, StringSplitOptions.None)
let openSubOrDump name (k:Microsoft.Win32.RegistryKey) = 
    let v = k.OpenSubKey(name)
    if isNull v then
        k.GetSubKeyNames().Dump(sprintf "did not find subKey `%s` in %s" name (k |> string))
        None
    else 
        //k.Dispose()
        Some v
        
let getValueOrDump name (k:Microsoft.Win32.RegistryKey) = 
    let valueNames = k.GetValueNames() 
    valueNames
    |> Seq.tryFind (fun v -> v = name)
    |> function 
        | Some _ -> k.GetValue name |> Some
        | None -> 
            k.Dump(sprintf "did not find %s in %s" name (k |> string))
            None
            
let traverseRegistryKeys baseKey keyNames valueName =
    keyNames
    |> List.fold (fun parentKey subKeyName -> parentKey |> Option.bind (openSubOrDump subKeyName)) (Some baseKey)
    |> function
        | Some k ->
            k 
            |> getValueOrDump valueName
            |> Option.map string
        | None -> None
        
let pathToList keyPath = 
    keyPath |> split "\\" |> List.ofSeq
    
let checkVersions() = 
    use ms = Microsoft.Win32.Registry.LocalMachine.OpenSubKey("Software\\Microsoft")
    //ms.GetSubKeyNames().Dump("subkeys")
    let is1Installed = 
//        let oldPathResult = 
//            let policy10 = ms |> openSubOrDump ".NETFramework" |> Option.bind(openSubOrDump "Policy") |> Option.bind(openSubOrDump "v1.0") |> Option.bind(openSubOrDump "3705")
//            policy10.Dump()
//            match policy10 with
//            | Some k->
//                let result = k.GetValue("Install") |> string |> (=) "1"
//                k.Dispose()
//                result
//            | None ->
//                false
        let newPathResult =
            traverseRegistryKeys ms [".NETFramework";"Policy";"v1.0";"3705"] "Install"
            |> Option.map ((=) "1")
            |> Option.getValueOrDefault
        newPathResult
    use netF= ms.OpenSubKey("NET Framework Setup\\NDP")
    let checkInstalledValue path valueName = 
        traverseRegistryKeys netF (pathToList path) valueName
        

    let checkInstalled path = 
        checkInstalledValue path "Install"
        |> Option.map ((=) "1")
        |> Option.getValueOrDefault
        
    let is11Installed = checkInstalled "v1.1.4322\\Install"

    let is2Installed = checkInstalled "v2.0.50727\\Install"
    let is3Installed = checkInstalled "v3.0\\Setup\\InstallSuccess"
    let is35Installed = checkInstalled "v3.5\\Install"
    let is4ClientInstalled = checkInstalled "v4\\Client\\Install"
    let is4FullInstalled = checkInstalled "v4\\Full\\Install"
    let is45Installed = checkInstalledValue "v4\\Full" "Release" |> Option.isSome
    [   "1.0",is1Installed
        "1.1",is11Installed
        "2.0",is2Installed
        "3.0", is3Installed
        "3.5", is35Installed
        "4.0 client", is4ClientInstalled
        "4.0 full", is4FullInstalled
        "4.5",is45Installed
    ] |> dict
        
    
checkVersions()