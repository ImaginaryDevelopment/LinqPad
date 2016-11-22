<Query Kind="FSharpProgram" />

//Environment.SetEnvironmentVariable("MinecraftServerDir", @"F:\minecraft\server", EnvironmentVariableTarget.Machine);

let dir = Environment.GetEnvironmentVariable("MinecraftServerDir")
Environment.CurrentDirectory <- dir
let versions = 
    Directory.GetFiles dir
    |> Seq.map Path.GetFileName
    |> Seq.filter(fun s -> s.StartsWith("minecraft_server"))
    //|> Seq.map (fun f -> f, Path.GetFileName f)
    
    |> Array.ofSeq
versions.Dump()
let selectedFile = Util.ReadLine("VersionFile?", null,versions)
Path.Combine(dir,selectedFile)
|> function
    | x when Path.GetExtension x = ".jar" ->
        sprintf "java -Xmx1024M -Xms1024M -jar \"%s\"" x
    | x when Path.GetExtension x = ".exe" -> x
    | x -> failwithf "Unexpected file extension %s" x

|> fun c -> Util.Cmd(c)
|> Dump