<Query Kind="FSharpProgram" />

//goal find machines/machine names on the local network

let ping attempts timeout (hostNameOrAddress:string) =
    let ping = new System.Net.NetworkInformation.Ping()
    let mutable pingReply: System.Net.NetworkInformation.PingReply = null
    for i in 0..attempts - 1 do
        if pingReply = null || pingReply.Status <> System.Net.NetworkInformation.IPStatus.Success then
            try
                pingReply <- ping.Send(hostNameOrAddress,timeout)
            with |ex -> ex.Dump()
    pingReply <> null && pingReply.Status = System.Net.NetworkInformation.IPStatus.Success
let valid = 
    Util.Cache( fun () -> 
        [1..255 - 1]
        |> Seq.map (sprintf "192.168.0.%i")
        |> Seq.filter( ping 1 10)
        )
valid
|> Dump

//TODO: add sniffing? http://www.codeproject.com/Articles/17031/A-Network-Sniffer-in-C