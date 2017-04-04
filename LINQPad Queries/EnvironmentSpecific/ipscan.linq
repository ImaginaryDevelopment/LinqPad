<Query Kind="FSharpExpression">
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.NetworkInformation</Namespace>
</Query>

//portScan http://stackoverflow.com/questions/13492134/find-all-ip-address-in-a-network
//parseIp "192.168.0.0.1"
// consider perf of this implementation: http://www.codeproject.com/Articles/10190/Implement-a-basic-IP-Scanner-for-a-local-LAN-in-C

let dumpt (t:string) x = x.Dump(t); x
let getNetworkInterfaces() = NetworkInterface.GetAllNetworkInterfaces()
//let getIpAddresses mask ip = 
//    let ip'= IPAddress.Parse(ip).GetAddressBytes()
//    let mask'= IPAddress.Parse(mask).GetAddressBytes()
//    let netId = BitConverter.GetBytes(BitConverter.ToUInt32(ip',0) &&& BitConverter.ToUInt32(mask',0))
//    let inv_mask = mask' |> Array.map (fun r -> ~~~r)
//    let brCast = BitConverter.GetBytes(BitConverter.ToUInt32(netId, 0) ^^^ BitConverter.ToUInt32(inv_mask,0))
//    brCast
let parseIp (ip:string) = 
        let split = ip.Split('.')
        let mutable ip = 0u
        for i in [0..3] do
            ip <- (ip <<< 8) + UInt32.Parse(split.[i])
        ip
        
let toIpString (v:UInt32) : string =
    let mutable bitmask:UInt32 = 4278190080u //.ToString("x") //0xff000000
    let mutable parts = Array.zeroCreate 4
    for i in [0..3] do
        let masked = v &&& (bitmask) >>> (3-i)*8
        bitmask<- bitmask >>> 8
        parts.[i] <- masked.ToString(Globalization.CultureInfo.InvariantCulture)
    String.Join(".",parts)
    
let calculateIpRange mask ip = //http://stackoverflow.com/questions/14327022/calculate-ip-range-by-subnet-mask
    
    // try to follow http://stackoverflow.com/questions/1470792/how-to-calculate-the-ip-range-when-the-ip-address-and-the-netmask-is-given
    let ipBytes = 
        IPAddress.Parse ip
        |> fun x -> x.GetAddressBytes()
    let mask = ~~~(UInt32.MaxValue >>> 25)
    //mask.ToString("x") |> dumpt "mask"
    let maskBytes = BitConverter.GetBytes(mask) |> List.ofSeq |> (if BitConverter.IsLittleEndian then List.rev else id)
    //(ipBytes,maskBytes) |> dumpt "ip,maskBytes" |> ignore
    let startBytes = Array.zeroCreate ipBytes.Length
    let endBytes = Array.zeroCreate ipBytes.Length
    for i in [0..ipBytes.Length-1] do
        startBytes.[i] <- ipBytes.[i] &&& maskBytes.[i]
        endBytes.[i] <- ipBytes.[i] ||| ~~~maskBytes.[i]
    IPAddress(startBytes),IPAddress(endBytes)

let dumpt (t:string) x = x.Dump(t); x
// tested successfully
let test() = // http://stackoverflow.com/questions/1470792/how-to-calculate-the-ip-range-when-the-ip-address-and-the-netmask-is-given
    calculateIpRange "255.255.255.0" "192.168.0.1"
    |> fun (start,stop) -> start |> string |> parseIp, stop |> string |> parseIp
    |> fun (start,last) -> [start+1u..last-1u] |> List.map toIpString
    |> dumpt "testRange"
    
let mutable pingsStarted = 0
let fDc = 
    let dc = DumpContainer()
    dc.Dump("dc")
    dc.Content <- pingsStarted
    (fun () -> dc.Content <- pingsStarted)
    
getNetworkInterfaces()
|> Seq.filter(fun ni -> ni.OperationalStatus <> OperationalStatus.Down)
|> Seq.map (fun ni -> ni,ni.GetIPProperties())
|> Seq.map (fun (ni,ip) -> ni,ip,ip.UnicastAddresses |> Seq.tryFind (fun ua -> ua.Address.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork))
|> Seq.choose(fun (ni,ip,uaOpt) -> match uaOpt with | Some ua -> Some(ni,ip,ua) | None -> None)
|> Seq.filter(fun (_,_,ua) -> not <| isNull ua.IPv4Mask && ua.Address |> string <> "127.0.0.1")
|> Array.ofSeq
|> dumpt "interfaces"
// to filter on a specific interface, this is helpful, otherwise comment it out
//|> Seq.filter(fun (ni,_,ua) -> ni.Name = "Xpress")
|> Seq.map (fun (ni,ip,ua) -> ni,ip,ua,calculateIpRange (ua.IPv4Mask |> string) (ua.Address |> string))
|> Seq.map (fun (ni,ip,ua,(start,last)) -> start,last,ni,ip,ua)
|> Seq.map(fun (start,last,ni,ip,ua) -> start |> string |> parseIp, last |> string |> parseIp, ni,ip,ua)
|> Seq.map(fun (start,last,ni,ip,ua) -> ([start+1u..last-1u] |> List.map toIpString), ni,ip,ua)
|> fun x -> x.AsParallel().Select(fun (ips,ni,ip,ua) ->
    ni.Name, ni.Description,ua.Address |> string, Util.OnDemand("IPInterfaceProps",fun () -> ip),
        ips.AsParallel().Select(fun ip ->
            
            async{
                let p = new Ping()
                
                let awaiter = Async.AwaitEvent p.PingCompleted
                p.SendAsync(ip,null)
                pingsStarted <- pingsStarted + 1
                fDc()
                let! args = awaiter
                
                return if args.Reply.Status = IPStatus.Success then Some ip else None
            }
            |> Async.RunSynchronously
        )
        |> Seq.choose id
        |> List.ofSeq
    
)
|> List.ofSeq
//|> Seq.map (fun asyncs -> asyncs |> Seq.map(fun a-> a) |> List.ofSeq)
//|> Array.Parallel.map(fun (ni,ip,uaOpt) ->
//    async{
//        let p = Ping()
//        p.SendAsync(
//        let! args = Async.AwaitEvent (p.PingCompleted)
//        return p
//    }
//)