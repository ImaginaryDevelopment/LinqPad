<Query Kind="FSharpProgram" />

// talk to minecraft
// https://gist.github.com/Fireflies/2480d14fbbb33b4bbae3
open System.Net.Sockets
open System.Text

let dumpt (title:string) x = 
    x.Dump(title) |> ignore
let inline printfnC col s = Util.RawHtml(sprintf "<font color=\"%O\">%s</font>" col s).Dump()
//printfnC ConsoleColor.DarkBlue "hello world"
let inline tee f x = f x; x
    
module Readers = 
    // returns the read byte array, and length read
    let read' (stream:NetworkStream) length =
        let buffer = Array.create length 0uy
        stream.Read(buffer,0,length),buffer
        buffer
    let readByte offset (buffer:byte[]) : byte*int =
        let b = buffer.[offset]
        b,offset + 1
//        
//    // needs testing
//    let readBytes offset length (buffer:byte[]) =
//        buffer.[offset.. length + offset], offset + length
//        
    let read offset buffer length : byte[]*int =
        let data = Array.create length 0uy
        Array.Copy(buffer, offset, data, 0, length) |> ignore
        data, (offset + length)
        
    let readVarInt offset' (buffer:byte[]) =
        let mutable value = 0
        let mutable size = 0
        let mutable b = 0
        let mutable offset = offset'
        
        let incrementb () = 
            let b', offset' = readByte offset buffer
            b <- int b'
            offset <- offset'
            b
        
        while (incrementb()) &&& (int 128uy) = (int 128uy) do
            value <- int (value ||| ((b &&& (int 127uy) ) <<< (size * 7))) //|> tee (dumpt "readVarInt value")
            size <- size + 1
            if size > 5 then
                raise <| new IOException "This VarInt is an imposter!"
        let result = value ||| ((b &&& (int 127uy)) <<< (size * 7) ) :> int
        result,offset
        
    let readString offset (buffer:byte[]) length =
        let data,offset = read offset buffer length
        Encoding.UTF8.GetString(data),offset
        
module Writers = 
        
    let write' (stream:NetworkStream) bytes offset length = 
        stream.Write(bytes,offset,length)
        
    let write stream bytes= 
        write' stream bytes 0 bytes.Length
    
    let writeShort (buffer:List<byte>) (x16:Int16) = 
        BitConverter.GetBytes x16
        |> buffer.AddRange
        
    let writeVarInt (buffer:List<byte>) x = 
        let mutable value:int = x
        
        while value &&& 128 <> 0 do
            (value &&& 127 ||| 128) 
            |> Convert.ToByte 
            //|> tee (dumpt "readVarInt value")
            |> buffer.Add
            value <- value |> Convert.ToUInt32 >>> 7 |> Convert.ToInt32
        value
        |> Convert.ToByte
        |> buffer.Add 
        
        //(x,buffer).Dump("after writeVarInt")
            
    let writeString (buffer:List<byte>) (data:string) = 
        let buffer' = Encoding.UTF8.GetBytes data
        writeVarInt buffer buffer'.Length
        buffer.AddRange buffer'
        
    let flush1 (buffer:List<byte>) stream ident = // default -1
        let buffer' = buffer.ToArray()
        buffer.Clear()
        let mutable add = 0
        let mutable packetData: byte[] = [| 0uy |] //byte literal suffix is uy
        if ident >= 0 then
            writeVarInt buffer ident
            packetData <- buffer.ToArray()
            add <- packetData.Length
            buffer.Clear()
        buffer'.Length + add |> writeVarInt buffer
        let bufferBytes = buffer.ToArray()
        buffer.Clear()
        let write = write stream
        write bufferBytes 
        write packetData
        write buffer'
        
    let flush buffer stream = flush1 buffer stream -1

module Minecraft =
    open Readers
    open Writers
    let handshake stream protocolVersion hostnameOrIp port nextState = // nextState should be status (1) on handshake
        let buffer = List<byte>()
        writeVarInt buffer protocolVersion
        writeString buffer hostnameOrIp
        writeShort buffer port
        writeVarInt buffer nextState
        flush1 buffer stream 0
        "handshake finished".Dump()
        
    type ColorChar =
        |Value of char
        |Color of ConsoleColor
        
    let writeMotd (description:string):ColorChar seq = 
        let colours = dict [
                        '0', ConsoleColor.Black
                        '1', ConsoleColor.DarkBlue
        ]
        Console.Write "Motd:"
        let chars = description.ToCharArray ()
        chars
        |> Seq.mapi (fun i c ->
                if c = '\u00A7' && colours.Keys.Contains chars.[i + 1] then
                    colours.[chars.[i + 1]] |> Color |> Some
                elif i>1 && chars.[i - 1] = '\u00A7' && colours.Keys.Contains c then
                    None
                else
                    ColorChar.Value c |> Some
            )
        |> Seq.choose id
        //|> printf "%A"
            
    type PingPayload = {Version:VersionPayload; Players:PlayersPayload; Description: string; Icon:string}// Icon is in base64
    and VersionPayload = { Protocol:int; Name:string}
    and PlayersPayload = {Max:int; Online:int; Sample: Player[]}
    and Player = {Name:string; Id:string}
    
    let statusRequest stream offset = 
        let writeBuffer = List<byte>()
        flush1 writeBuffer stream 0
        let buffer = Array.create 4096 0uy
        try
            //stream.ReadTimeout <- 1000
            let readLength = stream.Read(buffer, 0, buffer.Length)
            //readLength.Dump("stream read finished, processing")
            let offset = 0
            try
                let length,offset = readVarInt offset buffer
                let packet,offset = readVarInt offset buffer
                let jsonLength,offset = readVarInt offset buffer
                //(length,packet,jsonLength,offset).Dump(sprintf "Received packet 0x%s with a length of %i" (packet.ToString("X2")) length)
                let json,offset = readString offset buffer jsonLength
                //json.Dump(sprintf "json was '%A'" json)
                let pingPayload = Newtonsoft.Json.JsonConvert.DeserializeObject<PingPayload>(json)
                
                writeMotd pingPayload.Description |> Dump |> ignore
                pingPayload |> Dump |> ignore
                
                offset
            with | :? IOException as ex ->
                ex.Dump()
                offset
        with | :? IOException as ex ->
            ex.Dump()
            offset
            
    let getStatus protocol (hostnameOrIp:string) (port:Int16) = 
        use client = new TcpClient()
    
        client.Connect(hostnameOrIp, port |> Convert.ToInt32)
        //client.NoDelay <- false
        let stream = client.GetStream()   
        Console.WriteLine("Sending handshake request")
        handshake stream protocol hostnameOrIp port 1 // status
        let offset = statusRequest stream 0
        writeMotd 
        
    let login protocol (hostnameOrIp:string) (port:Int16) (username:string,password:string)= 
        use client = new TcpClient()
        client.Connect(hostnameOrIp, port |> Convert.ToInt32)
        client.Client.LocalEndPoint :?> System.Net.IPEndPoint
        |> (fun ep -> sprintf "connected to local port:%i" ep.Port)
        |> Dump
        |> ignore
        
        let stream = client.GetStream()   
        let offset = 0
        let buffer = List<byte>()
        flush1 buffer stream 0
        let sharedSecretLength,offset = readVarInt offset buffer
        let sharedSecret,offset = read
        // TODO: read secret bytes
//        [0..sharedSecretLength]
//        |> Seq.fold (fun (offset,bytes) i -> 
//            let byte,offset = readByte offset 
        let sharedSecret,offset = readBytes offset sharedSecretLength
        client.Connected |> Dump |> ignore
        
    let chat (stream:NetworkStream)  = 
        // writing a chat message doesn't currently work
        let msg = Encoding.UTF8.GetBytes("chat.post(Merry Christmas from F#)")
        stream.Write(msg, 0, msg.Length)
        
        
open Minecraft
    
let hostnameOrIp = "192.168.0.117"
let protocol = 47
let port = 25565s

getStatus protocol hostnameOrIp port

"Done with status".Dump()

let loginInfo = Util.GetPassword "Minecraft.UserName", Util.GetPassword "Minecraft.Password"
login protocol hostnameOrIp port loginInfo