<Query Kind="FSharpProgram" />

// talk to minecraft
// https://gist.github.com/Fireflies/2480d14fbbb33b4bbae3
open System.Net.Sockets
open System.Text

let dumpt (title:string) x = 
    x.Dump(title) |> ignore
let inline tee f x = 
    f x
    x
    
module Readers = 
    let readByte offset (buffer:byte[]) : byte*int =
        let b = buffer.[offset]
        b,offset + 1
        
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
        
    let private buffer = List<byte>()
    
    let writeShort (x16:Int16) = 
        BitConverter.GetBytes x16
        |> buffer.AddRange
        
    let writeVarInt x = 
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
            
    let writeString (data:string) = 
        let buffer' = Encoding.UTF8.GetBytes data
        writeVarInt buffer'.Length
        buffer.AddRange buffer'
        
    let flush1 stream ident = // default -1
        let buffer' = buffer.ToArray()
        buffer.Clear()
        let mutable add = 0
        let mutable packetData: byte[] = [| 0uy |] //byte literal suffix is uy
        if ident >= 0 then
            writeVarInt ident
            packetData <- buffer.ToArray()
            add <- packetData.Length
            buffer.Clear()
        buffer'.Length + add |> writeVarInt 
        let bufferBytes = buffer.ToArray()
        buffer.Clear()
        let write = write stream
        write bufferBytes 
        write packetData
        write buffer'
        
    let flush stream = flush1 stream -1

module Minecraft =
    open Readers
    open Writers
    let handshake stream protocolVersion hostnameOrIp port nextState = // nextState should be status (1) on handshake
        writeVarInt protocolVersion
        writeString hostnameOrIp
        writeShort port
        writeVarInt nextState
        flush1 stream 0
        "handshake finished".Dump()
        
    type PingPayload = {Version:VersionPayload; Players:PlayersPayload; Description: string; Icon:string}// Icon is in base64
    and VersionPayload = { Protocol:int; Name:string}
    and PlayersPayload = {Max:int; Online:int; Sample: Player[]}
    and Player = {Name:string; Id:string}
    let statusRequest stream offset = 
        flush1 stream 0
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
                Newtonsoft.Json.JsonConvert.DeserializeObject<PingPayload>(json).Dump()
                offset
            with | :? IOException as ex ->
                ex.Dump()
                offset
        with | :? IOException as ex ->
            ex.Dump()
            offset
    
    
open Minecraft

let sayHello() = 
    use client = new TcpClient()
    let hostnameOrIp = "192.168.0.117"
    let port = 25565s
    client.Connect(hostnameOrIp, port |> Convert.ToInt32)
    //client.NoDelay <- false
    let stream = client.GetStream()   
    Console.WriteLine("Sending handshake request")
    handshake stream 47 hostnameOrIp port 1
    let offset = statusRequest stream 0
    offset.Dump("after handshake")
    let msg = Encoding.UTF8.GetBytes("chat.post(Merry Christmas from F#)")
    stream.Write(msg, 0, msg.Length)
sayHello()    
