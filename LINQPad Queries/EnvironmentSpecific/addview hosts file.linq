<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\FsInteractive\BReusable.dll">C:\projects\FsInteractive\BReusable.dll</Reference>
  <Namespace>BReusable</Namespace>
  <Namespace>System.Security.Principal</Namespace>
</Query>

open BReusable.StringHelpers
// manage host file
// must be running as admin to edit
// would be better served to operate with a mailbox processor
[<AutoOpen>]
module Helpers = 
    let dumpt (t:string) x = x.Dump(t); x
let hostPath = @"C:\Windows\System32\drivers\etc\hosts"
let lines = File.ReadAllLines hostPath |> List.ofArray
// lines.Dump()
let hostEntries = 
    lines
    |> Seq.filter (fun l -> not <| l.TrimStart().StartsWith "#" && not <| String.IsNullOrWhiteSpace l)
    |> Seq.map (fun l -> l |> before " ", l|> after " " |> fun hosts -> hosts.Split())
    |> List.ofSeq
    
//hostEntries.Dump()
    
let toSelf = 
    hostEntries
    |> Seq.filter(fst >> (=) "127.0.0.1")
    |> Seq.map (snd)
    |> Seq.collect id
    |> List.ofSeq
    

let others = 
    hostEntries
    |> Seq.filter(fst >> (=) "127.0.0.1" >> not)
    |> List.ofSeq
    



let requireAdmin () = 
    let runningAsAdmin = 
        WindowsIdentity.GetCurrent()
        |> WindowsPrincipal
        |> fun wp -> wp.IsInRole(WindowsBuiltInRole.Administrator)
    if not runningAsAdmin then
        failwithf "Can't alter hosts file without admin permissions"


let mutable newLines = lines
let promptCommand () = Util.ReadLine("Add ipTarget (blank to stop, s to save, . for localhost/block)")
let mutable targetIp = promptCommand()
let dumpF=
    let dc = DumpContainer("host file not altered")
    dc.Dump("hostfile text")
    fun () ->
        dc.Content <- box newLines
// dumpt and dump container are failing to display until the top dc has contents
(toSelf,others)
|> dumpt "toSelf,others"
|> ignore

let dumpErr = 
    let dc = DumpContainer()
    dc.Dump("errorDisplay")
    fun (o:obj) ->
        dc.Content <- o
// consider adding the ability to add comments or start a combined localhost block of names
while not <| String.IsNullOrWhiteSpace targetIp do
    match targetIp with
    | "s" -> 
        requireAdmin()
        File.WriteAllLines(hostPath, newLines)
    | _ -> 
        let targetIp = 
            match targetIp with
            | "." -> "127.0.0.1"
            | _ -> targetIp
        let uri = Util.ReadLine("uri?")
        try
            let uri = Uri(uri)
        
            newLines <- List.append newLines [ sprintf "%s %s" targetIp uri.Host ] |> Seq.distinct |> List.ofSeq
            dumpF()
        with ex -> 
            dumpErr ex
    
    targetIp <- promptCommand()