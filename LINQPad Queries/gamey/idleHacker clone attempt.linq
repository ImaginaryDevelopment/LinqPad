<Query Kind="FSharpProgram" />

// attempt to make idle-hacking game // https://www.kongregate.com/games/SilverFinish/idle-hacking
module Helpers =
    let (|RMatchI|_|) (p:string) x =
        let r = Regex.Match(x,p, RegexOptions.IgnoreCase)
        if r.Success then
            Some r
        else None
    let dumpt t x = 
        printfn "displaying %s" t // in case dump is stuck because of UI lock elsewhere
        x.Dump(description=t)
        x
    let readLine () = 
    #if Interactive
        Console.ReadLine() // Util.ReadLine()
    #else // can't if Linqpad in F#
        Util.ReadLine()
    #endif
    let readLine1 (prompt:string) = 
    #if Interactive
        Console.Write(prompt)
        Console.ReadLine()
    #else
        Util.ReadLine(prompt)
    #endif
open Helpers
module Option = 
    let inline getOrDefault d x = match x with | Some x -> x | None -> d
module Reflection = 
    open Microsoft.FSharp.Reflection
    let fromString<'T> (s:string) = // https://stackoverflow.com/questions/21559497/create-discriminated-union-case-from-string
            FSharpType.GetUnionCases typeof<'T>
            |> Array.tryFind (fun c -> c.Name = s)
            |> Option.map (fun case -> FSharpValue.MakeUnion(case,Array.empty) :?> 'T)
    let getUnionCases(type') = FSharpType.GetUnionCases(type')
open Reflection
//module Rand =
//    let getBool (r:Random) = 
//        r.Next 1 = 0
//    type RandomState = private { Seed:int; Step:int}
//    type Randomizer = private {R:Random; RS:RandomState}
//    let getBool = 
//        function 
//        | {R=r; RS=rs} ->
//            getBool r, {R=r; RS={rs with Step = rs.Step + 1}}
module Rand =            
    type RandomState = private { Seed:int; Step:int}
    type Randomizer(rs) = 
        let r:Random = 
            let r = Random(rs.Seed)
            match rs.Step with
            | 0 -> r
            | x when x < 0 -> failwithf "Bad step"
            | x ->
                [0..x - 1]
                |> Seq.iter(fun _ -> r.Next() |> ignore)
                r
        let mutable step = rs.Step
     
        member __.GetBool() = 
            step <- step + 1
            if r.Next(1) = 0 then false else true
        member __.GetInt(lower,upper) = 
            step <- step + 1
            r.Next(lower,upper)
        // begin items that are derivatives
        
        member x.FromList l =
            let i = x.GetInt(0,List.length l)
            List.item i l
        member x.GetCase<'t>() = 
            let cases = getUnionCases(typeof<'t>)
            // step increment is built in
            let case = cases.[x.GetInt(0,cases.Length - 1)]
            fromString<'t> case.Name
            |> function
                | Some x -> x
                | None -> failwithf "Could not find union case %s" case.Name
        member x.Step = step
        member x.Seed = rs.Seed
    let getBool (r:Randomizer) =  r.GetBool()
    let getInt (r:Randomizer) (l,u) = r.GetInt(l,u)
open Rand

type Balance = {Usd:decimal; Bitcoin:decimal}
type DeviceType = |Phone | Printer | Computer | Supercomputer|Laptop|Tablet
type Device = {DeviceType:DeviceType; Mac:string} with
    static member Generate (r:Randomizer) = 
        {DeviceType=Phone; Mac="ab:ce:de:fd:ef:gf"}
type Security = 
    | WEP
    | WPA of port:uint8
    with 
        static member Cases = 
            Microsoft.FSharp.Reflection.FSharpType.GetUnionCases (typeof<Security>)
        static member Generate (r:Randomizer) = 
            let security = 
                let n = Security.Cases.[r.GetInt(0,Security.Cases.Length - 1)].Name 
                n
                |> fromString<Security> 
                |> function
                    | Some x -> x
                    | None -> failwithf "Could not find union case %s" n
            security
        static member GeneratePort (r:Randomizer) =
            function
            | WPA
            | WEP -> uint8 <| r.GetInt(1,100)
            
            
            
type Network = {Name:string; Address:string; Security:(Security*uint8) option; Devices: Device list} with
    static member Generate (r:Randomizer) =
//        let totalDevices = r.GetInt(4,8)
//        let unsecured = r.GetInt(3,totalDevices - 1)
        let name = 
            let pre = [ "Rip";"Say";"Lowa";"Tech"]
            let post = ["36";"37";"42";]
            r.FromList pre + r.FromList post
        let addr = 
            let range = [1..254]
            let getNet () = r.GetInt(1,254) |> string
            sprintf "%s.%s.%s.%s" (getNet()) (getNet()) (getNet()) (getNet())
            
        let security = 
            if r.GetBool() then 
                let sec = Security.Generate r 
                (sec,Security.GeneratePort r sec)
                |> Some
            else None
        {Name=name;Address = addr; Security = security; Devices = [0.. r.GetInt(1,4)] |> List.map(fun _ -> Device.Generate r)}
        
        
        
        

type Addon = | Rescan | Crack of Security | Optimization | ShortCommands | ShortcutIP | ShortcutMAC | HUD | TargetIP | LocateIP
type State = {Balance:Balance; Location: Network list; Injected:Map<string,Device list>;Addons:Addon Set;RState:RandomState} with
    
type Command = | Quit | Scan | Inject | Mine | Save

// wrapper for the repl shell    
type OuterCommand = 
    | InnerCommand of Command
    | Unknown
    | Quit

type Reply = 
    | Ok
    | Msg of string
    | Failure of string
    | Exception of Command*exn

let processCommand cmd state : Reply*State = 
    let replyCant () = Msg "I'm afraid I can't let you do that Dave"
    match cmd,state with
    | Scan,{Location=[]} -> 
        let r = Randomizer(state.RState)
        let genNet () = Network.Generate r
        // 4-8 devices
        let l = [0..r.GetInt(4-1,8-1)] |> List.map (ignore>>genNet)
        Msg (sprintf "%A" l), {state with Location=l}
    | Scan, {Location=x} ->
        Msg(sprintf "%A" x), state
type Message = Command * State (* State *) * AsyncReplyChannel<Reply*State>            
let mailbox = 
    let processor = new MailboxProcessor<Message>(fun inbox ->
        
        let rec loop() = 
            async{
                
                let! message = (* printfn"waiting for nextMessage"; *) inbox.Receive()
                let cmd, initialState, replyChannel = message
                let reply,state = 
                    try
                        processCommand cmd initialState
                    with ex -> Exception (cmd, ex), initialState

                //let msg (rc:AsyncReplyChannel<Reply*State>) (state:State) s = rc.Reply(Msg s, {state with MoveCount = state.MoveCount + 1})
                //let replyCant cantstate = replyChannel.Reply <| (Msg "I'm afraid I can't let you go that way dave",cantstate)
                replyChannel.Reply (reply,state)
                do! loop()
            }

        loop())
    processor.Start()
    processor
    
let rec takeInput (state:State) (s:string) : bool*State = 
    let inputMap = 
        match s with
        | RMatchI "scan|sc" _ -> OuterCommand.InnerCommand Command.Scan
        | RMatchI "quit" _ -> Quit
        | x -> x.Dump("did not understand"); Unknown
        
    let op (command:Command)  = (fun (replyChannel:AsyncReplyChannel<Reply*State>) -> (command,state,replyChannel))    
    match inputMap with 
    |InnerCommand cmd -> 
        let msg = op cmd
        let reply,newState  = mailbox.PostAndReply msg
        try
            match reply with
            | Ok -> printfn "Cart: %A" newState; true, newState
            | Msg s -> printfn "%s" s; true, newState
            | Exception (cmd, ex) -> printfn "Failed to process cmd '%A' input exception was %A" cmd ex;  false, newState
            | x -> printfn "bad reply! '%A'" x; false,newState
        with ex ->
            printfn "Failed to process cmd input exception was %A" ex;  false, newState
    | Unknown -> true, state
    | Quit -> false,state
       
let rec msgPump (state:State):State option = 
    let shouldContinue,newState =
        takeInput state <| readLine1 "Command?"
    if shouldContinue then msgPump newState
    else printfn "quitting!"; None
// new/load menu
let openingMenu 
let initialState = State.Initial
msgPump initialState |> dumpt "final state" |> ignore
printfn "msgPump finished, waiting for any key to exit"
#if Interactive
readLine () |> ignore<string>
#endif