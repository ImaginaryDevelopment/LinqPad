<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// auto journal things like revolt time max resource rates reached, etc.

let fileLocation = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), Path.GetFileNameWithoutExtension(Util.CurrentQueryPath)+".json")
fileLocation.Dump("Saves go to")

let readLine1 (prompt:string) = 
    #if Interactive
        Console.Write(prompt)
        Console.ReadLine()
    #else
        Util.ReadLine(prompt)
    #endif
let (|StringEqualsI|_|) x y = if not <| isNull x && not <| isNull y && String.Equals(x,y,StringComparison.InvariantCultureIgnoreCase) then Some() else None
let (|RMatchI|_|) (p:string) (x:string) = 
    let m = Regex.Match(x,p,RegexOptions.IgnoreCase)
    if m.Success then
        Some m
    else None
    
let jss = 
    JsonSerializerSettings(
        NullValueHandling = NullValueHandling.Ignore, 
        MissingMemberHandling = MissingMemberHandling.Ignore,
        PreserveReferencesHandling = PreserveReferencesHandling.None
    )
let deserialize<'T> x = JsonConvert.DeserializeObject<'T>(x,jss)
let serialize x = JsonConvert.SerializeObject(x, Formatting.Indented,jss)

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day

type Resource =
    | Gold of float<G>
    | Food of float<F>
    | Bp of float<Bp>
type Building = 
    | Farm
    | FairyTree
    | Well
    | Workshop
    | Stonehenge
    | Stable
    | MasterHouse
    | TownHall
    | Warehouse
    | Homestead
    | Castle
    | CityWall
    | GoldMine
    | IronMine
    | Throne
    | Fountain
type Tier = int
// position indicates tier
type Happiness = float list
type BuildingState = {Building:Building;Units:float;Level:int}
type State = { Buildings:BuildingState list; Happiness:Happiness; Resources:Resource list; } with
    static member Initial = {Buildings=List.empty; Resources=List.empty}
let (|ListItem|_|) i x = if List.length x < i then Some x.[i] else None
module Hap =
    let getBuildingTier =
        function
        | Farm
        | FairyTree -> Some 0
        | Workshop
        | StoneHenge
        | Well -> Some 1
        | Stable -> Some 2
        | MasterHouse -> Some 3
        | TownHall -> Some 4
        
    let getHappiness tier =
        function
        | {Happiness=ListItem tier x} ->
            Some x
        | _ -> None
    let setHappiness b x =
        function
        | {Happiness= h} as s ->
            match List.length h, getBuildingTier b with
            | _,Some t when t < 0 -> invalidOp "Tier can't be negative"
            | 0, Some 0 -> {s with Happiness = [x]}
            | 1, Some 1 -> List.
            


type JournalEntry =
    // Assimilated, record data for reference
    | Assim of string * DateTime * State
    | Save of string * DateTime * State
    | Revolution of string * DateTime * State

type Command = 
    |HelloWorld of string
    |Save of string*State
    |SetUnits of Building*float
    |SetLevel of Building * int
    |SetResource of Resource
    |SetHappiness of Building*float
    // no clicking damage yet perhaps? |Click
    |Load of State
// wrapper for the repl shell    
type OuterCommand = 
    | InnerCommand of Command
    | ListSaves
    | Unknown
    | Quit
    
type Reply =
    | Msg of string
    | Exception of Command * System.Exception

type Message = Command*State*AsyncReplyChannel<Reply*State>

let processCommand cmd initialState : Reply*State = 
    let replyCant () = Msg "I'm afraid I can't let you go that way dave"
    let msg (state:State) s = Msg s, state
    
    match cmd with
    | HelloWorld x ->
        msg initialState x
    | SetHappiness(b,x) ->
        
        msg {initialState with Happiness} "Set"
    | Save (saveTitle,state) ->
        let previousSaves = 
            if File.Exists fileLocation then
                File.ReadAllText fileLocation
                |> deserialize<JournalEntry[]>
                |> List.ofArray
            else List.empty
        let txt = 
            (JournalEntry.Save(saveTitle,DateTime.UtcNow,state))::previousSaves
            |> Array.ofList
            |> serialize
        File.WriteAllText(fileLocation,txt)
        Reply.Msg("Saved"),state
    | Load state ->
        Reply.Msg("Loaded"),state

let mailbox = 
        let processor = new MailboxProcessor<Message>(fun inbox ->
            
            let commandProcessor = processCommand
            let rec loop() = 
                async{
                    
                    let! message = (* printfn"waiting for nextMessage"; *) inbox.Receive()
                    let cmd, initialState, replyChannel = message
                    let reply,state = 
                        try
                            commandProcessor cmd initialState
                        with ex -> Exception (cmd, ex), initialState

                    //let msg (rc:AsyncReplyChannel<Reply*State>) (state:State) s = rc.Reply(Msg s, {state with MoveCount = state.MoveCount + 1})
                    //let replyCant cantstate = replyChannel.Reply <| (Msg "I'm afraid I can't let you go that way dave",cantstate)
                    replyChannel.Reply (reply,state)
                    do! loop()
                }

            loop())
        processor.Start()
        processor
// WIP from here    
let rec takeInput (state:State) (s:string) : bool*State = 
    let inputMap = 
        match s with
        | StringEqualsI "HelloWorld" -> InnerCommand <| Command.HelloWorld "Hello"
        | RMatchI "save (\w+)" r -> InnerCommand <| Command.Save (r.Groups.[1].Value,state)
        | StringEqualsI "list" -> OuterCommand.ListSaves
        | RMatchI "quit" _ -> Quit
        | x -> x.Dump("did not understand"); Unknown
        
    let op (command:Command)  = (fun (replyChannel:AsyncReplyChannel<Reply*State>) -> (command,state,replyChannel))    
    match inputMap with 
    |InnerCommand cmd -> 
        let msg = op cmd
        let reply,newState  = mailbox.PostAndReply msg
        try
            match reply with
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

let initialState = State.Initial
msgPump initialState |> dumpt "final state" |> ignore
printfn "msgPump finished, waiting for any key to exit"
#if Interactive
readLine () |> ignore<string>
#endif