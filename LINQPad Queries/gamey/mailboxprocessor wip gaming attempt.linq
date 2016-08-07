<Query Kind="FSharpProgram" />

type Crusader = {IsHuman:bool; DamagePerLevel:decimal; IsClicker:bool; Investment:decimal; Level:int;}

type State = { Campaign:string; Xp:int; Level:int; } //with
    //static member Initial = {Room="start"; Xp=0; Level =1; X=0; Y=0;Health = 4<Health>; MoveCount =0; Monster = None; Damage = Die.D4}

type Command = 
    |MoveCrusader
    // no clicking damage yet perhaps? |Click
    |Save of State
    |Load of State

type Reply =
    | RoomChange
    | Msg of string
    | Death of string
    | Exception of Command * System.Exception

type Message = Command*State*AsyncReplyChannel<Reply*State>

 let processCommand monsters cmd initialState : Reply*State = 
        let replyCant () = Msg "I'm afraid I can't let you go that way dave"
        let msg (state:State) s = Msg s, {state with MoveCount = state.MoveCount + 1}
        match cmd with
                    | Move dir -> 
                        let replyOpt,postEventState = moveEvent replyCant monsters dir initialState playerAttack monsterAttack map (checkArrivalEvent rng ) monsterChanceEvent removeMonster rng 
                        if replyOpt.IsSome then 
                            replyOpt.Value,postEventState |> monsterChanceEvent monsters
                        else
                            // did not move
                            msg postEventState <| sprintf "(noreply)moved to (%A, %A)" postEventState.X postEventState.Y
                    | Wait -> 
                        if initialState.Monster.IsSome then 
                            msg initialState <| sprintf "The %A would like to finish, what are you waiting for?" initialState.Monster.Value.Name 
                        else msg initialState "What are you waiting for? This dungeon isn't going to explore itself"
                    | Attack -> 
                        let replyOpt,combatResolvedState = initialState |> (playerAttack >> monsterAttack)
                        if replyOpt.IsSome then
                            replyOpt.Value,combatResolvedState
                        else
                            msg combatResolvedState "and now?"
                    | Load(loadState) -> Msg "loaded", loadState

let mailbox = 
        let processor = new MailboxProcessor<Message>(fun inbox ->
            
            let commandProcessor = processCommand monsters
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