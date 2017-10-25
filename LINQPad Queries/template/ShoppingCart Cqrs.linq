<Query Kind="FSharpProgram" />

// purpose: make a simple cqrs shopping cart (assumes, for now at least, single user)
// desired branches: multi-user; discounts

// my mailbox reference example: https://github.com/ImaginaryDevelopment/MudChallenge
// halfway through implementation found this F# shopping cart reference example: http://codebetter.com/matthewpodwysocki/2009/12/14/going-hybrid-implementing-a-shopping-cart-in-f/
module Schema = 
    // can't use units of measure with Guid =(
    type Id = int
    type ProductId = ProductId of Id
//    type CartId = CartId of Id
    type Product ={ProductId:ProductId; Name:string; Cost:decimal}
    // lets hold off on this until we have a working simple cart
    module Discounts = 
        type DiscountId = DiscountId of Id
        type DiscountType = 
            // use float for percentage off, is good?
            | PercentOff of float
            | Amount of decimal
        type Discount = {DiscountId:DiscountId; Type:DiscountType; Products:ProductId list}
    type Cart = {(* CartId:CartId; *) (* Discounts:DiscountId list;*) Products: ProductId list} with
        static member Initial = {Products = list.Empty}



open Schema
#if !Interactive
open System
open System.Text.RegularExpressions
type System.Object with
    member x.Dump() = 
        printfn "%A" x
    member x.Dump(description:string) =
        printfn "%s:%A" description x
#endif
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
    let readLine () = Console.ReadLine() // Util.ReadLine()
open Helpers
type State = Cart
type Command = 
    | HelloWorld of string
    | Add of ProductId
    | Remove of ProductId
    | Clear
    | Checkout
    
type Reply = 
    | Ok
    | Msg of string
    | Failure of string
    | Exception of Command*exn
let processCommand cmd startState : Reply * State = 
    let replyFail () = Failure "A failure!"
    match cmd with
    | HelloWorld msg ->
        Msg msg,startState
    | Add pId ->
        Ok, {startState with Products = pId::startState.Products}
    | Remove pId ->
        Ok, {startState with Products = startState.Products |> List.filter((=) pId)}
    // for now model checkout with simple start over
    | Checkout
    | Clear ->
        Ok, {startState with Products = List.empty}
        
        
    
[<AutoOpen>]
module AgentHelper =
    type Agent<'T> = MailboxProcessor<'T>

    
type Message = Command * State (* State *) * AsyncReplyChannel<Reply*State>    
let mailbox = 
    let mp = new Agent<Message>(fun inbox ->
        printfn "agent initializing inbox"
        let rec messageLoop() = async{
            printfn "getting msg"
            let! (cmd,startState,rc) = inbox.Receive()
            printfn "msg received!"
            let reply,state = 
                try
                    printfn "Processing command"
                    processCommand cmd startState
                    |> dumpt "processed"
                with ex -> Exception (cmd,ex), startState
                |> dumpt "reply and state, next loop starting"
            rc.Reply (reply,state)
            // why was this return! instead of do! ?
            do! messageLoop()
        }
        messageLoop()
    )
    mp.Start()
    mp

type CommandTranslateResult = | Success of Command | Bad of string
let commandTranslator = 
    function
    | RMatchI "Add (\d+)" r ->
        r.Groups.[1].Value 
        |> int
        |> ProductId
        |> Add
        |> Success
    | RMatchI "Remove (\d+)" r ->
        r.Groups.[1].Value
        |> int
        |> ProductId
        |> Remove
        |> Success
    | x -> 
        Bad x
        
let rec takeInput (state:State) (s:string) : bool*State = 
    printfn "Taking input"
    
    let op (command:Command)  = Some <| (fun (replyChannel:AsyncReplyChannel<Reply*State>) -> printfn "Reply channel fun activated"; (command,state,replyChannel))
//        let move dir = op <| Command.Move dir
    let inputMap = 
        match s with
        | RMatchI "Add (\d+)" r ->
            op <| Command.Add (r.Groups.[1].Value |> int |> ProductId)
//        | RMatchI "Remove (
        | x -> x.Dump("did not understand"); None
        
    printfn "input mapped"
    match inputMap with 
    |Some msg -> 
        printfn "Processing msg"
            
        let reply,newState  = mailbox.PostAndReply msg
        printfn "Replied"
        //(reply,newState).Dump("replied?")
        try
            match reply with
            | Ok -> true, newState
            | Msg s -> printfn "%s" s; true, newState
            | Exception (cmd, ex) -> printfn "Failed to process cmd '%A' input exception was %A" cmd ex;  false, newState
            | x -> printfn "bad reply! '%A'" x; false,newState
        with ex ->
            printfn "Failed to process cmd input exception was %A" ex;  false, newState
        |> fun x -> printfn "to be continued"; x
        |> dumpt "yay reply async finished"
        
    |None -> false,state
    |> dumpt "takeInput finished"
    
let rec msgPump (state:State):State option = 
    printfn "Msg pumping"
    let shouldContinue,newState =
        printfn "Command?"
        takeInput state <| readLine()
        |> dumpt "finished take Input"
    if shouldContinue then printfn "continuing shopping"; msgPump newState
    else printfn "quitting!"; None
    |> dumpt "Msg pump finished"

let initialState = State.Initial
msgPump initialState |> dumpt "final state" |> ignore
printfn "msgPump finished, waiting for any key to exit"
readLine () |> ignore<string>