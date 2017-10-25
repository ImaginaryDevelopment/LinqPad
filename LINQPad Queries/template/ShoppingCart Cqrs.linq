<Query Kind="FSharpProgram" />

// purpose: make a simple cqrs shopping cart (assumes, for now at least, single user)
// desired branches: multi-user; discounts
// cqrs: all commands should be serializable for playback to see the full state at any point 

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
type State = Cart
type Command = 
    | HelloWorld of string
    | Add of ProductId
    | Remove of ProductId
    | Clear
    | Checkout
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
    
let processCommand cmd startState : Reply * State = 
    printfn "Processing cmd %A" cmd
    let replyFail () = Failure "A failure!"
    match cmd with
    | HelloWorld msg ->
        Msg msg,startState
    | Add pId ->
        Ok, {startState with Products = pId::startState.Products}
    | Remove pId ->
        let before = startState
        let result = {startState with Products = startState.Products |> List.filter((=) pId >> not)}
        Ok, result
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
        let rec messageLoop() = async{
            let! (cmd,startState,rc) = inbox.Receive()
            let reply,state = 
                try
                    processCommand cmd startState
                with ex -> Exception (cmd,ex), startState
            rc.Reply (reply,state)
            // why was this `return!` instead of `do!` ?
            do! messageLoop()
        }
        messageLoop()
    )
    mp.Start()
    mp

type CommandTranslateResult = | Success of Command | Bad of string

let (|AddCmd|_|) =
    function
    | RMatchI "Add (\d+)" r ->
            Command.Add (r.Groups.[1].Value |> int |> ProductId)
            |> Some
    | _ -> None
    
let (|RemoveCmd|_|) = 
    function
    | RMatchI "Remove (\d+)" r -> 
        Command.Remove (r.Groups.[1].Value |> int |> ProductId)
        |> Some
    | _ -> None

let (|ClearCmd|_|) = 
    function
    | RMatchI "Clear" _ ->
        Command.Clear
        |> Some
    | _ -> None
    
let rec takeInput (state:State) (s:string) : bool*State = 
    let inputMap = 
        match s with
        | RMatchI "HelloWorld" -> InnerCommand <| Command.HelloWorld "Hello"
        | AddCmd cmd -> InnerCommand cmd 
        | RemoveCmd cmd -> InnerCommand cmd
        | RMatchI "Checkout" _ -> InnerCommand Command.Checkout
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

let initialState = State.Initial
msgPump initialState |> dumpt "final state" |> ignore
printfn "msgPump finished, waiting for any key to exit"
#if Interactive
readLine () |> ignore<string>
#endif