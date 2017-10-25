<Query Kind="FSharpProgram" />

// purpose: make a simple cqrs shopping cart (assumes, for now at least, single user)
// desired branches: multi-user; discounts

// my mailbox reference example: https://github.com/ImaginaryDevelopment/MudChallenge
// halfway through implementation found this F# shopping cart reference example: http://codebetter.com/matthewpodwysocki/2009/12/14/going-hybrid-implementing-a-shopping-cart-in-f/
module Schema = 
    // can't use units of measure with Guid =(
    type Id = Guid
    type ProductId = ProductId of Id
    type DiscountId = DiscountId of Id
    type CartId = CartId of Id
    type Product ={ProductId:ProductId; Name:string; Cost:decimal}
    // lets hold off on this until we have a working simple cart
    module Discounts = 
        type DiscountType = 
            // use float for percentage off, is good?
            | PercentOff of float
            | Amount of decimal
        type Discount = {DiscountId:DiscountId; Type:DiscountType; Products:ProductId list}
    type Cart = {CartId:CartId; (* Discounts:DiscountId list;*) Products: ProductId list}
open Schema

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
    let post (agent:Agent<'T>) message = agent.Post message
    
    let postAsyncReply (agent:Agent<'T>) messageConstr = agent.PostAndAsyncReply(messageConstr)
    
type Message = Command * State (* State *) * AsyncReplyChannel<Reply*State>    
let shoppingAgent x= new Agent<Message>(x)

let mp = shoppingAgent(fun inbox ->
    let rec messageLoop() = async{
        let! (cmd,startState,rc) = inbox.Receive()
        let reply,state = 
            try
                processCommand cmd startState
            with ex -> Exception (cmd,ex), startState
        
        return! messageLoop()
    }
    messageLoop()
)