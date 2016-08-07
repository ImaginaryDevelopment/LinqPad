<Query Kind="FSharpProgram" />

//ddd with cqrs/event sourcing?
//attempt based on http://cqrs.nu/tutorial

type Agent<'TMessage> = MailboxProcessor<'TMessage>


let agent = Agent.Start(fun (inbox:Agent<string>) ->
    let rec loop() =
        async{
            let! msg = inbox.Receive()
            match msg with 
            | ""  -> 
                printfn "Stopping Agent"
            | _ ->
                printfn "Message recieved: %s" msg
                return! loop()
        }
    loop()
    )

module AgentHelper =
    type Agent<'t> = MailboxProcessor<'t>
    let post (agent:Agent<'T>) message = agent.Post message
    let postAsyncReply (agent:Agent<'t>) messageConstr = agent.PostAndAsyncReply messageConstr
    

type Aggregate<'t> = {Id:Guid;EventsLoaded:int; Data:'t;}
    with member x.ApplyEvent(fEvent) : Aggregate<'t> * _ list = fEvent x |> (fun (a,events) -> {a with EventsLoaded = Seq.length events + a.EventsLoaded},events)

type Charge = {ChargeId:int; Amount:decimal; ProcedureCodeId:string}        

type ChargeCommand = | Delete | Add | Update
type Claim = {ClaimId:int; Charges: Charge seq; }

type ClaimCommand = 
    |Resubmit of Claim
    |DeleteCharge of Charge
    |UpdateCharge of Charge
    |Create of Claim
    |AddCharge of NewCharge:Charge
    
type Error = string    
type ClaimEvent = 
    |Resubmitted
    |Created
    |ChargeCreated of Charge
    |Changed of Claim
    |AddChargeFailed
    |ClaimValidationFailed of Error seq
    |ChargeDeleted of Claim

type ClaimAggregate = Aggregate<Claim>

let handleClaimCommand cmd (ca:Aggregate<Claim>): ClaimEvent seq = 
    match cmd with
    | Resubmit cl -> 
        if cl.ClaimId < 1 then upcast [ ClaimValidationFailed([sprintf "Claim to be resubmitted must have a valid claimId, but was %i" ca.Data.ClaimId ])]
        else upcast [Resubmitted]
    | DeleteCharge charge -> upcast [ChargeDeleted {ca.Data with Charges = ca.Data.Charges |> Seq.filter ((=) charge)}]
    
    
{Claim.ClaimId = 0; Charges = [ {Charge.ChargeId=0; Amount=15m; ProcedureCodeId="99213" }]}
|> Create
|> handleClaimCommand 
|> Dump
    
        


