<Query Kind="FSharpProgram" />

// solve for dps coefficients (assume no diminish?)
type Cat = |Spell |Atk
type Element = |Fire |Cold |Lightning
type DType = |Physical |Chaos |Elemental of Element
type ConversionSource = |NonChaos |DType // ignoring that Chaos is not a valid source :(
type Conv = {From:DType; IsExtra:bool; Amount:int; To:DType}
type DpsSource = 
    // raw damage%, spell damage%, atk damage%
    | Raw of Cat option
    | D of DType
    | Flat of DType
    | SkillLevel
    | Conversion of Conv
    | CritChance
    | CritMulti


// ignore breakpoints for now
// amount should be ... generic units?
let calcCoeff (amount:decimal<_>) (beforeDps:int) (afterDps:int) =
    decimal (afterDps - beforeDps)
    / amount
type Command =
    |AddSource of 
    |RemoveSource
    |Clear
    
module AgentHelper = 
    type Agent<'T> = MailboxProcessor<'T>  
    let post (agent:Agent<'T>) message = agent.Post message  
    let postAsyncReply (agent:Agent<'T>) messageConstr = agent.PostAndAsyncReply(messageConstr)

    let loop handler state =
        AgentHelper.Agent.Start(fun inbox ->
            let rec loop state =
                async{
                    let! (repl:AsyncReplyChannel<_>,msg) = inbox.Receive()
                    let (nextState,replyOpt) = handler state msg
                    replyOpt
                    |> Option.iter repl.Reply
                    return! loop nextState
                }
            loop state
        )
    
let processor state =
    function
    |AddSource ->
        