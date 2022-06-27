<Query Kind="FSharpProgram">
  <Connection>
    <ID>6d9036c3-dd08-4e55-8c56-0d7d31c5ebfc</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>ApplicationDatabase</Database>
    <UserName>xpu10</UserName>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

let dc = new TypedDataContext()
let debug = true
let dumpt t x = x.Dump(description=t); x
type KeyExpr<'t> = Expression<Func<'t,int>>
type Expr =
    static member Quote(e:Expression<System.Func<_,_>>) = e
// poll/listen (or take initial look, and await user to hit enter suggesting the payment was made) for a new payment, validate results
type Status = {LastPayment:int;LastJournalEntry:int; LastPaymentItem:int;Reversals:int;LastReversal:int*int}
type KeyDelegates = {PaymentKey:KeyExpr<Payment>; JEKey: KeyExpr<JournalEntry>; PyIKey: KeyExpr<PaymentItem>}
let getLastPaymentReversal () =
    query {
        for pr in dc.PaymentReversals do
            select (pr.PaymentId,pr.ReversalPaymentId, pr.Payment.Created, pr.Reversal.Created)
    }
    |> Seq.maxBy(fun (pyId,rPyId, pyC, rPyC) ->
        [pyC;rPyC] |> Seq.map (fun x -> x.GetValueOrDefault()) |> Seq.max
    )
    |> fun (pyId, rPyId, _,_) -> pyId,rPyId
    
let keys = {PaymentKey= Expr.Quote(fun (py:Payment) -> py.PaymentID); JEKey = Expr.Quote (fun je -> je.JournalEntryID)
            PyIKey = Expr.Quote (fun pyi -> pyi.PaymentItemID) }
let getMaxValues () =
    {   LastPayment= dc.Payments.Select(keys.PaymentKey).Max()
        LastJournalEntry=dc.JournalEntries.Select(keys.JEKey).Max()
        LastPaymentItem=dc.PaymentItems.Select(keys.PyIKey).Max()
        //shady, no idea how to track this
        LastReversal= getLastPaymentReversal()
        Reversals=dc.PaymentReversals.Count()
    }
let getTableName (x:Table<_>) =
    x.GetType().GenericTypeArguments.[0].Name
let diffStatus oldStatus newStatus = 
    let getNew (x:Table<'t>) (expr: Expression<Func<'t,bool>>) =
        let newItems = x.Where(expr) |> List.ofSeq
        let title = sprintf "new %ss" <| getTableName x
        newItems |> dumpt title |> ignore
        newItems
    
    if newStatus.LastPayment <= oldStatus.LastPayment && not debug then
        None
    else
        let newPayments = getNew dc.Payments 
                                (Expr.Quote(fun py -> oldStatus.LastPayment < py.PaymentID && py.PaymentID <= newStatus.LastPayment))
        let newJEs = getNew dc.JournalEntries (Expr.Quote(fun je -> oldStatus.LastJournalEntry < je.JournalEntryID && je.JournalEntryID <= newStatus.LastJournalEntry))
        let newPyIs = getNew dc.PaymentItems  <| Expr.Quote(fun pyi -> oldStatus.LastPaymentItem < pyi.PaymentItemID && pyi.PaymentItemID <= newStatus.LastJournalEntry)
            //dc.PaymentItems.Where(fun pyi -> pyi.PaymentItemID > oldStatus.LastPaymentItem && pyi.PaymentItemID <= newStatus.LastPaymentItem) |> List.ofSeq
        (newPayments,newJEs,newPyIs) |> Some
        
let status = getMaxValues()
status.Dump()

    
let isVoid = Util.ReadLine("Hit enter once payment has been made, Void or payment?", "payment",["payment";"void"]) = "void"

let afterPaymentStatus = getMaxValues()

afterPaymentStatus.Dump()

match isVoid,diffStatus status afterPaymentStatus with
| true, Some (pys,jes,pyis) ->
    // expect a voided payment added
    if pys |> Seq.exists(fun _ -> true) |> not then
        printfn "No voiding payment was created"
    if jes |> Seq.exists(fun _ -> true) |> not then
        printfn "No voiding je was created"
    if pyis |> Seq.exists(fun _ -> true) |> not then
        printfn "No voiding pyi was created, not sure one is required"
        
    ()
| _ -> printfn "unhandled case"    



