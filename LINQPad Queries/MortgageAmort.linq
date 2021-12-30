<Query Kind="FSharpProgram" />

// verified against https://www.calculator.net/mortgage-calculator.html
// nearly exact match

type IRate = decimal

module Inputs =
    let loanAmount = 299_250m

    let interestRate = 3.24m
    let years = 30;
   
type LoanEntry = {
    Balance:decimal
    TermRemaining:int
    InterestAccrued:decimal
    PrincipalPaid:decimal
}
    //with
    //    member x.ToDump() = sprintf "%A" x
type AmortizationResult = {
    LoanAmount: decimal
    InterestRate: decimal
    MonthlyPayment: decimal
    InterestPaid: decimal
    TotalPaid: decimal
    Schedule: LoanEntry list
}


module Calcs =
        
    // http://www.trelford.com/blog/post/loan.aspx   
    /// Calculates montly payment amount
    //let calculateMonthlyPayment loanAmount rate termMonths  =
    let calculateMonthlyPayment interestRate loanAmount months =
        if interestRate < 1m then failwith "Expected interest rate to be of the form 5.0m"
        let monthsPerYear = 12
        let rate = (interestRate / decimal monthsPerYear) / 100.0m
        let factor = rate + (rate / decimal (Math.Pow(float rate + 1.0, double months) - 1.))
        let payment = loanAmount * decimal factor
        Math.Round(payment,2)
        
    let advanceLoan payment rate le =
        let interest = rate / 12m * le.Balance
        let newBalance = le.Balance + interest - payment
        {
            Balance = newBalance
            TermRemaining = le.TermRemaining - 1
            InterestAccrued = interest
            PrincipalPaid = le.Balance - newBalance
        }
        
    let amortize interestRate years loanAmount =
        let start = {
            Balance = loanAmount
            TermRemaining = years * 12
            InterestAccrued = 0m
            PrincipalPaid = 0m
        } 
        let paymentAmount = calculateMonthlyPayment interestRate start.Balance start.TermRemaining
        let schedule =
            start
            |> Seq.unfold(fun le ->
                if le.TermRemaining > 0 then
                    let next = advanceLoan paymentAmount (interestRate/100m) le
                    Some (next,next)
                else None
            )
            |> List.ofSeq
        {
            LoanAmount= loanAmount
            InterestRate = interestRate
            MonthlyPayment = paymentAmount
            Schedule = schedule
            InterestPaid = schedule |> List.sumBy(fun le -> le.InterestAccrued)
            TotalPaid = schedule |> List.sumBy(fun le -> le.InterestAccrued + le.PrincipalPaid)
        }

//type LoanEntryDisplay = {
//    Balance: string
//    TermRemaining:
//    InterestAccrued:decimal
//    PrincipalPaid:decimal
//}
type AmortizationDisplay = {
    LoanAmount: string
    InterestRate: string
    MonthlyPayment: string
    InterestPaid: string
    TotalPaid: string
    Schedule: LoanEntry list
}
let formatMoney (x:decimal) =
    String.Format("{0:C}", x)
    
let formatAmort (le:AmortizationResult) = {
    LoanAmount = formatMoney le.LoanAmount
    InterestRate = sprintf "%.2f%%" le.InterestRate
    MonthlyPayment = formatMoney le.MonthlyPayment
    Schedule = le.Schedule
    InterestPaid = formatMoney le.InterestPaid
    TotalPaid = formatMoney le.TotalPaid
    
}
    
open Inputs
open Calcs


let result = amortize interestRate years loanAmount
Util.HorizontalRun(false, box <| formatAmort result, result)
|> Dump
|> ignore