
// microsoft's type provider for SQL
//download from https://www.nuget.org/api/v2/package/FSharp.Data.TypeProviders/
// install into a local project so it gets downloaded, then reference it is the easiest way

#r @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\packages\FSharp.Data.TypeProviders.0.0.1\lib\net40\FSharp.Data.TypeProviders.dll"
#r "System.Data"
#r "System.Data.Linq"

open System
open System.Data
open System.Data.Linq
// for linq!
open System.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq

[<Literal>]
let connectionString = "Data Source=prog7-pc;Initial Catalog=PmRewriteApplicationDatabase;User Id=Fer;Password=xpress;"
// https://msdn.microsoft.com/en-us/library/hh361033.aspx
type dbType = SqlDataConnection<connectionString>
[<Measure>]
type UserId

type ClaimMatcher = 
    |ClaimId of int
    |ApptId of int
    |UserIdEditing of int
    |SomeOtherId of string

module DbExplore =
    let db = dbType.GetDataContext(connectionString )
    //DbExplore.db.Users |> Seq.find (fun _ -> true);;
    
    let editClaimUser (userId:int<UserId>) claimId = 
        let claim = db.Claims |> Seq.find( fun c -> c.ClaimID = claimId)
        let claim = db.Claims.First(fun c -> c.ClaimID = claimId) // needs System.Linq
        let claim = 
            query {
                for c in db.Claims do
                where (c.ClaimID = claimId)
                select c
                head
            }

        claim.UserIdCurrentlyEditing <- Nullable (int userId)
        db.DataContext.SubmitChanges()
        claim

    let checkoutClaim userId claimId = 
        db.Connection.Open()
        use tran = db.Connection.BeginTransaction()
        let claim = db.Claims |> Seq.find( fun c -> c.ClaimID = claimId)

        // claim.UserIdCurrentlyEditing != userId
        if (claim.UserIdCurrentlyEditing ?<> userId) then
            tran.Rollback()
            null 
        elif claim.UserIdCurrentlyEditing ?= userId then
            claim
        else
            claim.UserIdCurrentlyEditing <- Nullable userId
            db.DataContext.SubmitChanges()
            tran.Commit()
            claim

    let tryCheckoutClaim userId claimId = 
        try
            let claim = checkoutClaim userId claimId
            if claim <> null then
                Some claim
            else
                None
        with | ex -> None

    let checkoutClaimByApptId userId apptId =
        db.Connection.Open()
        use tran = db.Connection.BeginTransaction()
        let claim = db.Claims |> Seq.find( fun c -> c.AppointmentID ?= apptId)

        if (claim.UserIdCurrentlyEditing ?<> userId) then
            tran.Rollback()
            null 
        elif claim.UserIdCurrentlyEditing ?= userId then
            claim
        else
            claim.UserIdCurrentlyEditing <- Nullable userId
            db.DataContext.SubmitChanges()
            tran.Commit()
            claim

    let tryTran cn f = 
        use tran = db.Connection.BeginTransaction()
        if f() then
           tran.Commit();
           else 
            tran.Rollback()

    let tryTran' f = 
        use tran = db.Connection.BeginTransaction()
        let doCommit, result = f()
        if doCommit then
            tran.Commit()
        else
            tran.Rollback()
        result

    let checkoutClaim' userId claimId =
        db.Connection.Open()
        let tryTran = tryTran db.Connection

        let fGet() = 
            let claim = db.Claims |> Seq.find( fun c -> c.ClaimID = claimId)
            if claim.UserIdCurrentlyEditing ?<> userId then
                true, claim.UserIdCurrentlyEditing
            elif claim.UserIdCurrentlyEditing ?= userId then
                false, claim.UserIdCurrentlyEditing
            else
                claim.UserIdCurrentlyEditing <- Nullable userId
                db.DataContext.SubmitChanges()
                true,claim.UserIdCurrentlyEditing
        tryTran' fGet

    let checkoutClaimByApptId' userId apptId = 
        db.Connection.Open()
        let tryTran = tryTran db.Connection

        let fGet() = 
            let claim = db.Claims |> Seq.find( fun c -> c.AppointmentID ?= apptId)
            if claim.UserIdCurrentlyEditing ?<> userId then
                true, claim.UserIdCurrentlyEditing
            elif claim.UserIdCurrentlyEditing ?= userId then
                false, claim.UserIdCurrentlyEditing
            else
                claim.UserIdCurrentlyEditing <- Nullable userId
                db.DataContext.SubmitChanges()
                true,claim.UserIdCurrentlyEditing
        tryTran' fGet

        // claim.UserIdCurrentlyEditing != userId
        
        //
//    let checkoutClaim' userId matcher = 
//        let claim = 
//            db.Claims 
//            |> match matcher with
//                | ClaimId claimId -> Seq.find( fun c -> c.ClaimID = claimId)
//                | UserIdEditing userId-> Seq.find( fun c -> c.UserIdCurrentlyEditing ?= userId)
//                | ApptId apptId -> Seq.find( fun c -> c.AppointmentID ?= apptId)
//                | SomeOtherId s -> Seq.find(fun c -> c.AccidentType = s)
//
//        if (claim.UserIdCurrentlyEditing ?<> userId) then
//            null 
//        elif claim.UserIdCurrentlyEditing ?= userId then
//            claim
//        else
//            claim.UserIdCurrentlyEditing <- Nullable userId
//            db.DataContext.SubmitChanges()
//            claim