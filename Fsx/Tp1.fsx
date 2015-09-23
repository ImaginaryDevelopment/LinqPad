// microsoft's type provider for SQL
// install into a local project so it gets downloaded, then reference it is the easiest way
#r @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\packages\FSharp.Data.TypeProviders.0.0.1\lib\net40\FSharp.Data.TypeProviders.dll"
#r "System.Data"
#r "System.Data.Linq"

open System
open System.Data
open System.Data.Linq

open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq

[<Literal>]
let connectionString = "Data Source=prog7-pc;Initial Catalog=PmRewriteApplicationDatabase;User Id=Fer;Password=xpress;"
// https://msdn.microsoft.com/en-us/library/hh361033.aspx
type dbType = SqlDataConnection<connectionString>


module DbExplore =
    let db = dbType.GetDataContext(connectionString )
    // query 1 - F# query syntax
    // what happens if you return this with the db in a proper using clause?
    let q() = 
        query{ 
            for a in db.Accidents do
            yield a
        }
    //query 2
    // doesn't care if the db is disposed after this call, array isn't lazy IEnumerable
    let loadAll = 
        db.Accidents |> Array.ofSeq
    // query 3 - dumb but still a demo
    // doesn't care if the db is disposed after call, array isn't lazy IEnumerable
    let getAccidentsWithValidIds() = 
        db.Accidents |> Seq.filter ( fun accident -> accident.AccidentID > 0) |> Array.ofSeq
    // query 4 - try to find accident with Id 1
    // doesn't care if the db is disposed, single item option is fetched
    let q4() =
        db.Accidents |> Seq.tryFind (fun accident -> accident.AccidentID = 1) 
    let countAccidents () = 
        db.Accidents |> Seq.length
    // query expressions and keywords https://msdn.microsoft.com/en-us/library/hh225374.aspx
    let doAnyNamesContain() = 
        query{
            for p in db.PatientsInfo do
            select p.LastName
            contains "D'I"
        }
    let countPatients () = 
        query {
            for p in db.Patients do
            //where(p.ForeignEHRID.HasValue)
            select p
            count
        }
    let getLastStatement () = 
        query{
        for s in db.Statements do
        last
        }