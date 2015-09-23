// https://github.com/fsprojects/SQLProvider
// http://fsprojects.github.io/SQLProvider/
// http://fsprojects.github.io/SQLProvider/general.html
#r @"C:\tfs\PM\packages\SQLProvider.0.0.9-alpha\lib\net40\FSharp.Data.SqlProvider.dll"

open System
open System.Linq
open FSharp.Data.Sql

[<Literal>]
let connectionString = "Data Source=prog7-pc;Initial Catalog=PmRewriteApplicationDatabase;User Id=Fer;Password=xpress;"
type sql = SqlDataProvider<ConnectionString = connectionString>
let ctx = sql.GetDataContext()

let getAccidents() = ctx.``[dbo].[Accidents]``
let loadAccidents() = 
    ctx.``[dbo].[Accidents]`` 
    |> Seq.toArray
//    |> Seq.map( fun a -> )

// join
let loadPatientsInfos () = 
    query { 
        for p in ctx.``[dbo].[Patients]`` do
        join pi in ctx.``[dbo].[PatientsInfo]`` on (p.PatientID = pi.PatientID)
        select (p,pi)
        // select new {p,pi}
    }

// join via FKey
let loadApptsByGuarantorType () = 
    query {
        for gt in ctx.``[dbo].[GuarantorTypes]`` do
        for appointment in gt.FK_Appointments_GuarantorTypes_GuarantorTypeId do
        select (gt, appointment)
    }
let getBillingStages()= 
    [ 
        ctx.``[dbo].[BillingStage]``.Individuals.``As Description``.``8, Completed``
        ctx.``[dbo].[BillingStage]``.Individuals.``11``
    ]

//let loadAccidents() = ctx.``Stored Procedures``
