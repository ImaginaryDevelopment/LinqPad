// http://fsprojects.github.io/FSharp.Data.SqlClient/

// contains 3 type providers
#r @"C:\tfs\PM\packages\FSharp.Data.SqlClient.1.7.0\lib\net40\FSharp.Data.SqlClient.dll"

[<Literal>]
let connectionString = "Data Source=prog7-pc;Initial Catalog=PmRewriteApplicationDatabase;User Id=Fer;Password=xpress;"
// xpresstechnol
//[<Literal>]
//let query = "
//    SELECT TOP(@TopN) PatientId
//    From Patients
//    order by patientid
//"
open FSharp.Data

type BillingStrategy = SqlEnumProvider<"SELECT Description, BillStageID FROM BillingStage", connectionString>

//needs SQL server >= 11
//type PmRewrite = SqlProgrammabilityProvider<connectionString>

//needs SQL server >= 11
//type PatientQuery = SqlCommandProvider<query,connectionString>