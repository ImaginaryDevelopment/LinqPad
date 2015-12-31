// microsoft
// EF type provider
#r @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\packages\FSharp.Data.TypeProviders.0.0.1\lib\net40\FSharp.Data.TypeProviders.dll"
#r "System.Data.Entity.dll"
#r "System.Data.Linq.dll"
open System.Data.Linq
open System.Data.Entity
open Microsoft.FSharp.Data.TypeProviders

[<Literal>]
let connectionString = "Data Source=prog7-pc;Initial Catalog=PmRewriteApplicationDatabase;User Id=Fer;Password=xpress;MultipleActiveResultSets=true;"

type EntityConnection = SqlEntityConnection<ConnectionString=connectionString,
                                                    Pluralize = false>
let context = EntityConnection.GetDataContext()

let getAccidents() = context.AdmitFacility |> Array.ofSeq