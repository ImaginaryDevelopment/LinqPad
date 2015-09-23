// OData type providers
// http://www.odata.org/odata-services/
// sample: https://msdn.microsoft.com/en-us/library/hh156504.aspx
#r @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\packages\FSharp.Data.TypeProviders.0.0.1\lib\net40\FSharp.Data.TypeProviders.dll"
open Microsoft.FSharp.Data.TypeProviders
#r "System.Data.Services.Client"
module NorthWindSamples = 
    type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">
    let db = Northwind.GetDataContext()
    let getCustomers() = db.Customers |> Array.ofSeq

    
// did not work:
//module OdataReferenceSamples = 
//    type ODataRef = ODataService<"http://services.odata.org/V4/OData/OData.svc/">
    
