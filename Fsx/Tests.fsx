open System
let cString = "Data Source=(local);Integrated Security=SSPI;Initial Catalog=PmRewriteApplicationDatabase;app=Tests.fsx"
#I "bin/Debug"
#r "Pm.Schema"
#r "Pm.Dal"
let assertion b =
    assert b
    if not b then
        failwithf "Assertion failed"
let assertNotEmpty src = 
    assertion (Seq.isEmpty src |> not)

let facilities = Pm.Dal.Facilities.load(cString)
assertNotEmpty facilities
let facility = facilities |> Seq.head
let AppointmentTests facilityId = 
    assertNotEmpty <| Pm.Dal.Appointment.load facilityId cString
    assertNotEmpty <| Pm.Dal.Appointment.load2 facilityId cString

AppointmentTests facility.FacilityId


