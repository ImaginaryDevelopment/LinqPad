<Query Kind="FSharpProgram">
  
</Query>

// adapted from http://stackoverflow.com/questions/2779746/is-there-a-textwriter-interface-to-the-system-diagnostics-debug-class
type HandyOutStream(f, flushOpt:(unit->unit) option) =
    inherit System.IO.Stream()
    override x.Write(buffer,offset,count) = 
        f(Encoding.Unicode.GetString(buffer,offset,count))
    override x.CanRead with get() = false
    override x.CanSeek with get() = false
    override x.CanWrite with get() = true
    override x.Flush() = match flushOpt with | Some f -> f() | None -> ()
    override x.Length with get() = raise <| InvalidOperationException()
    override x.Read (buffer, offset,count) = raise <| InvalidOperationException()
    override x.Seek (offset, origin) = raise <| InvalidOperationException()
    override x.SetLength value = raise <| InvalidOperationException()
    override x.Position with get() = raise <| InvalidOperationException() and set _ = raise <| InvalidOperationException()
    
type HandyTextWriter(f, flushOpt) as self =
    inherit StreamWriter(new HandyOutStream(f, flushOpt), Encoding.Unicode, 1024)
    do
        self.AutoFlush <- true
        
let dc = new TypedDataContext()
//dc.Log <- HandyTextWriter(Debug.Write, Some Debug.Flush)    
//dc.Log <- HandyTextWriter(Console.Write, None)
dc.Log <- HandyTextWriter(Console.Write, Some (Console.Out.Flush)) // None)

//let myOrderFunction (a: Appointments) = a.AppointmentID
//dc.Appointments.OrderByDescending(myOrderFunction).Dump()

// select appointmentid from appointments order by appointmentid desc
dc.Appointments.OrderByDescending(fun a -> a.AppointmentID).Dump()

// select claimid from claims order by claimid desc
dc.Claims.OrderByDescending(fun c -> c.ClaimID).Dump()

// select patientid from patients order by patientid desc
dc.Patients.OrderByDescending(fun p -> p.PatientID).Dump()