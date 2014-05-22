<Query Kind="FSharpProgram" />

type String with 
	member x.Before(delimiter:string) : string = 
		x.Substring(0,x.IndexOf(delimiter))
	member x.After(delimiter:string) : string =
		x.Substring(x.IndexOf(delimiter)+delimiter.Length)
	member x.BeforeOrSelf(delimiter:string) : string =
		match x with 
		| x when x.Contains(delimiter) -> x.Before(delimiter)
		| _ -> x
	member x.AfterOrSelf(delimiter:string) : string =
		match x with
		| x when x.Contains(delimiter) -> x.After(delimiter)
		| _ -> x
//[<AbstractClass; Sealed>] // http://stackoverflow.com/questions/13101995/defining-static-classes-in-f
//[<Obsolete("not necessary in F#")>]
//type LambdaOp private () =
type [<Measure>]  minute
//type [<Measure>] second

//let seconds_per_minute = 60<second> / 1<minute>

type System.Int32 with
	member x.Minutes = 
		let y:double = double x // http://msdn.microsoft.com/en-us/library/dd233220.aspx
		TimeSpan.FromMinutes(y)
	member x.AsMinutes =
		let y:decimal = decimal x
		y * 1.0m<minute>

type System.DateTime with
	member dt.ToTimeString =
		dt.ToString().After(" ")
	member dt.UnixTicks =
		let d1 = new DateTime(1970,1,1)
		let d2 = dt.ToUniversalTime()
		let ts = new TimeSpan(d2.Ticks - d1.Ticks)
		ts.TotalMilliseconds
	member dt.StartOfWeek (weekStart : DayOfWeek) =
		let diff = int dt.DayOfWeek - int weekStart
		let absDiff =float ( if diff<0 then diff+7 else diff)
		dt.AddDays(-1.0*absDiff).Date

type System.Random with
	member rnd.NextBool = rnd.NextDouble() > 0.5
type System.Xml.Linq.XElement with
	member node.GetAttribValOrNull xname = // c# idiomatic
		let xa = node.Attribute(xname)
		if xa = null then null else xa.Value
	member node.GetAttribVal xname = // f# idiomatic ?
		match node.Attribute(xname) with
		| x when x <> null -> Some(x.Value)
		| _ -> None
//[<EntryPoint>]
let main args=
	Debug.Assert ("testing".Before( "ing") = "test")
	Debug.Assert ("testing".After("test") = "ing")
	Debug.Assert ("testing".BeforeOrSelf("ING") = "testing")
	Debug.Assert ("testing".BeforeOrSelf("ng")="testi")
	let dtStart= DateTime(2014,5,22)
	
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Monday) = DateTime(2014,5,19))
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Tuesday) = DateTime(2014,5,20))
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Wednesday) = DateTime(2014,5,21))
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Thursday) = DateTime(2014,5,22))
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Friday) = DateTime(2014,5,16))
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Saturday) = DateTime(2014,5,17))
	Debug.Assert ( dtStart.StartOfWeek(DayOfWeek.Sunday) = DateTime(2014,5,18))
	let XName n = System.Xml.Linq.XNamespace.None+ n
	let xmlWithAttrib = System.Xml.Linq.XElement(XName "element",System.Xml.Linq.XAttribute(XName "test","1") )
	Debug.Assert( xmlWithAttrib.GetAttribVal(XName "test") = Some("1"))
	Debug.Assert( xmlWithAttrib.GetAttribVal(XName "test1") = None)
	Debug.Assert( xmlWithAttrib.GetAttribValOrNull(XName "test1") = null)
	Debug.Assert( xmlWithAttrib.GetAttribValOrNull(XName "test") = "1")
	0
	
main() |> Dump