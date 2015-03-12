<Query Kind="FSharpProgram" />

type String with 

	member this.Before(delimiter:string) : string = 
		this.Substring(0,this.IndexOf(delimiter))
				
	member x.After(delimiter:string) : string =
		x.Substring(x.IndexOf(delimiter) + delimiter.Length)
	
	// inferred types
	member x.BeforeOrSelf delimiter =
		if x.Contains(delimiter) then x.Before(delimiter) else x
		
	// pattern matching option
	member x.AfterOrSelf(delimiter:string) : string =
		match x with
		| y when x.Contains(delimiter) -> y.After(delimiter)
		| _ -> x
		
	// extension method
	member x.IsNullOrEmpty () = 
		String.IsNullOrEmpty(x)
	// extension property
	member x.IsNullOrWhiteSpace =
		String.IsNullOrWhiteSpace(x)
	member x.split ( [<ParamArray>] delimiters: string array) = 
		x.Split(delimiters, StringSplitOptions.None)
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
	member el.GetAttribValOrNull xname = // return value is (idiomatic c#) string or null
		let xa = el.Attribute(xname)
		if xa = null then null else xa.Value
	member el.GetAttribVal xname = // return value is  (idiomatic f#?)  option<string>
		match el.Attribute(xname) with
		| x when x <> null -> Some(x.Value)
		| _ -> None
	member el.GetXmlNode =
		let xmlDoc = XmlDocument()
		(
			use xmlReader = el.CreateReader()
			xmlDoc.Load(xmlReader)
		)
		xmlDoc
	member el.IndexPosition() = //assumes all siblings are the same element type
		if el = null then raise <| new ArgumentNullException("el") //guard clause
		match el.Parent with
		| p when p <> null -> Seq.findIndex (fun elem -> elem = el) [|for i in el.Parent.Elements(el.Name) do yield i|]
		| _ -> -1

	member el.GetAbsoluteXPath () =
		if el = null then raise <| new ArgumentNullException("el")
		let relativeXPath (e:XElement) =
			let index = e.IndexPosition()
			let name = e.Name.LocalName;
			
			if index = -1 then "/" + name else sprintf "/%s[%d]" name index
		let ancestors = el.Ancestors() |> Seq.map (relativeXPath)
		//let reversed =
		String.Concat(ancestors.Reverse()) + relativeXPath(el)

let inline dump x = 
	x |> Dump
	x
	
type System.Xml.XmlNode with
	member node.GetXElement =
		let xDoc = XDocument()
		(
			use xmlWriter = xDoc.CreateWriter()
			node.WriteTo(xmlWriter)
		)
		xDoc.Root


//[<EntryPoint>]
let main args=
	assert ("testing".Before( "ing") = "test")
	assert ("testing".After("test") = "ing")
	assert ("testing".BeforeOrSelf("ING") = "testing")
	assert ("testing".BeforeOrSelf("ng")="testi")
	let dtStart= DateTime(2014,5,22)
	
	assert ( dtStart.StartOfWeek(DayOfWeek.Monday) = DateTime(2014,5,19))
	assert ( dtStart.StartOfWeek(DayOfWeek.Tuesday) = DateTime(2014,5,20))
	assert ( dtStart.StartOfWeek(DayOfWeek.Wednesday) = DateTime(2014,5,21))
	assert ( dtStart.StartOfWeek(DayOfWeek.Thursday) = DateTime(2014,5,22))
	assert ( dtStart.StartOfWeek(DayOfWeek.Friday) = DateTime(2014,5,16))
	assert ( dtStart.StartOfWeek(DayOfWeek.Saturday) = DateTime(2014,5,17))
	assert ( dtStart.StartOfWeek(DayOfWeek.Sunday) = DateTime(2014,5,18))
	(
		let XName n = System.Xml.Linq.XNamespace.None + n
		let child = XElement(XName "child" )
		let child2 = XElement(XName "child")
		let attrib =System.Xml.Linq.XAttribute(XName "test","1")
		let xmlWithAttrib = System.Xml.Linq.XElement(XName "element",attrib,child,child2)
		Debug.Assert( xmlWithAttrib.GetAttribVal(XName "test") = Some("1"))
		Debug.Assert( xmlWithAttrib.GetAttribVal(XName "test1") = None)
		Debug.Assert( xmlWithAttrib.GetAttribValOrNull(XName "test1") = null)
		Debug.Assert( xmlWithAttrib.GetAttribValOrNull(XName "test") = "1")
		Debug.Assert(xmlWithAttrib.GetAbsoluteXPath() = "/element")
		Debug.Assert(child.GetAbsoluteXPath() = "/element/child[1]")
		Debug.Assert(child2.GetAbsoluteXPath() = "/element/child[2]")
		// attrib.GetAbsoluteXPath().Dump() // not defined, XAttribute is not an XElement
	)
	0
	
main() |> Dump