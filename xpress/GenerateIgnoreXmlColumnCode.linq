<Query Kind="FSharpProgram">
  <NuGetReference>Humanizer</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Humanizer</Namespace>
</Query>

// Brandon D'Imperio
// http://imaginarydevelopment.blogspot.com

let dc = new TypedDataContext()
//options
let interestedType = typeof<DrFirstMedications>

let camelize = false // use camel case when declaring variable names
let fixId = false // use personId instead of personID when declaring variable names
let indention = "\t" // change to spaces if desired
type Targets = |C |F

let targetText = Targets.C // set the language you want to come out here

// begin code
let targetProperties = 
	interestedType.GetFields()
	|> Seq.filter (fun prop -> prop.FieldType <> typeof<XElement>)
	|> Seq.map (fun prop -> prop.Name)
	|> Seq.sort
let targetTextF selector decompose assign = 
	let selection = sprintf "dc.%s.Select( fun m -> \r\n%s\r\n%s)" interestedType.Name selector indention
	sprintf "%s\r\n |> Array.ofSeq\r\n|> Array.map ( \r\n%sfun (%s)\r\n%s%s->\r\n%s%s(%s)\r\n%s)" selection indention decompose indention indention indention interestedType.Name assign indention
let targetTextC selector assign = 
	let selection :string = sprintf "%s.Select( m => new{\r\n%s\r\n%s})\r\n%s.ToArray()\r\n" interestedType.Name selector indention indention
	sprintf "%s%s.Select( m=>\r\n\tnew %s{%s}\r\n%s)" selection indention indention assign indention


//targetProperties.Dump()

let perLine x indent seq = 
	seq 
	|> Seq.mapi (fun i s -> if i % x = 0 then sprintf "\r\n%s%s" indent s else s)
let selectorF = 
	targetProperties
	|> Seq.map (fun s -> sprintf "\t\tm.%s" s)
	|> fun s -> String.Join(",\r\n", s)
let decomposed : (string*string) seq= 
	targetProperties 
	|> Seq.map
		(fun s-> 
			if fixId && s.EndsWith("ID") then
				s,(s.Substring(0, s.Length-1) + "d")
			else s,s)
	|> Seq.map (fun (name,field) -> if camelize then (name,field.Camelize()) else name,field)
let decomposeF =
	decomposed
	|> Seq.map snd
	|> perLine 4 "\t\t\t"
	|> fun props -> String.Join(", ",props)
let assign =
	let assignF = 
		decomposed
		|> Seq.map (fun (s,camel) -> sprintf "%s = %s" s camel)
		
	let assignC = 
		decomposed
		|> Seq.map (fun (s,camel) -> sprintf "%s = m.%s" s camel)
		
	match targetText with
	|F -> assignF
	|C -> assignC
	|> perLine 4 "\t\t\t"
	|> fun s-> String.Join(",", s)
	
//selectorF.Dump()

if targetText = C then 
	targetTextC selectorF assign
	else  targetTextF selectorF decomposeF assign
|> Dump