<Query Kind="FSharpProgram" />

// check hint paths for files that don't exist or are absolute
//let onlySlnProjects = true
[<AutoOpen>]
module preferences = 
	let includeAllXaml = true
	let interestedRefName = Some "Foundation"
[<AutoOpen>]
module helpers = 
	let optFromBool s = if s then Some() else None
	let optTFromBool s f = if f s then Some s else None
	let containsC v (c:StringComparison) (s:string)  = s.IndexOf(v,c) >= 0
	let endsWithC v (c:StringComparison) (s:string)  = s.EndsWith(v,c)
	//let boolFromOpt o = match o with |Some _ -> true |None -> false
	let getEnvVar var = 
		match System.Environment.GetEnvironmentVariable(var) with
		| null -> None
		| value -> Some value
	// get attrib value if attribute exists and is not null
	let getAttribValueOrNone  name (x:XElement) = 
			let xa= x.Attribute(XNamespace.None + name)
			if xa=null then None
				else
					if xa.Value = null then None
					else Some xa.Value		
	let dump x = x.Dump(); x
	
//	let includeEndsWith ending x = 
//		let include' = getAttribValueOrNone x "Include"
//		match include' with
//		|Some x -> 
//			//x.Dump("includes!"); 
//			x.EndsWith(ending) 
//		| None -> false
	let (|IncludeEndsWith|_|) ending xe = 
		let f x= optTFromBool x <| endsWithC ending StringComparison.InvariantCultureIgnoreCase
		let f' = Option.bind f
		
		(getAttribValueOrNone "include"  >> f')
		//if includeEndsWith ending x then Some x else None
	
	let isNotNullOrEmpty s = 
		System.String.IsNullOrEmpty s |> not
	
	let isNotNullNoneOrEmpty s =
		match s with 
		| Some s -> System.String.IsNullOrEmpty(s) |> not
		| None -> false
	let (|InvariantContainsI|_|) value (input:string) = optTFromBool input <| containsC value StringComparison.InvariantCultureIgnoreCase
	let (|InvariantEndsWithI|_|) value (input:string) = optTFromBool input <| endsWithC value StringComparison.InvariantCultureIgnoreCase
	let boolFromAp (|Pattern|_|) v = match v with |Pattern -> true |_ -> false
	
let baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"))
let projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);

let csProjects=
	projects
	|> Seq.filter (fun f->f.EndsWith(".csproj") && f.Contains("test", StringComparison.InvariantCultureIgnoreCase)=false //don't check testing projects
		//&& nonSlnProjects.All(non=>f.EndsWith(non)==false)
		
		)//.Take(2);
	|> Array.ofSeq
csProjects.Count().Dump ("checking projects")

type BaseItem = {Path:string;Doc:XDocument;RootNs:XNamespace;ProjNode:XElement}
let baseItems=
		query {
			for i in csProjects	do
			//let isSlnProject=i.Contains("NonSln")=false && i.Contains("playground")=false
			let doc=XDocument.Load(i)
			let rootns=doc.Root.Name.Namespace
			let proj=doc.Element(rootns+"Project") //.DumpIf(fun x->x=null,i+" has no project element")
			where (proj<>null)
			select { Path=i; Doc=doc;RootNs=rootns;ProjNode=proj}
		}
		|> Array.ofSeq

type Reference = {HintPath:string;Reference:string;Path:string;AbsPath:string;Exists:bool}
let nonPackageReferences = 
		let getFullHintPath path hintPath = 
			let path = System.IO.Path.GetDirectoryName(path)
			System.IO.Path.Combine(path,hintPath)
			|> System.IO.Path.GetFullPath
			
		let mySeq= seq{
			for i in baseItems do
				for ig in i.ProjNode.Elements(i.RootNs+"ItemGroup") do
					
					for r in ig.Elements(i.RootNs+"Reference") do
						r.ToString().Dump("test ref")
						let hintPath=r.Element(i.RootNs+"HintPath")
						if hintPath<>null then //&& hp.Value.Contains("packages")==false
							let absPath=if hintPath.Value.Contains(":") then hintPath.Value else getFullHintPath i.Path hintPath.Value
							let exists= System.IO.File.Exists(absPath) //TODO: solve for checking exists on network drives
							if not exists || hintPath.Value.Contains(":") || hintPath.Value.Contains("\\\\") then
								yield {HintPath=hintPath.Value;Reference=r.ToString();Path=i.Path;AbsPath=absPath;Exists=exists}	
		}
		mySeq

type GroupedReferenceValue = {Exists:bool; Ref:Object}	
 
( 
	let npr=
		nonPackageReferences 
		|> Seq.groupBy (fun pr-> pr.Path) 
		|> Seq.map (fun (k,group) -> 
					(k,group|>
						Seq.map (fun pr ->{Exists=pr.Exists; Ref =Util.OnDemand(pr.HintPath, (fun _ -> (pr.Reference,pr.AbsPath)))})
					)
				)
	npr.Dump("nonpackage")
)
	
type ProjectReference ={Project:string;Condition:XAttribute;Items: seq<XElement>} 	
let hasInclude (x:XElement) = getAttribValueOrNone "Include" x

let includeIgnores = [ "Compile";"None";"EmbeddedResource"]
let shouldIgnore (x:XElement) = includeIgnores |> Seq.exists(fun ignore' -> ignore' = x.Name.LocalName) |> not
let references= seq { for i in baseItems do
						for itemGroup in i.ProjNode.Elements(i.RootNs+"ItemGroup") do
							let allIncludes = itemGroup.Nodes().Cast<XElement>() |> Seq.choose hasInclude |> Array.ofSeq
							
							// xaml files can do their own referencing, I think
							let xaml = if includeAllXaml then allIncludes |> Seq.choose ((|IncludeEndsWith|_|) "xaml") else Seq.empty
								
//							if Seq.isEmpty xaml = false then 
//								xaml.Dump("xaml includes")
								
							let items = Seq.concat [
												allIncludes |> Seq.filter shouldIgnore
												xaml 
												]
								
							if Seq.isEmpty items = false then
								yield {Project=i.Path;Condition=itemGroup.Attribute(XNamespace.None+"Condition");Items= items}
	}

(
	
	let hasInclude (x:XElement,name):bool = getAttribValueOrNone x "Include" |> Option.bind (fun v -> optFromBool <| v.Contains(name))  |> boolFromOpt
	match interestedRefName with
	|Some interestedRefName ->
		let filtered = references |> 
						Seq.filter (fun r ->
							Seq.exists (fun i -> hasInclude(i,interestedRefName)) r.Items
						) |> Seq.map( fun pr -> {pr with Items = pr.Items |> Seq.filter (fun i-> i.Attribute(XNamespace.None+"Include").Value.Contains(interestedRefName)) |> Seq.toArray
						})
		filtered.Dump(interestedRefName) 
	| _ -> ()
)
references.Dump("references")