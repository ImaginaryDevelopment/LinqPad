<Query Kind="FSharpProgram" />

// check hint paths for files that don't exist or are absolute
//let onlySlnProjects = true

let baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"))
let projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);

let csProjects=projects.Where(fun f->f.EndsWith(".csproj") && f.Contains("test", StringComparison.InvariantCultureIgnoreCase)=false //don't check testing projects
		//&& nonSlnProjects.All(non=>f.EndsWith(non)==false)
		
		)//.Take(2);
csProjects.Count().Dump ("checking projects")
type BaseItem = {Path:string;Doc:XDocument;RootNs:XNamespace;ProjNode:XElement}
let baseItems=
		let baseQuery = query {
			for i in csProjects	do
			//let isSlnProject=i.Contains("NonSln")=false && i.Contains("playground")=false
			let doc=XDocument.Load(i)
			let rootns=doc.Root.Name.Namespace
			let proj=doc.Element(rootns+"Project") //.DumpIf(fun x->x=null,i+" has no project element")
			where (proj<>null)
			select { Path=i; Doc=doc;RootNs=rootns;ProjNode=proj}
		}
		baseQuery.ToArray()

type Reference = {HintPath:string;Reference:string;Path:string;AbsPath:string;Exists:bool}
let nonPackageReferences = 
		let mySeq= seq{
			for i in baseItems do
				for ig in i.ProjNode.Elements(i.RootNs+"ItemGroup") do
					for r in ig.Elements(i.RootNs+"Reference") do
						let hintPath=r.Element(i.RootNs+"HintPath")
						if hintPath<>null then //&& hp.Value.Contains("packages")==false
							let absPath=if hintPath.Value.Contains(":") then hintPath.Value else System.IO.Path.GetFullPath((System.IO.Path.GetDirectoryName(i.Path)+"\\"+hintPath.Value))
							let exists= System.IO.File.Exists(absPath) //TODO: solve for checking exists on network drives
							if not exists || hintPath.Value.Contains(":") || hintPath.Value.Contains("\\\\") then
								yield {HintPath=hintPath.Value;Reference=r.ToString();Path=i.Path;AbsPath=absPath;Exists=exists}	
		}
		mySeq

type GroupedReferenceValue = {Exists:bool; Ref:Object}	
 
( 
	let npr=nonPackageReferences |>
				Seq.groupBy (fun pr-> pr.Path) |>
				Seq.map (fun (k,group) -> 
					(k,group|>
						Seq.map (fun pr ->{Exists=pr.Exists; Ref =Util.OnDemand(pr.HintPath, (fun _ -> (pr.Reference,pr.AbsPath)))})
					)
				)
	npr.Dump("nonpackage")
)

//	nonPackageReferences.Where (pr => pr.IsSlnProject==false)
//		.GroupBy (pr => pr.Path,pr=>new{ pr.Value,pr.reference,pr.Exists})	
//		.Dump("nonSlnNonPackage");
	
type ProjectReference ={Project:string;Condition:XAttribute;Items: seq<XElement>} 	
let references= seq { for i in baseItems do
						for ig in i.ProjNode.Elements(i.RootNs+"ItemGroup") do
							yield {Project=i.Path;Condition=ig.Attribute(XNamespace.None+"Condition");Items= ig.Nodes().Cast<XElement>()}
	}

(
	let interestedRefName = "log4net"
	let hasInclude (x:XElement,name):bool = x.Attribute(XNamespace.None+"Include").Value.Contains(name)
	let filtered = references |> 
					Seq.filter (fun r ->
						r.Items.Any (fun i -> hasInclude(i,interestedRefName))
					) |> Seq.map( fun pr -> {pr with Items = pr.Items |> Seq.filter (fun i-> i.Attribute(XNamespace.None+"Include").Value.Contains(interestedRefName)) |> Seq.toArray
					})
	filtered.Dump(interestedRefName) 
)
references.Dump("references")