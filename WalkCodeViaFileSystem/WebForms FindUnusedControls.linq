<Query Kind="FSharpProgram" />

// check webforms pages for never referenced controls and pages
let onlySlnProjects = true
let baseDir=Util.ReadLine("Directory?", System.Environment.GetEnvironmentVariable("devroot"))
let projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories)

let pathExclusions = [ "test";"playground";"nonsln"]

let csProjects=
	
	let contains (f:string) exclusion = 
		f.Contains(exclusion, StringComparison.InvariantCultureIgnoreCase)
	let exclude filename = 
		pathExclusions |> Seq.exists (fun exclusion -> contains filename exclusion) |> not
	projects
	|> Seq.filter (fun f->f.EndsWith(".csproj"))
	|> Seq.filter exclude
		//.Take(2);

csProjects.Count().Dump ("checking projects")

// csProjects.Dump()

type CsProjFileItem = {Path:string;Doc:XDocument;RootNs:XNamespace;ProjNode:XElement}
let csProjFiles=
		let baseQuery = query {
			for i in csProjects	do
			let doc=XDocument.Load(i)
			let rootns=doc.Root.Name.Namespace
			let proj=doc.Element(rootns+"Project") //.DumpIf(fun x->x=null,i+" has no project element")
			where (proj<>null)
			select { Path=i; Doc=doc;RootNs=rootns;ProjNode=proj}
		}
		baseQuery.ToArray()
		
type FormReference = {Namespace:string;ControlName:string;VarName:string}

for csp in csProjFiles do 
	let webformsItems = 
			let mySeq= seq{
				for i in csProjFiles do
					for ig in i.ProjNode.Elements(i.RootNs+"ItemGroup") do
						for r in ig.Elements(i.RootNs+"Content") do
							let name = r.Attribute(XNamespace.None + "Include")
							if name<>null && (name.Value.EndsWith(".aspx") || name.Value.EndsWith(".ascx")) then
								let basePath = System.IO.Path.GetDirectoryName i.Path
								let fullPath = System.IO.Path.Combine(basePath,name.Value)
								if System.IO.File.Exists fullPath && System.IO.File.ReadAllText(fullPath).Contains("CodeBehind=") then
									
									let directoryPath = System.IO.Path.GetDirectoryName fullPath
									let filename = System.IO.Path.GetFileName(fullPath)
									yield filename,directoryPath
			}
			mySeq  // |> Seq.sortBy (fun x -> not x.IsSlnProject) //orderby i.IsSlnProject descending
	
	webformsItems.Count().Dump("checking webforms controls in "+ csp.Path)
	// webformsItems.Dump()
	let webformsItemMap = 
		webformsItems |> Seq.map(fun (filename,directoryPath) ->
		let designerItems =
			let designer = System.IO.Path.Combine(directoryPath,(filename + ".designer.cs"))
			let lines = 
				System.IO.File.ReadAllLines designer
				|> Seq.filter(fun l-> l.Contains("System.")=false &&  l.TrimStart().StartsWith("protected"))
				|> Seq.map (fun l-> l.After("global::").Before(";"))
				|> Seq.map (fun l -> {FormReference.Namespace = l.Before(" ").BeforeLast(".");ControlName=l.Before(" ").AfterLast(".");VarName= l.After(" ")})
			lines
		filename,directoryPath,designerItems)
	let usedReferences = 
		webformsItemMap 
		|> Seq.map (fun (filename,dirPath,designerItems) -> designerItems) 
		|> Seq.concat 
		|> Seq.map (fun d->d.ControlName) 
		|> Seq.distinct
		|> Seq.sort
	let notReferenced = 
		webformsItems 
		|> Seq.map(fun (filename,directoryPath) -> filename)
		|> Seq.filter(fun f -> f.EndsWith(".ascx"))
		
		|> Seq.filter (fun e-> usedReferences.Contains(e.Before("."))=false)
	notReferenced.Dump("not referenced")


