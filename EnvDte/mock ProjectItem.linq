<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System.Runtime.CompilerServices</Namespace>
</Query>


let makePi name kind fileNames fcm = 
    let name = ref name
    let isDirty =ref  false
    let saved = ref false
    let empty = null
    let fileNames:string[] = fileNames
    { 
        new EnvDTE.ProjectItem
        with
            member this.FileNames with get(index:int16) = fileNames.[ int(index)]
            member this.Open(viewKind) = null
            member this.Remove() = ()
            member this.get_IsOpen s = false
            member this.Kind = kind
            member this.ProjectItems = null
            member this.ExpandView() = ()
            member this.DTE = null
            member this.Properties = null
            member this.Object = null
            member this.Collection = null
            member this.Saved with get() = !saved and set(value) = saved := value
            member this.ExtenderCATID = null
            member this.Name 
                with get() = !name
                and set(value) = name := value
            member this.get_Extender (s:string) : obj = null
            member this.FileCount = 1s
            member this.SaveAs nfn = false
            member this.ContainingProject = null
            member this.SubProject = null
            member this.Document = null
            member this.Save fn = ()
            member this.IsDirty with get() = !isDirty  and set(value) = isDirty := value
            member this.Delete () = ()
            member this.get_FileCodeModel():EnvDTE.FileCodeModel = fcm
            member this.get_ExtenderNames() = null
            member this.get_ConfigurationManager () = null
    }

let WriteLine (s:string)= Console.WriteLine(s)
let makeCodeFile fullPath =
	let name = Path.GetFileName(fullPath)
	makePi name "{6BB5F8EE-4483-11D3-8BCF-00C04F8EC28C}" [| fullPath |] null
let projectItems ()= //leave this as a sequence, don't eager load the whole directory of cs files into memory
	Directory.GetFiles(@"C:\TFS\XC-SourceDev\Source-development","*.cs", SearchOption.AllDirectories)
	//|> fun files -> files.Dump("Files"); files
	|> Seq.map (fun s-> makeCodeFile s )
	//|> Seq.map ( fun s-> s.FileNames(0s).Dump("filename"); s)
	|> Array.ofSeq
let codeModelItems = 
	projectItems().Where(fun pi -> (* pi.FileCodeModel <> null  && *) File.Exists(pi.FileNames(0s)))
	|> Array.ofSeq
WriteLine(sprintf "found %A code models" (codeModelItems.Count()))
let sprocCallFastRegex = Regex(@"^.*""usp.*$",RegexOptions.Compiled ||| RegexOptions.Multiline)
let sprocCallRegex = Regex(@"^(?!\s*//).*""usp.*$",RegexOptions.Compiled ||| RegexOptions.Multiline)
let getSprocCalls (s:string) =
	
	seq{
		
		let fastMatches = sprocCallFastRegex.Matches(s).Cast<Match>()
		yield! (fastMatches |> Seq.filter (fun fm -> sprocCallRegex.IsMatch(fm.Value)))
	}
	|> Array.ofSeq
	
type CodeModelFileWithText = { PI:ProjectItem; Text:string; Filename: string}
let codeModelFilesWithText = codeModelItems.Select(fun pi -> {PI= pi; Filename=pi.FileNames(0s); Text = File.ReadAllText(pi.FileNames(0s))}).ToArray();

let sprocCodeFiles = 
	codeModelFilesWithText
	|> Seq.map (fun cmf -> cmf,getSprocCalls cmf.Text)
	|> Seq.filter(fun (cmf,sprocCalls) -> sprocCalls.Any())
	|> Seq.map (fun (cmf,sprocCalls) -> (cmf, sprocCalls |> Seq.map (fun sc -> sc.Value) ))
	|> Array.ofSeq
	
WriteLine (sprintf "found %A sproc calling code models" sprocCodeFiles.Length)

sprocCodeFiles
|> Seq.iter 
	(
		fun (c,s) -> 
			let cText = sprintf "%s" c.Filename
			WriteLine( cText + "\t"+ String.Join(",",s))
	)




