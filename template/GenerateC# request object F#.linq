<Query Kind="FSharpProgram">
  <NuGetReference Prerelease="true">Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <Namespace>Microsoft.CSharp</Namespace>
  <Namespace>System.CodeDom</Namespace>
  <Namespace>System.CodeDom.Compiler</Namespace>
</Query>

// http://stevegilham.blogspot.com/2011/05/hello-codedom-building-c-and-f-code.html
// consider instead: http://stackoverflow.com/questions/22879776/using-the-open-source-released-roslyn-to-read-code-file-and-generate-new-code

type System.String with
	member x.Before(delimiter:string) = 
		x.Substring(0,x.IndexOf(delimiter))
	member x.BeforeAnyOf(delimiters:string list) = 
		let index,delimiter = 
			delimiters 
			|> Seq.map ( fun delimiter -> x.IndexOf(delimiter),delimiter)
			|> Seq.filter( fun (index,delimiter) -> index >=0)
			|> Seq.minBy (fun (index, delimiter) -> index)
		x.Substring(0,index)
		
	member x.After(delimiter:string) = 
		x.Substring(x.IndexOf(delimiter) + delimiter.Length)
let GenerateCode (compileUnit: CodeCompileUnit) (provider:CodeDomProvider) =
	let sb = StringBuilder()
	use sw = new StringWriter(sb)
	use tw = new IndentedTextWriter(sw, "    ")
	provider.GenerateCodeFromCompileUnit(compileUnit, tw, new CodeGeneratorOptions())
	tw.Close()
	sb.ToString()
	
let GenerateCSharpCode( compileUnit: CodeCompileUnit) =
	use provider = new CSharpCodeProvider()
	GenerateCode compileUnit provider
let requestNs,requestClass = 
	let fullText:string = System.IO.File.ReadAllText(requestTargetFile)
	let lines = fullText.SplitLines()
	
	let requestNs =  fullText.After("namespace").Before("{").Trim()
	let requestClass = fullText.After("class ").BeforeAnyOf([":";"{";"<"])
	requestNs,requestClass
	
let compileUnit = CodeCompileUnit()
let globalNs = CodeNamespace()
for ns in [ "System"; "System.ComponentModel.DataAnnotations";"System.Runtime.Serialization"] do
	globalNs.Imports.Add(CodeNamespaceImport(ns))
	
compileUnit.Namespaces.Add(globalNs) |> ignore
let samples = CodeNamespace(requestNs)
compileUnit.Namespaces.Add( samples ) |> ignore
type CodeDomProperty = {Name:string; Required:bool; ReturnType:Type}
let class1 = 
	let generateProperty (codeDomProperty:CodeDomProperty) = 
		let indentation = "\t\t\t"
		let typeMap = dict [ 
						"System.Boolean","bool"
						"System.Guid", "Guid"
						"System.Int32", "int"
						"System.Decimal", "decimal"
						"System.Collections.Generic.IEnumerable`1","IEnumerable"
						]
		let lookupOrDefault t = 
			let found,value = typeMap.TryGetValue(t)
			if found then value else t
		let returnType =
			
			if codeDomProperty.ReturnType.Name.StartsWith("Nullable") 
				then 
					(lookupOrDefault <| Nullable.GetUnderlyingType(codeDomProperty.ReturnType).ToString())+"?"
				else 
					lookupOrDefault <| codeDomProperty.ReturnType.ToString()
		let cstm = CodeSnippetTypeMember(Text = (sprintf "%s%s %s %s {get; set;}" indentation "public" returnType (codeDomProperty.Name)) )
		let addPropAttr tag = cstm.Text <- sprintf "%s[%s]\r\n%s" indentation tag cstm.Text
		addPropAttr "DataMember"
		if codeDomProperty.Required then 
			addPropAttr "Required"
		cstm
	let cls = CodeTypeDeclaration(requestClass)
	let codeAttrDecl = CodeAttributeDeclaration("DataContract")
	cls.CustomAttributes.Add(codeAttrDecl) |> ignore
	let properties = [ 
						{CodeDomProperty.Name="QuotaGroupGuid";ReturnType = typedefof<Guid>; Required = true}
						{Name="ProjectGuid";ReturnType = typedefof<Guid>; Required = true}
						
						{Name="RequestedCompletes";ReturnType = typeof<Nullable<int>>; Required = false}
						{Name="QuotaGroupName";ReturnType = typedefof<String>; Required = false}
						{Name="BidResponseRate";ReturnType = typeof<Nullable<decimal>>; Required = false}
						{Name="IsClosed";ReturnType = typeof<Nullable<bool>>; Required = false}
						{Name="IncidenceRate";ReturnType = typeof<Nullable<decimal>>; Required = false}
						{Name="AllowDynamicSurveys";ReturnType = typeof<Nullable<bool>>; Required = false}
						{Name="PanelSources";ReturnType = typeof<IEnumerable<int>>; Required = false}
						//{Name="ExternalSupplierSources";ReturnType = typeof<IEnumerable<int>>; Required = false}
						
						{Name="CompleteRewardOverride";ReturnType = typeof<Nullable<decimal>>; Required = false}
						{Name="TerminateRewardOverride";ReturnType = typeof<Nullable<decimal>>; Required = false}
						{Name="OverQuotaRewardOverride";ReturnType = typeof<Nullable<decimal>>; Required = false}
						
						{Name="CompletesTrigger";ReturnType = typeof<Nullable<int>>; Required = false}
						{Name="OverQuotasTrigger";ReturnType = typeof<Nullable<int>>; Required = false}
						{Name="TerminatesTrigger";ReturnType = typeof<Nullable<int>>; Required = false}
					]
	
	for p in properties do
		let propDom = generateProperty p
		cls.Members.Add propDom |> ignore
	cls

samples.Types.Add(class1) |> ignore
GenerateCSharpCode compileUnit |> Dump