<Query Kind="FSharpProgram">
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

type ModelCollector private() = 
    inherit CSharpSyntaxWalker()
    let implementedInterfaces = Dictionary<string,List<string>>()
    member x.ImplementedInterfaces = implementedInterfaces
    override x.VisitBaseList(node:BaseListSyntax) = 
        let parentIdentifier = (node.Parent :?> ClassDeclarationSyntax).Identifier.ValueText
        let bases = node.Types.Select(fun t -> t.Type).OfType<IdentifierNameSyntax>().Select(fun ins -> ins.Identifier.ValueText).ToList()
        x.ImplementedInterfaces.Add(parentIdentifier,bases)
        (parentIdentifier,bases,x.ImplementedInterfaces).Dump("implementedInterfaces")
        base.VisitBaseList(node)
    static member testClass = """class ObjectCreationMethodInfo:IComponent {     	public string Type{get;set;} public string MethodName {get;set;} public bool CallsCreate{get;set;} }"""
    static member VisitClassInterfaces(root:CompilationUnitSyntax) = 
        let mc = ModelCollector()
        mc.Visit(root)
        mc.ImplementedInterfaces
    
let text = ModelCollector.testClass
let tree = CSharpSyntaxTree.ParseText(text)
		
let root = tree.GetRoot() :?> CompilationUnitSyntax
let interfaces = ModelCollector.VisitClassInterfaces(root)
interfaces.Dump("implemented final")
		