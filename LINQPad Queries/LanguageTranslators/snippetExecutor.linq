<Query Kind="FSharpProgram" />

// purpose: run a vs code snippet for F#
type String with
    static member indexOf (delimiter:string) (value:string) = value.IndexOf(delimiter)
    static member after delimiter (value:string) = value.[String.indexOf delimiter value + delimiter.Length ..]
    static member before delimiter (value:string) = value.[0.. String.indexOf delimiter value - 1]
    
type XElement with
    static member descendants (value:XElement) = value.Descendants()
    static member byLocalName localname (xe:XElement) = xe.Name.LocalName = localname
    static member findDescendant localname xe = XElement.descendants xe |> Seq.find (XElement.byLocalName localname)
    static member tryFindDescendant localname (xe:XElement) = XElement.descendants xe |> Seq.tryFind (XElement.byLocalName localname)
    static member findDescendants localname xe = XElement.descendants xe |> Seq.filter (XElement.byLocalName localname)
    static member value(xe:XElement) = xe.Value
    static member attribValue name (xe:XElement) = xe.Attribute(XNamespace.None + name).Value
    
    static member tryAttribValue name (xe:XElement) = 
        match xe.Attribute(XNamespace.None + name) with
        | null -> None
        | x -> Some x.Value
        
let searchFolders = [
    """C:\Users\PROG7\Documents\Visual Studio 2015\Code Snippets\"""
    """C:\Users\PROG7\Documents\Visual Studio 2010\Code Snippets\"""
    """C:\Users\PROG7\Documents\Visual Studio 2013\Code Snippets\"""
    ]
let searchFolders' = searchFolders |> Seq.filter Directory.Exists
let snippets = searchFolders' |> Seq.collect (fun folder ->  Directory.EnumerateFiles(folder, "*.snippet", SearchOption.AllDirectories))
//snippets.Dump()
//snippets |> Seq.map (String.after "Snippets") |> Dump
//snippets |> Seq.map (String.before "Snippets") |> Dump

let sampleSnippet = """<?xml version="1.0" encoding="utf-8"?>
<CodeSnippets xmlns="http://schemas.microsoft.com/VisualStudio/2008/CodeSnippet">
  <CodeSnippet Format="1.0.0">
    <Header>
      <SnippetTypes>
        <SnippetType>Expansion</SnippetType>
      </SnippetTypes>
      <Title>StaticAccessor</Title>
      <Author>Brandon D'Imperio</Author>
      <Description>Statically Typed Generic access</Description>
      <HelpUrl>
      </HelpUrl>
      <Shortcut>getstat</Shortcut>
    </Header>
    <Snippet>
      <Declarations>
        <Literal Editable="true">
          <ID>PropName</ID>
          <ToolTip>IDisposable field</ToolTip>
          <Default>Bar</Default>
          <Function>
          </Function>
        </Literal>
        <Literal Editable="true">
          <ID>map</ID>
          <Default>getValueOrDefault</Default>
        </Literal>
      </Declarations>
      <Code Language="FSharp"><![CDATA[
        $PropName$ = $map$ (^a: (member $PropName$: _) data)
       ]]></Code>
    </Snippet>
  </CodeSnippet>
</CodeSnippets>"""
let saveSnippet paths text name= 
    for p in paths do
        let dir = Path.GetDirectoryName p
        let parent = Path.GetDirectoryName dir
        
        if Directory.Exists parent then
            printfn "Creating a folder @ %s in %s" dir parent
            Directory.CreateDirectory(dir) |> ignore
        let target = Path.Combine(p,name)
        printfn "writing to %s" target
        File.WriteAllText(target,text) |> ignore
        
saveSnippet (
    [ (* trailing \ is required *)
        """Documents\Visual Studio 2013\Code Snippets\Visual F#\"""
        """Documents\Visual Studio 2015\Code Snippets\Visual F#\"""
    ]|> Seq.map (fun f-> Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),f))) sampleSnippet "StaticAccessorF.snippet"
    
let selectedSnippet = 
    let defaultValue = "sampleSnippet"
    let snippets = List.ofSeq snippets
    let selections = defaultValue::snippets |> Array.ofSeq
    let selection = Util.ReadLine("Snippet?",defaultValue,selections)
    if selection = "sampleSnippet" then sampleSnippet else File.ReadAllText selection
let xml = XElement.Parse(selectedSnippet)

let xmlsnippets = XElement.tryFindDescendant "CodeSnippet" xml
type SnippetLanguage = 
    |FSharp
    |CSharp 
    |Sql
    |Xaml
    
type Declaration = { Id:string; Default:string; Function:string option}
type CodeSnippet = { Type:string; Title:string; Shortcut:string; Declarations: Declaration seq; Code:(SnippetLanguage*string) seq}

let mapSnippet snippetXml = 
    let getDecValue name = XElement.findDescendant name >> XElement.value
    let tryGetDecValue name xe = 
        xe
        |> XElement.tryFindDescendant name 
        |> Option.bind (XElement.value >> Some)
    let mapLanguage s = 
        match s with
        |"FSharp" -> SnippetLanguage.FSharp
    Util.OnDemand("mapping snippet", fun () -> snippetXml)
    //snippetXml.Dump("mapping snippet")
    let header = snippetXml |> XElement.findDescendant "Header"
    let snippet = snippetXml |> XElement.findDescendant "Snippet"
    let declarations = 
        snippet 
        |> XElement.findDescendant "Declarations" 
        |> XElement.findDescendants "Literal"
        |> (fun x -> x.Dump(); x)
        |> Seq.map (fun l -> 
            {
            
                Id= l |> getDecValue "ID"
                Default= l |> getDecValue "Default"
                Function = l |> tryGetDecValue "Function"
        })
    {
        Type = header |> XElement.findDescendant "SnippetType" |> XElement.value
        Title= header  |> XElement.findDescendant "Title" |> XElement.value
        Shortcut = header |> XElement.findDescendant "Shortcut" |> XElement.value
        Declarations = declarations
        Code = 
            snippet
            |> XElement.findDescendants "Code" 
            |> Seq.map (fun c -> (c |> XElement.attribValue "Language" |> mapLanguage) , c.Value)
        
    }
let notifySnippet = mapSnippet xml
//notifySnippet |> Dump

let notifyCode = 
    let replacements = 
        notifySnippet.Declarations
        |> Seq.map (fun dec -> dec.Id,Util.ReadLine(dec.Id, dec.Default))
    let code = 
        notifySnippet.Code 
        |> Seq.find (function | (SnippetLanguage.FSharp,_) -> true) 
        |> snd
//	let mutable state = code
//	for (id,replacement) in replacements do
//		state <- state.Replace("$"+id+"$",replacement )
//	state
    Seq.fold(fun (acc:string) (id,replacement) -> acc.Replace(sprintf "$%s$" id,replacement)) code replacements


notifyCode.Dump()