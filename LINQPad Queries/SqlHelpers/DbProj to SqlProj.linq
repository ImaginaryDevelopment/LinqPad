<Query Kind="FSharpProgram" />

//read a dbproj and update a sql proj from the information

let before(delimiter:string) (text:string) = text.Substring(0,text.IndexOf(delimiter))
let after(delimiter:string) (text:string) = text.Substring(text.IndexOf(delimiter) + delimiter.Length)

type SqlProjType =
    |Table (*     <Build Include="Schema Objects\Schemas\dbo\Tables\ServiceFacilities.table.sql">
      <SubType>Code</SubType>
      <AnsiNulls>On</AnsiNulls>
      <QuotedIdentifier>On</QuotedIdentifier>
    </Build> -> 
        <Build Include="Schema Objects\Schemas\dbo\Tables\ServiceFacilities.table.sql">
      <SubType>Code</SubType>
      <AnsiNulls>On</AnsiNulls>
      <QuotedIdentifier>On</QuotedIdentifier>
    </Build>
    *)
    |Proc (*     <Build Include="Schema Objects\Schemas\dbo\Programmability\Stored Procedures\uspGetSearchPatients.proc.sql">
      <SubType>Code</SubType>
      <AnsiNulls>On</AnsiNulls>
      <QuotedIdentifier>On</QuotedIdentifier>
    </Build> ->
        <Build Include="Schema Objects\Schemas\dbo\Programmability\Stored Procedures\uspGetSearchPatients.proc.sql">
      <SubType>Code</SubType>
      <AnsiNulls>On</AnsiNulls>
      <QuotedIdentifier>On</QuotedIdentifier>
    </Build>
    *)
    |Function (*     <Build Include="Schema Objects\Schemas\dbo\Programmability\Functions\UTILfn_Split.function.sql">
      <SubType>Code</SubType>
      <AnsiNulls>On</AnsiNulls>
      <QuotedIdentifier>On</QuotedIdentifier>
    </Build> ->
        <Build Include="Schema Objects\Schemas\dbo\Programmability\Functions\UTILfn_Split.function.sql">
      <SubType>Code</SubType>
      <AnsiNulls>On</AnsiNulls>
      <QuotedIdentifier>On</QuotedIdentifier>
    </Build>
    *)
    |Schema
    |Folder
    |PreDeploy (* <PreDeploy Include="Scripts\Pre-Deployment\Script.PreDeployment.sql">
      <SubType>Code</SubType>
    </PreDeploy> -> 
    <PreDeploy Include="Scripts\Pre-Deployment\Script.PreDeployment.sql" />
    *)
    |PostDeploy
    |PreDeployFile
    |PostDeployFile
    
let tryGetAttribValue (xe:XElement) s =
    let attrib = xe.Attribute(XNamespace.None + s)
    if isNull attrib then None else Some attrib.Value
    
let getElements (name) (xe:XElement) = xe.Elements(name)
let getAllElements (element:XElement) = element.Elements()

let mapDbProjToSqlProj (sqlProjType, xe:XElement) = 
    match sqlProjType with // have not accounted for removing namespace that appears to be coming in, but isn't useful or needed
    |Table
    |Proc
    |Function
    |Schema
    |Folder
        -> 
            xe.SetAttributeValue(XNamespace.None + "xmlns", null)
            xe |> Some
    |PreDeploy -> XElement(XNamespace.None + "PreDeploy", XAttribute(XNamespace.None + "Include", xe.Attribute(XNamespace.None   + "Include").Value)) |> Some
    | dt -> xe |> Some
    
    
        
let readType (xe:XElement) = 
    let include' = xe.Attribute(XNamespace.None + "Include").Value
    if xe.Name.LocalName = "Folder"                         then SqlProjType.Folder |> Some
    elif include'.EndsWith(".function.sql")                 then SqlProjType.Function |> Some
    elif include'.EndsWith(".proc.sql")                     then SqlProjType.Proc |> Some
    elif include'.EndsWith(".table.sql")                    then SqlProjType.Table |> Some
    elif xe.Name.LocalName = "PreDeploy"                    then SqlProjType.PreDeploy |> Some
    elif xe.Name.LocalName = "PostDeploy"                   then SqlProjType.PostDeploy |> Some
    elif include'.StartsWith("Scripts\\Post-Deployment")    then SqlProjType.PostDeployFile |> Some
    elif include'.StartsWith("Scripts\\Pre-Deployment")     then SqlProjType.PreDeployFile |> Some
    elif include'.EndsWith(".schema.sql")                   then SqlProjType.Schema |> Some
    else
        printfn "%A - %A" include' xe
        None
    |> function
        | Some pt -> (pt,xe) |> Some
        | None -> None
    
let dbProjInfo = 
    let target = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\ApplicationDatabase.dbproj"
    let text = File.ReadAllText target
    let xml = XDocument.Parse text
    xml.Root
    |> getElements (xml.Root.Name.Namespace + "ItemGroup")
    |> Seq.map getAllElements
    |> Seq.collect id
    |> Seq.choose readType
    |> Seq.map (fun (t,e) -> t, mapDbProjToSqlProj(t,e))
    |> Seq.map (fun (t,e) -> sprintf "%A" t,e)
    
    
dbProjInfo.Dump()