<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Data.Entity.Design.dll</Reference>
  <GACReference>Microsoft.VisualStudio.TextTemplating.Interfaces.10.0, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
</Query>

// .tt translations
open System
open Microsoft.VisualStudio.TextTemplating
type CultureInfo = System.Globalization.CultureInfo
type Dte = EnvDTE.DTE
type Project = EnvDTE.Project
type ProjectItem = EnvDTE.ProjectItem
type ProjectKinds = EnvDTE80.ProjectKinds
type PluralizationService = System.Data.Entity.Design.PluralizationServices.PluralizationService

// MultipleOutputHelper.ttinclude
[<AllowNullLiteral>]
type Block () =
        member val Name:string = null with get,set
        member val Project:EnvDTE.Project = null with get,set
        member val Start = 0 with get,set
        member val Length = 0 with get,set
        
type Manager internal (host,template) = 

    let mutable currentBlock:Block = null
    let files = List<Block>()
    let footer = Block()
    let header = Block()
    let host : ITextTemplatingEngineHost = host
    let template : StringBuilder = template
    //protected List<String> generatedFileNames = new List<String>();
    let mutable generatedFileNames : string list = []

    member __.GeneratedFileNames with get() = generatedFileNames    
    member __.StartNewFile(name, ?project) =
        if isNull name then raise <| new ArgumentNullException("name")
        let project = defaultArg project null
        currentBlock <- new Block(Name=name,Project=project)
        
    member x.StartFooter () = x.CurrentBlock <- footer
    member x.StartHeader () = x.CurrentBlock <- header
    
    member x.EndBlock () =
        if isNull currentBlock then
            ()
        else
            currentBlock.Length <- template.Length - currentBlock.Start
            if currentBlock <> header && currentBlock <> footer then
                files.Add currentBlock
            x.CurrentBlock <- null
            
    abstract Process : split:bool -> unit
    default x.Process split = 
        if split then
            x.EndBlock()
            let headerText = template.ToString(header.Start, header.Length)
            let footerText = template.ToString(footer.Start, footer.Length)
            let outputPath = Path.GetDirectoryName(host.TemplateFile)
            files.Reverse()
            for block in files do
                let fileName = Path.Combine(outputPath, block.Name)
                let content = headerText + template.ToString(block.Start, block.Length) + footerText
                generatedFileNames <- fileName::generatedFileNames
                x.CreateFile fileName content
                template.Remove(block.Start, block.Length) |> ignore

    abstract CreateFile : fileName:string -> content:string -> unit
    default x.CreateFile fileName content = 
        if x.IsFileContentDifferent fileName content then
            File.WriteAllText(fileName, content)
            
    abstract GetCustomToolNamespace: fileName: string -> string
    default x.GetCustomToolNamespace fileName = null
    
    abstract DefaultProjectNamespace:  string with get
    default x.DefaultProjectNamespace with get() = null
    
    abstract IsFileContentDifferent : fileName:string -> newContent:string -> bool
    default __.IsFileContentDifferent fileName newContent =
        File.Exists fileName && File.ReadAllText(fileName) = newContent
        |> not
        
    member x.CurrentBlock
        with get() = currentBlock
        and set v = 
            if not <| isNull x.CurrentBlock then
                x.EndBlock()
            if not <| isNull v then
                v.Start <- template.Length
            currentBlock <- v
            
    static member Create(host:ITextTemplatingEngineHost,template) =
        match host with 
        // TODO: re-enable this
        | :? IServiceProvider -> VsManager(host,template) :> Manager
        | _ -> Manager(host,template)
            
and VsManager private (host,dte,template,templateProjectItem,checkOutAction, projectSyncAction) =
        inherit Manager(host,template)
        
        let templateProjectItem:ProjectItem = null
        let dte: Dte = dte
        let checkOutAction: Action<string> = checkOutAction // Action<String> 
        let projectSyncAction: Action<string seq> = projectSyncAction //Action<IEnumerable<String>> 

        override __.DefaultProjectNamespace with get() = templateProjectItem.ContainingProject.Properties.Item("DefaultNamespace").Value.ToString()
        override __.GetCustomToolNamespace fileName = dte.Solution.FindProjectItem(fileName).Properties.Item("CustomToolNamespace").Value.ToString()
        
        override __.Process split =
            if isNull templateProjectItem.ProjectItems then
                ()
            else 
                base.Process split
            projectSyncAction.EndInvoke(projectSyncAction.BeginInvoke(base.GeneratedFileNames, null, null))

        override x.CreateFile fileName content =
            if x.IsFileContentDifferent fileName content then
                x.CheckoutFileIfRequired(fileName)
                File.WriteAllText(fileName, content)
                
        //static member private x.CreateVsManager(
        internal new(host:ITextTemplatingEngineHost , template:StringBuilder ) =
            let hostServiceProvider = host :?> IServiceProvider
            if isNull hostServiceProvider then
                raise <| ArgumentNullException("Could not obtain IServiceProvider")
            let  dte = hostServiceProvider.GetService(typeof<Dte>) :?> Dte
            if isNull dte then
                raise <| ArgumentNullException("Could not obtain DTE from host")
            
            let templateProjectItem = dte.Solution.FindProjectItem(host.TemplateFile)
            let checkOutAction fileName = dte.SourceControl.CheckOutItem fileName |> ignore
            let projectSyncAction = fun keepFileNames -> VsManager.ProjectSync(templateProjectItem, keepFileNames)
            VsManager(host,dte,template,templateProjectItem,Action<_>(checkOutAction), Action<_>(projectSyncAction))
        
        static member WriteLnToOutputPane(dte:Dte) (s:string) =
            let window = dte.Windows.Item(EnvDTE.Constants.vsWindowKindOutput).Object :?> EnvDTE.OutputWindow
            window.ActivePane.Activate ()
            window.ActivePane.OutputString (s + Environment.NewLine)

        static member ProjectSync(templateProjectItem:ProjectItem, keepFileNames:string seq)  =
            let keepFileNameSet = HashSet<string>(keepFileNames)
            let projectFiles = Dictionary<string, ProjectItem>()
            let dte = templateProjectItem.Collection.DTE
            let project = templateProjectItem.Collection.ContainingProject
            VsManager.WriteLnToOutputPane dte ("Starting ProjectSync for t4 in " + project.Name + "\\" + templateProjectItem.Name)
            let templateProjectDirectory = Path.GetDirectoryName project.FullName
            let projects = dte.Solution.Projects.OfType<Project>()
            let inline isInCurrentProject (fileName:string) =  fileName.StartsWith(templateProjectDirectory)
            let originalFilePrefix = Path.GetFileNameWithoutExtension(templateProjectItem.get_FileNames(0s)) + "."
            for projectItem in templateProjectItem.ProjectItems do
                projectFiles.Add(projectItem.get_FileNames(0s), projectItem)

            // Remove unused items from the project
            for pair in projectFiles do
                if not <| keepFileNames.Contains pair.Key && not <| (Path.GetFileNameWithoutExtension(pair.Key) + ".").StartsWith(originalFilePrefix) then
                    pair.Value.Delete()

            // Add missing files to the project
            for fileName in keepFileNameSet do
                if isInCurrentProject fileName then
                    if not <| projectFiles.ContainsKey(fileName) then
                        templateProjectItem.ProjectItems.AddFromFile(fileName) |> ignore
                        VsManager.WriteLnToOutputPane dte ("added " + fileName)
                else // add to another project
                    let targetProject = projects.First(fun p -> p.Kind <> ProjectKinds.vsProjectKindSolutionFolder && fileName.StartsWith(Path.GetDirectoryName p.FullName))
                    VsManager.WriteLnToOutputPane dte ("Generating into " + targetProject.FullName)
                    targetProject.ProjectItems.AddFromFile fileName |> ignore
                    
        member x.CheckoutFileIfRequired fileName =
            let sc = dte.SourceControl
            if not <| isNull sc && sc.IsItemUnderSCC fileName && not <| sc.IsItemCheckedOut fileName then
                checkOutAction.EndInvoke(checkOutAction.BeginInvoke(fileName, null, null))
                
// -----------------------------------                
// translation of EnvDteHelper.ttinclude

let rec getSolutionFolderProjects (solutionFolder:Project) = 
    let list = new List<Project>()
    for i in [1 .. solutionFolder.ProjectItems.Count - 1] do
            let subProject = solutionFolder.ProjectItems.Item(i).SubProject
            if isNull subProject then 
                ()
            else 
                // If this is another solution folder, do a recursive call, otherwise add
                if subProject.Kind = ProjectKinds.vsProjectKindSolutionFolder then
                    list.AddRange(getSolutionFolderProjects subProject)
                else
                    list.Add(subProject)
    list
    
let recurseSolutionProjects (dte:Dte) : #seq<Project> = 
    let projects = dte.Solution.Projects
    let list = new List<Project>()
    let item = projects.GetEnumerator()
    while item.MoveNext() do
    	let project = item.Current :?> Project
    	if isNull project then
            ()
        else
    		if project.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then
    			list.AddRange(getSolutionFolderProjects project)
    		else
    			list.Add(project)
    list
    
// -----------------------------------

// translation of DataModelToF.ttinclude

type ColumnDescription = {ColumnName:string; Type:string; Length:int; Nullable:bool; IsIdentity:bool}

let generateTypeComment columnCount = sprintf "/// %i properties" columnCount

let mapNullableType(targetType:string , nullable:bool, useOptions:bool ) = targetType + (if nullable then (if useOptions then " option" else " Nullable") else String.Empty)

let mapSqlType(type' : string, nullable:bool , useOptions:bool ) = 
    match type'.ToLower() with
        |"char"
        |"nchar"
        |"nvarchar"
        |"varchar" -> "string"
        |"bit" -> mapNullableType("bool", nullable, useOptions)
        |"date"
        |"datetime"
        |"smalldatetime" -> mapNullableType("DateTime", nullable, useOptions)
        |"uniqueidentifier" -> mapNullableType("Guid",nullable, useOptions)
        |"int" -> mapNullableType("int", nullable, useOptions)
        |"decimal" -> mapNullableType("decimal", nullable, useOptions)
        |"float" -> mapNullableType("float", nullable, useOptions)
        |_ -> if isNull type' then String.Empty else type'

let generateColumnComment (cd:ColumnDescription) = sprintf "/// %s (%i) %s" (if isNull cd.Type then "null" else cd.Type) cd.Length (if cd.Nullable then "null" else "not null")

let generateInterface (typeName:string , columns:IEnumerable<ColumnDescription> , appendLine:int * string -> unit, writeable:bool , useOptions:bool ) =
    appendLine(0, generateTypeComment (columns.Count()))
    appendLine(0,"type I" + typeName + (if writeable  then "RW" else String.Empty) + " =")
    if writeable then
        appendLine(1,"inherit I" + typeName)
        
    for cd in columns do
        appendLine(1, generateColumnComment cd)
        appendLine(1, "abstract member " + cd.ColumnName + ":" + mapSqlType(cd.Type, cd.Nullable, useOptions) + " with get" + (if writeable then ",set" else String.Empty))
        
    appendLine(0,String.Empty)
        
let getDefaultValue(mappedType:string ) =
    if mappedType.EndsWith("Nullable") then
        "Nullable()"
    elif mappedType.EndsWith("option") then
        "None"
    else 
        match mappedType.ToLower() with
            |"int" -> "0"
            |"bool" -> "false"
            |"decimal" -> "0m"
            |"float" -> "0."
            |"datetime" -> "System.DateTime.MinValue"
            |"uniqueidentifier" -> "Guid.Empty"
            |_ -> "null"
    
let generateRecord(typeName:string, columns: ColumnDescription seq, appendLine:int * string -> unit, useOptions:bool) =
    appendLine(0, generateTypeComment (columns.Count()))
    if not useOptions then
        appendLine(0,"[<NoComparison>]")

    appendLine(0, "type " + typeName + "Record =")
    appendLine(1, "{")

    for cd in columns do
        appendLine(1, generateColumnComment(cd))
        appendLine(1, cd.ColumnName + ":" + mapSqlType(cd.Type,cd.Nullable,useOptions))

    appendLine(1,"}")
    appendLine(1,"interface I" + typeName + " with")

    for cd in columns do
        appendLine(2, "member x." + cd.ColumnName + " with get () = x." + cd.ColumnName)

    appendLine(1,"static member Zero () = ")
    appendLine(2,"{")

    for cd in columns do
        let mapped = mapSqlType(cd.Type, cd.Nullable, useOptions)
        appendLine(2, cd.ColumnName + " = " + getDefaultValue(mapped))

    appendLine(2,"}")
    appendLine(0,String.Empty)

let toCamel s = // https://github.com/ayoung/Newtonsoft.Json/blob/master/Newtonsoft.Json/Utilities/StringUtils.cs
    if String.IsNullOrEmpty s then
        s
    elif not <| Char.IsUpper s.[0] then
        s
    else 
        let camelCase = Char.ToLower(s.[0], CultureInfo.InvariantCulture).ToString(CultureInfo.InvariantCulture)
        if (s.Length > 1) then
            camelCase + s.Substring(1)
        else 
            camelCase

let generateModule(typeName:string, columns:IEnumerable<ColumnDescription>, schemaName:string , tableName:string , appendLine:int * string -> unit, useOptions:bool ) =
    let camelType = toCamel typeName
    appendLine(0, "module " + typeName + "Helpers =")
    appendLine(1, "open Microsoft.FSharp.Core.Operators.Unchecked")
    appendLine(1, String.Empty)
    appendLine(1, "let tableName = \"" + typeName + "\"")
    appendLine(1, "let ToRecord (i" + typeName + ":I" + typeName + ") =")
    appendLine(2, "{")

    for cd in columns do
        let mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
        appendLine(2, cd.ColumnName + " = i" + typeName + "." + cd.ColumnName)

    appendLine(2, "}")
    appendLine(0, String.Empty)

    appendLine(1, "let toRecord (" + camelType + ":I"+ typeName + ") =")
    appendLine(2, "{");

    for cd in columns do
        let mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
        appendLine(2, cd.ColumnName + " = " + camelType + "." + cd.ColumnName)

    appendLine(2, "}")
    appendLine(0,String.Empty)

    appendLine(1, "let FromF (camelTypeF:Func<string,obj option>) =")
    appendLine(2, "{")

    let mapConverter(type' : string , nullable: bool, useOptions:bool) = 
        match type'.ToLower() with 
            |"char"
            |"nchar"
            |"nvarchar"
            |"varchar" -> "ToString"
            |"bit" -> "ToBoolean"
            |"date"
            |"datetime"
            |"smalldatetime" -> "ToDateTime"
            |"uniqueidentifier" -> "ToGuid" // invalid
            |"int" -> "ToInt32"
            |"decimal" -> "ToDecimal"
            |"float"  -> "ToDouble"
            |_ -> if isNull type' then String.Empty else type'

    for cd in columns do
        let mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
        let converter = mapConverter(cd.Type,cd.Nullable,useOptions)
        appendLine(2, cd.ColumnName + " = ")
        appendLine(3, "match camelTypeF.Invoke \"" + cd.ColumnName + "\" with ")

        if cd.Nullable && (mapped <> typeof<string>.Name) && (mapped <> "string") && mapped <> "String" then
            appendLine(3, "|Some x -> Nullable (Convert." + converter + " x )")
        else
            appendLine(3, "|Some x -> Convert." + converter + " x ")
            appendLine(3, "|None -> Unchecked.defaultof<_>")

    appendLine(2, "}")

    appendLine(0,String.Empty)

    appendLine(1, "let inline toRecordStp (" + camelType + ": ^a) =")
    appendLine(2, "{")

    for cd in columns do
        let mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
        appendLine(2, cd.ColumnName + " = (^a: (member " + cd.ColumnName + ": _) " + camelType + ")")

    appendLine(2, "}")

    appendLine(0, String.Empty)
    appendLine(1,"let createInsert (r:I" + typeName + ") =")
    appendLine(2,"let quoted s = \"'\" + s + \"'\"")
    let mapValue (cd:ColumnDescription, prefix:string) :string  = 
        match cd.Type.ToLower() with
            |"varchar" -> "if String.IsNullOrEmpty " + prefix + cd.ColumnName+ " then \"null\" else quoted " + prefix + cd.ColumnName
            |"int" -> if cd.Nullable then "if isNull (box " + prefix + cd.ColumnName + ") then \"null\" else " + prefix + cd.ColumnName + " |> string" else prefix + cd.ColumnName + " |> string"
            |_ ->  if cd.Nullable then "if isNull (box " + prefix + cd.ColumnName + ") then \"null\" else " + prefix + cd.ColumnName + " |> string |> quoted" else prefix + cd.ColumnName + " |> string |> quoted"

    appendLine(2,"[")

    for cd in columns.Where(fun c -> not c.IsIdentity) do
        let mapped = "\"" + cd.ColumnName + "\", " + mapValue(cd,"r.")
        appendLine(3,mapped)

    appendLine(2,"]")
    appendLine(2,"|> fun pairs -> sprintf \"insert into " + schemaName + "." + tableName + "(%s) values (%s)\" (String.Join(\",\", pairs |> Seq.map fst )) (String.Join(\",\", pairs |> Seq.map snd))" )
    appendLine(0,String.Empty)
        
let mapFieldNameFromType(columnName:string) = 
    match toCamel columnName with
    | "type" ->  "type'"
    | camel -> camel
        
let generateClass(typeName:string, columns:IEnumerable<ColumnDescription> , appendLine:int * string -> unit, useOptions:bool ) =
    appendLine(0, generateTypeComment (columns.Count()))
    appendLine(0, "type "+ typeName + "N (model:" + typeName + "Record) = ")
    appendLine(0, String.Empty)
    appendLine(1, "let propertyChanged = new Event<_, _>()");
    appendLine(0, String.Empty)

    appendLine(0, String.Empty)
    for cd in columns do // https://fadsworld.wordpress.com/2011/05/18/f-quotations-for-inotifypropertychanged/
        let camel = mapFieldNameFromType(cd.ColumnName)
        appendLine(1, "let mutable "+ camel + " = model." + cd.ColumnName)

    appendLine(0, String.Empty)

    appendLine(1, "interface I" + typeName + " with")

    for cd in columns do
        appendLine(2,generateColumnComment cd)
        appendLine(2, "member x." + cd.ColumnName + " with get () = x." + cd.ColumnName)

    appendLine(1, "interface I" + typeName + "RW with" )

    for cd in columns do
        appendLine(2,generateColumnComment cd)
        appendLine(2, "member x." + cd.ColumnName + " with get () = x." + cd.ColumnName + " and set v = x." + cd.ColumnName + " <- v")

    appendLine(0, String.Empty)
    appendLine(1, "member x.MakeRecord () =")
    appendLine(2, "{")

    for cd in columns do
        appendLine(2, cd.ColumnName + " = x." + cd.ColumnName)

    appendLine(2, "}")

    appendLine(0, String.Empty)

    appendLine(1, "interface INotifyPropertyChanged with")
    appendLine(2, "[<CLIEvent>]")
    appendLine(2, "member x.PropertyChanged = propertyChanged.Publish")
    appendLine(1, "abstract member RaisePropertyChanged : string -> unit")
    appendLine(1, "default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, PropertyChangedEventArgs(propertyName))")

    appendLine(0, String.Empty)
    appendLine(1, "abstract member SetAndNotify<'t> : string * 't byref * 't -> bool")
    appendLine(1, "default x.SetAndNotify<'t> (propertyName, field: 't byref, value:'t) =")
    appendLine(2, "if obj.ReferenceEquals(box field,box value) then false")
    appendLine(2, "else")
    appendLine(3, "field <- value")
    appendLine(3, "x.RaisePropertyChanged(propertyNam)");
    appendLine(3, "true")

    appendLine(0, String.Empty)
    appendLine(1, "abstract member SetAndNotify<'t,'b> : string * 'b * 't Action * 't -> bool")
    appendLine(1, "default x.SetAndNotify<'t,'b> (propertyName, baseValue:'b, baseSetter: 't Action, value:'t) =")
    appendLine(2, "if obj.ReferenceEquals(box baseValue,box value) then false")
    appendLine(2, "else");
    appendLine(3, "baseSetter.Invoke value")
    appendLine(3, "x.RaisePropertyChanged(propertyName)")
    appendLine(3, "true");

    for cd in columns do
        let camel = mapFieldNameFromType cd.ColumnName
        appendLine(0, String.Empty)
        appendLine(1, generateColumnComment cd)
        appendLine(1, "member x." + cd.ColumnName)
        appendLine(2, "with get() = " + camel)
        appendLine(2, "and set v = ")
        appendLine(3, camel + " <- v")
        appendLine(3, "x.RaisePropertyChanged \"" + cd.ColumnName + "\"")
        
let generate(host:ITextTemplatingEngineHost, dte:Dte, generationEnvironment:StringBuilder , targetProjectName:string , tables:string seq, cString:string , doMultiFile:bool) =
    let appendLine text = generationEnvironment.AppendLine(text) |> ignore
    generationEnvironment.AppendLine(host.TemplateFile) |> ignore
    let appendLine' (indentLevels, text) = generationEnvironment.AppendLine(String.Join(String.Empty,Enumerable.Repeat("    ",indentLevels)) + text) |> ignore
    let useOptions = false
    let manager = Manager.Create(host, generationEnvironment)

    let projects = recurseSolutionProjects dte
    let targetProject = projects.First(fun p -> p.Name = targetProjectName)
    let targetProjectFolder = Path.GetDirectoryName(targetProject.FullName)
    let pluralizer = PluralizationService.CreateService(CultureInfo "en") // https://msdn.microsoft.com/en-us/library/system.data.entity.design.pluralizationservices.pluralizationservice(v=vs.110).aspx

    generationEnvironment.AppendLine("Main file output") |> ignore
    for p in projects do
        appendLine (p.Name + " " + p.FullName)

    use cn = new System.Data.SqlClient.SqlConnection(cString)
    cn.Open()
    for tableName in tables do
        manager.StartNewFile(Path.Combine(targetProjectFolder,tableName + ".generated.fs"),targetProject)
        let typeName = pluralizer.Singularize(tableName)
        let columns = List<ColumnDescription>()
        let identities = List<string>()
        use cmd = new System.Data.SqlClient.SqlCommand("sp_help " + tableName,cn)
        use r = cmd.ExecuteReader()
        r.NextResult() |> ignore // ignore the first table
        while r.Read() do // column info
            // columns and info
            let columnName = r.["Column_name"].ToString()
            let type' = r.["Type"].ToString()
            // var computed = r["Computed"];
            let length = Convert.ToInt32(r.["Length"])
            // var prec = r["Prec"];
            columns.Add {ColumnName=columnName; Type= type'; Length=length; Nullable = r.["Nullable"].ToString() ="yes"; IsIdentity = false}

        r.NextResult() |> ignore
        while r.Read() do // identities
            if r.["Seed"] <> box System.DBNull.Value then // only valid identities (sql uses the identity column to say there are none defined instead of an empty set)
                identities.Add(r.["Identity"].ToString())
        let columns = 
            columns
            |> Seq.map (fun c -> if identities.Contains(c.ColumnName) then {c with IsIdentity = true} else c)
            |> fun s -> s.ToList()
            
//        for i in identities do
//            try
//                columns.First(fun c -> c.ColumnName = i).IsIdentity <- true
//            with |ex -> raise <| ArgumentOutOfRangeException(tableName + ":" + i, ex)
        let columns = columns.OrderBy(fun c -> c.ColumnName).ToList()
        appendLine ("namespace Pm.Schema.DataModels." + pluralizer.Pluralize(typeName) + " // Generated by item in namespace " + manager.DefaultProjectNamespace )

        appendLine String.Empty
        appendLine "open System"
        appendLine "open System.ComponentModel"
        appendLine "open System.Linq.Expressions"
        appendLine String.Empty
        appendLine "open FSharp.NullHelpers"
        
        generateInterface (typeName, columns, appendLine', false, useOptions)
        generateInterface (typeName, columns, appendLine', true, useOptions)
        generateRecord(typeName, columns, appendLine', useOptions)
        generateModule(typeName, columns, "dbo", tableName, appendLine', useOptions)
        generateClass(typeName, columns, appendLine', useOptions)

        manager.EndBlock()


    manager.Process(doMultiFile)
