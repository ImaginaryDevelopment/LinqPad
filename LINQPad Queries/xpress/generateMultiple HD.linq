<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll">C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio\2017\Enterprise\Common7\IDE\PublicAssemblies\envdte.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio\2017\Enterprise\Common7\IDE\PublicAssemblies\envdte80.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Data.Entity.Design.dll</Reference>
  <GACReference>Microsoft.VisualStudio.TextTemplating.Interfaces.10.0, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

// this thing works fine via linqpad (translating reference paths only)
// can't work in vs2017, as db projects don't open


// TODO: Convert/import from SqlGenerator : inputs for GenerateAccountingInserts
// TODO: check if the other manager creation code has anything needed or useful
// TODO: Convert input data from AccountingGenerator


#if INTERACTIVE
#r "System.Core"
#r "System.Data.Entity.Design"
#r "FSharp.Core"
#r "EnvDTE"
#r "EnvDTE80"
//#I @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v4.0"
#r "Microsoft.VisualStudio.TextTemplating.Interfaces.10.0.dll"
//#I @"C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\"
#r @"C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll"
#r @"C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe"
#endif

// write the .tt that calls the F# and generates into EnvDTE?
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.VisualStudio.TextTemplating

open CodeGeneration
open CodeGeneration.DataModelToF
open CodeGeneration.SqlMeta
open CodeGeneration.SqlMeta.ColumnTyping
open CodeGeneration.GenerateAllTheThings

open MacroRunner
open MacroRunner.DteWrap
open MacroRunner.MultipleOutputHelper.Managers
open Macros.SqlMacros


let failing s = 
    if Debugger.IsAttached then
        Debugger.Log(1,"failing", s)
        Debugger.Break()
    failwith s
let dumpt t x=
    x.Dump(description=t)
    x
let dumpft t f x= 
    f x |> dumpt |> ignore
    x
let after (delim:string) (x:string) = 
    x.Substring(x.IndexOf(delim) + delim.Length)
let before (delim:string) (x:string) = 
    let i = x.IndexOf(delim)
    x.Substring(0, i)
    
// items that we aren't generating the SQL for, but need datamodels generated from the sql db schema    
let dataModelsToGen : TableIdentifier list = [
        {TableIdentifier.Schema="dbo"; Name="Client"}
        {TableIdentifier.Schema="dbo"; Name="Event"}
        {TableIdentifier.Schema="dbo"; Name="Result"}
    ]
    
let dte = 
    try
        Macros.VsMacros.getWindowNames()
        |> Seq.find(fun wn -> wn.Contains("HealthDesigns"))
        |> dumpt "VisualStudioWindowName"
        |> Macros.VsMacros.getDteByWindowName
        |> dumpft "VisualStudioProcInfo" (fun p -> p.Name)
        //System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE.DTE
    with ex ->
        let msg = "could not find or hook into a vs instance"
        ex.Dump(msg)
        reraise()
    
printfn "Got dte for solution %s at %s" dte.Solution.FileName dte.Application.Version
let cString = 
    dte.Solution.FindProjectItem("BuildTime.fs").FileNames 0s
    |> IO.File.ReadAllLines
    |> Seq.skipWhile (fun l -> not <| l.StartsWith("let BrandonConnection")) 
    |> Seq.head
    |> after "\""
    |> before "\""
let activeDocumentFullNameOpt = if not <| isNull dte.ActiveDocument then Some dte.ActiveDocument.FullName else None
printfn "activeDocument is %A" activeDocumentFullNameOpt
printfn "Got dte for solution %s" dte.Solution.FileName
let doMultiFile = true
let targetSqlProjectName = "ApplicationDatabase"
let targetCodeProjectName = "HD.Schema"

let refData : ReferenceData list = [
            //type ReferenceData = {Schema:string; Table:string; Column:string; ValuesWithComments: IDictionary<string,string>}
//            {   ReferenceData.FKeyId= {Table={Schema="dbo";Name="GuarantorTypes"};Column="GuarantorTypeId"}
//                GenerateReferenceTable=false
//                ValuesWithComment= dict[
//                                        "SELF",null
//                                        "THIRD PARTY", null
//                                        "Insurance & Self", null ]
//            }
]

let codeTableBlacklist = []

let columnBlacklist = 
    [
        "Result", Set ["isCp"]
//        "Claims", Set ["_CurrentLevel_"; "_MaxLevel_"]
//        "Charge", Set ["TotalAmount"]
    ]
    |> Map.ofSeq
let measureList = 
    [
        "ClientId"
        "EventId"
        "ParticipantId"
        "ResultId"
        "UserId"
    ]
let measureBlacklist = []

type TableInput() = 
     member val Name:string = null with get,set
     member val Schema:string = null with get,set
     member val Columns:ColumnInput seq = Seq.empty with get,set

// StringBuilder ge = GenerationEnvironment;
let sb = System.Text.StringBuilder()
let appendLine text (sb:System.Text.StringBuilder) = 
    sb.AppendLine text

sb
|> appendLine "Main File Output"
|> appendLine (sprintf "Started at %O" DateTime.Now)
|> ignore

let scriptFullPath = Path.Combine(__SOURCE_DIRECTORY__,__SOURCE_FILE__)

let manager = 
    // if this script is in the solution it is modifying, we need the EnvDTE.ProjectItem representing it, otherwise where does the main (non sub-file) output go?
    let templateProjectItem:EnvDTE.ProjectItem = dte.Solution.FindProjectItem(scriptFullPath)
    printfn "Script is at %s" scriptFullPath
    if not <| isNull templateProjectItem then
        printfn "ProjectItem= %A" (templateProjectItem.FileNames(0s))
    let dteWrapper = wrapDte dte
    MultipleOutputHelper.Managers.VsManager(None, dteWrapper, sb, templateProjectItem)
let pluralizer = 
    Macros.VsMacros.createPluralizer()
    |> function
        | Choice1Of2 p -> p
        | _ -> failwith "Could not locate pluralizer"
    
let cgsm = 
        {
            TargetProjectName= targetCodeProjectName
            TargetNamespace= "HD.Schema.DataModels"
            CString = cString
            UseOptionTypes= false
            ColumnBlacklist= columnBlacklist
            Measures= measureList |> Set.ofSeq
            MeasuresBlacklist= measureBlacklist |> Set.ofSeq
            IncludeNonDboSchemaInNamespace= true
            Pluralize=pluralizer.Pluralize
            Singularize=pluralizer.Singularize
            TypeGenerationBlacklist = Set []
            GenerateValueRecords= false
            SprocSettingMap= Some {
                SprocInputMapBlacklist = Set [  ]
                SprocBlacklist=Set ["sp_alterdiagram"
                                    "sp_creatediagram"
                                    "sp_dropdiagram"
                                    "sp_helpdiagramdefinition"
                                    "sp_helpdiagrams"
                                    "sp_renamediagram"
                                    "sp_upgraddiagrams"]; GenerateSprocInputRecords=true}

            UseCliMutable= false
            GetMeasureNamepace= Some (fun _ -> "HD.Schema")
            AdditionalNamespaces= Set ["HD.Schema.BReusable"]
        }
        
// these are the items we will generate into a sql project
// but currently it also expects they will already be created in the sql server =(
let toGen : TableInput list = []

let toGen2 = 
    toGen
    |> List.map (fun g ->
        GenerateAllTheThings.TableInput(Schema=g.Schema,Name=g.Name, Columns= g.Columns)
    )
    |> fun items -> { SqlGenerationConfig.SqlItems = items; TargetSqlProjectName= targetSqlProjectName; InsertionConfig = Some {InsertsGenerationConfig.InsertTitling = "SqlGenerator"; TargetInsertRelativePath= @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql"; AdditionalReferenceData= refData}}
sb
|> appendLine "Main File Output"
|> appendLine (sprintf "Started at %O" DateTime.Now)
|> ignore

let generatePartials = false

let results = 
    //runFullGeneration scriptFullPath generatePartials toGen addlCodeTables |> Map.ofDictionary
    //    type TableGenerationInfo = {Schema:string; Name:string; GenerateFull:bool}
    let iDte = GenerateAllTheThings.DteGenWrapper(dte)
    GenerateAllTheThings.runGeneration  
        (sprintf "%s.linq" Util.CurrentQuery.Name) sb iDte manager cgsm [toGen2] dataModelsToGen
    let r = manager.Process doMultiFile
    r
// not important, just nice to have, clean up of opened documents in VS
try
    results
    |> Map.ofDictionary
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map snd
    |> Seq.iter (fun fullPath ->
        dte.Documents.GetEnumerator()
        |> Seq.ofIEnumerator
        |> Seq.cast<EnvDTE.Document>
        |> Seq.tryFind(fun d -> d.FullName = fullPath)
        |> Option.iter (fun d -> d.Close())
    )
    
    
    // below works, but with the documents created all getting closed, we don't need it anymore
    let reactivateLastWindow() = 
        activeDocumentFullNameOpt
        |> Option.iter(fun fullName ->
            fullName.Dump("was active")
            for d in dte.Documents do
                if d.FullName = fullName then
                    fullName.Dump("activating!")
                    d.Activate()
        )
    ()
with ex -> ex.Dump("not important, but nice to have")