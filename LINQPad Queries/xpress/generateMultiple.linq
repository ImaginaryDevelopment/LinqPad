<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll">C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll</Reference>
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe</Reference>
  <GACReference>Microsoft.VisualStudio.TextTemplating.Interfaces.10.0, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
</Query>

// this thing works fine via linqpad (translating reference paths only)


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
open CodeGeneration.GenerateAllTheThings

open MacroRunner
open MacroRunner.DteWrap
open MacroRunner.MultipleOutputHelper.Managers

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
    
// items that we aren't generating the SQL for, but need datamodels generated from the sql db schema    
let dataModelsToGen = [
        {Schema="dbo"; Name="Appointments"; GenerateFull = false}
        //{Schema="dbo"; Name="ReferralSources"; GenerateFull = false}
    ]
    
let dte = 
    Macros.VsMacros.getWindowNames()
    |> Seq.find(fun wn -> wn.Contains("PracticeManagement"))
    |> dumpt "VisualStudioWindowName"
    |> Macros.VsMacros.getDteByWindowName
    |> dumpft "VisualStudioProcInfo" (fun p -> p.Name)
    //System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE.DTE
    
printfn "Got dte for solution %s" dte.Solution.FileName
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
let targetCodeProjectName = "Pm.Schema"
let targetInsertRelativePath = @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql"

let refData : ReferenceData list = [
            //type ReferenceData = {Schema:string; Table:string; Column:string; ValuesWithComments: IDictionary<string,string>}
            {ReferenceData.Schema="dbo";Table="GuarantorTypes";Column="GuarantorTypeId"; 
                ValuesWithComments= dict[
                                        "SELF",null
                                        "THIRD PARTY", null
                                        "Insurance & Self", null ]
            }
]

let codeTableBlacklist = ["Payments"]

let columnBlacklist = 
    [
        "Claims",["_CurrentLevel_"; "_MaxLevel_"]
        "Charge", ["TotalAmount"]
    ]
    |> Map.ofSeq
let measureList = 
    [
        "AccountId"
        "AppointmentId"
        "ChargeId"
        "AdmitFacilityId"
        "FacilityId"
        "PatientId"
        "PatientInfoId"
        "PayerId"
        "PaymentId"
        
        "UserId"
    ]
let measureBlacklist =
    [
        "PatientIdentificationID"
    ]

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
    let dteWrapper = VsManager.WrapDte dte
    MultipleOutputHelper.Managers.VsManager(None, dteWrapper, sb, templateProjectItem)
    
let csgm = 
        {
            TargetProjectName= targetCodeProjectName
            TargetNamespace= "Pm.Schema.DataModels"
            CString = cString
            UseOptionTypes= false
            ColumnBlacklist= columnBlacklist
            Measures= measureList
            MeasuresBlacklist= measureBlacklist
            IncludeNonDboSchemaInNamespace= true
            GenerateValueRecords= false
        }
// these are the items we will generate into a sql project
let toGen : TableInput list = 
    [
        TableInput(
            Schema="dbo",
            Name="Payment",
            Columns=
                [
                    { ColumnInput.create "PaymentID" typeof<int> with Attributes = ["identity";"primary key"]}
                    ColumnInput.createFKeyedNColumn<int> "AppointmentId" {Schema="dbo";Table="Appointments";Column="AppointmentId"} 
                    { ColumnInput.createFKeyedColumn<string> 
                        "PaymentTypeId" 
                        { Schema="Accounts"; Table="PaymentType"; Column="PaymentTypeId"} 
                            with 
                                Length=Some 50
                                GenerateReferenceTable = true
                                ReferenceValuesWithComment= dict[ "Patient",null;"ThirdParty",null;"Era",null]
                                Comments= [
                                            "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                                        ]
                    }
                    {   ColumnInput.createFKeyedColumn<string> 
                            "PaymentMethodId"
                            { Schema="Accounts"; Table="PaymentMethod"; Column = null}
                            with
                                Length = Some 50
                                GenerateReferenceTable = true
                                ReferenceValuesWithComment = 
                                    dict[ 
                                        "Cash",null;"CC",null;"Check",null;"Ach",null;"Fsa",null;"Other","for when Era amount is 0 or a catch-all"
                                    ]
                    }
                    { ColumnInput.createFKeyedColumn<string>
                                "PaymentStatusId"
                                { Schema="Accounts"; Table="PaymentStatus"; Column = null}
                                with
                                    Length = Some 50
                                    GenerateReferenceTable = true
                                    ReferenceValuesWithComment = (["New"; "Partial"; "Complete"] |> Seq.map (fun s-> s,null) |> dict)
                    }
                    { ColumnInput.create "TotalAmount" typeof<decimal> with
                                Precision=Some 12;Scale=Some 2 // see: http://stackoverflow.com/questions/2377174/how-do-i-interpret-precision-and-scale-of-a-number-in-a-database
                                Comments = [ "was Amount (18,2)"]
                    }
                    ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
                    ColumnInput.createFKeyedNColumn<int> "PayerID" { Schema="dbo"; Table="Payers"; Column = null } 
                    ColumnInput.createPatientIdColumn null Nullability.AllowNull List.empty
                    { ColumnInput.create "Created" typeof<DateTime> with AllowNull = Nullability.AllowNull; Comments = ["was timestamp"]}
                    {ColumnInput.create "TransactionNumber" typeof<string> with AllowNull=Nullability.AllowNull; Length=Some 30; Comments = ["was checkNumber now will store check number or ACH number (when applicable)"]}
                    {ColumnInput.create "Rcd" typeof<DateTime> with AllowNull = Nullability.AllowNull; Comments=["Payment Recvd"]}
                    ColumnInput.create "IsElectronic" typeof<bool>
                    ColumnInput.createFKeyedNColumn<int> "CCItemID" { Schema="Accounts" ; Table="CCItem";Column=null}
                    {ColumnInput.create "Comments" typeof<string> with UseMax = true; AllowNull = Nullability.AllowNull}

                            ])
        TableInput(
            Schema = "Accounts",Name="CCItem",
            Columns = [
                {ColumnInput.create "CCItemID" typeof<int> with Attributes = ["identity"; "primary key"] }
                ColumnInput.makeNullable50 "ResponseCode"
                ColumnInput.makeNullable50 "ResponseDescription"
                ColumnInput.makeNullable50 "TransactionID"
                ColumnInput.makeNullable50 "TransactionType"
                ColumnInput.makeNullable50 "CardType"
                ColumnInput.makeNullable50 "MaskedAcctNum"
                ColumnInput.makeNullable50("ExpDate")
                ColumnInput.makeNullable50("AcctNumSource")
                ColumnInput.makeNullable50("CardholderName")
                ColumnInput.makeNullable50("Alias")
                ColumnInput.makeNullable50("ProcessorResponse")
                ColumnInput.makeNullable50("BatchNum")
                ColumnInput.makeNullable50("BatchAmount")
                ColumnInput.makeNullable50("ApprovalCode")
            ]
        )
        TableInput(
            Schema="Accounts", Name="PaymentItem",
            Columns = [
                { ColumnInput.create "PaymentItemID" typeof<int> with Attributes=["identity";"primary key"]}
                ColumnInput.createFKeyedColumn<int> "PaymentID" {FKeyInfo.Schema="dbo";Table="Payment"; Column=null}
                {ColumnInput.AllowNull = Nullability.AllowNull
                 Name = "PaymentItemTypeId"
                 Type = typeof<string>
                 Length = Some 50
                 Precision = None
                 Scale = None
                 UseMax = false
                 Attributes = List.empty
                 FKey = Some {FKeyInfo.Schema="Accounts"; Table="PaymentItemType"; Column="PaymentItemTypeId"}
                 Comments = List.empty
                 GenerateReferenceTable = true
                 ReferenceValuesWithComment = 
                    ["EraPayment"; "EraAdjustment"; "PtRespDeductible"; "PtRespCoPay";"PtRespCoIns";"Other"] 
                    |> Seq.map (fun x -> x,null) 
                    |> dict
                 IsUnique = false}
                { ColumnInput.createFKeyedNColumn<string> 
                            "PaymentTierId"
                            { Schema="Accounts"; Table="PaymentTier"; Column = null}
                            with
                                Length = Some 50
                                GenerateReferenceTable = true
                                ReferenceValuesWithComment = 
                                    [ "Primary"; "Secondary"; "Tertiary"; "Worker'sComp"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                }
                { ColumnInput.createFKeyedNColumn<string> "PtRespTypeId" 
                            { Schema="Accounts"; Table="PtRespType"; Column = null}
                            with
                                Length = Some 50
                                GenerateReferenceTable = true
                                ReferenceValuesWithComment =
                                    ["Deductible"; "CoIns"; "CoPay"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                }
                ColumnInput.create "Created" typeof<DateTime> 
                {ColumnInput.create "Amount" typeof<Decimal> with Precision= Some 8; Scale=Some 2}
                {ColumnInput.create "PatientResponsiblityAmt" typeof<Decimal> with Precision= Some 8; Scale=Some 2}
                ColumnInput.createFKeyedNColumn<int> "ChargeID" { Schema="dbo"; Table="Charge"; Column = null}
                ColumnInput.makeNullable50 "RemarkCode"
                ColumnInput.makeNullable50 "AdjustmentCode"
                {ColumnInput.createFKeyedColumn<string> "PaymentItemStatusId" 
                            {Schema="Accounts"; Table="PaymentItemStatus";Column=null} 
                            with 
                                Length = Some 50
                                GenerateReferenceTable=true
                                ReferenceValuesWithComment =
                                    ["Posted"; "Unposted"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict}
                {ColumnInput.create "Comments" typeof<string> with UseMax=true; AllowNull= Nullability.AllowNull}

            ]

        )
    ]

let toGenAccounting = 
    [
        TableInput(Schema="Accounts", Name="Account",
            Columns = [
                {ColumnInput.create "AccountID" typeof<int> with Attributes=["identity";"primary key"]}
                {ColumnInput.createFKeyedColumn<string> "AccountTypeId" 
                            {Schema="Accounts"; Table="AccountType";Column=null} 
                            with 
                                Length = Some 50
                                GenerateReferenceTable=true
                                ReferenceValuesWithComment =
                                    ["Patient"; "Payer"; "System"; "ThirdParty"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict}
                {ColumnInput.create "Name" typeof<string> with 
                    IsUnique= true
                    Length = Some 150
                    
                }
                ColumnInput.createFKeyedNColumn<int> "PayerID" {Schema="dbo"; Table="Payers"; Column=null}
                
                
            ]
        )
        TableInput(Schema="Accounts", Name="JournalEntry",
            Columns=[
                {ColumnInput.create "JournalEntryID" typeof<int> with Attributes=["identity";"primary key"]}
                ColumnInput.createFKeyedColumn<int> "CreditAccountID" 
                            {Schema="Accounts"; Table="Account";Column="AccountID"}
                ColumnInput.createFKeyedColumn<int> "DebitAccountID" 
                            {Schema="Accounts"; Table="Account";Column="AccountID"}
                {ColumnInput.create "Amount" typeof<decimal> 
                    with 
                        Precision=Some 12
                        Scale=Some 2
                }
                ColumnInput.createFKeyedNColumn<int> "ChargeID" {Schema="dbo";Table="Charge"; Column=null}
                ColumnInput.createFKeyedNColumn<int> "PaymentID" {Schema="dbo";Table="Payment"; Column=null}
                ColumnInput.createFKeyedNColumn<int> "PaymentItemID" {Schema="Accounts"; Table="PaymentItem"; Column=null}
                ColumnInput.createFKeyedNColumn<int> "PatientID" {Schema="dbo"; Table="Patients"; Column=null}
                ColumnInput.createFKeyedNColumn<int> "AppointmentID" {Schema="dbo"; Table="Appointments"; Column=null}
                ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
                ColumnInput.create "Entered" typeof<DateTime>
                {ColumnInput.create "Comments" typeof<string> with UseMax=true; AllowNull=Nullability.AllowNull}
            ]
        )
    ] 
    |> List.map (fun g ->
            GenerateAllTheThings.TableInput(Schema=g.Schema,Name=g.Name, Columns= g.Columns)
        )
    |> fun items -> {SqlGenerationConfig.SqlItems= items;TargetSqlProjectName = targetSqlProjectName; InsertionConfig = Some {InsertsGenerationConfig.InsertTitling="Accounting"; TargetInsertRelativePath= @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingGeneratorInserts.sql";AdditionalReferenceData = List.empty}}

sb
|> appendLine "Main File Output"
|> appendLine (sprintf "Started at %O" DateTime.Now)
|> ignore

let cgsm = 
    {
        TargetProjectName= targetCodeProjectName
        TargetNamespace= "Pm.Schema.DataModels"
        CString = cString
        UseOptionTypes= false
        ColumnBlacklist= columnBlacklist
        Measures= measureList
        MeasuresBlacklist= measureBlacklist
        IncludeNonDboSchemaInNamespace= true
        GenerateValueRecords= false
    }
let generatePartials = false

match toGen |> Seq.tryFind(fun g -> g.Columns |> Seq.exists(fun c -> c.Type = typeof<obj>)) with
    | Some g ->
        g.Dump()
        failwithf "abort"
    | _ -> ()
let toGen2 = 
    [
        toGen
        |> List.map (fun g ->
            GenerateAllTheThings.TableInput(Schema=g.Schema,Name=g.Name, Columns= g.Columns)
        )
        |> fun items -> { SqlGenerationConfig.SqlItems = items; TargetSqlProjectName= targetSqlProjectName; InsertionConfig = Some {InsertsGenerationConfig.InsertTitling = "SqlGenerator"; TargetInsertRelativePath= @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql"; AdditionalReferenceData= refData}}
        toGenAccounting
    ]
let results = 
    //runFullGeneration scriptFullPath generatePartials toGen addlCodeTables |> Map.ofDictionary
    //    type TableGenerationInfo = {Schema:string; Name:string; GenerateFull:bool}
    GenerateAllTheThings.runGeneration  
        (sprintf "%s.linq" Util.CurrentQuery.Name) sb dte manager cgsm toGen2 dataModelsToGen
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
        |> Seq.tryFind(fun d-> d.FullName = fullPath)
        |> Option.iter (fun d -> d.Close())
    )
    
    //|> dumpt "results"
    
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