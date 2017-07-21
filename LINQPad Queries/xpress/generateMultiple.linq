<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll">C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll</Reference>
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe</Reference>
  <GACReference>Microsoft.VisualStudio.TextTemplating.Interfaces.10.0, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
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
    
// items that we aren't generating the SQL for, but need datamodels generated from the sql db schema    
let dataModelsToGen : TableIdentifier list = [
        {TableIdentifier.Schema="dbo"; Name="Appointments"}
        {Schema="dbo"; Name="Payers"}
        {Schema="dbo"; Name="Users"}
        {Schema="dbo"; Name="ReferralSources"}
        {Schema="dbo"; Name="AuditLog"}
        {Schema="dbo"; Name="Intellidox"}
        {Schema="dbo"; Name="HIPAAAuthorizations"}
        {Schema="dbo"; Name="DocumentTypes"}
        {Schema="dbo"; Name="PayerProfileInfo"}
        // we need to have the GuarantorId field on this be mapped to measure PatientId
        {Schema="dbo"; Name="PatientsInfo"}
        {Schema="dbo"; Name="Patients"}
        {Schema="dbo"; Name="Charge"}
        {Schema="dbo"; Name="CodeChargeMappings"}
        {Schema="dbo"; Name="Codes"}
        
        {Schema="dbo"; Name="PaperworkType"}
        {Schema="dbo"; Name="Facilities"}
        {Schema="dbo"; Name="Claims"}
        {Schema="dbo"; Name="ClaimPaperworkItem"}
        {Schema="dbo"; Name="ClaimFilingIndicator"}
        {Schema="dbo"; Name="ChargeProfiles"}
        {Schema="dbo"; Name="B2BEligibility"}
    ]
    
let dte = 
    try
        Macros.VsMacros.getWindowNames()
        |> Seq.find(fun wn -> wn.Contains("PracticeManagement"))
        |> dumpt "VisualStudioWindowName"
        |> Macros.VsMacros.getDteByWindowName
        |> dumpft "VisualStudioProcInfo" (fun p -> p.Name)
        //System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE.DTE
    with ex ->
        let msg = "could not find or hook into a vs instance"
        ex.Dump(msg)
        reraise()
    
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
            {   ReferenceData.FKeyId= {Table={Schema="dbo";Name="GuarantorTypes"};Column="GuarantorTypeId"}
                GenerateReferenceTable=false
                ValuesWithComment= dict[
                                        "SELF",null
                                        "THIRD PARTY", null
                                        "Insurance & Self", null ]
            }
]

let codeTableBlacklist = ["Payments"]

let columnBlacklist = 
    [
        "Claims", Set ["_CurrentLevel_"; "_MaxLevel_"]
        "Charge", Set ["TotalAmount"]
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
        "PaymentItemId"
        "PayerProfileId"
        "PayerProfileInfoId"
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
    let dteWrapper = wrapDte dte
    MultipleOutputHelper.Managers.VsManager(None, dteWrapper, sb, templateProjectItem)
    
let cgsm = 
        {
            TargetProjectName= targetCodeProjectName
            TargetNamespace= "Pm.Schema.DataModels"
            CString = cString
            UseOptionTypes= false
            ColumnBlacklist= columnBlacklist
            Measures= measureList |> Set.ofSeq
            MeasuresBlacklist= measureBlacklist |> Set.ofSeq
            IncludeNonDboSchemaInNamespace= true
            TypeGenerationBlacklist = Set [
                                        "PaymentReversal"
            ]
            GenerateValueRecords= false
            SprocSettingMap= Some {
                SprocInputMapBlacklist = Set [  "uspAppointmentInsWithClaim"
                                                "uspClaimsInsUpdInput"
                                                "uspGuarantorProfileInfoInsUpd"
                                                "uspPatientsInfoInsUpd"
                ]
                SprocBlacklist=Set ["sp_alterdiagram"
                                    "sp_creatediagram"
                                    "sp_dropdiagram"
                                    "sp_helpdiagramdefinition"
                                    "sp_helpdiagrams"
                                    "sp_renamediagram"
                                    "sp_upgraddiagrams"]; GenerateSprocInputRecords=true}

            UseCliMutable= false
            GetMeasureNamepace= Some (fun _ -> "Pm.Schema")
            AdditionalNamespaces= Set ["Pm.Schema.BReusable"]
        }
        
// these are the items we will generate into a sql project
// but currently it also expects they will already be created in the sql server =(
let toGen : TableInput list = 
    [
        TableInput(
            Schema="dbo",
            Name="PaymentReversal",
            Columns = [
                {ColumnInput.createFKeyedInt "PaymentId" (FKeyIdentifier {Table={Schema="dbo"; Name="Payment"}; Column="PaymentId"}) with Nullability = PrimaryKey}
                {ColumnInput.createFKeyedInt "ReversalPaymentId" (FKeyIdentifier {Table={Schema="dbo"; Name="Payment"}; Column="PaymentId"}) with Nullability = PrimaryKey}
            ]
        )
        TableInput(
            Schema="Diags",
            Name="ScanStatistic",
            Columns = [
                ColumnInput.createPKIdentity "ScanStatisticID"
                ColumnInput.create "BarcodeCaptured" ColumnType.Bit
                ColumnInput.createFKeyedInt "FacilityId" (FKeyIdentifier {Table={Schema="dbo";Name="Facilities"};Column="FacilityID"})
                ColumnInput.createUserIdColumn null Nullability.AllowNull List.empty
                {ColumnInput.createFKeyedInt "PatientId"(FKeyIdentifier{Table={Schema="dbo"; Name="Patients"}; Column=null}) with Nullability=AllowNull}
                ColumnInput.create "DeviceSerialNumber" ColumnType.IntColumn
                ColumnInput.create "ScanHeight" ColumnType.IntColumn
                ColumnInput.create "ScanWidth" ColumnType.IntColumn
                ColumnInput.create "Resolution" ColumnType.IntColumn
                ColumnInput.create "ScannerType" ColumnType.IntColumn
                ColumnInput.create "SlibVersion" (ColumnType.StringColumn 255)
                ColumnInput.create "CalibrationThreshold" ColumnType.IntColumn
            ]
        )
        TableInput(
            Schema="dbo",
            Name="ReportJob",
            Columns = [
                ColumnInput.createPKIdentity "ReportJobID"
                ColumnInput.createFKey "CustomReportName" (ColumnType.StringColumn 50) (FKeyIdentifier {Table={Schema="dbo"; Name="CustomReport";};Column="Name"})
                ColumnInput.create "ReportJobName" (ColumnType.StringColumn 255) 
                ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
                {ColumnInput.createFKeyedInt "FacilityID" (FKeyIdentifier {Table={Schema="dbo";Name="Facilities"};Column="FacilityID"}) with Nullability = AllowNull}
                {ColumnInput.create "RangeStart" ColumnType.DateTimeColumn with Nullability = Nullability.AllowNull}
                {ColumnInput.create "RangeEnd" ColumnType.DateTimeColumn with Nullability = Nullability.AllowNull}
                {ColumnInput.create "Status" (ColumnType.StringColumn 40) with Nullability = Nullability.AllowNull}
                {ColumnInput.create "CommandLine" (ColumnType.StringMax) with Nullability = Nullability.AllowNull}
                {ColumnInput.create "CreatedUtc" (ColumnType.DateTimeColumn) with DefaultValue="getutcdate()"}
                {ColumnInput.create "PID" (ColumnType.IntColumn) with Nullability = Nullability.AllowNull}
                {ColumnInput.create "Data" ColumnType.StringMax with Nullability = Nullability.AllowNull}
                {ColumnInput.create "Error" ColumnType.StringMax with Nullability = Nullability.AllowNull}
                
            ])
        
        TableInput(
            Schema="dbo",
            Name="Payment",
            Columns=
                [
                    ColumnInput.createPKIdentity "PaymentID"
                    {ColumnInput.createFKeyedInt "AppointmentId" (FKeyIdentifier {Table={Schema="dbo";Name="Appointments"};Column="AppointmentId"}) with Nullability = AllowNull}
                    {ColumnInput.createFKey "PaymentTypeId" (ColumnType.StringColumn 50)
                        
                        (FKeyWithReference 
                            {   FKeyId={Table={Schema="Accounts";Name="PaymentType"};Column="PaymentTypeId"} 
                                GenerateReferenceTable = true
                                ValuesWithComment= dict[ "Patient",null;"ThirdParty",null;"Era",null]
                                }) 
                        with 
                            Comments= [
                                            "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                                        ]
                    }
                    ColumnInput.createFKey "PaymentMethodId" (ColumnType.StringColumn 50)
                            (FKeyWithReference 
                                {   FKeyId={Table={ Schema="Accounts"; Name="PaymentMethod"}; Column= null}
                                    GenerateReferenceTable = true
                                    ValuesWithComment = 
                                        dict[ 
                                            "Cash",null;"CC",null;"Check",null;"Ach",null;"Fsa",null;"Other","for when Era amount is 0 or a catch-all"
                                        ]})

                    ColumnInput.createFKey "PaymentStatusId" (ColumnType.StringColumn 50)
                            (FKeyWithReference 
                                {   FKeyId={Table={Schema="Accounts"; Name="PaymentStatus"}; Column= null}
                                    GenerateReferenceTable = true
                                    ValuesWithComment = (["New"; "Partial"; "Complete"] |> Seq.map (fun s-> s,null) |> dict)}
                            )
                    {ColumnInput.create "TotalAmount" (ColumnType.DecimalColumn (Some {Precision=Precision.Create(12uy).Value;Scale=Scale.Create(2uy).Value})) with
//                                Precision=Some 12;Scale=Some 2 // see: http://stackoverflow.com/questions/2377174/how-do-i-interpret-precision-and-scale-of-a-number-in-a-database
                                Comments = [ "was Amount (18,2)"]
                    }
                    ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
                    {ColumnInput.createFKeyedInt "PayerID" (FKeyIdentifier {Table={ Schema="dbo"; Name="Payers"}; Column= null }) with Nullability = AllowNull} 
                    ColumnInput.createPatientIdColumn null Nullability.AllowNull List.empty
                    {ColumnInput.create "Created" ColumnType.DateTimeColumn with Nullability = Nullability.AllowNull; Comments = ["was timestamp"]}
                    {ColumnInput.create "TransactionNumber" (ColumnType.StringColumn 30) with Nullability=Nullability.AllowNull; Comments = ["was checkNumber now will store check number or ACH number (when applicable)"]}
                    {ColumnInput.create "Rcd" ColumnType.DateTimeColumn with Nullability = Nullability.AllowNull; Comments=["Payment Recvd"]}
                    ColumnInput.create "IsElectronic" ColumnType.Bit
                    {ColumnInput.createFKeyedInt "CCItemID" (FKeyIdentifier {Table={ Schema="Accounts" ; Name="CCItem"}; Column=null})with Nullability = AllowNull} 
                    {ColumnInput.create "Comments" StringMax with Nullability = Nullability.AllowNull}

                            ])
        TableInput(
            Schema = "Accounts",Name="CCItem",
            Columns = [
                ColumnInput.createPKIdentity "CCItemID"
                ColumnInput.makeNullable50 "ResponseCode"
                ColumnInput.makeNullable50 "ResponseDescription"
                ColumnInput.makeNullable50 "TransactionID"
                ColumnInput.makeNullable50 "TransactionType"
                ColumnInput.makeNullable50 "CardType"
                ColumnInput.makeNullable50 "MaskedAcctNum"
                ColumnInput.makeNullable50 "ExpDate"
                ColumnInput.makeNullable50 "AcctNumSource"
                ColumnInput.makeNullable50 "CardholderName"
                ColumnInput.makeNullable50 "Alias"
                ColumnInput.makeNullable50 "ProcessorResponse"
                ColumnInput.makeNullable50 "BatchNum"
                ColumnInput.makeNullable50 "BatchAmount"
                ColumnInput.makeNullable50 "ApprovalCode"
            ]
        )
        TableInput(
            Schema="Accounts", Name="PaymentItem",
            Columns = [
                ColumnInput.createPKIdentity "PaymentItemID"
                ColumnInput.createFKeyedInt"PaymentID" (FKeyIdentifier {Table={Schema="dbo";Name="Payment"};Column=null})
                {ColumnInput.Nullability = Nullability.AllowNull
                 Name = "PaymentItemTypeId"
                 ColumnType = ColumnType.StringColumn 50
                 DefaultValue = null
                 FKey = Some (FKeyWithReference {
                                FKeyId={Table={Schema="Accounts"; Name="PaymentItemType"}; Column="PaymentItemTypeId"}
                                GenerateReferenceTable = true
                                ValuesWithComment = 
                                    ["EraPayment"; "EraAdjustment"; "PtRespDeductible"; "PtRespCoPay";"PtRespCoIns";"Other"] 
                                    |> Seq.map (fun x -> x,null) 
                                    |> dict 
                            })
                 Comments = List.empty
                 IsUnique = NotUnique}
                {ColumnInput.createFKey // createFKeyedNColumn<string> 
                            "PaymentTierId"
                            (ColumnType.StringColumn 50)
                            (FKeyWithReference {
                                FKeyId={Table={ Schema="Accounts"; Name="PaymentTier"}; Column= null}
                                GenerateReferenceTable = true
                                ValuesWithComment = 
                                    [ "Primary"; "Secondary"; "Tertiary"; "Worker'sComp"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                            })
                    with Nullability = AllowNull
                }            
                { ColumnInput.createFKey //<string> 
                    "PtRespTypeId" 
                    (ColumnType.StringColumn 50)
                    (FKeyWithReference {
                        FKeyId={Table={ Schema="Accounts"; Name="PtRespType"}; Column= null}
                        GenerateReferenceTable = true
                        ValuesWithComment =
                                    ["Deductible"; "CoIns"; "CoPay"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                        })
                            with
                                Nullability = AllowNull
                                
                }
                ColumnInput.create "Created" DateTimeColumn 
                ColumnInput.create "Amount" (DecimalColumn (Some {Precision=Precision.Create(8uy).Value; Scale= Scale.Create(2uy).Value}))
                ColumnInput.create "PatientResponsiblityAmt" (DecimalColumn (Some {Precision=Precision.Create(8uy).Value; Scale= Scale.Create(2uy).Value}))
                {ColumnInput.createFKeyedInt "ChargeID" (FKeyIdentifier {Table={ Schema="dbo"; Name="Charge"}; Column= null}) with Nullability=AllowNull}
                ColumnInput.makeNullable50 "RemarkCode"
                ColumnInput.makeNullable50 "AdjustmentCode"
                ColumnInput.createFKey "PaymentItemStatusId" (StringColumn 50)
                            (FKeyWithReference {
                                FKeyId={Table={ Schema="Accounts"; Name="PaymentItemStatus"}; Column=null}
                                GenerateReferenceTable=true
                                ValuesWithComment =
                                    ["Posted"; "Unposted"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                            })
                            
                {ColumnInput.create "Comments" StringMax with Nullability= Nullability.AllowNull}

            ]

        )
    ]

let toGenAccounting = 
    [
        TableInput(Schema="Accounts", Name="Account",
            Columns = [
                ColumnInput.createPKIdentity "AccountID"
                ColumnInput.createFKey "AccountTypeId" (StringColumn 50)
                            (FKeyWithReference {
                                FKeyId={Table={Schema="Accounts"; Name="AccountType"}; Column=null}
                                GenerateReferenceTable=true
                                ValuesWithComment =
                                    ["Patient"; "Payer"; "System"; "ThirdParty"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                            })
                           
                {ColumnInput.create "Name" (StringColumn 150) with IsUnique=Unique}
                {ColumnInput.createFKeyedInt "PayerID" (FKeyIdentifier {Table={Schema="dbo"; Name="Payers"}; Column=null}) with Nullability=AllowNull}
                
            ]
        )
        TableInput(Schema="Accounts", Name="JournalEntry",
            Columns=[
                ColumnInput.createPKIdentity "JournalEntryID"
                ColumnInput.createFKeyedInt "CreditAccountID" (FKeyIdentifier
                            {Table={Schema="Accounts";Name="Account"};Column="AccountID"})
                ColumnInput.createFKeyedInt "DebitAccountID" (FKeyIdentifier
                            {Table={Schema="Accounts";Name="Account"};Column="AccountID"})
                ColumnInput.create "Amount" (DecimalColumn (Some {Precision=Precision.Create(12uy).Value; Scale= Scale.Create(2uy).Value}))
                {ColumnInput.createFKeyedInt "ChargeID" (FKeyIdentifier{Table={Schema="dbo"; Name="Charge"}; Column=null}) with Nullability=AllowNull}
                {ColumnInput.createFKeyedInt "PaymentID"        (FKeyIdentifier{Table={Schema="dbo"; Name="Payment"}; Column=null})  with Nullability=AllowNull}
                {ColumnInput.createFKeyedInt "PaymentItemID"    (FKeyIdentifier{Table={Schema="Accounts"; Name="PaymentItem"}; Column=null}) with Nullability=AllowNull}
                {ColumnInput.createFKeyedInt "PatientID"        (FKeyIdentifier{Table={Schema="dbo"; Name="Patients"}; Column=null}) with Nullability=AllowNull}
                {ColumnInput.createFKeyedInt "AppointmentID"    (FKeyIdentifier{Table={Schema="dbo"; Name="Appointments"}; Column=null}) with Nullability=AllowNull}
                ColumnInput.create "EffectiveDate" (ColumnType.Custom "smalldatetime")
                ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
                ColumnInput.create "Entered" ColumnType.DateTimeColumn
                {ColumnInput.create "Comments" StringMax with Nullability=Nullability.AllowNull}
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

let generatePartials = false

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