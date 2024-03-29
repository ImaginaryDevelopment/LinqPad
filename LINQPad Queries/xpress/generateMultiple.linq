<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\BCore.ADO.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\BCore.ADO.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\BCore.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\BCore.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\CodeGeneration.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\CodeGeneration.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\CodeGeneration.Sql.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\CodeGeneration.Sql.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\CodeGeneration.VS.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\CodeGeneration.VS.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\EnvDTE.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\EnvDTE.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\EnvDTE80.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\EnvDTE80.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.10.0.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.10.0.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.11.0.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.11.0.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.8.0.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.8.0.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.9.0.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.9.0.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.Shell.Interop.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TeamFoundation.VersionControl.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TeamFoundation.VersionControl.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TextManager.Interop.8.0.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TextManager.Interop.8.0.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TextManager.Interop.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TextManager.Interop.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TextTemplating.Interfaces.10.0.dll">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\Microsoft.VisualStudio.TextTemplating.Interfaces.10.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Data.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Data.Entity.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Data.Entity.Design.dll</Reference>
  <GACReference>Microsoft.VisualStudio.TextTemplating.Interfaces.10.0, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Humanizer</NuGetReference>
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
open CodeGeneration.VS
open CodeGeneration.DataModelToF
open CodeGeneration.SqlMeta
open BCore.CodeGeneration.SqlWrapCore
open BCore.CodeGeneration.SqlWrapCore.ColumnTyping
open CodeGeneration.GenerateAllTheThings

//open MacroRunner
open CodeGeneration.VS.DteWrap
open CodeGeneration.VS.MultipleOutputHelper.Managers
open Macros.SqlMacros
open PureCodeGeneration

let debug = false

let failing s = 
    if Debugger.IsAttached then
        Debugger.Log(1,"failing", s)
        Debugger.Break()
    failwith s
let dumpt t x=
    x.Dump(description=t)
    x
let dumpft t f x= 
    f x |> dumpt t |> ignore
    x
let after (delim:string) (x:string) = 
    x.Substring(x.IndexOf(delim) + delim.Length)
let before (delim:string) (x:string) = 
    let i = x.IndexOf(delim)
    x.Substring(0, i)
    
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
        {Schema="dbo"; Name="PatientInfo"}
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
//        |> dumpft "VisualStudioProcInfo" (fun p -> p.Name)
        //Microsoft.VisualStudio.Shell.Interop.
        //System.Runtime.InteropServices.Marshal.get
        //let x =  System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE")
        //:?> EnvDTE.DTE
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

let codeTableNolist = ["Payments"; "ScanStatistic"]

let columnNolist = 
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
let measureNolist =
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
    let templateProjectItem:EnvDTE.ProjectItem option = dte.Solution.FindProjectItem scriptFullPath |> Option.ofUnsafeNonNullable
    printfn "Script is at %s" scriptFullPath
    templateProjectItem
    |> Option.iter(fun templateProjectItem ->
        printfn "ProjectItem= %A" (templateProjectItem.FileNames 0s)
    )
    let dteWrapper = wrapDte dte
    VsManager(None, dteWrapper, sb, templateProjectItem,debug)
let pluralizer = 
    //Macros.VsMacros.createPluralizer()
    //|> function
    //    | Choice1Of2 p -> p
    //    | _ -> failwith "Could not locate pluralizer"
    
let cgsm = 
        {
            TargetProjectName= targetCodeProjectName
            TargetNamespace= "Pm.Schema.DataModels"
            TypeScriptGenSettingMap= None 
//                {
//                    TargetProjectName= typeScriptProjectName
//                    ColumnNolist = columnNolist
//                    TargetFolderOpt = typeScriptFolderName
//                }
            CString = cString
//            UseOptions= false
            NullableHandling= PureCodeGeneration.NullableHandling.UseNullable
            ColumnNolist= columnNolist
            Measures= measureList |> Set.ofSeq
            MeasuresNolist= measureNolist |> Set.ofSeq
            IncludeNonDboSchemaInNamespace= true
            Pluralize= Humanizer.InflectorExtensions.Pluralize
            //pluralizer.Pluralize
            Singularize=Humanizer.InflectorExtensions.Singularize //pluralizer.Singularize
            TypeGenerationNolist = Set [
                                        "PaymentReversal"
            ]
            GenerateValueRecords= false
            SprocSettingMap= Some {
                SprocInputMapNolist = Set [     "uspAppointmentInsWithClaim"
                                                "uspClaimsInsUpdInput"
                                                "uspGuarantorProfileInfoInsUpd"
                                                "uspPatientInfoInsUpd"
                ]
                SprocNolist=Set [   "sp_alterdiagram"
                                    "sp_creatediagram"
                                    "sp_dropdiagram"
                                    "sp_helpdiagramdefinition"
                                    "sp_helpdiagrams"
                                    "sp_renamediagram"
                                    "sp_upgraddiagrams"]; GenerateSprocInputRecords=true}

            Mutable= PureCodeGeneration.Mutability.Immutable
            GetMeasureNamepace= Some (fun _ -> "Pm.Schema")
            AdditionalNamespaces= Set ["Schema.BReusable"]
        }
        
// these are the items we will generate into a sql project
// but currently it also expects they will already be created in the sql server =(
let toGen : TableInput list = 
    let facilityFKey = FKeyIdentifier {Table={Schema="dbo";Name="Facilities"};Column="FacilityID"}
    let facilityFKeyColumn =  ColumnInput.createFKeyedInt "FacilityId" facilityFKey
    [
        TableInput(Schema="Admit", Name="Level",
            Columns=[
                ColumnInput.createPKIdentity "ID"
                ColumnInput.create "Display" (ColumnType.StringColumn 50)
            ]
        )
        TableInput(Schema="dbo", Name="EraPayment", 
            Columns=[
                {ColumnInput.createFKeyedInt "EraPaymentID" (FKeyIdentifier {Table={Schema="dbo"; Name="Payment"}; Column="PaymentId"}) with Nullability = PrimaryKey}
                ColumnInput.create "DeliveryMethod" (ColumnType.StringColumn 50)
                ColumnInput.create "DeliveryName" (ColumnType.StringColumn 50)
                ColumnInput.create "Name" (ColumnType.StringColumn 50)
                {ColumnInput.create "CreatedUtc" (ColumnType.DateTimeColumn) with DefaultValue="getutcdate()"} 
            ]
        )
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
                facilityFKeyColumn 
                ColumnInput.createUserIdColumn null Nullability.AllowNull List.empty
                {ColumnInput.createFKeyedInt "PatientId"(FKeyIdentifier{Table={Schema="dbo"; Name="Patients"}; Column=null}) with Nullability=AllowNull}
                ColumnInput.create "DeviceSerialNumber" ColumnType.IntColumn
                ColumnInput.create "ScanHeight" ColumnType.IntColumn
                ColumnInput.create "ScanWidth" ColumnType.IntColumn
                ColumnInput.create "Resolution" ColumnType.IntColumn
                ColumnInput.create "ScannerType" ColumnType.IntColumn
                ColumnInput.create "SlibVersion" (ColumnType.StringColumn 255)
                ColumnInput.create "CalibrationThreshold" ColumnType.IntColumn
                {ColumnInput.create "CreatedUtc" (ColumnType.DateTimeColumn) with DefaultValue="getutcdate()"}
            ]
        )
        TableInput(
            Schema="dbo",
            Name="ReportJob",
            Columns = [
                ColumnInput.createPKIdentity "ReportJobID"
                ColumnInput.createFKey "CustomReportName" (ColumnType.StringColumn 50) (FKeyIdentifier {Table={Schema="dbo"; Name="CustomReport"};Column="Name"})
                ColumnInput.create "ReportJobName" (ColumnType.StringColumn 255) 
                ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
                { facilityFKeyColumn with Nullability = Nullability.AllowNull}
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
                    {ColumnInput.create "IsPaper" ColumnType.Bit with DefaultValue = "0"}
                    {ColumnInput.createFKeyedInt "CCItemID" (FKeyIdentifier {Table={ Schema="Accounts" ; Name="CCItem"}; Column=null})with Nullability = AllowNull} 
                    {ColumnInput.create "Comments" StringMax with Nullability = Nullability.AllowNull}
                    {facilityFKeyColumn with Nullability = Nullability.AllowNull}

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
                                    ["EraPayment"; "EraAdjustment";"Other"] 
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
                    "AdjTypeId" 
                    (ColumnType.StringColumn 50)
                    (FKeyWithReference {
                        FKeyId={Table={ Schema="Accounts"; Name="AdjTypeId"}; Column= null}
                        GenerateReferenceTable = true
                        ValuesWithComment =
                                    ["PR"; "CO"; "OA"]
                                    |> Seq.map (fun x -> x,null)
                                    |> dict
                        })
                            with
                                Nullability = AllowNull
                                
                }
                {ColumnInput.create "Created" (ColumnType.DateTimeColumn) with DefaultValue="getutcdate()"} 
                ColumnInput.create "Amount" (DecimalColumn (Some {Precision=Precision.Create(8uy).Value; Scale= Scale.Create(2uy).Value}))
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
                { ColumnInput.create "ClaimLevel" ColumnType.IntColumn with Nullability=Nullability.AllowNull}
                
                // probably not needed, as manual journal entries wouldn't have a payment id or payment item id, and all system created entries that I can think of would.
                //{ColumnInput.create "ManualEntry" ColumnType.Bit with Nullability = Nullability.AllowNull}
                // Desired feature, an AchivedChargeID column (no fkey, used to store the chargeId that the JE used to point to before the chargeId was deleted from the system
                //{ColumnInput.create "
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
    
let fGetNotifyOptions (ti:TableIdentifier) : NotifyClassOptions =
    if ti.Name= "PatientInfo" then
        {SettersCheckInequality=true;AllowPropertyChangeOverride=false}
    else {SettersCheckInequality=false;AllowPropertyChangeOverride=false}
let results = 
    let cn = BCore.ADO.AdoHelper.Connector.CreateCString cString
    //runFullGeneration scriptFullPath generatePartials toGen addlCodeTables |> Map.ofDictionary
    //    type TableGenerationInfo = {Schema:string; Name:string; GenerateFull:bool}
    let iDte = DteGenWrapper(dte)
    let ga = {GetSqlMeta= CodeGeneration.SqlMeta.getSqlMeta;SqlSprocs= getSqlSprocs cn;MapSqlSprocParams=mapSprocParams cn}
    GenerateAllTheThings.runGeneration
        (sprintf "%s.linq" Util.CurrentQuery.Name) debug sb iDte manager cgsm toGen2 dataModelsToGen ga fGetNotifyOptions
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