<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll">C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\CodeGeneration.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio\2017\Enterprise\Common7\IDE\PublicAssemblies\envdte.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio\2017\Enterprise\Common7\IDE\PublicAssemblies\envdte80.dll</Reference>
  <Reference Relative="..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe</Reference>
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
open CodeGeneration.GenerateJS.TypeScript

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


// test jsGeneration
let indentation,memberIndentation = "","    "
[   "EraUserData", 
        [
            SimpleNamed {Name="EraPaymentID";Type=typeof<int Nullable>}
            SimpleNamed {Name="PaymentItemID";Type=typeof<int Nullable>}
            SimpleNamed {Name="ChargeID";Type=typeof<int>}
            SimpleNamed {Name="AdjustmentCodeId";Type=typeof<string>}
            SimpleNamed {Name="PatientResponsibilityAmt";Type=typeof<decimal>}
        ]
    "PaymentCreationUserData",
        [
            SimpleNamed {Name="PayerId";Type=typeof<int>}
            SimpleNamed {Name="EffectiveDate";Type=typeof<DateTime>}
            SimpleNamed {Name="TotalAmount";Type=typeof<decimal>}
            SimpleNamed {Name="TransactionNumber";Type=typeof<string>}
            SimpleNamed {Name="IsPaper";Type=typeof<bool>}
        ]
    "EraCreationUserData", 
        [
            Custom ("EraUserData", false, "EraUserData")
            Custom ("PaymentUserData", false, "PaymentCreationUserData")
        ]
]
|> Seq.map (fun (name, props) -> generateInterface name indentation memberIndentation props)
|> Dump
|> ignore