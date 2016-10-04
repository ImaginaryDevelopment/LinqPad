<Query Kind="FSharpProgram">
  <Reference>&lt;CommonProgramFiles&gt;\Microsoft Shared\MSEnv\PublicAssemblies\envdte.dll</Reference>
  <Reference>&lt;CommonProgramFiles&gt;\Microsoft Shared\MSEnv\PublicAssemblies\envdte80.dll</Reference>
</Query>

//"C:\Program Files (x86)\Common Files\Microsoft Shared\MSEnv\PublicAssemblies\envdte.dll"
//#r "EnvDTE"
//#r "EnvDTE80"
let dte = System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.14.0") :?> EnvDTE80.DTE2
let findInFiles (dte: EnvDTE80.DTE2) = 
    dte.Find.
    dte.FileName |> Dump |> ignore
    dte.Solution.FullName |>Dump |> ignore