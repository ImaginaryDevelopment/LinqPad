<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Reference Relative="..\..\..\FunctionalWpf\WpfComponents\bin\Debug\WpfComponents.dll">C:\projects\FunctionalWpf\WpfComponents\bin\Debug\WpfComponents.dll</Reference>
  <Reference Relative="..\..\..\FunctionalWpf\WpfTypes\bin\Debug\WpfTypes.dll">C:\projects\FunctionalWpf\WpfTypes\bin\Debug\WpfTypes.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>Microsoft.FSharp.Quotations</Namespace>
  <Namespace>Microsoft.FSharp.Quotations.Patterns</Namespace>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Markup</Namespace>
</Query>

// username/password

let dumpt t x = x.Dump(description=t); x
let dumpCount t (x: _ list) = x.Length.Dump(description=t); x
// functional wpf
// http://fsharpcode.blogspot.it/2011/07/functional-wpf-part-five-simple-example.html

let pboxName = "INeedCandy"

#if INTERACTIVE
#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#endif

let assemblyName,assemblyPath,appMainPath = 
    let t = typeof<WpfTypes.PasswordBoxAssistant>
    t.Dump()
    let fullPath = t.Assembly.Location
    fullPath.Dump()
    let name = Path.GetFileNameWithoutExtension t.Assembly.Location
//    let r = sprintf "clr:namespace:%s;assembly=%s" t.Namespace name
    let r = sprintf "clr:namespace:%s;assembly=WpfTypes" t.Namespace //name
    r.Dump()
    name, fullPath,r
    //"clr-namespace:FunctionalFun.UI;assembly=WpfTypesC"
open WpfTypes
open WpfTypes.WpfHelpers
type System.Object with
    static member cast<'T> (o:obj) = match o with | :? 'T as t -> Some t | _ -> None

let run app data = 
    // force load type into app domain
    WpfTypes.PasswordBoxAssistant.BindPasswordProperty |> ignore
    let context = ParserContext()
    context.XamlTypeMapper <- XamlTypeMapper([| |])
    context.XamlTypeMapper.SetAssemblyPath(assemblyName,assemblyPath)
    
    app
    |> FunctionalParsing.parseApplication
    |> dumpt "Raw Xaml"
    |> fun x -> XamlReader.Parse(x,context)
    |> fun x -> printfn "Parsed just fine!"; x
    |> fun x -> (x :?> System.Windows.Application)
    |> fun x -> 
        x.Dispatcher.UnhandledException.Add(fun t ->
            t.Dump("t")
            ()
        )
        x.Startup.Add (fun t ->
            printfn "Startup may have completed"   
            ()
        )
        
        x.MainWindow.DataContext <- data

        try
            x.MainWindow.Show()
            
            printfn "Showing just fine"
            
        with _ ->
            reraise()
        
        x.Run()
        
   |> ignore


open FunctionalXaml

// Main Window
let mainWindow = 
    let w,x,f = WpfTypes.LoginComponent.makeLoginWindow ()
    x.Dump()
    w.ShowDialog()
    f().Dump("result")
    w
             
// Application                                
//let sampleApplication = application (mainWindow, Map["ff",appMainPath])

// Run the app with the given dataContext                                                               
//[<STAThread()>]
//do run sampleApplication dataContext
//credential.Dump()