<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Reference Relative="..\..\..\WpfTypes\WpfTypes\bin\Debug\WpfTypes.dll">C:\projects\WpfTypes\WpfTypes\bin\Debug\WpfTypes.dll</Reference>
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

#if INTERACTIVE
#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#endif

type Color = Red | Green | Blue
type Orientation = Horizontal | Vertical

type Attribute = 
    | Width of int
    | Height of int

type DataTemplate = DataTemplate of FFrameworkElement
and FFrameworkElement
    = Label of string * Attribute list
    | Button of String
    | TextBox of Attribute list * Expr
    | PasswordBox of Attribute list * Expr
    | StackPanel of FFrameworkElement list * Attribute list * Orientation
    | Border of Color * FFrameworkElement
    | ItemsControl of DataTemplate
    
type Window = Window of FFrameworkElement * Attribute list
type Application = Application of Window

module PartTwo = 
    let width = Width
    let height = Height
    let stackpanel attrs orient xs = StackPanel(xs,attrs,orient)
    let border c x = Border(c,x)
    let label attrs s = Label(s,attrs)
    let button = Button
    let textbox attrs b = TextBox(attrs,b)
    let itemscontrol = ItemsControl
    let datatemplate = DataTemplate
    let application = Application
    let window attrs c = Window(c, attrs)
    let passbox attrs b = PasswordBox(attrs,b)
    
open PartTwo

let parseColor =
    function
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"
    
let parseOrientation o = 
    match o with
    | Horizontal -> "Horizontal"
    | Vertical -> "Vertical"

let parseAttribute attr = 
    match attr with
    | Width x -> sprintf @"Width = ""%i""" x
    | Height x -> sprintf @"Height = ""%i""" x

let parseAttributes = 
    List.fold (fun acc x -> sprintf "%s %s" acc (parseAttribute x)) ""
    
let rec getPropertyName b = 
    match b with
    | Lambda (_,e) -> getPropertyName e
    | PropertyGet (_, info, _) -> info.Name
    | _ -> b.Dump(); printfn "bad expression %A" b; failwith "bad expression"
    
let parseBinding b =
    getPropertyName b
    |> sprintf @"{Binding Path=%s}"

let rec parseDataTemplate (DataTemplate x) = 
    sprintf @"<DataTemplate>%s</DataTemplate>" (parseFrameworkElement x)
    

and parseFrameworkElement x = 
    match x with
    | Label (s, attrs) -> sprintf @"<Label Content=""%s"" %s/>" s
                                          (parseAttributes attrs)
    | Button s -> sprintf @"<Button Content=""%s""/>" s
    | TextBox (attrs, b) -> sprintf @"<TextBox %s Text=""%s""/>"
                                    (parseAttributes attrs)
                                    (parseBinding b)
    | PasswordBox (attrs,b) -> 
        let name = getPropertyName b
//        let attrs = parseAttributes attrs
        //let bindAssist = sprintf @"ff:PasswordBoxAssistant.BindPassword=""true"" ff:PasswordBoxAssistant.BoundPassword=""{Binding Path=%s}""" name
//        let attrs = sprintf "%s" attrs
        sprintf @"<PasswordBox x:Name=""Mine"" %s />" 
            (parseAttributes attrs)
            
            

    | StackPanel (xs, attrs, orient) ->
             let (+) x y = sprintf "%s\n%s" x y
             sprintf @"<StackPanel Orientation = ""%s"" %s>%s
                       </StackPanel>"                                                                             
                     (parseOrientation orient)
                     (parseAttributes attrs)
                     (List.fold (fun acc x -> acc + (parseFrameworkElement x)) "" xs)
    | Border (c, x) -> sprintf @"<Border BorderBrush=""%s""
                                         BorderThickness=""2"">%s</Border>"
                               (parseColor c)
                               (parseFrameworkElement x)
    | ItemsControl t -> 
             sprintf @"<ItemsControl ItemsSource=""{Binding .}""> 
                       <ItemsControl.ItemTemplate>%s</ItemsControl.ItemTemplate>
                       </ItemsControl>" (parseDataTemplate t)
    

let parseWindow (Window (c, attrs)) =
            sprintf @"<Window %s>%s</Window>"
                    (parseAttributes attrs)
                    (parseFrameworkElement c)

let assemblyName,assemblyPath,appMainPath = 
    let t = typeof<FunctionalFun.UI.PasswordBoxAssistant>
    t.Dump()
    let fullPath = t.Assembly.Location
    fullPath.Dump()
    let name = Path.GetFileNameWithoutExtension t.Assembly.Location
//    let r = sprintf "clr:namespace:%s;assembly=%s" t.Namespace name
    let r = sprintf "clr:namespace:%s;assembly=WpfTypes" t.Namespace //name
    r.Dump()
    name, fullPath,r
    //"clr-namespace:FunctionalFun.UI;assembly=WpfTypesC"
    
let parseApplication (Application c) =
        sprintf @"<Application
                   xmlns=""http://schemas.microsoft.com/winfx/2006/xaml/presentation""
                   xmlns:x=""http://schemas.microsoft.com/winfx/2006/xaml""
                   xmlns:ff=""%s"">
                   <Application.MainWindow>%s</Application.MainWindow></Application>"
            appMainPath    
            (parseWindow c)
            
module WpfHelpers =            
    open System.Windows
    open System.Windows.Media
    open System.Windows.Markup
    let getXaml(o:obj) = 
        XamlWriter.Save(o)
    let tryDumpXaml (o:obj) =
        (getXaml o).Dump("xaml?")
        
    // duplicate problem
    let rec walkChildren (x:obj) : obj seq = // turn element into sequence of children
        let walkVisualChildren fChildren (dObj:DependencyObject)  =
            seq{
                match System.Windows.Media.VisualTreeHelper.GetChildrenCount(dObj) with
                    | 0 -> ()
                    | x ->
                        let items = [0..x - 1] |> Seq.map(fun i -> VisualTreeHelper.GetChild(dObj, i))
                        yield! items |> Seq.cast<obj>
                        yield! items |> Seq.cast<obj> |> Seq.collect fChildren
            }
        seq{
            match x with
            | null -> ()
            | :? ItemsControl as ic -> // items control won't have items stuff until rendered =(
                let items = 
                    [0.. ic.Items.Count - 1]
                    |> Seq.map ic.ItemContainerGenerator.ContainerFromIndex
                    |> List.ofSeq
                yield! items |> dumpCount "items control items" |> Seq.cast<obj>
            | :? DependencyObject as dObj -> 
                let items = LogicalTreeHelper.GetChildren(dObj) |> Seq.cast<obj> |> List.ofSeq
                yield! items 
                yield! items |> Seq.collect walkChildren
                let vChildren = walkVisualChildren walkChildren dObj |> Seq.cast<obj>
                yield! vChildren
            | :? String as s -> ()
            | x -> x.GetType().Name.Dump("no match, non dep object")
                
                    
        }
open WpfHelpers
type System.Object with
    static member cast<'T> (o:obj) = match o with | :? 'T as t -> Some t | _ -> None
let run app data = 
    // force load type into app domain
    FunctionalFun.UI.PasswordBoxAssistant.BindPasswordProperty |> ignore
    let context = ParserContext()
    context.XamlTypeMapper <- XamlTypeMapper([| |])
    context.XamlTypeMapper.SetAssemblyPath(assemblyName,assemblyPath)
    app
    |> parseApplication 
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
        if true then
            x.MainWindow.DataContext <- data
            printfn "Set datacontext just fine"
        printfn "Finding password box!"
        let handlePbBinding () = 
            let bindIt (pb:PasswordBox) = 
                let binding = System.Windows.Data.Binding()
                binding.Path <- PropertyPath("Password")
                let bp = FunctionalFun.UI.PasswordBoxAssistant.BoundPasswordProperty
    
                pb.SetBinding(bp, binding) |> ignore
                FunctionalFun.UI.PasswordBoxAssistant.SetBindPassword pb (box true)
                printfn "done with binding"
                
            let pb = x.MainWindow.FindName("Mine") |> fun x -> x.Dump("found it?"); x |> fun x -> x :?> PasswordBox 
            if isNull pb then
                let pbOpt = 
                    let children = walkChildren x.MainWindow |> List.ofSeq
                    children |> dumpCount "main window children" |> ignore
                    (children |> Seq.map(fun x -> (x.GetType().Name), getXaml x)).Dump("children types")
                    children |> Seq.choose(function | :? PasswordBox as pb -> Some pb | _ -> None) |> Seq.tryHead
                pbOpt.Dump("somebody think of the children!?")
                match pbOpt with
                | Some pb ->
                    printfn "found it alternative method"
                    bindIt pb
                    true
               
                | None -> false
            else                 
                bindIt pb
                true
        try
                handlePbBinding() |> ignore
            with ex ->
                ex.Dump("pbBinding exception")
        try
            x.MainWindow.Show()
            
            printfn "Showing just fine"
            
        with _ ->
            
            //System.AppDomain.CurrentDomain.GetAssemblies().Dump()
            reraise()
        
        x.Run()
   |> ignore
type LoginCredential() = 
    member val Username:string = null with get,set //{ mutable Username:string; mutable Password:string}
    member val Password:string = null with get,set
    member val BindPassword:bool = false with get,set
    member val BoundPassword:string = String.Empty with get,set


let dataContext = [LoginCredential()]


// Header
let header = [ label [width 100] "User Name"
               label [width 100] "Password"
                ] |> stackpanel [] Horizontal

//// Row
let row = [ textbox [width 100] <@@ fun (x:LoginCredential) -> x.Username @@>
            passbox [width 100] <@@ fun (x:LoginCredential) -> x.Password @@> ]
            |> stackpanel [] Horizontal

// Data Template
//let sampleTemplate = datatemplate row

// Final composition
let sampleGrid = [ header
                   row
                   button "submit" ] |> stackpanel [width 250] Vertical
                                     |> border Blue

// Main Window
let mainWindow = window [width 400; height 200] sampleGrid
             
// Application                                
let sampleApplication = application mainWindow

// Run the app with the given dataContext                                                               
[<STAThread()>]
do run sampleApplication dataContext