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
let debug = true

type Cont<'T> = 'T list
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
        try
            XamlWriter.Save(o)
            |> Some
        with ex ->
            ex.Dump("Failed getXaml")
            None

    let tryDumpXaml (o:obj) = (getXaml o).Dump("xaml?")
        
    type Tree<'T> = 
        | Leaf of 'T*('T Tree list)
    module Tree =
        let rec map f (Leaf(leaf,children))=
                Leaf(f leaf, children |> List.map (map f))
        let rec length (Leaf(leaf,children)) = 
            1 + (children |> Seq.map length |> Seq.sum)
        let rec flatten (Leaf(leaf,children) as l) =
        
            let result = 
                seq{
                    yield leaf
                    yield! children |> Seq.collect flatten
                }
                |> List.ofSeq
            
            if children.Length <> result.Length - 1 then
                printfn "Flatten was bad"
            if result.Length <> length l then
                printfn "Flatten WAS bad"
            result
            |> Seq.ofList
        
                
    type Child =
        | Logical of obj
        | Visual of DependencyObject
        | Both of DependencyObject
        with 
            static member GetValue =
                function
                | Logical o -> o
                | Visual v 
                | Both v -> box v
    type ChildTree = Tree<Child>
    
    let unionall<'T> (left:'T list) (right:obj list) fJoin =
        let minCount = if left.Length > 0 || right.Length > 0 then min left.Length right.Length |> max 1 else 0
        let result = 
            seq{
                yield! 
                    left
                    |> Seq.map(fun l ->
                        Some l, right |> Seq.tryFind (fJoin l)
                    )
                let rights = 
                    right
                    |> Seq.filter(fun r ->
                        left |> Seq.exists(fun l -> fJoin l r) |> not
                    )
                    |> Seq.map(fun r -> None, Some r)
                    |> List.ofSeq
                if right.Length > left.Length && right.Length < 1 then
                    printfn "bad right"
                
                yield! rights
                    
            }
            |> List.ofSeq
        printfn "Left:%i,Right:%i,Result:%i" left.Length right.Length result.Length
        if result.Length < minCount then
            printfn "bad unionall"
        result
        
    let getVisualChildren (dObj:DependencyObject) : DependencyObject seq =
        let getVChild i = VisualTreeHelper.GetChild(dObj,i)
        seq{
            match System.Windows.Media.VisualTreeHelper.GetChildrenCount dObj with
            | 0 -> ()
            | x ->
                let items = [0..x - 1] |> Seq.map getVChild
                yield! items
        }
    let getItemsControlElements (ic:ItemsControl) =
        [0.. ic.Items.Count - 1]
        |> Seq.map ic.ItemContainerGenerator.ContainerFromIndex
    let mapChildTree = List.map(Tree.map(function | Visual v -> "Visual", v.GetType().Name | Logical l -> "Logical", l.GetType().Name | Both v -> "Both", v.GetType().Name))
    // duplicate problem
    let rec walkChildren (x:obj) : ChildTree seq = // turn element into sequence of children
        printfn "walking children"
        seq{
            match x with
            | null -> ()
            | :? ItemsControl as ic -> // items control won't have items stuff until rendered =(
                let items = 
                    getItemsControlElements ic
                    |> Seq.map(fun item -> ChildTree.Leaf (Visual item, walkChildren item |> List.ofSeq))
                    |> List.ofSeq
                yield! items |> dumpCount "items control items"
            | :? DependencyObject as dObj -> 
                let vChildren = getVisualChildren dObj |> List.ofSeq
                let items = LogicalTreeHelper.GetChildren(dObj) |> Seq.cast<obj> |> List.ofSeq
                let minCount = 
                    if vChildren.Length > 0 && items.Length > 0 then min vChildren.Length items.Length else 0
                printfn "dObj children: %i, %i" vChildren.Length items.Length
//                vChildren |> Seq.map(fun v -> v.GetType().Name,getXaml v) |> dumpt "some children" |> ignore
//                items |> Seq.map(fun v -> v.GetType().Name,getXaml v) |> dumpt "item children" |> ignore
                // will this finally fix duplicates?
                let mated = unionall vChildren items (fun x y -> Object.ReferenceEquals(x,y)) |> List.ofSeq
                if mated.Length < minCount then
                    printfn "bad unionall afterall"
                let unionMapped = 
                    mated
                    // can't get 2 None, but what are we gonna do?
                    |> Seq.choose(
                        function
                        | Some dObj,None ->
                            
                            ChildTree.Leaf(Visual dObj, walkChildren dObj |> List.ofSeq)
                            |> Some
                        | Some dObj, Some _ ->
                            ChildTree.Leaf(Both dObj, walkChildren dObj |> List.ofSeq)
                            |> Some
                        | None, Some l ->
                            ChildTree.Leaf(Logical l, walkChildren l |> List.ofSeq)
                            |> Some
                        | None, None ->
                            printfn "None? this shouldn't have happened"
                            None
                    )
                    |> List.ofSeq
                if unionMapped.Length < minCount then
                    printfn "bad unionmap"
                
                yield! unionMapped
                    
                
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
                    children |> Seq.map Tree.length |> Seq.sum |> dumpt "main window children" |> ignore
                    //children |> List.map(Tree.map mapChildTree)  |> dumpt "Tree" |> ignore
                    (children |> Seq.map(fun x -> (x.GetType().Name), getXaml x)).Dump("children types")
                    let allMyChildren = children |> Seq.collect Tree.flatten |> Seq.map Child.GetValue |> List.ofSeq
                    allMyChildren.Length.Dump("allMyChildren")
                    allMyChildren |> Seq.map getXaml |> dumpt "all my treedom" |> ignore
                    allMyChildren|> Seq.choose(function | :? PasswordBox as pb -> Some pb | _ -> None) |> Seq.tryHead
                match pbOpt with
                | Some pb ->
                    printfn "found it alternative method"
                    bindIt pb
                    true
               
                | None -> 
                    printfn "Did not find it =("
                    false
            else
                printfn "Found it directly"
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