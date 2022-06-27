<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>Microsoft.FSharp.Quotations</Namespace>
  <Namespace>Microsoft.FSharp.Quotations.Patterns</Namespace>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Markup</Namespace>
</Query>

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

type DataTemplate = DataTemplate of FrameworkElement
and FrameworkElement
    = Label of string * Attribute list
    | Button of String
    | TextBox of Attribute list * Expr
    | StackPanel of FrameworkElement list * Attribute list * Orientation
    | Border of Color * FrameworkElement
    | ItemsControl of DataTemplate
    
type Window = Window of FrameworkElement * Attribute list
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
    
let rec parseBinding b =
    match b with
    | Lambda (_,e) -> parseBinding e
    | PropertyGet (_, info, _) -> sprintf @"{Binding Path=%s}" info.Name

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


let parseApplication (Application c) =
        sprintf @"<Application
                   xmlns=""http://schemas.microsoft.com/winfx/2006/xaml/presentation""
                   xmlns:x=""http://schemas.microsoft.com/winfx/2006/xaml"">
                   <Application.MainWindow>%s</Application.MainWindow></Application>"
                (parseWindow c)
let run app data = app |> parseApplication                    
                       |> XamlReader.Parse
                       |> fun x -> (x :?> System.Windows.Application)
                       |> fun x -> x.MainWindow.DataContext <- data
                                   x.MainWindow.Show()
                                   x.Run()
                       |> ignore

type Person (firstName, lastName, age) =
    static member New (firstName, lastName, age) =
            new Person(firstName, lastName, age)
    member this.LastName
                with get () = lastName
                and set (value : string) = ()
    member this.FirstName
                with get () = firstName
                and set (value : string) = ()
    member this.Age
                with get () = age
                and set (value : int) = ()

let dataContext = [("Homer", "Simpson", 46)
                   ("Marge", "Simpson", 42)
                   ("Lisa", "Simpson", 9)
                   ("Bart", "Simpson", 12) ] |> List.map (Person.New)


// Header
let header = [ label [width 100] "First Name"
               label [width 100] "Last Name"
               label [width 50] "Age" ] |> stackpanel [] Horizontal

// Row
let row = [ textbox [width 100] <@@ fun (x:Person) -> x.FirstName @@>
            textbox [width 100] <@@ fun (x:Person) -> x.LastName @@>
            textbox [width 50] <@@ fun (x:Person) -> x.Age @@> ]
            |> stackpanel [] Horizontal

// Data Template
let sampleTemplate = datatemplate row

// Final composition
let sampleGrid = [ header
                   itemscontrol sampleTemplate
                   button "submit" ] |> stackpanel [width 250] Vertical
                                     |> border Blue

// Main Window
let mainWindow = window [width 400; height 200] sampleGrid
             
// Application                                
let sampleApplication = application mainWindow

// Run the app with the given dataContext                                                               
[<STAThread()>]
do run sampleApplication dataContext