<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Windows.Presentation.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
</Query>

// demonstrate making a frame control's Arrows and Nav Tab enlarging

module Option =
    let inline ofCast<'t> (x:obj) =
        match x with
        | :? 't as v -> Some v
        | _ -> None
module Seq =
    let inline ofType<'t> (items:_ seq) =
        items |> Seq.choose(box >> Option.ofCast<'t>)
module WpfHelpers =
    open System.Reflection
    let getChildren (x:Panel) =
        x.Children
    let getChildrenOfType<'t> (x:Panel) : 't seq =
        x
        |> getChildren
        |> Seq.cast<obj>
        |> Seq.ofType<'t>
    let getTemplateChild(x :FrameworkElement) =
        let flags = BindingFlags.NonPublic ||| BindingFlags.Instance
        let tcm = x.GetType().GetMember("get_TemplateChild", flags).[0] :?> MethodInfo
        let result = tcm.Invoke(x,null)
        result :?> UIElement
        
    let getDockPanel (sender:FrameworkElement) =
        // if the frame has been drawn/nav enabled, child will be a border first
        let rec tryDP tryAgain (x:UIElement):DockPanel=
            match x with
            | :? Border as b ->
                if tryAgain then
                    tryDP false b.Child
                else
                    failwith "Border found inside border?"
            | :? DockPanel as dp ->
                dp
            | null ->
                invalidArg "sender" "No dockpanel, sender was null"
            | x ->
                x.Dump()
                failwithf "that's no dockpanel, it's a %sstation!" <| x.GetType().Name
        getTemplateChild sender
        |> tryDP true
    
    let fixArrowSize (frame:Frame) =
        let adjustGridControl size (c:Control) =
            c.MinHeight <- size
            c.MinWidth <- size
            c
            |> Option.ofCast<Button>
            |> Option.iter(fun b ->
                let content = b.Content
                let child = getTemplateChild b :?> Grid
                child.MinHeight <- size
                child.MinWidth <- size
            )
        match getDockPanel frame with
        | null -> ()
        | dp ->
            let grid =
                dp
                |> getChildrenOfType<Grid>
                |> Seq.head
            grid
            |> getChildrenOfType<System.Windows.Shapes.Path>
            |> Seq.tryHead
            |> Option.iter (fun path ->
                grid.Children.Remove path
                let size = 40.0
                grid
                |> getChildrenOfType<Control>
                |> Seq.iter (adjustGridControl size)
            )
       
       
        
        ()

let window = Window()
        
let generateDemoFrame gridRow =
    let f = System.Windows.Controls.Frame()
    f.NavigationUIVisibility <- System.Windows.Navigation.NavigationUIVisibility.Visible

    let page =
        Page(Content=Button(Content=sprintf "Hello from row %i (index %i)" gridRow (gridRow + 1)))
    f.Navigate(page) |> ignore
    f.SetValue(Grid.RowProperty,gridRow)
    f
    
let frame2 = generateDemoFrame 2
let grid =
    let g = Grid()
    g.RowDefinitions.Add(RowDefinition(MinHeight=80.,Height=GridLength 80.))
    g.RowDefinitions.Add(RowDefinition(Height=GridLength 200.))
    g.RowDefinitions.Add(RowDefinition(Height=GridLength 200.))

    generateDemoFrame 1
    |> g.Children.Add
    |> ignore<int>
    
    g.Children.Add frame2 |> ignore<int>
    g

window.Content <- grid

window.Dump()
// change size after display
WpfHelpers.fixArrowSize frame2