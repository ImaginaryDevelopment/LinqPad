<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <NuGetReference>FontAwesome.WPF</NuGetReference>
  <Namespace>FontAwesome.WPF</Namespace>
</Query>

#if INTERACTIVE
//#I @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\WPF\"
#I @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5"
#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml"
#r "UIAutomationTypes.dll"
#r "C:\Program Files (x86)\Linqpad\Beta5\LINQPad.exe" 
#r @"C:\Users\Brandon\AppData\Local\LINQPad\NuGet.FW46\FontAwesome.WPF\FontAwesome.WPF.4.5.0.7\lib\net40\FontAwesome.WPF.dll"
// TODO: use fontawesome for the paging buttons

// LINQPad.Util.DisplayWebPage("www.google.com")
#endif

let display html = 
        let tempPath = 
            System.IO.Path.GetTempPath()
            |> (fun d-> System.IO.Path.Combine(d, System.IO.Path.GetTempFileName() + ".html"))
        System.IO.File.WriteAllText(tempPath, html)
        System.Diagnostics.Process.Start(tempPath)

#if LINQPAD 
    
#else
[<AutoOpen>]
module LinqPad =
    let dumpHtml<'t> (x:'t) =
        let writer = LINQPad.Util.CreateXhtmlWriter true
        writer.Write x
        let html = writer.ToString()
        html
    let dumpHtml2 items = LINQPad.Util.ToHtmlString items

    type Query = {Name:string}
    type Util = 
        static member CurrentQuery = {Name= __SOURCE_FILE__ } // mock of LinqPad's ObjectModel.Query
    type System.Object with
        member x.Dump() = printfn "%A" x
        member x.Dump(s) = printfn "%s - %A" s x

#endif

open System.Windows
open System.Windows.Controls
open System.Windows.Data
open FontAwesome.WPF.Converters

type UcData() as this = 
    inherit UserControl()

    do
        this.DataContext <- this
        this.IsVisibleChanged.Add (fun e -> printfn "%A" (e.NewValue,e.OldValue, e.Property))
        let grid = 
            let (>=>) (parent:ContentControl) (child:ContentControl) = parent.Content <- child; child 
            let createBorder content = 
                let b = Border()
                b.Child <- content
                b

            let createButton content = 
                let b = Button()
                b.Content <- content
                b

            let createFAButton ()= 
                let b = Button()
                let t = FontAwesome.WPF.FontAwesome()
                t.Icon <- FontAwesome.WPF.FontAwesomeIcon.Flag
                b.Content <- t
                b.Click.Add (fun ev -> ev.Dump() |> ignore )
                b

            let createStackPanel orientation items = 
                let sp = StackPanel()
                sp.Orientation <- orientation
                items |> Seq.iter(sp.Children.Add >> ignore<int>)
                sp

            let mockContainer x = x

            let createItemsPresenter = mockContainer // HACK: ItemsPresenter has no settable content or child properties, just return the element passed

            let itemsPresenter () = 
                let virtualizingStackPanel = 
                    let createPageButton i = 
                            let cp = ContentPresenter()
                            let button = createButton  (sprintf "%i" i)
                            cp.Content <- button
                            cp
                    let contentPresenter = createPageButton 1// 1 radio button container
                      
                    let contentPresenter2 = createPageButton 2// 2 radio button container
                    let contentPresenter3 = createPageButton 3
                    let contentPresenter4 = createPageButton 4
                    let vp = VirtualizingStackPanel()
                    vp.Orientation <- Orientation.Horizontal
                    [ contentPresenter; contentPresenter2; contentPresenter3; contentPresenter4]
                    |> Seq.iter (fun c -> vp.Children.Add c |> ignore)
                    vp
                createItemsPresenter virtualizingStackPanel

            let numericElementsPresenter() : StackPanel = 
                let itemsControl =
                    let border = 
                        let itemsPresenter = itemsPresenter ()// shows the 1,2,3,4 page numbers
                        let b = Border()
                        b.Child <- itemsPresenter
                        b
                    let ic = ItemsControl()
                    ic.Items.Add border |> ignore
                    ic
                let fakeNumericElementsPresenter = createStackPanel Orientation.Horizontal [| itemsControl |]
                fakeNumericElementsPresenter
            let itemGrid () = 
                    let g = Grid()
                    let sp = 
                        let moveToFirstPageButton = createButton "First Page" // consider using https://fortawesome.github.io/Font-Awesome/icons/
                        let moveToPreviousPageButton = createFAButton () 
                        let border = 
                            let nep = numericElementsPresenter()
                            createBorder nep 
                        let moveToNextPageButton = Button()
                        let moveToLastPageButton = Button()

                        createStackPanel Orientation.Horizontal [ moveToFirstPageButton :> UIElement; upcast moveToPreviousPageButton; upcast border; upcast moveToNextPageButton; upcast moveToLastPageButton]

                    g.Children.Add sp |> ignore<int>
                    g

            let radDataPager =
                let dataPagerPresenter = 
                    let border =
                        let grid = itemGrid()
                        createBorder grid
                    createStackPanel Orientation.Horizontal [ border ]

                mockContainer dataPagerPresenter
            let g = Grid()
            g.Children.Add radDataPager |> ignore
            g

        this.Content <- grid 

        // compare and constrast this.Content <- vs. this.AddChild
        // this.AddChild (Button())

// http://stackoverflow.com/a/28675069/57883
type TestWindow(content:obj) as this = 
    inherit Window()
    do
        this.Title <- Util.CurrentQuery.Name //  "NoXamlWpf" 
        this.Content <- content

    member x.foo() = ()

    new (content:UIElement) = 
        let sp = StackPanel()
        sp.Children.Add content |> ignore
        TestWindow(sp :> obj)

    new (control: ContentControl) = // http://referencesource.microsoft.com/#PresentationFramework/src/Framework/System/Windows/Controls/ContentControl.cs,f8ee23c685054e71
            control.BorderThickness <- Thickness(2.0)
            control.MinHeight <- 100.
            control.MinWidth <- 100.
            TestWindow(control :> UIElement)

let testUc () = 
    let w = new TestWindow(UcData()) 
    w.ShowDialog()

let testDumpContainer(x:obj) =
    dumpHtml x
    //dumpHtml2 [| x |]
    |> display

testUc()