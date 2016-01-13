<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
</Query>

#if INTERACTIVE
#I @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\WPF\"
#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml"
#r "UIAutomationTypes.dll"
#r "C:\Program Files (x86)\Linqpad\Beta5\LINQPad.exe" 
// LINQPad.Util.DisplayWebPage("www.google.com")
#endif

open System.Windows
open System.Windows.Controls
type UcData() as this = 
    inherit UserControl()

    do
        this.DataContext <- this
        this.IsVisibleChanged.Add (fun e -> printfn "%A" (e.NewValue,e.OldValue, e.Property))
        this.Content <- Button()

        // compare and constrast this.Content <- vs. this.AddChild
        // this.AddChild (Button())

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
    let display html = 
        let tempPath = 
            System.IO.Path.GetTempPath()
            |> (fun d-> System.IO.Path.Combine(d, System.IO.Path.GetTempFileName() + ".html"))
        System.IO.File.WriteAllText(tempPath, html)
        System.Diagnostics.Process.Start(tempPath)

    type Query = {Name:string}
    type Util = 
        static member CurrentQuery = {Name= __SOURCE_FILE__ } // mock of LinqPad's ObjectModel.Query
    type System.Object with
        member x.Dump() = printfn "%A" x
        member x.Dump(s) = printfn "%s - %A" s x

#endif
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
    // another option is: 
    //dumpHtml2 [| x |]
    |> display

// testUc()
testDumpContainer (1,5)