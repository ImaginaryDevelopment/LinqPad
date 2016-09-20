<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\ReachFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>System.Windows.Xps.Packaging</Namespace>
</Query>

// face sheet
//http://stackoverflow.com/questions/910814/loading-xaml-at-runtime
//https://blogs.msdn.microsoft.com/ashish/2007/08/14/dynamically-loading-xaml/

open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup

let getType x = x.GetType()
module Cereal = 
    open Newtonsoft.Json
    type private Deserializer<'t> =
        static member deserializeObject<'t> x = JsonConvert.DeserializeObject<'t>(x)
    let serialize usePretty x =
        JsonConvert.SerializeObject(x, if usePretty then Formatting.Indented else Formatting.None)
    let deserialize<'t> x = 
        JsonConvert.DeserializeObject<'t> x
   
module Xps = 
    open System.Windows.Controls
    open System.Windows.Documents
    open System.Windows.Media
//    
//    type FittedDocumentPaginator (baseDp, scale) = //http://stackoverflow.com/questions/1267046/wpf-flowdocument-scale-to-fit-page
//        inherit DocumentPaginator()
//        let mutable pageSize = Size.Empty
//        let sTransform = ScaleTransform(scale,scale)
//        
//        member x.Base:DocumentPaginator = baseDp
//        member x.Scale:float = scale
//        override x.GetPage pageNumber = 
//            let page = base.GetPage pageNumber
//            page.Visual :?> ContainerVisual
//            |> fun cv -> cv.Transform <- sTransform
//            page
//        override __.IsPageCountValid = true
//        override __.PageCount = 1
//        override __.PageSize with get() = pageSize and set v = pageSize <-v
//        override __.Source = baseDp.Source
//        
//        
//    let printFittedPreview (flow:FlowDocument) title documentWidth doResetFlow =
//        let pd = System.Windows.Controls.PrintDialog()
//        
//        
//        if pd.ShowDialog().GetValueOrDefault() then
//            let pageMargins = flow.PagePadding
//            flow.PagePadding <- Thickness(15.)
//            let scale = 
//                match documentWidth / pd.PrintableAreaWidth with
//                | _scale when _scale < 1. -> 1.
//                | x -> x
//            
//            let invScale = 1. / scale
//            flow.PageHeight <- pd.PrintableAreaHeight * scale
//            flow.PageWidth <- pd.PrintableAreaWidth * scale
//            let dp = flow :> IDocumentPaginatorSource |> fun idps -> idps.DocumentPaginator
//            let fdp = FittedDocumentPaginator(dp,invScale)
//            pd.PrintDocument(fdp, title)
//            if doResetFlow then
//                flow.PageHeight <- System.Double.NaN
//                flow.PageWidth <- System.Double.NaN
//                flow.PagePadding <- pageMargins
//            ()
//       
    
    // trying https://blogs.msdn.microsoft.com/fyuan/2007/03/10/convert-xaml-flow-document-to-xps-with-style-multiple-page-page-size-header-margin/
    type DocumentPaginatorWrapper(paginator:DocumentPaginator, pageSize:Size, margin:Size) =
        inherit DocumentPaginator()
        do
            paginator.PageSize <- Size(pageSize.Width - margin.Width * 2., pageSize.Height - margin.Height * 2.)
        let mutable typeFace:Typeface = null
        let move (rect:Rect) =
            if rect.IsEmpty then 
                rect
            else
                Rect(rect.Left + margin.Width, rect.Top + margin.Height, rect.Width, rect.Height)
        override x.GetPage pageNumber = 
            let page = paginator.GetPage pageNumber
            let newPage = ContainerVisual()
            let drawTitle() = 
                let title = DrawingVisual()
                
                use ctx = title.RenderOpen()
                if isNull typeFace then
                    typeFace <- Typeface("Times New Roman")
                let text = FormattedText(sprintf "Page %i" (pageNumber + 1), Globalization.CultureInfo.CurrentCulture, FlowDirection.LeftToRight, typeFace, 14., Brushes.Black)
                ctx.DrawText(text, Point(0., -96. / 4.)) // 1/4 inch above page content
                title
            let drawBackground() = 
                let background = DrawingVisual()
                use ctx = background.RenderOpen()
                ctx.DrawRectangle( SolidColorBrush(Color.FromRgb(240uy,240uy,240uy)), null, page.ContentBox)
                background
            let title = drawTitle()
            let bg = drawBackground()
            newPage.Children.Add bg |> ignore<int>
            let smallerPage = ContainerVisual()
            smallerPage.Children.Add page.Visual |> ignore<int>
            smallerPage.Transform <- MatrixTransform(0.95, 0., 0., 0.095, 0.025 * page.ContentBox.Width, 0.025 * page.ContentBox.Height) :> Transform
            newPage.Children.Add smallerPage |> ignore<int>
            newPage.Children.Add title |> ignore<int>
            newPage.Transform <- TranslateTransform(margin.Width, margin.Height)
            new DocumentPage(newPage, pageSize, move page.BleedBox, move page.ContentBox)
            
        override __.IsPageCountValid = paginator.IsPageCountValid
        override __.PageCount = paginator.PageCount
        override __.PageSize with get() = paginator.PageSize and set v = paginator.PageSize <- v
        override __.Source = paginator.Source
            
        
module Behaviors = 
    type CloseThisWindowCommand private () =
        let canExecuteChanged = new Event<_, _>()
    
        member x.CanExecute (parameter:obj) =
            match parameter with
            | null -> 
                printfn "CanExecute parameter was null"
                false
            | :? Window -> 
                printfn "type of parameter is %s" (parameter.GetType().Name)
                true
            | :? DependencyObject as depObj ->
                printfn "type of parameter is %s" (parameter.GetType().Name)
                match LogicalTreeHelper.GetParent depObj with
                | null -> 
                    match System.Windows.Media.VisualTreeHelper.GetParent depObj with
                    | null ->
                        printfn "Unable to find parent of parameter"
                        false
                    | vParent -> 
                        x.CanExecute vParent
                | lParent ->
                    x.CanExecute lParent
    
            | _ -> 
                printfn "type of parameter is %s" (parameter.GetType().Name)
                false
        member x.Execute (parameter:obj) = 
            if x.CanExecute parameter then
                parameter :?> Window
                |> fun w -> w.Close()
    
        interface ICommand with
            member x.CanExecute(parameter) = x.CanExecute parameter
            [<CLIEvent>]
            member x.CanExecuteChanged = canExecuteChanged.Publish
            member x.Execute(parameter) = x.Execute parameter
    
        static member Instance = new CloseThisWindowCommand()

let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\PatientFaceSheet.xaml"

let rootElement = 
    
    let text = Regex.Replace(File.ReadAllText path, "x:Class=\".+\"", String.Empty)
    XamlReader.Parse text

let flow = rootElement :?> System.Windows.Documents.FlowDocument

type DisplayStrategy = // consider adding each of these: https://wpf.2000things.com/2011/03/23/254-types-of-containers-for-hosting-flowdocument/
    | FlowGridDocScrollViewer
    | FlowDocScrollViewerRaw
    | FlowPrintDialog
    | ScaledFlowDocument
//let inline toFormatString (f:string) (a:^a) = ( ^a : (member ToString:string -> string) (a,f))
let inline showDialog x = (^a: (member ShowDialog:unit -> bool Nullable) x)

let displayFlow vm ds=
    let window = 
        match ds with
        // yay centering
        |FlowGridDocScrollViewer ->
            let fdsv = FlowDocumentScrollViewer(Document = flow)
            let grid = Grid()
            grid.Children.Add(fdsv) |> ignore
            Window(Content=grid)
        |FlowDocScrollViewerRaw ->
            let fdsv = FlowDocumentScrollViewer(Document = flow)
            Window(Content=fdsv)
        |FlowPrintDialog ->
            //let fdsv = FlowDocumentScrollViewer(Document = flow)
            let pd = PrintDialog()
            if pd.ShowDialog().GetValueOrDefault() then
                printfn "PageWidth was %A, will be %A" flow.PageWidth pd.PrintableAreaWidth
                flow.PageWidth <- pd.PrintableAreaWidth
//                let copyString = XamlWriter.Save(flow)
//                let copy:Documents.FlowDocument = XamlReader.Parse copyString :?> Documents.FlowDocument
                flow.PageHeight <- pd.PrintableAreaHeight
                let fdsv = FlowDocumentScrollViewer(Document = flow)
                Window(Content=fdsv)
                
            else
                Window(Content = flow)
//            let dps = flow :> System.Windows.Documents.IDocumentPaginatorSource
//            let paginator = dps.DocumentPaginator
//            pd.ShowDialog()
//            Window(Content = pd)
        |ScaledFlowDocument -> // trying http://stackoverflow.com/questions/7931961/wpf-printing-to-fit-page
//            flow.PageWidth <- 100.
//            flow.PageHeight <- 1000.
//            flow.PagePadding <- Thickness(0.5)
//            Xps.printFittedPreview flow "Heavy flow" 800. false
            
            Window(Content = flow)

    window.DataContext <- vm
    window
type FacilityInfo = {FacilityName:string; FacilityAddress1:string; FacilityAddress2:string; FacilityPhone:string;FacilityFax:string}
type PatientInfo = {PatientID: int;FirstName:string;MiddleInitial:string;LastName:string;DOB:DateTime;Age:string; Address1:string; City:string; State:string; Zip:string; MaritalStatus:string; Gender:string;PrimaryPhone:string;SecondaryPhone:string;EmergencyContactLastName:string;EmergencyContactFirstName:string;EmergencyContactPhone:string; EmailAddress:string}
type PatientSummary = {FirstName:string; LastName:string; MiddleInitial:string; DOB:DateTime; Age:string;} 
    with 
        static member fromPatientInfo (x:PatientInfo):PatientSummary = 
            //{FirstName=x.FirstName; LastName=x.LastName; MiddleInitial= }
            let cereal :PatientSummary = Cereal.serialize false x |> Cereal.deserialize
            cereal

type PayerInfo = {Name:string; PayerType:string; Subscriber:PatientSummary;SubscriberRelationship:string;}
type ViewModel =  {Facility:FacilityInfo;Patient:PatientInfo;Payers:PayerInfo list; Gender:string; Generated:DateTime; LastVisit:DateTime option}

let vm = 
    let savePath = Path.Combine (Path.GetDirectoryName path, sprintf "%s.vm.json" (Path.GetFileNameWithoutExtension path))
    let rawText = File.ReadAllText savePath
    let deserialized:ViewModel = Cereal.deserialize rawText
    // make any updates/property additions here
    // ....
    // 
    let vm = {deserialized with Gender= deserialized.Patient.Gender}
    let reserialized = Cereal.serialize true vm
    if rawText <> reserialized then
        printfn "Writing vm update to filesystem"
        File.WriteAllText(savePath,reserialized)
    vm
    
type Listener(created:DateTime) = 
    inherit TraceListener("DataBindingErrorListener")
    override x.Write (msg:string) = printf "%s" msg
    override x.WriteLine (msg:string) = printfn "%s" msg
    member __.Created= created
    
// sometimes this works, sometimes it doesn't
let listenForBindingErrors() = 
    let getTypeName x = x.GetType().Name
    let alreadyAdded = PresentationTraceSources.DataBindingSource.Listeners |> Seq.cast<TraceListener> |> Seq.exists (getTypeName >> (=) typeof<Listener>.Name)
    if not alreadyAdded then
        new Listener(DateTime.Now)
        |> PresentationTraceSources.DataBindingSource.Listeners.Add
        |> Some
    else
        printfn "Trace listener(s) already attached (%i)" PresentationTraceSources.DataBindingSource.Listeners.Count
        PresentationTraceSources.DataBindingSource.Listeners 
        |> Seq.cast<TraceListener> 
        |> Seq.map getTypeName 
        |> List.ofSeq
        |> printfn "TraceListenerTypes: %A"
        None
        
listenForBindingErrors() |> ignore

flow.DataContext <- vm
let displayForShow() = 
    let window = displayFlow vm DisplayStrategy.FlowGridDocScrollViewer
    
    window.KeyDown.Add(fun k -> 
        if k.Key = Key.Escape then
            window.Close())
    
    //// show to query via code, then hide and show dialog to interact and view
    //window.Show()
    //
    //window.Hide()
    
    window.ShowDialog()
    |> ignore<bool Nullable>

displayForShow()
//consider adding printing/print preview to this script
// http://stackoverflow.com/questions/4263204/wpf-printing-flowdocument-not-centering