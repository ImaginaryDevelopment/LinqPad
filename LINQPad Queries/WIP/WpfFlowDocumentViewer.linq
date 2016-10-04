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
// things attempted:
    // runtime exception in http://stackoverflow.com/questions/1267046/wpf-flowdocument-scale-to-fit-page (attempt commented out in previous commit)

//http://stackoverflow.com/questions/910814/loading-xaml-at-runtime
//https://blogs.msdn.microsoft.com/ashish/2007/08/14/dynamically-loading-xaml/

open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup

[<AutoOpen>]
module Helpers = 
    let dumpt (t:string) x = x.Dump(t); x
    let flip f y x = f x y
    let getType x = x.GetType()
    let regReplace (delimiter:string) (replacement:string) (text:string) = Regex.Replace(text,delimiter,replacement)
    let regRemove x t = regReplace x String.Empty t
    
    let replace (delimiter:string) (replacement:string) (text:string) = text.Replace(delimiter,replacement)
    let after (delimiter:string) (x:string) =  
            match x.IndexOf delimiter with
            | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
            | i -> x.Substring(i + delimiter.Length)
    let before (delimiter:string) (x:string) = x.Substring(0, x.IndexOf delimiter)
    let remove d (s:string) = s |> before d |> flip (+) (s|> after d)
    let splitLines (x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    module Tuple2 = 
        let fromCurry x y = (x,y)
    
    let getLine i s = s|> splitLines |> Seq.mapi Tuple2.fromCurry |> Seq.find(fun (lineIndex,l) -> lineIndex = i)
    let getLines indexes s = s|> splitLines |> Seq.mapi Tuple2.fromCurry |> Seq.filter (fun (lineIndex,_) -> indexes |> Seq.contains lineIndex)
    
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
        
    let ``wrapPaginator8x6x.5`` (dps:IDocumentPaginatorSource) = 
        DocumentPaginatorWrapper(dps.DocumentPaginator, Size(768., 676.), Size(48.,48.))
    
module Packaging = 
    open System.IO.Packaging
    open System.Windows.Documents
    open System.Windows.Xps.Serialization
    let saveAsXps fileNameWithoutExtension (dps:IDocumentPaginatorSource) = 
        use container = Package.Open(fileNameWithoutExtension + ".xps", FileMode.Create)
        use xpsDoc = new XpsDocument(container, CompressionOption.Maximum)
        use xpsPp = new XpsPackagingPolicy(xpsDoc)
        use rsm = new XpsSerializationManager(xpsPp, false)
        rsm.SaveAsXaml dps

let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\PatientFaceSheet.xaml"

let rootElement = 
    
    let text = 
        File.ReadAllText path
        |> regRemove "x:Class=\".+\"" 
        // replace behaviors namespace,static resources
        |> regRemove "xmlns:b=\".+\"" 
        |> regRemove "<b:.+>" 
        // replace converters namespace, static resources
        |> regRemove "xmlns:converter=\".+\""
        |> regRemove "<converter:.+>"
        // remove converter calls
        |> regRemove ",\s*Converter={StaticResource.*?}"
        |> regRemove "Converter=\"{StaticResource.*?}\""
        |> remove ", Converter={x:Static local:PatientFaceSheet.PhoneFormattingConverter}"
    Util.OnDemand("raw xaml", fun () -> text).Dump() |> ignore
    XDocument.Parse(text).DumpFormatted() |> ignore
    try
        XamlReader.Parse text
    with ex ->
        if ex.Message.Contains("Line number '") then
            let lineNumber = ex.Message |> after "Line number '" |> before "'" |> int
            text
            |> getLines [lineNumber-1 .. lineNumber + 1]
            |> dumpt (sprintf "errant line:%i" lineNumber)
            |> ignore
        reraise()

let flow = rootElement :?> System.Windows.Documents.FlowDocument

type DisplayStrategy = // consider adding each of these: https://wpf.2000things.com/2011/03/23/254-types-of-containers-for-hosting-flowdocument/
    | FlowGridDocScrollViewer
    | FlowDocScrollViewerRaw
    | FlowDocReader
    | SaveParse
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
        |FlowDocReader ->
            let fdr = FlowDocumentReader()
            fdr.Document <- flow
            Window(Content = fdr)
        |SaveParse ->
            flow
            |> XamlWriter.Save
            |> XamlReader.Parse
            |> fun x -> x :?> Documents.FlowDocument
            |> fun flow -> Window(Content = FlowDocumentScrollViewer(Document=flow))
            
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
        |ScaledFlowDocument -> 
            // trying https://blogs.msdn.microsoft.com/fyuan/2007/03/10/convert-xaml-flow-document-to-xps-with-style-multiple-page-page-size-header-margin/
            // consider trying http://stackoverflow.com/questions/7931961/wpf-printing-to-fit-page
//            flow.PageWidth <- 100.
//            flow.PageHeight <- 1000.
//            flow.PagePadding <- Thickness(0.5)
            let fdr = FlowDocumentReader()
            fdr.Document <- flow
            Packaging.saveAsXps @"C:\Users\Brandon\Documents\XAML Documents\scaledFlow1" fdr.Document
            Window(Content = flow)

    window.DataContext <- vm
    window
type FacilityInfo = {FacilityName:string; FacilityAddress1:string; FacilityAddress2:string; FacilityPhone:string;FacilityFax:string}
type PatientInfo = {PatientID: int;FirstName:string;MiddleInitial:string;LastName:string;DOB:DateTime;Age:string; Address1:string; City:string; State:string; Zip:string; MaritalStatus:string; Gender:string;PrimaryPhone:string;SecondaryPhone:string;EmergencyContactLastName:string;EmergencyContactFirstName:string;EmergencyContactPhone:string; EmailAddress:string;LastAppointment: DateTime Nullable}
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
    let vm = {deserialized with Patient = {deserialized.Patient with LastAppointment = Option.toNullable deserialized.LastVisit}}
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
    let window = displayFlow vm DisplayStrategy.FlowDocScrollViewerRaw
    let closeOnEscape (w:Window) = 
        w.KeyDown.Add (fun e -> 
            if e.Key = Key.Escape then
                w.Close()
        )
    closeOnEscape window
//    window.KeyDown.Add(fun k -> 
//        if k.Key = Key.Escape then
//            window.Close())
    
    //// show to query via code, then hide and show dialog to interact and view
    //window.Show()
    //
    //window.Hide()
    
    window.ShowDialog()
    |> ignore<bool Nullable>

displayForShow()
//consider adding printing/print preview to this script
// http://stackoverflow.com/questions/4263204/wpf-printing-flowdocument-not-centering