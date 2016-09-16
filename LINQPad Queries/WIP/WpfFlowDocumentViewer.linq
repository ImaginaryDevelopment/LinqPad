<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
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


let rootElement = 
    use s = new FileStream(@"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\PatientDataGrid\PatientFaceSheet.xaml", FileMode.Open)
    XamlReader.Load s

let flow = rootElement :?> System.Windows.Documents.FlowDocument

type DisplayStrategy = 
    | WindowDirect
    | WindowContentWrapped
    | FlowGridDocScrollViewer
    | FlowDocScrollViewerRaw
    
//let inline toFormatString (f:string) (a:^a) = ( ^a : (member ToString:string -> string) (a,f))
let inline showDialog x = (^a: (member ShowDialog:unit -> bool Nullable) x)

let displayFlow vm ds=
    let window = 
        match ds with
        // doesn't seem to display correctly
        | WindowDirect -> Window(Content = flow)
        // doesn't seem to display correctly
        | WindowContentWrapped ->
            let container = ContentControl(Content=flow,HorizontalContentAlignment = HorizontalAlignment.Center, HorizontalAlignment=HorizontalAlignment.Center)
            Window(Content=container,Title="Hello aunt flow")
        // yay centering
        |FlowGridDocScrollViewer ->
            let fdsv = FlowDocumentScrollViewer(Document = flow)
            let grid = Grid()
            grid.Children.Add(fdsv) |> ignore
            Window(Content=grid)
        |FlowDocScrollViewerRaw ->
            let fdsv = FlowDocumentScrollViewer(Document = flow)
            Window(Content=fdsv)
    window.DataContext <- vm
    window
type FacilityInfo = {FacilityName:string; FacilityAddress1:string; FacilityAddress2:string; FacilityPhone:string;FacilityFax:string}
type PatientInfo = {PatientID: int;FirstName:string;MiddleInitial:string;LastName:string;DOB:DateTime;Age:string; Address1:string}
type ViewModel =  {Facility:FacilityInfo;Patient:PatientInfo;Generated:DateTime}

let vm = 
    let facilityInfo = {FacilityName="Qualified Emergency Group, PA"; FacilityAddress1 = "11048-9 Baymeadows Road"; FacilityAddress2 = "Jacksonville, FL 32256"; FacilityPhone ="(904) 954-7911"; FacilityFax= "(904) 111-1111"} // "Phone: (904) 854-7911 | Fax: (904) 111-1111"}
    let patientInfo = {PatientID=2530; FirstName="Heather"; MiddleInitial="M"; LastName="Hutto"; DOB=DateTime(1977,10,19);Age="38yr";Address1="1832 Jefferson Rd"}
    {Facility = facilityInfo;Patient= patientInfo; Generated = DateTime.Now}
    
type Listener() = 
    inherit TraceListener("DataBindingErrorListener")
    override x.Write (msg:string) = printf "%s" msg
    override x.WriteLine (msg:string) = printfn "%s" msg
    
// sometimes this works, sometimes it doesn't
let listenForBindingErrors() = 
    let alreadyAdded = PresentationTraceSources.DataBindingSource.Listeners |> Seq.cast<TraceListener> |> Seq.exists (getType >> (=) typeof<Listener>)
    if not alreadyAdded then
        new Listener()
        |> PresentationTraceSources.DataBindingSource.Listeners.Add
        |> Some
    else
        printfn "Trace listener already attached"
        None
        
listenForBindingErrors() |> ignore

flow.DataContext <- vm

let window = displayFlow vm FlowDocScrollViewerRaw

window.KeyDown.Add(fun k -> 
    if k.Key = Key.Escape then
        window.Close())

//// show to query via code, then hide and show dialog to interact and view
//window.Show()
//
//window.Hide()

window.ShowDialog()
|> ignore<bool Nullable>
//consider adding printing/print preview to this script
// http://stackoverflow.com/questions/4263204/wpf-printing-flowdocument-not-centering