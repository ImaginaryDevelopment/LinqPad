<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
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
    window.DataContext <- vm
    window
type FacilityInfo = {FacilityName:string; FacilityAddress1:string; FacilityAddress2:string; FacilityPhone:string;FacilityFax:string}
type PatientInfo = {PatientID: int;FirstName:string;MiddleInitial:string;LastName:string;DOB:DateTime;Age:string; Address1:string; City:string; State:string; Zip:string; MaritalStatus:string; Gender:string;PrimaryPhone:string;SecondaryPhone:string;EmergencyContactLastName:string;EmergencyContactFirstName:string;EmergencyContactPhone:string}
type PatientSummary = {FirstName:string; LastName:string; MiddleInitial:string; DOB:DateTime; Gender:string; Age:string;} 
    with 
        static member fromPatientInfo (x:PatientInfo):PatientSummary = 
            //{FirstName=x.FirstName; LastName=x.LastName; MiddleInitial= }
            let cereal :PatientSummary = Cereal.serialize false x |> Cereal.deserialize
            cereal

type PayerInfo = {Name:string; PayerType:string; Subscriber:PatientSummary;}
type ViewModel =  {Facility:FacilityInfo;Patient:PatientInfo;Payers:PayerInfo list; Generated:DateTime; LastVisit:DateTime option}

let vm = 
    let savePath = Path.Combine (Path.GetDirectoryName path, sprintf "%s.vm.json" (Path.GetFileNameWithoutExtension path))
    let rawText = File.ReadAllText savePath
    let deserialized:ViewModel = Cereal.deserialize rawText
    // make any updates/property additions here
    // ....
    // 
    let vm = {deserialized with Payers = [ {Name="BCBS Florida"; PayerType="Primary"; Subscriber = PatientSummary.fromPatientInfo deserialized.Patient}]}
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