<Query Kind="FSharpProgram">
  <Reference>C:\tfs\practicemanagement\trunk\bin\Newtonsoft.Json.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Dal.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Domain.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Schema.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\PracticeManagement.exe</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\PracticeManagement.Foundation.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Windows.Presentation.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>global.Schema</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Pm.Dal.DataModels</Namespace>
  <Namespace>Pm.Schema</Namespace>
  <Namespace>Pm.Schema.DataModels</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
</Query>

// walks the entire flow document (possible after it has been shown, once that worked, stopped looking for other ways)

let (|ValueString|_|) =
    function
    | null | "" -> None
    | x when String.IsNullOrWhiteSpace x -> None
    | x -> Some x
let trim (x:string) = x.Trim()  
let replace (delim:string) (r:string) (x:string) = x.Replace(delim,r)
let deserialize<'T> x = JsonConvert.DeserializeObject<'T>(x)
let serialize (x:obj) =
    let jss = JsonSerializerSettings(NullValueHandling=NullValueHandling.Ignore)
    JsonConvert.SerializeObject(x,Formatting.Indented,jss)

let makePhone last4 =
    sprintf "904-555-%s" last4

let facility: Pm.Dal.DataModels.FacilityDataModel = FacilityDataModel()
facility.FacilityAddress1 <- "6622 Southpoint Drive South, Suite 370"
facility.FacilityCity <- "Jacksonville"
facility.FacilityState <- "FL"
facility.FacilityZip <- "32216"
let appt =
    ApptPtDM<Pm.Dal.DataModels.PatientDataModel>(
        AppointmentID = 133<_>
    )
appt.PresentingCondition <- "Flu"
appt.AppointmentStartTime <- DateTime.Now
appt.AppointmentCheckInTime <- Nullable DateTime.Now
appt.Patient.DOB <- Nullable <| DateTime.Today.AddYears 1
appt.Patient.PatientID <- 318<_>
appt.AppointmentPatientID <- 318<_>
appt.Patient.PCP <- 939
appt.Patient.PrimaryPhone <- makePhone "1234"
appt.Patient.EmergencyContactLastName <- "Espinosa"
appt.Patient.EmergencyContactFirstName <- "Carla"
appt.Patient.EmergencyContactPrimaryPhone <- makePhone "4321"

let prov = Pm.Dal.DataModels.ProviderDataModel()
let phone = "904-296-1189"
    
let pcp = 
    PracticeManagement.Foundation.DataModels.PcpDataModel(
        DisplayName = "Last, First MI Title",
        PcpSpeciality = "PCPSpeciality",
        PcpPhone = makePhone "fone",
        PcpFax = makePhone "1fax",
        PracticeName = "Practice Name",
        PcpCity = "City",
        PcpState = "FL",
        PcpZip = "12345"
        
    )

let primaryPayer = "Dr. Elliot Reid"
let visitType = "VisitType"
module FlowManipulation =
    open System.Windows
    open System.Windows.Controls.Primitives
    open System.Windows.Documents
    open System.Windows.Media
    let getObjTypeName (x:obj) =
        let t = x.GetType()
        sprintf "%s-%s" t.Name t.FullName
      
    let andChildren f x=
        seq{
            yield x
            yield! f x
        }
        
    // translated from http://www.nullskull.com/a/1446/wpf-customized-find-control-for-flowdocuments.aspx
    let rec getLogicalChildren(o: DependencyObject) =
        LogicalTreeHelper.GetChildren o
        |> Seq.cast<obj>
        |> Seq.choose (
            function
            | :? DependencyObject as o -> Some o
            | _ -> None
        )
        |> Seq.collect(fun x ->
            seq{
                yield x
                yield! getLogicalChildren x
            }
            
        )
    let getVisualChildren (o:DependencyObject) =
        [0..VisualTreeHelper.GetChildrenCount o - 1]
        |> Seq.map(fun i -> VisualTreeHelper.GetChild(o,i))
        
    let getNames(x:DependencyObject) =
        seq{
            yield x.GetValue(FrameworkElement.NameProperty) :?> string
            match x with
            | :? Paragraph as p -> yield p.Name
            | :? InlineUIContainer as c -> yield c.Name
            | :? FrameworkElement as fe ->
                yield fe.Name
            | _ -> failwithf "unexpected type %s" <| getObjTypeName x
            match x.GetType().GetProperty("Name", BindingFlags.Instance|||BindingFlags.Public|||BindingFlags.NonPublic) with
            | null -> ()
            | pi ->
//                printfn "Found a name property! on %s" <| getObjTypeName x
                yield pi.GetValue(x) :?> string
        }
        |> Seq.choose (|ValueString|_|)
        |> List.ofSeq
        |> fun names -> getObjTypeName x, names
        
    // yield all child elements, walk hierarchy
    let rec getElements (x:obj) :DependencyObject seq =
        match x with
        // items above DependencyObject throw exceptions
//        | :? FlowDocument as fd -> fd.Blocks |> Seq.cast<Block> |> Seq.collect(box >> andChildren getElements)
//        | :? InlineUIContainer as i ->  i.Child |> box |> andChildren getElements
//        | :? Paragraph as p -> p.Inlines |> Seq.cast<obj> |> Seq.collect(andChildren getElements)
        | :? DependencyObject as x -> 
            seq{
                for l in getLogicalChildren x do
                    yield l
                    yield! getElements l
                let maybe = 
                    match x with
                    | :? Visual as v -> Some x
        //                    | :? Visual3D as v -> Some <| box v
                    | _ -> None
                match maybe with
                | Some x ->
                    yield x
                    for v in getVisualChildren x do
                        yield! getElements v
                | None -> ()
            }
//        | :? BlockUIContainer as c -> seq { yield box c.Child;yield! getElements c.Child}
//        | :? Border as b -> b.Child |> box |> andChildren getElements
//        | :? Grid as g -> g.Children |> box |> andChildren getElements
//        | :? UIElementCollection as ui -> ui|> Seq.cast<UIElement> |> Seq.collect(box >> andChildren getElements)
//        | :? UniformGrid as ui -> ui.Children |> Seq.cast<obj> |> Seq.collect(box>>andChildren getElements)
//        | :? Control as c -> Seq.empty
        | _ ->
            let t = x.GetType()
            (t.Name,t.FullName).Dump("Unhandled")
            invalidOp "match cases incomplete"
        
    // try to account for logical and visual possibly returning the same thing
//    let getDistinctElements       


open FlowManipulation

let doc = PracticeManagement.Billing.ConsentFormFlowDocument(facility,appt,appt.Patient,prov,pcp,primaryPayer, Nullable 12.25m, "CC", Nullable 1, visitType,null)


//let inline getLength s = (^a: (member Length: _) s)
let inline addChildren x c = (^a: (member Children:UIElementCollection) c).Add x |> ignore<int>
let inline getDocDisplay x =
    
    let w= System.Windows.Window()
    let c = 
//        let x = System.Windows.Controls.Grid()
        let x = DockPanel()
        w.Content <- x
        x
    let r = System.Windows.Controls.FlowDocumentReader()
    r.ViewingMode <- FlowDocumentReaderViewingMode.Scroll
    r.Document <- x
    c |> addChildren r
    w.Content <- c
    w
getDocDisplay doc
|> Dump
|> ignore
// find only seems to work after display
doc

|> getElements // "imgLogo"
|> List.ofSeq
|> fun (items:_ list) ->
    printfn "Found %i controls" items.Length
    items
//|>  List.choose (getNames>>(fun (n,names) -> if names |> List.exists(fun _ -> true) then Some (n,names) else None))
|> Seq.ofType<Image>
|> List.ofSeq
|> function
    | [] -> ()
    | images ->
        let logo = System.Windows.Media.Imaging.BitmapImage()
        logo.BeginInit()
        logo.UriSource <- new Uri(@"file://C:\Users\bdimp\Downloads\logo.png")
        logo.EndInit()
        images
        |> List.iter(fun i ->
            i.Source <- logo
        )
|> dumpt "imgLogo?"
|> ignore
//