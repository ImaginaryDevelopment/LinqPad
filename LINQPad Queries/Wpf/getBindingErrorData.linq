<Query Kind="FSharpProgram" />

// parse error log, find binding expression errors, format them

let split (d:string) (s:string) = s.Split([| d |], StringSplitOptions.None)
let trim (s:string) = s.Trim()

type BindingError = { PathError:string; ContextName:string; ContextType:string; BindingPath:string; TargetElement:string; TargetName:string; BoundProperty:string}
type BindingIssue = 
    | BindingError of BindingError
    | Other of string

let (|OtherBinding|BindingError|) (s:string) = 
    let m = Regex.Match(s, "path error: '(?<PathError>\w*)' property not found on '(?<ContextName>\w*)' ''(?<ContextType>\w*)' \(HashCode=\d+\)'\. BindingExpression:Path=(?<BindingPath>\w+).*;\s*DataItem='\w+'.* target element is '(?<TargetElement>\w+)' \(Name='(?<TargetName>\w*)'\); target property is '(?<BoundProperty>\w*)'")
    if m.Success then
        BindingError {
            PathError= m.Groups.["PathError"].Value
            ContextName=m.Groups.["ContextName"].Value
            ContextType=m.Groups.["ContextType"].Value
            BindingPath=m.Groups.["BindingPath"].Value
            TargetElement=m.Groups.["TargetElement"].Value
            TargetName=m.Groups.["TargetName"].Value
            BoundProperty= m.Groups.["BoundProperty"].Value}
    else OtherBinding s
let getErrors text = 
    let hashReg = "\(hash=(\d+)\)"
    let processError s = 
        match s with
        | OtherBinding b -> (Unchecked.defaultof<_>, s)
        | BindingError x -> (x, s) //BindingIssue.BindingError x
        
    
    let hashes = Regex.Matches(text, "\(hash=(\d+)\)") |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups.[1].Value) |> Set.ofSeq
    text
    |> split "\r\n"
    |> Seq.map trim
    |> Seq.filter (startsWith "System.Windows.Data")
    |> Seq.groupBy (fun t -> 
        let m = Regex.Match(t, hashReg)
        if m.Success then Some m.Groups.[1].Value else None)
    |> Seq.map (fun (k,items) -> k, items |> Seq.map processError |> List.ofSeq)
    //|> Seq.sortByDescending fst
let sampleErrorLogs =
    [
    """
    System.Windows.Data Error: 40 : BindingExpression path error: 'DefaultCulture' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=DefaultCulture; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadDateTimePicker' (Name=''); target property is 'Culture' (type 'CultureInfo')
System.Windows.Data Error: 40 : BindingExpression path error: 'AdmitFacilityID' property not found on 'object' ''ComboBoxItem' (Name='')'. BindingExpression:Path=AdmitFacilityID; DataItem='ComboBoxItem' (Name=''); target element is 'RadComboBox' (Name='rcbAdmitFacility'); target property is 'NoTarget' (type 'Object')
System.Windows.Data Error: 40 : BindingExpression path error: 'AdmitFacilityID' property not found on 'object' ''ComboBoxItem' (Name='')'. BindingExpression:Path=AdmitFacilityID; DataItem='ComboBoxItem' (Name=''); target element is 'RadComboBox' (Name='rcbAdmitFacility'); target property is 'NoTarget' (type 'Object')
System.Windows.Data Warning: 56 : Created BindingExpression (hash=9616996) for Binding (hash=16165537)
System.Windows.Data Warning: 58 :   Path: 'Occurrence.Appointment.ReferralPcp'
System.Windows.Data Warning: 62 : BindingExpression (hash=9616996): Attach to Telerik.Windows.Controls.RadComboBox.SelectedValue (hash=31333790)
System.Windows.Data Warning: 67 : BindingExpression (hash=9616996): Resolving source 
System.Windows.Data Warning: 70 : BindingExpression (hash=9616996): Found data context element: RadComboBox (hash=31333790) (OK)
System.Windows.Data Warning: 78 : BindingExpression (hash=9616996): Activate with root item AppointmentDialogViewModel (hash=37631263)
System.Windows.Data Warning: 107 : BindingExpression (hash=9616996):   At level 0 using cached accessor for AppointmentDialogViewModel.Occurrence: RuntimePropertyInfo(Occurrence)
System.Windows.Data Warning: 104 : BindingExpression (hash=9616996): Replace item at level 0 with AppointmentDialogViewModel (hash=37631263), using accessor RuntimePropertyInfo(Occurrence)
System.Windows.Data Warning: 101 : BindingExpression (hash=9616996): GetValue at level 0 from AppointmentDialogViewModel (hash=37631263) using RuntimePropertyInfo(Occurrence): Occurrence (hash=1142532974)
System.Windows.Data Warning: 108 : BindingExpression (hash=9616996):   At level 1 - for Occurrence.Appointment found accessor ReflectPropertyDescriptor(Appointment)
System.Windows.Data Warning: 104 : BindingExpression (hash=9616996): Replace item at level 1 with Occurrence (hash=1142532974), using accessor ReflectPropertyDescriptor(Appointment)
System.Windows.Data Warning: 101 : BindingExpression (hash=9616996): GetValue at level 1 from Occurrence (hash=1142532974) using ReflectPropertyDescriptor(Appointment): AppointmentDataModel (hash=28067317)
System.Windows.Data Warning: 108 : BindingExpression (hash=9616996):   At level 2 - for AppointmentDataModel.ReferralPcp found accessor RuntimePropertyInfo(ReferralPcp)
System.Windows.Data Warning: 104 : BindingExpression (hash=9616996): Replace item at level 2 with AppointmentDataModel (hash=28067317), using accessor RuntimePropertyInfo(ReferralPcp)
System.Windows.Data Warning: 101 : BindingExpression (hash=9616996): GetValue at level 2 from AppointmentDataModel (hash=28067317) using RuntimePropertyInfo(ReferralPcp): <null>
System.Windows.Data Warning: 80 : BindingExpression (hash=9616996): TransferValue - got raw value <null>
System.Windows.Data Warning: 84 : BindingExpression (hash=9616996): TransferValue - implicit converter produced <null>
System.Windows.Data Warning: 89 : BindingExpression (hash=9616996): TransferValue - using final value <null>
System.Windows.Data Error: 40 : BindingExpression path error: 'PcpID' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=PcpID; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadComboBox' (Name=''); target property is 'SelectedValuePath' (type 'String')
System.Windows.Data Warning: 56 : Created BindingExpression (hash=49322371) for Binding (hash=38849420)
System.Windows.Data Warning: 58 :   Path: 'DataContext.Pcps'
System.Windows.Data Warning: 60 : BindingExpression (hash=49322371): Default mode resolved to OneWay
System.Windows.Data Warning: 61 : BindingExpression (hash=49322371): Default update trigger resolved to PropertyChanged
System.Windows.Data Warning: 62 : BindingExpression (hash=49322371): Attach to System.Windows.Data.CollectionContainer.Collection (hash=33683578)
System.Windows.Data Warning: 67 : BindingExpression (hash=49322371): Resolving source 
System.Windows.Data Warning: 70 : BindingExpression (hash=49322371): Found data context element: <null> (OK)
System.Windows.Data Warning: 78 : BindingExpression (hash=49322371): Activate with root item FrameworkElement (hash=19618706)
System.Windows.Data Warning: 107 : BindingExpression (hash=49322371):   At level 0 using cached accessor for FrameworkElement.DataContext: DependencyProperty(DataContext)
System.Windows.Data Warning: 104 : BindingExpression (hash=49322371): Replace item at level 0 with FrameworkElement (hash=19618706), using accessor DependencyProperty(DataContext)
System.Windows.Data Warning: 101 : BindingExpression (hash=49322371): GetValue at level 0 from FrameworkElement (hash=19618706) using DependencyProperty(DataContext): SchedulerControlViewModel (hash=17326375)
System.Windows.Data Warning: 108 : BindingExpression (hash=49322371):   At level 1 - for SchedulerControlViewModel.Pcps found accessor RuntimePropertyInfo(Pcps)
System.Windows.Data Warning: 104 : BindingExpression (hash=49322371): Replace item at level 1 with SchedulerControlViewModel (hash=17326375), using accessor RuntimePropertyInfo(Pcps)
System.Windows.Data Warning: 101 : BindingExpression (hash=49322371): GetValue at level 1 from SchedulerControlViewModel (hash=17326375) using RuntimePropertyInfo(Pcps): ObservableCollection`1 (hash=14784198 Count=7)
System.Windows.Data Warning: 80 : BindingExpression (hash=49322371): TransferValue - got raw value ObservableCollection`1 (hash=14784198 Count=7)
System.Windows.Data Warning: 89 : BindingExpression (hash=49322371): TransferValue - using final value ObservableCollection`1 (hash=14784198 Count=7)
Exception thrown: 'System.Reflection.AmbiguousMatchException' in mscorlib.dll
System.Windows.Data Error: 40 : BindingExpression path error: 'Accidents' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=Accidents; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadComboBox' (Name=''); target property is 'ItemsSource' (type 'IEnumerable')
Exception thrown: 'System.Reflection.AmbiguousMatchException' in mscorlib.dll
System.Windows.Data Error: 40 : BindingExpression path error: 'States' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=States; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadComboBox' (Name=''); target property is 'ItemsSource' (type 'IEnumerable')
System.Windows.Data Error: 40 : BindingExpression path error: 'SelectedAppointment' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=SelectedAppointment.PropertyCasualtyClaimNumber; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'TextBox' (Name=''); target property is 'Text' (type 'String')
System.Windows.Data Error: 40 : BindingExpression path error: 'IsEditAppointment' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=IsEditAppointment; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'Label' (Name=''); target property is 'Visibility' (type 'Visibility')
System.Windows.Data Error: 40 : BindingExpression path error: 'Providers' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=Providers; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadComboBox' (Name=''); target property is 'ItemsSource' (type 'IEnumerable')
Exception thrown: 'System.Reflection.AmbiguousMatchException' in mscorlib.dll
System.Windows.Data Error: 40 : BindingExpression path error: 'IsEditAppointment' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=IsEditAppointment; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadComboBox' (Name='rcboProviders'); target property is 'Visibility' (type 'Visibility')
System.Windows.Data Error: 40 : BindingExpression path error: 'IsCreatePatientEnabled' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=IsCreatePatientEnabled; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadButton' (Name=''); target property is 'IsEnabled' (type 'Boolean')
System.Windows.Data Error: 40 : BindingExpression path error: 'IsInsuranceSelected' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=IsInsuranceSelected; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadButton' (Name=''); target property is 'IsEnabled' (type 'Boolean')
System.Windows.Data Error: 40 : BindingExpression path error: 'AppointmentPaymentCommand' property not found on 'object' ''AppointmentDialogViewModel' (HashCode=37631263)'. BindingExpression:Path=AppointmentPaymentCommand; DataItem='AppointmentDialogViewModel' (HashCode=37631263); target element is 'RadButton' (Name=''); target property is 'Command' (type 'ICommand')
System.Windows.Data Error: 4 : Cannot find source for binding with reference 'RelativeSource FindAncestor, AncestorType='System.Windows.Controls.ItemsControl', AncestorLevel='1''. BindingExpression:Path=HorizontalContentAlignment; DataItem=null; target element is 'ComboBoxItem' (Name=''); target property is 'HorizontalContentAlignment' (type 'HorizontalAlignment')
System.Windows.Data Error: 4 : Cannot find source for binding with reference 'RelativeSource FindAncestor, AncestorType='System.Windows.Controls.ItemsControl', AncestorLevel='1''. BindingExpression:Path=VerticalContentAlignment; DataItem=null; target element is 'ComboBoxItem' (Name=''); target property is 'VerticalContentAlignment' (type 'VerticalAlignment')
System.Windows.Data Error: 4 : Cannot find source for binding with reference 'RelativeSource FindAncestor, AncestorType='System.Windows.Controls.ItemsControl', AncestorLevel='1''. BindingExpression:Path=HorizontalContentAlignment; DataItem=null; target element is 'ComboBoxItem' (Name=''); target property is 'HorizontalContentAlignment' (type 'HorizontalAlignment')
System.Windows.Data Error: 4 : Cannot find source for binding with reference 'RelativeSource FindAncestor, AncestorType='System.Windows.Controls.ItemsControl', AncestorLevel='1''. BindingExpression:Path=VerticalContentAlignment; DataItem=null; target element is 'ComboBoxItem' (Name=''); target property is 'VerticalContentAlignment' (type 'VerticalAlignment')
"""
    ]
sampleErrorLogs
|> Seq.map getErrors
|> List.ofSeq
|> Dump