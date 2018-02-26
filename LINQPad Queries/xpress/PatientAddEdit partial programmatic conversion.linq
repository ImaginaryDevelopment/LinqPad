<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\PresentationCore.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\PresentationFramework.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Windows.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Xaml.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\WindowsBase.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Data</Namespace>
</Query>

// was made to figure out what was going on in live app, turns out StackPanel ignores Horizontal Stretch in children
[<Serializable>]
type MyMarshal() =
    inherit MarshalByRefObject()
    let ka = Util.KeepRunning()
    member __.Dispose() = ka.Dispose()
    member __.Print s = printfn "%s" s
type IChild =
    abstract member Initialize : MyMarshal -> unit
let parseGridLength txt = 
    match txt with
    | null | "" -> None
    | "Auto" -> 
        printfn"Making an Auto"
        GridLength(0.,GridUnitType.Auto) |> Some
        
    | "*" -> 
        printfn"Making a Star"
        GridLength(1.,GridUnitType.Star) |> Some
    | txt -> 
        printfn "Parsing txt"
        let isStar = txt.EndsWith "*" 
        let n,gt = 
            if isStar then txt.[0..txt.Length - 2] |> float, GridUnitType.Star else float txt, GridUnitType.Pixel
        GridLength(n, gt) |> Some
let makeRd (txt:string) = 
    match parseGridLength txt with
    | None -> RowDefinition()
    | Some gl -> RowDefinition(Height=gl)
let makeCd (mm:MyMarshal) = 
    parseGridLength
    >> function
    | None -> 
//        mm.Print "Making a cd"
        ColumnDefinition()
    | Some gl -> 
//        mm.Print <| sprintf "making a width cd: %A" gl
        ColumnDefinition(Width = gl)
    
let addGridItem (g:Grid) row column (item:UIElement) =
    let rowSafe = row <= g.RowDefinitions.Count - 1  
    let columnSafe = column <= g.ColumnDefinitions.Count - 1
    let baseErrMsg = "requested to add item to grid"
    match rowSafe,columnSafe, g.RowDefinitions.Count - 1, g.ColumnDefinitions.Count - 1 with
    | false, true, maxRow,_ -> failwithf "%s with bad row index %i (max is %i)" baseErrMsg row maxRow
    | true,false, _, maxC -> failwithf "%s with bad column index %i (max is %i)" baseErrMsg column maxC
    | false, false, maxR, maxC -> failwithf "%s with bad row and column index %i %i (max %i %i)" baseErrMsg row column maxR maxC
    | true, true, _,_ -> 
        item.SetValue(Grid.RowProperty, row)
        g.Children.Add item |> ignore<int>
    
let makeBordered item = 
    Border(Child=item, BorderThickness =Thickness(1.), BorderBrush=System.Windows.Media.SolidColorBrush(Color = System.Windows.Media.Colors.Blue))
let makeGrid(mm:MyMarshal) = 
 
    mm.Print "making grid"
    let grid = // Patient Information Grid
    
        let grid = Grid(HorizontalAlignment=HorizontalAlignment.Stretch)
        [ 
         "32"
         "2"
         "34"
         "34*"
         "34*"
         "34*"
         "34*"
         "34*"
         "35"
        ]
        |> Seq.map makeRd
        |> Seq.iter grid.RowDefinitions.Add
        mm.Print "finished making row defs"
        [
            "Auto"
            "*"
            "Auto"
            "*"
            "Auto"
            "*"
        ]
        |> Seq.map (makeCd mm)
        |> Seq.iter grid.ColumnDefinitions.Add
        mm.Print "finished making col defs"
        let widthProxyName = "lblWidthProxy"
        let lblWidthProxy =  Label(Name=widthProxyName, HorizontalAlignment = HorizontalAlignment.Stretch, MinWidth=200.)
        addGridItem grid 0 5 (
            lblWidthProxy
        )
        addGridItem grid 2 5 (
            TextBox(Text="txtPatientLastName")
        )
        addGridItem grid 3 5
            (
                let outerSp = // Grid.Row = 3 Grid.Column = 5
                    let sp = StackPanel(Orientation=Orientation.Horizontal)
                    let txtSsn = TextBox(HorizontalAlignment=HorizontalAlignment.Stretch, Text="txtSsn")
                    sp.Children.Add txtSsn |> ignore<int>
                    let innerSp = 
                        let sp = StackPanel()
                        let cbItin = CheckBox(Name="ITIN_CheckBox", Content="IsITIN", FontSize=8.)
                        sp.Children.Add cbItin |> ignore<int>
                        sp
                    sp.Children.Add innerSp |> ignore<int>
                    makeBordered sp
                let b = Binding("Width", Source= lblWidthProxy)
                outerSp.SetBinding(StackPanel.WidthProperty,b) |> ignore
                makeBordered outerSp
            )
            
        grid
    grid
type MyTraceListener(mm:MyMarshal) = 
    inherit TraceListener()
    override __.Write msg = mm.Print msg
    override __.WriteLine msg = mm.Print msg
        

    
let startApp (mm:MyMarshal) () = 
    let a = System.Windows.Application()
    a.Startup.Add(fun _ -> 
        Debug.Listeners.Add(new MyTraceListener(mm)) |> ignore<int>
        Trace.Listeners.Add(new MyTraceListener(mm)) |> ignore<int>
        Trace.AutoFlush <- true
        Debug.AutoFlush <- true
        if not Debugger.IsAttached then
            PresentationTraceSources.Refresh()
        PresentationTraceSources.DataBindingSource.Listeners.Add(new MyTraceListener(mm)) |> ignore<int>
        PresentationTraceSources.DataBindingSource.Switch.Level <- SourceLevels.Error
        PresentationTraceSources.DataBindingSource.Flush()
        ()
    )
    let w = Window()
    w.Content <- makeBordered <| makeGrid mm
    mm.Print "I'm in a new app domain!"
    mm.Print <| sprintf "App domain is %s" AppDomain.CurrentDomain.FriendlyName
    a.Run(w)
    |> ignore<int>
    mm.Print "Finished"
    
[<Serializable>]
type Child() =
    inherit MarshalByRefObject()
    let mutable mm = None
    member __.Initialize (mmToSet:MyMarshal) = 
        mm <- Some mmToSet 
        startApp (mmToSet) ()
    interface IChild with
        member x.Initialize( mm) = x.Initialize(mm)
let makeAddHandlerDisposable fAdd fRemove fDelegate =
    fAdd fDelegate
    { new IDisposable with
        member __.Dispose() = 
            fRemove fDelegate
    }
let assm = lazy(System.Reflection.Assembly.GetExecutingAssembly())

let ownDomain f =
    let ad = Util.CreateAppDomain("WpfTest", AppDomain.CurrentDomain.Evidence, AppDomain.CurrentDomain.SetupInformation)
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.iter(fun m ->
        try
            ad.Load (m.FullName) |> ignore
        with ex ->
            printfn "Failing to load %s" m.FullName
//            ex.Dump()
    )
    
    let mm = MyMarshal()
    mm.Print "Testing mm"
    let child = 
        try
            ad.CreateInstanceFromAndUnwrap(assm.Value.CodeBase, typeof<Child>.FullName)  :?> IChild
        with _ -> 
            reraise() 
    printfn "made a baby. IsNull? %A" (isNull (box child))
    child.Initialize(mm)
    printfn "Initialized"
    try
        printfn "Finished app, closing"
        AppDomain.Unload ad
        mm.Dispose()
    with ex ->
        printfn "exceptional"
        mm.Dispose()
        reraise()
    printfn "Moving on with life"
    
//StartApp()
ownDomain startApp

