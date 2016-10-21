<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>System.Drawing</Namespace>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Media</Namespace>
</Query>

// measure screen distances
// perhaps also get colors? Complexity: peek through transparency layer possible?

        
            
let window = 
    
    let transparency = SolidColorBrush(Media.Color.FromArgb(90uy,100uy,100uy, 100uy))
    let window = 
        Window(
            AllowsTransparency= true,
            WindowStyle= WindowStyle.None,
            Background= transparency,
            WindowState= WindowState.Maximized
        )
    //window.WindowStartupLocation <- WindowStartupLocation....
    
    
    window

//window.
let captured = window.CaptureMouse()
captured.Dump("Captured?")
let sp = //,labelX,labelY = 
    let sp = StackPanel(Height=80., VerticalAlignment= VerticalAlignment.Top, Orientation=Orientation.Horizontal)
    sp.Background<- SolidColorBrush(Media.Color.FromRgb(255uy,255uy, 255uy))
    
//    let labelX = Label(Content = "Click1X:")
//    sp.Children.Add(labelX) |> Dump
//    let labelY = Label(Content = "Click1Y:")
//    sp.Children.Add(labelY) |> Dump
//    let labelWidth = Label(Content = "Width:")
//    sp,labelX,labelY
    sp
let addLabel (fContent: 't option -> string) = 
    let label = Label(Content = fContent None)
    sp.Children.Add(label) |> ignore<int>
    (fun x -> label.Content <- fContent x)
let valueStringOrEmpty = function |None -> null | Some x -> box x
let fLabelPos1 = addLabel (valueStringOrEmpty >> sprintf "Click1:%A")
let fLabelPos2 = addLabel (fun o -> sprintf "Click2:%A" o)
let fDistanceX = addLabel (fun o -> sprintf "DistanceX:%A" o)
let fDistanceY = addLabel (fun o -> sprintf "DistanceY:%A" o)
//let dc = 
//    let dc = DumpContainer()
//    dc.Dump()
//    dc
    
window.Content <- sp
window.KeyDown.Add(fun k -> 
        if k.Key = Input.Key.Escape then
            window.Close())
let mutable change1,pos1,pos2,(distanceX: Point option),(distanceY: Point option) = true, None, None, None, None
//let s = 
//    //window.MouseMove.Throttle(TimeSpan.FromSeconds 1.)
//    window.MouseMove.Sample(TimeSpan.FromSeconds 1.)
//    |> Observable.subscribe(fun _ -> 
//        dc.Content <- null
//        dc.Content <- (DateTime.Now,PInvoke.getPixelColor())
//        )
    
    
    
    

window.MouseLeftButtonUp.Add (fun e -> 
    let position = e.GetPosition(window)
    let getWpfControlPixelColor () = 
        let targetBitmap = Imaging.RenderTargetBitmap(int window.ActualWidth, int window.ActualHeight, 96., 96., PixelFormats.Default)
        targetBitmap.Render(window)
        if position.X <= float targetBitmap.PixelWidth && position.Y <= float targetBitmap.PixelHeight then
            //crop
            let cropped = Imaging.CroppedBitmap(targetBitmap, Int32Rect(int position.X, int position.Y,1,1))
            let pixels = Array.create 4 0uy
            cropped.CopyPixels(pixels, 4, 0)
            let color = Color.FromArgb(255uy, pixels.[0], pixels.[1], pixels.[2])
            (pixels,color).Dump("Color!")
            ()
    if change1 then
        pos1 <- Some position
        fLabelPos1 pos1
        change1 <- false
    else
        pos2 <- Some position
        fLabelPos2 pos2
        change1 <- true
    match pos1,pos2 with
    | Some p1, Some p2 -> 
        Math.Max(p1.X, p2.X) - Math.Min(p1.X, p2.X) |> Some |> fDistanceX
        Math.Max(p1.Y, p2.Y) - Math.Min(p1.Y, p2.Y) |> Some |> fDistanceY
    | _ -> fDistanceX None
    )
window.ShowDialog()
|> ignore