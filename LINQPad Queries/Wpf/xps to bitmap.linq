<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\ReachFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Printing.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>System.Windows.Documents</Namespace>
  <Namespace>System.Windows.Xps</Namespace>
  <Namespace>System.Windows.Xps.Packaging</Namespace>
</Query>

// xps to high res image:
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media.Imaging
module NativeMethods =
    open System.Runtime.InteropServices
    open System.Security
    [<DllImport("gdi32.dll")>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private DeleteObject(nativeint hObject)
    let deleteObject x = DeleteObject x
    
    // https://stackoverflow.com/questions/94456/load-a-wpf-bitmapimage-from-a-system-drawing-bitmap
    // https://stackoverflow.com/a/7035036/57883
    type SafeHBitmapHandle [<SecurityCritical>](prexistingHandle:nativeint,ownsHandle) as this =
        inherit Microsoft.Win32.SafeHandles.SafeHandleZeroOrMinusOneIsInvalid(ownsHandle)
        do
            this.SetHandle prexistingHandle
        override this.ReleaseHandle() = deleteObject this.handle
        interface IDisposable with
            member x.Dispose() = x.ReleaseHandle() |> ignore
        
    
type ident = string
type LeakCheckStream<'t when 't :> Stream>(identifier:ident,wrapped:'t) =
    inherit Stream()
    let ident2 = Guid.NewGuid()
    let mutable isDisposed = false
    do printfn "Creating stream %s, %A" identifier ident2
    override __.CanRead with get() = wrapped.CanRead
    override __.CanSeek with get() = wrapped.CanSeek
    override __.CanTimeout with get() = wrapped.CanTimeout
    override __.CanWrite with get() = wrapped.CanWrite
    override __.Length with get() = wrapped.Length
    override __.Position with get() = wrapped.Position and set v = wrapped.Position <-v
    override __.ReadTimeout with get() = wrapped.ReadTimeout and set v = wrapped.ReadTimeout <-v
    override __.WriteTimeout with get() = wrapped.WriteTimeout and set v = wrapped.WriteTimeout <-v
    member __.CopyToAsync(destination:Stream) = wrapped.CopyToAsync (destination)
    member __.CopyToAsync(destination:Stream,bufferSize:int32) = wrapped.CopyToAsync (destination,bufferSize)
    override __.CopyToAsync(destination:Stream,bufferSize:int32,cancellationToken:CancellationToken) = wrapped.CopyToAsync (destination,bufferSize,cancellationToken)
    member __.CopyTo(destination:Stream) = wrapped.CopyTo (destination)
    member __.CopyTo(destination:Stream,bufferSize:int32) = wrapped.CopyTo (destination,bufferSize)
    override __.Close() = wrapped.Close ()
    override __.Flush() = wrapped.Flush ()
    member __.FlushAsync() = wrapped.FlushAsync ()
    override __.FlushAsync(cancellationToken:CancellationToken) = wrapped.FlushAsync (cancellationToken)
    override __.BeginRead(buffer,offset,count,callback,state) = wrapped.BeginRead (buffer,offset,count,callback,state)
    override __.EndRead(asyncResult) = wrapped.EndRead asyncResult
    member __.ReadAsync(buffer:byte[],offset:int32,count:int32) = wrapped.ReadAsync (buffer,offset,count)
    override __.ReadAsync(buffer:byte[],offset:int32,count:int32,cancellationToken:CancellationToken) = wrapped.ReadAsync (buffer,offset,count,cancellationToken)
    override __.BeginWrite(buffer,offset,count,callback,state) = wrapped.BeginWrite (buffer,offset,count,callback,state)
    override __.EndWrite(asyncResult) = wrapped.EndWrite asyncResult
    member __.WriteAsync(buffer:byte[],offset:int32,count:int32) = wrapped.WriteAsync (buffer,offset,count)
    override __.WriteAsync(buffer:byte[],offset:int32,count:int32,cancellationToken:CancellationToken) = wrapped.WriteAsync (buffer,offset,count,cancellationToken)
    override __.Seek(offset,origin) = wrapped.Seek (offset,origin)
    override __.SetLength(value) = wrapped.SetLength value
    override __.Read(buffer,offset,count) = wrapped.Read (buffer,offset,count)
    override __.ReadByte() = wrapped.ReadByte ()
    override __.Write(buffer,offset,count) = wrapped.Write (buffer,offset,count)
    override __.WriteByte(value) = wrapped.WriteByte value
    override __.InitializeLifetimeService() = wrapped.InitializeLifetimeService ()
    override __.CreateObjRef(requestedType) = wrapped.CreateObjRef requestedType
    member __.Stream = wrapped
    
    interface IDisposable with
        member x.Dispose() =
            printfn "Disposing a stream %s %A" identifier ident2
            isDisposed <- true
            wrapped.Dispose()
    override x.Finalize() =
        if not <| isDisposed then
            eprintfn "Item undisposed! %s %A" identifier ident2
        else printfn "I was disposed! %s %A" identifier ident2
        base.Finalize()
    
type DisposeLink<'t>(underlyingDisposable:IDisposable,target:'t) =
    member __.Value = target
    member __.Dispose() =
        // Disposals should never throw
        let tryDispose (x:IDisposable) =
            try
                x.Dispose()
            with ex -> eprintfn "%s" ex.Message
        match box target with
        | :? IDisposable as disp -> tryDispose disp
        | _ -> ()
        tryDispose underlyingDisposable
    interface IDisposable with
        member x.Dispose() = x.Dispose()
module DisposeLink =
    let disposeLink<'t> target underlier =
        new DisposeLink<'t>(underlier,target)
        
    let map f (dl:DisposeLink<_>) =
        let next = f dl.Value
        disposeLink next dl
    
module Xps =
    let loadXps path f =
        use doc = new XpsDocument(path,FileAccess.ReadWrite)
        f doc
    let toFds (xps:XpsDocument) =
        let fds = xps.GetFixedDocumentSequence()
        fds
    // scaling: https://stackoverflow.com/questions/13144615/rendertargetbitmap-renders-image-of-a-wrong-size
    let scaleRender pgW pgH scale =
        let maxWidth = Math.Round(21.0 / 2.54 * 96.0); // A4 width in pixels at 96 dpi
        let maxHeight = Math.Round(29.7 / 2.54 * 96.0); // A4 height in pixels at 96 dpi
//        double scale = 1.0;
        let scale = min scale (maxWidth / pgW)
        let scale = max scale (maxHeight / pgH)
        let cv = ContainerVisual(Transform = ScaleTransform(scale,scale))
        cv.Children.Add(page.Visual);

        let w = pgW * scale |> int 
        let h = pgH * scale |> int
        let rt = new RenderTargetBitmap( w,h,96, 96, PixelFormats.Default)
        rt

    let asBitmapStream (xps:XpsDocument) =
        let fds = xps.GetFixedDocumentSequence()
        [0..fds.DocumentPaginator.PageCount - 1]
        |> Seq.map(fun i ->
//            let docPage = fds.References.[i]
            let docPage = fds.DocumentPaginator.GetPage i
//            let rt = RenderTargetBitmap(docPage.Size.Width, docPage.Size.Height, 96.0,96.0, System.Windows.Media.PixelFormats.Bgra32)
            let sz = docPage.Size
            let w,h =int sz.Width,int sz.Height
            (fds.DocumentPaginator.PageCount,i,w,h).Dump()
            // fix this: https://stackoverflow.com/questions/13144615/rendertargetbitmap-renders-image-of-a-wrong-size
            let rt = RenderTargetBitmap(w,h, 100.0,100.0, System.Windows.Media.PixelFormats.Default)
            rt.Render(docPage.Visual)
            let encoder = BmpBitmapEncoder()
            encoder.Frames.Add(BitmapFrame.Create rt)
            // unfortunately some recipients need to keep the stream open
            let stream = new LeakCheckStream<_>(sprintf "Page %i" i, new MemoryStream())
            encoder.Save stream
            stream
        )
    let getBitmaps(xps:XpsDocument) =
        asBitmapStream xps 
        |> Seq.map (fun x ->
            let bm = new System.Drawing.Bitmap(x)
            bm.PixelFormat.Dump()
            new DisposeLink<_>(x,bm)
        )
    let toWpfImage(bi:BitmapSource) =
        let i = Image()
        i.Source <- bi
        i.Dump()
    let getBitmapImages(xps:XpsDocument) =
        xps
        |> getBitmaps
        |> Seq.map(fun x ->
            use hBitmap = new NativeMethods.SafeHBitmapHandle(x.Value.GetHbitmap(),true)
            let next = System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(hBitmap.DangerousGetHandle(),IntPtr.Zero, Int32Rect.Empty, BitmapSizeOptions.FromEmptyOptions())
            new DisposeLink<_>(x,next)
            // alternative option at https://social.msdn.microsoft.com/Forums/vstudio/en-US/87871d07-1ee0-4cec-830f-71beb53b2e17/pixelformat-why-is-the-difference?forum=wpf
            )
module Drawing =
    open System.Drawing
    open System.Drawing.Imaging
    let getEncoderInfo mimeType =
        ImageCodecInfo.GetImageEncoders()
        |> Seq.tryFind(fun e ->
//            (e.MimeType, mimeType).Dump("mimes")
            e.MimeType = mimeType
        )
    let prepareEncoder isInitial =
        let items =
            [
                new EncoderParameter(Encoder.Compression, int64 EncoderValue.CompressionNone)
                new EncoderParameter(Encoder.SaveFlag, (if isInitial then EncoderValue.MultiFrame else EncoderValue.FrameDimensionPage) |> int64)
            ]
        let eps = new EncoderParameters(items.Length)
        items
        |> Seq.iteri(fun i ep ->
            eps.Param.[i] <- ep
        )
        eps
        
    // https://stackoverflow.com/questions/398388/convert-bitmaps-to-one-multipage-tiff-image-in-net-2-0
    let bitmapToTiff ident (bitmap:Bitmap) =
        let stream = new LeakCheckStream<_>(ident,new MemoryStream())
        bitmap.Save(stream, Imaging.ImageFormat.Tiff)
        let tiff = Image.FromStream(stream)
        tiff
        
        
        

    [<NoComparison>]
    type TiffSaveType =
        |TiffFile of string
        |TiffStream of Stream
    let bitmapsToTiff tst items =
        let mutable tiff = null
        use ep = prepareEncoder false
        items
        |> Seq.iter(fun bm ->
            let imaged = Image.FromStream(bm)
            if isNull tiff then
                let mime = "image/tiff"
                use ep = prepareEncoder true
                
                match getEncoderInfo mime with
                | None -> failwithf "Could not find encoder %s" mime
                | Some ei ->
                    tiff <- imaged :?> Bitmap
                    match tst with
                    | TiffFile filepath ->
                        if File.Exists filepath then
                            failwithf "File Exists at %s" filepath
                        tiff.Save(filepath,ei,ep)
                    | TiffStream str ->
                        tiff.Save(str,ei,ep)
            else
                tiff.SaveAdd(imaged,ep)
        )
        use ep = new EncoderParameters(1)
        ep.Param.[0] <- new EncoderParameter(Encoder.SaveFlag, int64 EncoderValue.Flush)
        tiff.SaveAdd ep
        
        
let printPreview (fds:FixedDocumentSequence)=
    let fdsv = System.Windows.Controls.DocumentViewer(Document=fds)
    let w = Window(Content=fdsv)
    let d = Util.KeepRunning()
    w.Closed.Add(fun _ ->
        d.Dispose()
    )
    w.Dump()
let previewXps path =
    Xps.loadXps path Xps.toFds
    |> printPreview 
//(
let dc = lazy(
    let dc = DumpContainer()
    dc.Dump()
    dc
    )
let processOne path =
    Xps.loadXps path Xps.getBitmaps
    
    |> Seq.mapi(fun i bm ->
        let target = Path.Combine(Path.GetTempPath(),Path.GetFileNameWithoutExtension path + ".tiff")
        let dl = bm |> DisposeLink.map(Drawing.bitmapToTiff(string i))
        dl.Dump()
        dl
        |> fun t -> t.Value.Save(target)
        printfn "Done saving"
        dc.Value.Content <- target
        Util.ReadLine(sprintf "Done with %s?" target) |> ignore<string>
        bm.Dispose()
        File.Delete target
        printfn "File deleted %s" target
        // this appears to show multiple streams that are not disposed when finalized
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        )
    |> Dump
    |> ignore
    
let processAll doc = // one doc per page
    let target = Path.Combine(Path.GetTempPath(),Path.GetFileNameWithoutExtension doc + ".tiff")
    Xps.loadXps doc Xps.asBitmapStream
    |> Drawing.bitmapsToTiff (Drawing.TiffSaveType.TiffFile target)
    target
    
Directory.EnumerateFiles(path,"*.xps")
|> Seq.iter(fun fn ->
    let target = processAll fn
    printfn "Done saving"
    dc.Value.Content <- target
    let response = Util.ReadLine(sprintf "Done with %s?" fn)
    File.Delete target
    response
    |> function
        |null | "" -> ()
        | x -> failwithf "Stop the presses"
)
//let print =    
//     PrintServer server = new LocalPrintServer();
//
//    using (PrintQueue printQueue = server.GetPrintQueue(settings.PrinterName))
//    {
//        var ticket = new PrintTicket();
//
//        XpsDocumentWriter writer = PrintQueue.CreateXpsDocumentWriter(printQueue);
//        DocumentPaginator paginator = fixedDocumentSequence.DocumentPaginator;
//        writer.Write(paginator, ticket);