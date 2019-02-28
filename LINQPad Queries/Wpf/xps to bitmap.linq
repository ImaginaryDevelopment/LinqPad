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
    do printfn "Creating stream %s" identifier
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
            printfn "Disposing a stream %s" identifier
            wrapped.Dispose()
module Xps =
    let loadXps path f =
        use doc = new XpsDocument(path,FileAccess.ReadWrite)
        f doc
    let toFds (xps:XpsDocument) =
        let fds = xps.GetFixedDocumentSequence()
        fds
    let asBitmapStream (xps:XpsDocument) =
        let fds = xps.GetFixedDocumentSequence()
        [0..fds.DocumentPaginator.PageCount - 1]
        |> Seq.map(fun i ->
//            let docPage = fds.References.[i]
            let docPage = fds.DocumentPaginator.GetPage i
//            let bitmap = BitmapImage()
//            let rt = RenderTargetBitmap(docPage.Size.Width, docPage.Size.Height, 96.0,96.0, System.Windows.Media.PixelFormats.Bgra32)
            let sz = docPage.Size
            let w,h =int sz.Width,int sz.Height
            (fds.DocumentPaginator.PageCount,i,w,h).Dump()
            let rt = RenderTargetBitmap(w,h, 96.0,96.0, System.Windows.Media.PixelFormats.Default)
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
            bm
        )
    let toWpfImage(bi:BitmapSource) =
        let i = Image()
        i.Source <- bi
        i.Dump()
    let getBitmapImages(xps:XpsDocument) =
        xps
        |> getBitmaps
        |> Seq.map(fun x ->
            use hBitmap = NativeMethods.SafeHBitmapHandle(x.GetHbitmap(),true)
            System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(hBitmap.DangerousGetHandle(),IntPtr.Zero, Int32Rect.Empty, BitmapSizeOptions.FromEmptyOptions())
//            let bi = BitmapImage()
//            bi.BeginInit()
//            x.Seek(0L,SeekOrigin.Begin) |> ignore
//            bi.StreamSource <- x
//            bi.CacheOption <- BitmapCacheOption.OnLoad
//            bi.EndInit()
//            bi
            )
module Drawing =
    open System.Drawing
    // https://stackoverflow.com/questions/398388/convert-bitmaps-to-one-multipage-tiff-image-in-net-2-0
    let bitmapToTiff ident (bitmap:Bitmap) =
        let stream = new LeakCheckStream<_>(ident,new MemoryStream())
        bitmap.Save(stream, Imaging.ImageFormat.Tiff)
        let tiff = Image.FromStream(stream)
        tiff
        
        
    
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
(
    System.Windows.Application() |> ignore
)
Xps.loadXps @"C:\Users\bdimp\Desktop\testform.xps" Xps.getBitmaps
|> Seq.mapi(fun i bm -> Drawing.bitmapToTiff(string i) bm)
|> Seq.truncate 1
|> Dump
    
    
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