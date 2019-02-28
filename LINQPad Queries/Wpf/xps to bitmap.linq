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

module Xps =
    let loadXps path f =
        use doc = new XpsDocument(path,FileAccess.ReadWrite)
        f doc
    let toFds (xps:XpsDocument) =
        let fds = xps.GetFixedDocumentSequence()
        fds
    let asBitmapStream (xps:XpsDocument) f =
        let fds = xps.GetFixedDocumentSequence()
        [0..fds.DocumentPaginator.PageCount - 1]
        |> List.map(fun i ->
//            let docPage = fds.References.[i]
            let docPage = fds.DocumentPaginator.GetPage i
//            let bitmap = BitmapImage()
//            let rt = RenderTargetBitmap(docPage.Size.Width, docPage.Size.Height, 96.0,96.0, System.Windows.Media.PixelFormats.Bgra32)
            let sz = docPage.Size
            let w,h =int sz.Width,int docPage.Size.Height
            (fds.DocumentPaginator.PageCount,i,w,h).Dump()
            let rt = RenderTargetBitmap(w,h, 96.0,96.0, System.Windows.Media.PixelFormats.Default)
            rt.Render(docPage.Visual)
            let encoder = BmpBitmapEncoder()
            encoder.Frames.Add(BitmapFrame.Create rt)
            let stream = new MemoryStream()
            encoder.Save stream
            f stream
        )
    let getBitmaps(xps:XpsDocument) =
        asBitmapStream xps (fun x -> new System.Drawing.Bitmap(x))
        
    
let printPreview (fds:FixedDocumentSequence)=
    let fdsv = System.Windows.Controls.DocumentViewer(Document=fds)
    let w = Window(Content=fdsv)
    let d = Util.KeepRunning()
    w.Closed.Add(fun _ ->
        d.Dispose()
    )
    w.Dump()
let previewXps path =
    Xps.loadXps path  Xps.toFds
    |> printPreview 
Xps.loadXps @"testform.xps" Xps.getBitmaps
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