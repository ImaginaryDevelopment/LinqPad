<Query Kind="FSharpProgram">
  <Namespace>Microsoft.Win32.SafeHandles</Namespace>
  <Namespace>System.Drawing</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

module PInvoke = 
        [<DllImport("user32.dll")>]
        extern bool private GetCursorPos(Point& lpPoint);

        [<DllImport("gdi32.dll", CharSet = CharSet.Auto, SetLastError = true, ExactSpelling = true)>]
        extern int private BitBlt(IntPtr hDC, int x, int y, int nWidth, int nHeight, IntPtr hSrcDC, int xSrc, int ySrc, int dwRop);
        let getCursorPos () = 
            let mutable cursor = Point()
            if GetCursorPos( &cursor ) then
                Some cursor
            else None
            
        // get color of a screen pixel ->
        // http://stackoverflow.com/questions/1483928/how-to-read-the-color-of-a-screen-pixel
        let getPixelColor () = 
            match getCursorPos() with
            | Some pos ->
            
                let mutable screenPixel = new Bitmap(1,1, Imaging.PixelFormat.Format32bppArgb)
                use gdest = Graphics.FromImage(screenPixel)
                use gsrc = Graphics.FromHwnd IntPtr.Zero
                let srcHdc = gsrc.GetHdc()
                let hdc = gdest.GetHdc()
                let ret = BitBlt(hdc, 0,0,1,1,srcHdc, int pos.X, int pos.Y,  box CopyPixelOperation.SourceCopy :?> int )
                gdest.ReleaseHdc()
                gsrc.ReleaseHdc()
                screenPixel.GetPixel(0,0)
                |> Some
            | None -> None
        
let t = Util.ReadLineAsync<string>("Stop?")
let dc = 
    let dc = DumpContainer()
    dc.Dump()
    dc
let t2 = System.Threading.Tasks.Task.Run(fun () ->
    while t.GetAwaiter().IsCompleted |> not do
        dc.Content <- null
        match PInvoke.getPixelColor() with
        | None -> dc.Content <- box DateTime.Now
        | Some c -> dc.Content <- box (DateTime.Now, sprintf "%A" c, c.Name)
        System.Threading.Thread.Sleep(1000)
)

while t.GetAwaiter().IsCompleted |> not do
    System.Threading.Thread.Sleep(100)
    