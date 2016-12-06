<Query Kind="FSharpProgram">
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

// get pixel color from screen
//http://stackoverflow.com/questions/753132/how-do-i-get-the-colour-of-a-pixel-at-x-y-using-c
module PInvoke =
    [<DllImport("user32.dll")>]
    extern IntPtr private GetDC(IntPtr hwnd);

    [<DllImport("user32.dll")>]
    extern Int32 private ReleaseDC(IntPtr hwnd, IntPtr hdc);

    [<DllImport("gdi32.dll")>]
    extern System.UInt32 private GetPixel(IntPtr hdc, int nXPos, int nYPos);
    
    let getPixelColor x y =
        let hdc = GetDC(IntPtr.Zero)
        let pixel = GetPixel(hdc,x,y)
        ReleaseDC(IntPtr.Zero, hdc) |> ignore
        let red = pixel &&& (Convert.ToUInt32 0xFF)
        let green = pixel &&& (Convert.ToUInt32 0xFF00) >>> 8
        let blue = pixel &&& (Convert.ToUInt32 0xFF0000) >>> 16
        red,green,blue
module Bitmappy =
    open System.Drawing
    let colorFromRgb (r, g, b) = Color.FromArgb(255,int r,int g, int b)
    
    let makeColorBitmap (width:int) height color = 
        let bm = new Bitmap(width,height)
        use g = Graphics.FromImage(bm)
        g.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
        use br = new System.Drawing.SolidBrush(color)
        g.FillRectangle(br, Rectangle(0,0,width,height))
        use pen = new Pen(Color.Blue, 5.f)
        g.DrawRectangle(pen, Rectangle(0,0,width,height))
        bm
    let bitmapToBytes (bm:Bitmap) = 
        let converter = ImageConverter()
        converter.ConvertTo(bm,typeof<byte[]>) :?> byte[]
        
open PInvoke
open Bitmappy
let color1,color2 = getPixelColor 0 0 |> colorFromRgb, getPixelColor 200 200
(color1,color2)
|> Dump
|> ignore

Bitmappy.makeColorBitmap 200 200 color1
|> Bitmappy.bitmapToBytes
|> Util.Image
|> Dump
|> ignore