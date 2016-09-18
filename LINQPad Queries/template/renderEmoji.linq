<Query Kind="FSharpExpression">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Drawing</Namespace>
</Query>

// render emoji to images
// http://stackoverflow.com/questions/21675067/convert-text-to-image

let createBitmapImage (fontName:string) text = 
    use o = new Bitmap(1,1)
    use font = new Font(fontName, 20.f, FontStyle.Regular, GraphicsUnit.Pixel) // "Consolas"
    use og = Graphics.FromImage(o)
    let width,height = og.MeasureString(text,font) |> fun m -> m.Width, m.Height
    
    let b = new Bitmap(o, Size(int width, int height))
    use bg = Graphics.FromImage(b)
    bg.Clear(Color.White)
    
    bg.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
    bg.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
    bg.DrawString(text,font, new SolidBrush(Color.FromArgb(102,102,102)),0.f,0.f)
    bg.Flush()
    b
// not quite as good from a having to guess output size needed standpoint, but better in surfaced params    
let convertTextToImage (fontName:string) fontSize bgColor fgColor text = 
    let width,height = int fontSize + 10 ,int fontSize + 10 // 40,40
    let bmp = new Bitmap(width,height)
    use g = Graphics.FromImage(bmp)
    use font = new Font(fontName, fontSize)
    use bgBrush = new SolidBrush(bgColor)
    g.FillRectangle(bgBrush, 0, 0, width,height)
    use fgBrush = new SolidBrush(fgColor)
    g.DrawString(text,font, fgBrush, 0.f,0.f)
    g.Flush()
    font.Dispose()
    g.Dispose()
    bmp
    
let getAllFonts () = 
    use fonts = new Text.InstalledFontCollection()
    fonts.Families
let emojiFonts = getAllFonts() |> Seq.filter(fun f -> f.Name.IndexOf("Emoji", StringComparison.InvariantCultureIgnoreCase) >= 0) |> List.ofSeq
[   "🐶"
    "🚀"
]

//|> Seq.map (fun s -> s,s |>Encoding.UTF8.GetBytes) // , (convertTextToImage "Consolas" 20.f (Color.White) (Color.FromArgb(102,102,102)) s)
|> Seq.map (fun s -> 
    //s, 
    Encoding.UTF8.GetBytes s, 
    // both work
    emojiFonts |> Seq.map (fun font -> font.Name, convertTextToImage font.Name 14.f (Color.White) (Color.FromArgb(102,102,102)) s)
    //emojiFonts |> Seq.map (fun font -> font.Name, createBitmapImage font.Name s)
)