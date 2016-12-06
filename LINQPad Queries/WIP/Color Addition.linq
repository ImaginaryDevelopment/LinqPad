<Query Kind="FSharpProgram">
  <Namespace>System.Drawing</Namespace>
</Query>

// add 2 web colors
type ColorAddition = 
    |ForegroundBackGround // foreground is dominant and if Alpha is 1 then it's always just foreground
    |Average // average each color part together 
    |Additive // light model

let a = ColorTranslator.FromHtml "#f9f9f9"
let b = ColorTranslator.FromHtml "#dff0d8"
let meth = ColorAddition.Average
    
type Color(x) =
    do
        if x > 1m then failwithf "value too large for Color"
        if x < 0m then failwithf "value too small for Color"
    member __.Value = x

type RgbColor = { A: Color; R:Color; G:Color; B:Color} 
    with 
        member x.ToDrawingColor() = Drawing.Color.FromArgb(x.A.Value * 255m |> int, x.R.Value * 255m |> int, x.G.Value * 255m |> int, x.B.Value * 255m |> int)
        member x.ToDump() = {Alpha = x.A.Value; Red= x.R.Value;Green= x.G.Value;Blue= x.B.Value}
and RgbDisplay = { Alpha:decimal; Red:decimal; Green:decimal; Blue:decimal}        
let toDrawingcolor (x:RgbColor) = x.ToDrawingColor()    

let getCv (c:Color) = c.Value
let addRgb x y = 
    let applyAll f = {A= f (fun c -> c.A); R = f (fun c -> c.R); G = f (fun c -> c.G); B = f(fun c -> c.B)}
    match meth with
    |ForegroundBackGround ->
        let fg,bg = x,y
        let a = 1m - (1m - (getCv fg.A)) * (1m - (getCv bg.A))
        let transparencyTolerance = Double.Parse("1.0E-6") |> decimal
        if a < transparencyTolerance then // fully transparent
            { A=Color a; R=Color 0m; G=Color 0m; B=Color 0m;}
        else
            //r.R = fg.R * fg.A / r.A + bg.R * bg.A * (1 - fg.A) / r.A;
            let getColor f = (f fg |> getCv) * (getCv fg.A) / a + (f bg |> getCv) * (getCv bg.A) * (1m - (getCv fg.A)) / a
            let r = getColor (fun c -> c.R)
            let g = getColor (fun c -> c.G)
            let b = getColor (fun c -> c.B)
            let result = {A= Color a;R= Color r;G= Color g;B= Color b}
            (fg,bg,result).Dump()
            result
    | Average ->
        let getAvg f = 
            let x, y = f x |> getCv, f y |> getCv
            (x + y) / 2m 
            |> Color
            
        //{A= getAvg (fun c -> c.A); R = getAvg (fun c -> c.R); G = getAvg (fun c -> c.G); B = getAvg(fun c -> c.B)}            
        applyAll getAvg
        
    | Additive ->
        let getAddMin f = 
            let x,y = f x |> getCv, f y |> getCv
            Math.Min(1m, x + y)
            |> Color
        //{A = getAddMin (fun c -> c.A); R = getAddMin (fun c -> c.R); G = getAddMin (fun c -> c.G); B = getAddMin (fun c -> c.B) }
        applyAll getAddMin
        
type AddResult<'T> = { X:'T; Y:'T; Result:'T}
let addResult x y r = {X = x; Y=y; Result=r}
type AddColorResults = {Swatches:AddResult<Bitmap>; Html: AddResult<string>; Rgbs: AddResult<RgbColor>}
let toRgbColor (x:System.Drawing.Color) : RgbColor = 
    let normalize x = decimal x / 255m
    let toCv x : Color = x |> normalize |> Color
    
    {RgbColor.A = toCv x.A; R = toCv x.R; G= toCv x.G; B= toCv x.B}    
    
let toBitMap (x:int,y) c = 
    let bmp = new Bitmap(x,y)
    use gfx = Graphics.FromImage(bmp)
    use brush = new SolidBrush(toDrawingcolor c)
    gfx.FillRectangle(brush, 0,0,x,y)
    bmp
let toBitMap20 = toBitMap (20,20)
addRgb (toRgbColor a) (toRgbColor b)
|> fun x -> ( 
    let swatches = addResult (a |> toRgbColor |> toBitMap20) (b |> toRgbColor |> toBitMap20) (toBitMap20 x)
    {
        Swatches = swatches
        Html = addResult (ColorTranslator.ToHtml a) (ColorTranslator.ToHtml b) (x.ToDrawingColor() |> ColorTranslator.ToHtml)
        Rgbs = addResult (toRgbColor a) (toRgbColor b) x
    }
)
    //x, x.ToDrawingColor(), ColorTranslator.ToHtml(x.ToDrawingColor()))
|> Dump
|> ignore