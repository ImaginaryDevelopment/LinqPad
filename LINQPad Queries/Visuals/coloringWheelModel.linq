<Query Kind="FSharpProgram" />

let zero,one,t55 = 0.0, 1.0, 255.0
type Number1(x) = 
    do
        if x < zero || x > one then
            failwithf "Invalid value %f" x
    member __.Value = x
    override __.ToString() = x.ToString()
    member private __.ToDump() = x.ToString()
    member __.ToString(fmt) = x.ToString(format=fmt)
    
type Number255(x) = 
    do
        if x < zero || x > t55 then
            failwithf "Invalid value %f" x
    member __.Value = x
    override __.ToString() = x.ToString()
    member private __.ToDump() = x.ToString()
    member __.ToString(fmt) = x.ToString(format=fmt)
    
type Cmy = { Cyan1: Number1; Magenta1: Number1; Yellow1: Number1}
// [<Measure>] type RgbValue

type Rgb = { R:Number255; G:Number255; B:Number255}
type Cmyk = {CyanK: Number1; MagentaK:Number1; YellowK: Number1; BlackK:Number1}

module Conversions = // guidance from https://github.com/THEjoezack/ColorMine/tree/master/ColorMine/ColorSpaces/Conversions
    let getCmyFromCmyk (cmyk:Cmyk) = 
        let inline f a = a * (one - cmyk.BlackK.Value) + cmyk.BlackK.Value
        {Cyan1=Number1(f cmyk.CyanK.Value); Magenta1= Number1(f cmyk.MagentaK.Value); Yellow1=Number1(f cmyk.YellowK.Value)}
    let getRgbFromCmy (cmy:Cmy) = 
        let inline f x = 
            (one - x) * t55
            |> Number255
        {R= f cmy.Cyan1.Value; G= f cmy.Magenta1.Value; B =f cmy.Yellow1.Value}
    let getCmyFromRgb (rgb:Rgb) = 
        let f (x:Number255) = one - (x.Value / t55)
        {   Cyan1= Number1(f rgb.R)
            Magenta1= Number1(f rgb.G)
            Yellow1= Number1(f rgb.B) }
            
type ColorTracker () = 
    let mutable cyanK = Number1(zero)
    let mutable yellowK = Number1(zero)
    let mutable magentaK = Number1(zero)
    let mutable blackK = Number1(zero)
    let getCmyk() = {   CyanK= cyanK
                        MagentaK= magentaK
                        YellowK = yellowK
                        BlackK = blackK}
    let getCmy() = 
        getCmyk()
        |> Conversions.getCmyFromCmyk 
        
    let getRgb() = 
        let cmy = getCmy()
        Conversions.getRgbFromCmy cmy

    let rgbToCmy (rgb:Rgb) =
        Conversions.getCmyFromRgb rgb
            
    let setCmykViaCmy cmy = 
        let black = min cmy.Cyan1.Value cmy.Magenta1.Value |> min cmy.Yellow1.Value
        let cyan = cmy.Cyan1.Value - black
        let m = cmy.Magenta1.Value - black
        let y = cmy.Yellow1.Value - black
        //(cmy, black,cyan,m,y).Dump("to cmyk")
        cyanK <- Number1(cyan)
        magentaK <- Number1(m)
        yellowK <- Number1(y)
        blackK <- Number1(black)
        
    let setViaRgb (rgb:Rgb) =     
        rgb
        |>rgbToCmy
        |>setCmykViaCmy
        
    member x.CyanK
        with get() = cyanK.Value
        and set v = 
            cyanK <- Number1(v)
    
    member x.MagentaK
        with get() = magentaK.Value
        and set v = 
            magentaK <- Number1(v)
    member x.YellowK
        with get() = yellowK.Value
        and set v = 
            yellowK <- Number1(v)
    member x.BlackK
        with get() = blackK.Value
        and set v = 
            blackK <- Number1(v)
    member x.Cmy = getCmy()
    
    member x.C
        with get() = getCmy().Cyan1.Value
        and set v = 
            { getCmy() with Cyan1 = Number1 v}
            |> setCmykViaCmy
            
    member x.Y = getCmy().Yellow1.Value
    member x.M = getCmy().Magenta1.Value
    
    member x.R
        with get() = x.Rx |> byte
        and set (v:byte) =
            x.Rx <- float v
    member x.G
        with get() = x.Gx |> byte
        and set (v:byte) = 
            x.Gx <- float v
    member x.B
        with get() = x.Bx |> byte
        and set (v:byte) = 
            x.Bx <- float v
    member x.Rx
        with get() = getRgb().R.Value
        and set (v) = 
            let currentRgb = getRgb()
            {currentRgb with R = Number255 v}
            |> setViaRgb
            |> ignore
    member x.Gx
        with get() = getRgb().G.Value
        and set (v) = 
            {getRgb() with G = Number255 v}
            |> setViaRgb
    member x.Bx
        with get() = getRgb().B.Value
        and set (v) = 
            {getRgb() with B = Number255 v}
            |> setViaRgb

let ct = ColorTracker()

ct.Cmy
|> Dump
|> ignore


 // should be 130uy,65uy,65uy
ct.Rx <- 255.0
ct.Gx <- 255.0
ct.Bx <- 255.0

ct.Cmy
|> Dump
|> ignore

ct.Rx <- 254.0
ct.Gx <- 128.0
ct.Bx <- 1.0

(ct,ct.Cmy)
|> Dump
|> ignore