<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Windows.Forms.DataVisualization.Charting</Namespace>
</Query>

// Eventually : simulate progress bars and purchases?
type Augment = 
    | CellDensity
    | CellHardness
type Stats = {EnergyPower:double;AugLevels:Map<Augment,double>}
type [<Measure>] Gold
// maths from https://gamedevelopment.tutsplus.com/articles/numbers-getting-bigger-the-design-and-math-of-incremental-games--cms-24023
let getPrice multiplier baseCost baseRate (level:int64) =
    let getIncomeRate (baseRate:float<_>) (level:int64) =
        float level * float baseRate
    let effect = getIncomeRate baseRate level
    baseCost * multiplier ** ((effect/baseRate) - 1.0)
    
type Purchasable = {Price:float<Gold>;Level:int64;CostFactor:float;EffectConstant:float}
let getPurchaseEffectiveness currentLevel mult =
        let effective = currentLevel * mult
        let next = currentLevel + 1.0 |> (*) mult
        next - effective
        
// linear improvements with exponential costs

// mult is typically between 1.07 and 1.15
let getAugBase =
    function
    |CellDensity -> {Price=15.0<Gold>;CostFactor=1.15; Level=1L;EffectConstant=0.1}
    |CellHardness -> {Price=100.0<Gold>;Level=1L;CostFactor= 1.15;EffectConstant=0.5}
    
let getAugInfo currentLevel aug =
    let name = sprintf "%A" aug
    getAugBase aug
    |> fun x ->
        {x with Price=getPrice x.CostFactor x.Price x.EffectConstant currentLevel;Level=currentLevel}
        //{Price= getPrice cost mult currentLevel;Level=currentLevel + 1.0;Increase=mult}
        
let addAug x (ch:LINQPadChart<int64>) =
    let name subtitle = sprintf "%A %s" x subtitle
    // linear
    let getPower i =
        getAugBase x |> fun x -> x.EffectConstant * (float i)
    // exponential
    let getCost i =
        getAugInfo i x |> fun x -> x.Price
    ch
        .AddYSeries(getCost >> box,
            seriesType=Util.SeriesType.Line,
            name=name "Cost",
            useSecondaryYAxis=false)   
        .AddYSeries(yFunc=Func<_,_>(getPower >> box),
            seriesType=Util.SeriesType.Line,
            name=name "Power",
            useSecondaryYAxis=true)
//let maxChartLevel = 51            
let getFocusedLevels () =        
    [0..9]
    //|> List.map (float>> fun i -> getAugInfo i CellDensity) //, getAugInfo i CellHardness)
    |> List.map float
    |> List.map(fun x -> Math.Pow(10.0,x) |> int64)    

let showFocusedLevels () =
    getFocusedLevels()
    |> List.map(fun x -> x, getAugInfo x CellDensity, getAugInfo x CellHardness)
    
let chartPriceVsValue () =
    
    getFocusedLevels()
    |> fun levels ->
        //items.Dump("aug stuffs")
        Util
            .Chart(levels,(fun a -> box a), seriesType = Util.SeriesType.Line) //(fun a -> box <| a.Increase * a.Level),Util.SeriesType.Line)
        |> addAug CellDensity
        |> addAug CellHardness
        |> fun chart ->
            let w = chart.ToWindowsChart()
            (
                let area = w.ChartAreas.[0]
                area.CursorX.Interval <- 1.0e10
                area.CursorY.Interval <- 1.0e10
                
                (
                    let axisX = area.AxisX
                    axisX.Title <- "Level"
                    //axisX.Minimum <-1.0
                    //axisX.Interval <- 10.0
                    //axisX.Maximum <- float maxChartLevel
                    axisX.Enabled<- AxisEnabled.True
                    axisX.IsLogarithmic <- true
                )
                
                
                (
                    let axisY = area.AxisY
                    axisY.Title <- "Cost"
                    //axisY.Minimum <-1.0
                    //axisY.Interval <- 100.0
                    axisY.IsLogarithmic <- true
                )
                (
                    let axisY2 = area.AxisY2
                    axisY2.Title <- "Power"
                )
            
                w.ChartAreas.[0].AxisY2.Title <-"Power"
            )
            w.GetType().GetMembers()
            |> Seq.map (fun m ->
                m.Name,m.MemberType
            )
            |> fun x -> x.Dump(w.GetType().FullName)
            
            w
            
showFocusedLevels()
|> Dump
|>ignore