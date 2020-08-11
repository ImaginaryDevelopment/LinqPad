<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type min
// game cycles (1 per 3 sec)
[<Measure>] type cyc
//let asMin = (*) 1.0<1/min>
let toSec = (*) 60.0<s/min>
// if double speed is on, 3, otherwise 6
let getSecFromCyc : float</cyc> -> float</s> = (*) 6.0<cyc/s>
let toMin : float<1/s> -> float<1/min> = (*) (60.0<1/min> / 1.0<1/s>)

let inline sci x = float x |> fun x -> x.ToString("0.##e-0")
// already includes merchant bonus, yes?
let income = 4.1e15</min> // rate given is per minute
let merchantBonus = 2.2
let goal = 100.0e15
let owned = 25.06e15
type Purchase = {   Name:string
                    ProducedPerUpgrade:int
                    UpgradeCost:float
                    ItemSalePrice:float<1/cyc>}

let getRateFromPurchase countProduced (ratePerItem:float<1/s>) : float<1/min> = 
    ratePerItem * float countProduced * merchantBonus
    |> toMin
let inline getNewTime cost (nextIncome:float<'t>) = 
    (goal - owned + cost) / nextIncome
    
//    
//let electricalParts = 
//    let income = income + getRateFromPurchase 2 119.6e12<1/s>
//    let cost = 84.7e12
//    getNewIncome cost income
    
let rec formatTime (x:float<s>) = TimeSpan(0,0,int x).ToString()

let calcTime = toSec >> formatTime
type Output = {Title:string; FinishIn:string; RecoupTime:string; IncomeChange:string; Cost:string; TotalIncome:string}
let makeOutput name cost (rateChange:float</min>) rate time = 
    let recou = 
        if cost = 0.0 then 0.0<min> else cost / rateChange 
    {Title=name;Cost=sci cost;TotalIncome= sci rate; RecoupTime= calcTime recou;FinishIn=calcTime (time + recou *1.22);IncomeChange=sci rateChange}
    
let getPurchaseChange x =
    let itemIncome = x.ItemSalePrice |> getSecFromCyc |> getRateFromPurchase x.ProducedPerUpgrade 
    let income = income + itemIncome
    let t = getNewTime x.UpgradeCost income
    makeOutput x.Name x.UpgradeCost  itemIncome income t
    
[
    makeOutput "current" 0.0 0.0</min> income ((goal-owned) / income)
    getPurchaseChange {UpgradeCost=130.3e12; Name="elecParts"; ItemSalePrice=64.0e9</cyc>;ProducedPerUpgrade=2}
    getPurchaseChange {UpgradeCost=37.0e12; Name="electroMagnet"; ItemSalePrice=38.4e9</cyc>;ProducedPerUpgrade=1}
    getPurchaseChange {UpgradeCost=564.6e12; Name="electricalCircuit";ItemSalePrice=768.0e9<1/cyc>;ProducedPerUpgrade=1}
    getPurchaseChange {UpgradeCost=5.8e15; Name="transmitter";ItemSalePrice=3.8e12</cyc>;ProducedPerUpgrade=1}
    getPurchaseChange {UpgradeCost=22.0e15; Name="receiver";ItemSalePrice=10.2e12</cyc>;ProducedPerUpgrade=1}
]
//receiver 00:21:51 00:04:57 -> X 18:08
|> Dump
|> ignore