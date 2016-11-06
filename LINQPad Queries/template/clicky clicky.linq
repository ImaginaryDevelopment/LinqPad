<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\Accessibility.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Deployment.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Serialization.Formatters.Soap.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>InputSimulator</NuGetReference>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

let is = WindowsInput.InputSimulator()
//is.Mouse.MoveMouseBy(100,100)
//is.Mouse
//is.Mouse.RightButtonClick()
let clickCenterOfRightScreen() =
    //is.Mouse.MoveMouseTo(2687. , 504. )
    Cursor.Position <- System.Drawing.Point(2687,504)
    is.Mouse.LeftButtonClick()
clickCenterOfRightScreen() |> ignore
//
let getMousePosition() = 
    Cursor.Position
let setMousePosition x y = 
    Cursor.Position <- System.Drawing.Point(x,y)
    
let keepClicking() =
    while true do
        clickCenterOfRightScreen() |> ignore
        System.Threading.Thread.Sleep(2000)
module CotLi = 
    // 1    2   3
    // 4    5   6
    let ClickLevelUp currentViewSlot =
        ()      
()