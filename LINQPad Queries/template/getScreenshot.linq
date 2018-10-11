<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Drawing</Namespace>
  <Namespace>System.Drawing.Imaging</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// WIP : get pixel color
// not accounting for dual monitor yet
// maybe eventually do screen OCR?
// some guidance from https://stackoverflow.com/questions/1163761/capture-screenshot-of-active-window

let snip (bounds:Rectangle) = 
    let bm = new Bitmap(bounds.Width, bounds.Height)
    use g = Graphics.FromImage(bm)
    g.CopyFromScreen(Point(bounds.X,bounds.Y),Point.Empty, bounds.Size)
    bm
    
// start with more than a pixel to see if the mouse is occluding what we actually want
let snipPixel (pt:Point) =
    Rectangle(pt.X, pt.Y,pt.X + 10,pt.Y + 10)
    |> snip
// start with getting a snippet of the screen
let getWholeScreen () =
//    let screens = Screen.AllScreens
    let bounds = Screen.GetBounds(Point.Empty)
    let bm = new Bitmap(bounds.Width, bounds.Height)
    use g = Graphics.FromImage(bm)
    g.CopyFromScreen(Point.Empty,Point.Empty, bounds.Size)
    bm
    
let promptMouseMove msg =
    Util.ReadLine(msg) |> ignore<string>
    Cursor.Position
let getFromMouse () =
    let topLeft = promptMouseMove "top Left?"
    let bottomRight= promptMouseMove "bottom right?"
    let bounds = Rectangle(topLeft.X, topLeft.Y, bottomRight.X - topLeft.X, bottomRight.Y - topLeft.Y)
    bounds
    
let getPixelFromMouse () =
    let topLeft = promptMouseMove "top Left?"
    snipPixel topLeft

   
getPixelFromMouse ()