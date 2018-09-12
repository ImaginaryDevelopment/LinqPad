<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Drawing</Namespace>
  <Namespace>System.Drawing.Imaging</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// get pixel color

// start with getting a snippet of the screen

// not accounting for dual monitor yet
let getView () =
    let bounds = Screen.GetBounds(Point.Empty)
    let bm = new Bitmap(bounds.Width, bounds.Height)
    use g = Graphics.FromImage(bm)
    g.CopyFromScreen(Point.Empty,Point.Empty, bounds.Size)
    bm
    
    
getView()