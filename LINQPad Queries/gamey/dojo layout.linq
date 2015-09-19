<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Drawing.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Drawing</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

type Direction = 
	| North
	| South
	| West
	| East
type Rooms = 
	| TwoWay
	| Elbow
	| ThreeWay
	| Connector
	| ClanHall

type Room = { XSpan:int; YSpan:int; Orientation:Direction; Symbol:char;Pointer:Cursor} (*  byte*byte*Direction -> bool  *)
// HasConnector: byte -> byte -> Direction -> bool
let singleRoomHasConnector (orientations:Direction list) xspan yspan d = 
		if xspan <> 0uy then false
		elif yspan <> 0uy then false
		else
			Seq.exists (fun orientation -> orientation = d) orientations

let twoWayNorthSouth = 
	let orient = Direction.North 
	{Room.XSpan=0; YSpan = 0; Orientation = orient;Pointer=Cursors.IBeam; Symbol = '|'; (* HasConnector = singleRoomHasConnector  [orient;Direction.South] *) }

let twoWayWestEast = 
	let orient = Direction.West
	//{twoWayNorthSouth with Orientation = Direction.West; Symbol = '-'; HasConnector = twoWayHasConnector orient Direction.East}
	{Room.XSpan=0; YSpan = 0; Orientation = orient;Symbol = '-'; Pointer = Cursors.NoMoveHoriz (* HasConnector = singleRoomHasConnector  [orient;Direction.East] *) }
	
let fourWay = {twoWayNorthSouth with Symbol = '+'; Pointer= Cursors.Cross (* HasConnector =singleRoomHasConnector [Direction.East;Direction.South;Direction.North;Direction.West] *) }

let mutable currentTool:Room option = None
let mutable lastTool = Some(twoWayWestEast)
let indexOf item array =
	Array.IndexOf(array,item)
let droppedObjects = 
	dict [ 
		None,null
		Some(twoWayNorthSouth),List<PointF>()
		Some(twoWayWestEast), List<PointF>()
		Some(fourWay), List<PointF>()
		]
let getNextTool () = 
	let keyArray = droppedObjects.Keys.ToArray()
	let currentIndex = indexOf currentTool keyArray
	let nextIndex =(currentIndex+1) % keyArray.Length
	keyArray.[nextIndex]

let go = 
	use frm =new System.Windows.Forms.Form()
	let lblCanvas = new Label()
	
	lblCanvas.Text <- "label1"
	lblCanvas.Dock <- DockStyle.Fill
	lblCanvas.Paint.Add (fun e ->
		let numOfCells = 10
		let cellSize:int = Math.Min(lblCanvas.Height, lblCanvas.Width) / 10
		let g = e.Graphics
		use p = new Pen(Color.Black, 2.f)
		use f = new Font("Consolas",18.f)
		for y in 0 .. numOfCells do
			g.DrawLine(p, 0, y*cellSize, numOfCells * cellSize, y * cellSize)
			g.DrawLine(p, y*cellSize,0, y * cellSize, numOfCells * cellSize)
		for obj in droppedObjects.Keys do
			if obj.IsSome then 
				for pt in droppedObjects.[obj] do
				g.DrawString(obj.Value.Symbol.ToString(),f,Brushes.Blue, pt)
			
	)
	let changeTool t =
		lastTool <- currentTool
		currentTool <- t
		lblCanvas.Cursor <- if t.IsSome then t.Value.Pointer else Cursors.Default

	frm.KeyUp.Add (fun (e:KeyEventArgs)->
		match e.KeyCode with
		| Keys.N -> currentTool <- Some(twoWayNorthSouth)
		| Keys.W -> currentTool <- Some(twoWayWestEast)
		| Keys.F -> currentTool <- Some(fourWay)
		| _ -> ()
		if currentTool.IsSome 
			then lblCanvas.Cursor <- currentTool.Value.Pointer
			else lblCanvas.Cursor <- Cursors.Default
		)
	lblCanvas.MouseClick.Add (fun (e:MouseEventArgs) ->
		if e.Button= MouseButtons.Right then 
			//change current tool
//			let prevTool = currentTool
//			currentTool <-lastTool
//			lastTool <- prevTool
			changeTool (getNextTool())
		else 
			let x =float32( Cursor.Position.X)
			let y =float32(Cursor.Position.Y)
			let locX =float32 e.X //  (lblCanvas.PointToClient(Cursor.Position).X)
			let locY =float32 e.Y // (lblCanvas.PointToClient(Cursor.Position).Y)
			lblCanvas.Text <- sprintf "%A:%A\r\n%A,%A" x y locX locY
			e.Dump();
			if currentTool.IsSome then droppedObjects.[currentTool].Add(PointF(locX,locY))
			// if(currentTool == null) return;
			// if(droppedObjects.ContainsKey(currentTool)==false) droppedObjects.Add(currentTool,new List<Point>());
			// droppedObjects[currentTool].Add( new Point(x,y));
	)
	frm.Size <- System.Drawing.Size(900,900)
	frm.Controls.Add lblCanvas
	
	frm.ShowDialog() |> ignore
	frm.Dispose()