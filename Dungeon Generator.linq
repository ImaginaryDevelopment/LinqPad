<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.ComponentModel</Namespace>
</Query>

//http://roguebasin.roguelikedevelopment.org/index.php?title=Java_Example_of_Dungeon-Building_Algorithm
void Main()
{
	//initial stuff used in making the map
		int x = 80; int y = 25; int dungeon_objects = 50;
 		
		//convert a string to a int, if there's more then one arg
//		if (args.length >= 1)
//			dungeon_objects = Integer.parseInt(args[0]);
//		if (args.length >= 2)
//			x = Integer.parseInt(args[1]);
// 
//		if (args.length >= 3)
//			y = Integer.parseInt(args[2]);
		//create a new class of "dungen", so we can use all the goodies within it
		var generator = new Dungeon();
 
		//then we create a new dungeon map
		if (generator.createDungeon(x, y, dungeon_objects)){
			//always good to be able to see the results..
			var dungeonArray=generator.GetDungeon();
			generator.Rooms.Dump();
			char[,] dungeon= new char[y,x];
			for (int j = 0; j < y; j++)
			for (int i = 0; i < x; i++)
			{
				dungeon[j,i]=generator.GetCellTile(i,j);
			}
			dungeon.Dump();
			//generator.showDungeon();
		}
}

//a list over tile types we're using
public enum Tile{
	[Description("_")]
	Unused,
	[Description(".")]
	DirtWall,
	DirtFloor,
	StoneWall,
	Corridor,
	Door,
	Upstairs,
	Downstairs,
	Chest
}
public struct Vector2{
	public int X;
	public int Y;
}

/// Having direction AND magnitude!
public struct Vector2d{
	public int x;
	public int y;
	public int xMod;
	public int yMod;
	public IEnumerable<Vector2> WalkX(){
		//if xMod is positive we start at x and walk until xMod
		//if xMod is negative we start at xMod and walk until x
		var destination=x+xMod;
		for(var i=Math.Min(x,destination);i<Math.Max(x,destination);i++)
		{
			var v=new Vector2(){ X=i,Y=this.y};
			yield return v;
		}
	}
	public IEnumerable<Vector2> WalkY(){
		//if xMod is positive we start at x and walk until xMod
		//if xMod is negative we start at xMod and walk until x
		var destination=y+yMod;
		for(var i=Math.Min(y,destination);i<Math.Max(y,destination);i++)
		{
			var v=new Vector2(){ X=this.x,Y=i};
			yield return v;
		}
	}
	public IEnumerable<Vector2> Walk(){
		foreach(var i in WalkX())
		foreach(var j in WalkY())
		{
			yield return new Vector2{ X=i.X, Y=j.Y};
		}
		
	}
	
}
	
// Define other methods and classes here
public class Dungeon{
	//max size of the map
	private int xmax = 80; //80 columns
	private int ymax = 25; //25 rows
 
	//size of the map
	private int xsize = 0;
	private int ysize = 0;
 
	//number of "objects" to generate on the map
	private int objects = 0;
 
	//define the %chance to generate either a room or a corridor on the map
	//BTW, rooms are 1st priority so actually it's enough to just define the chance
	//of generating a room
	private int chanceRoom = 75; 
 
	//our map
	private Tile[] dungeon_map = { };
 
	//the old seed from the RNG is saved in this one
	private long oldseed = 0;
 
	//misc. messages to print
	private String msgXSize = "X size of dungeon: \t";
	private String msgYSize = "Y size of dungeon: \t";
	private String msgMaxObjects = "max # of objects: \t";
	private String msgNumObjects = "# of objects made: \t";
 
 
	//setting a tile's type
	private void setCell(int x, int y, Tile celltype){
		dungeon_map[x + xsize * y] = celltype;
	}
 	public bool IsCellPassable(int x, int y,bool underground=true){
		switch(GetCellType(x,y)){
			case Tile.Chest:
			case Tile.Corridor:
			case Tile.DirtFloor:
			case Tile.Door:
			case Tile.Downstairs:
				return true;
			case Tile.Unused:
				return !underground;
			default:
				return false;
		}
		
	}
	//returns the type of a tile
 	public Tile GetCellType(int x, int y){
		return dungeon_map[x + xsize * y];
	}
	
	int getRandSign(){
		var now = DateTime.Now;
		long seed = now.Ticks+ oldseed;
		oldseed = seed;
 		
		var randomizer = new Random((int)seed);
		var negative=randomizer.NextBool();
		return negative? -1 : 1;
	}
	
	//The RNG. the seed is based on seconds from the "java epoch" ( I think..)
	//perhaps it's the same date as the unix epoch
	private int getRand(int min, int max){
		var t=min;
		min=Math.Min(min,max);
		max=Math.Max(t,max);
		//the seed is based on current date and the old, already used seed
		var now = DateTime.Now;
		long seed = now.Ticks+ oldseed;
		oldseed = seed;
 		
		var randomizer = new Random((int)seed);
		int n = max - min + 1;
		int i = randomizer.Next(n);
		if (i < 0)
			i = -i;
		//Console.WriteLine("seed: " + seed + "\tnum:  " + (min + i));
		return min + i;
	}
	
 	bool InBounds(int x, int y){
		return x>=0 && x<xsize && y>=0 && y<ysize;
	}
	
	bool MakeCorridor(Vector2d v, bool randomizeLength){
		if(v.yMod==0 && v.xMod==0 || v.xMod !=0 && v.yMod!=0)
			throw new InvalidOperationException("only one direction must be set for a corridor");
		if(randomizeLength)
		{
			if(v.xMod!=0)
				v.xMod=getRand(v.x,v.xMod);
			if(v.yMod!=0)
				v.yMod=getRand(v.y,v.yMod);
		}
		
		Tile floor =Tile.Corridor;
		if(!InBounds(v.x,v.y)) return false;
		var points=v.Walk().ToArray();
		if(points.Any (p => !InBounds(p.X,p.Y) || GetCellType(p.X,p.Y)!=Tile.Unused))
			return false;
		foreach(var p in points) setCell(p.X,p.Y,floor);
		return true;
	}
	
	IList<Vector2d> _rooms=new List<Vector2d>();
	
	public IEnumerable<Vector2d> Rooms{get{return _rooms.ToArray();}}
	
	bool MakeRoom(Vector2d v, bool randomizeLength)
	{
		if(randomizeLength)
		{
			v.xMod=getRand(v.x,v.xMod);
			v.yMod=getRand(v.y,v.yMod);
		}
		var floor=Tile.DirtFloor;
		var wall=Tile.DirtWall;
		var points=v.Walk().ToArray();
		if(points.Any (p => !InBounds(p.X,p.Y) || GetCellType(p.X,p.Y)!=Tile.Unused))
		{
			var outofBounds=points.Any (p => !InBounds(p.X,p.Y));
			Vector2 used=new Vector2();
			if(!outofBounds)
				used=points.FirstOrDefault (p => GetCellType(p.X,p.Y)!=Tile.Unused);
			new{ v,
				notInBounds=points.Any(p => !InBounds(p.X,p.Y)),
				ptOutOfBounds=points.FirstOrDefault (p => !InBounds(p.X,p.Y)),
				used}.Dump("MakeRoom failing");
			
			return false;
		}
		foreach(var p in points)
		{
			setCell(p.X,p.Y,p.X==v.x || p.X==v.xMod || p.Y==v.y || p.Y==v.yMod ? wall:floor);
		}
		_rooms.Add(v);
		return true;
	}
 
	
 	public Tile[] GetDungeon(){
		return dungeon_map;
	}
	
	
	public char GetCellTile(int x, int y){
	switch(GetCellType(x, y)){
				case Tile.Unused:
					return '_';
					break;
				case Tile.DirtWall:
					return '+';
					break;
				case Tile.DirtFloor:
					return '.';
					break;
				case Tile.StoneWall:
					return 'O';
					break;
				case Tile.Corridor:
					return '#';
					break;
				case Tile.Door:
					return 'D';
					break;
				case Tile.Upstairs:
					return '<';
					break;
				case Tile.Downstairs:
					return '>';
					break;
				case Tile.Chest:
					return '*';
					break;
					default :
					throw new ArgumentOutOfRangeException("x,y");
				};
	}
	
	//used to print the map on the screen
	public void showDungeon(){
		for (int y = 0; y < ysize; y++){
			for (int x = 0; x < xsize; x++){
				//System.out.print(getCell(x, y));
				Console.Write( GetCellTile(x,y));
			}
			if (xsize <= xmax) Console.WriteLine();
		}
	}
 
	//and here's the one generating the whole map
	public bool createDungeon(int inx, int iny, int inobj){
		if (inobj < 1) objects = 10;
		else objects = inobj;
 
		//adjust the size of the map, if it's smaller or bigger than the limits
		if (inx < 3) xsize = 3;
		else if (inx > xmax) xsize = xmax;
		else xsize = inx;
 
		if (iny < 3) ysize = 3;
		else if (iny > ymax) ysize = ymax;
		else ysize = iny;
 	
		Console.WriteLine(msgXSize + xsize);
		Console.WriteLine(msgYSize + ysize);
		Console.WriteLine(msgMaxObjects + objects);
 
		//redefine the map var, so it's adjusted to our new map size
		dungeon_map = new Tile[xsize * ysize];
 
		//start with making the "standard stuff" on the map
		for (int y = 0; y < ysize; y++){
			for (int x = 0; x < xsize; x++){
				//ie, making the borders of unwalkable walls
				if (y == 0) setCell(x, y, Tile.StoneWall);
				else if (y == ysize-1) setCell(x, y, Tile.StoneWall);
				else if (x == 0) setCell(x, y, Tile.StoneWall);
				else if (x == xsize-1) setCell(x, y, Tile.StoneWall);
 
				//and fill the rest with dirt
				else setCell(x, y, Tile.Unused);
			}
		}
		
 
		/*******************************************************************************
		And now the code of the random-map-generation-algorithm begins!
		*******************************************************************************/
 
		//start with making a room in the middle, which we can start building upon
		var initialRoomParams=new Vector2d{ x=xsize/2, y=ysize/2, xMod=getRandSign()*8, yMod=getRandSign()*8};
		if(!MakeRoom(initialRoomParams,true))
			initialRoomParams.Dump("failed to create first room");
	
			
 
		//keep count of the number of "objects" we've made
		int currentFeatures = 1; //+1 for the first room we just made
 
		//then we sart the main loop
		for (int countingTries = 0; countingTries < 100; countingTries++){
			//check if we've reached our quota
			if (currentFeatures == objects){
				break;
			}
 
			//start with a random wall
			int newx = 0;
			int xmod = 0;
			int newy = 0;
			int ymod = 0;
			int validTile = -1;
			//1000 chances to find a suitable object (room or corridor)..
			//(yea, i know it's kinda ugly with a for-loop... -_-')
			int corridorMaxLength=6;
			
			for (int testing = 0; testing < 1000; testing++){
				newx = getRand(1, xsize-1);
				newy = getRand(1, ysize-1);
				validTile = -1;
				
				var landing=GetCellType(newx, newy);
				if (landing == Tile.DirtFloor || landing == Tile.Corridor || landing==Tile.Door){
					//check if we can reach the place
					
					if (GetCellType(newx, newy+1) == Tile.DirtFloor || GetCellType(newx, newy+1) == Tile.Corridor){
						validTile = 0; //south
						xmod = 0;
						ymod = -1;
					}
					else if (GetCellType(newx-1, newy) == Tile.DirtFloor || GetCellType(newx-1, newy) == Tile.Corridor){
						validTile = 1; //east
						xmod = +1;
						ymod = 0;
					}
					else if (GetCellType(newx, newy-1) == Tile.DirtFloor || GetCellType(newx, newy-1) == Tile.Corridor){
						validTile = 2; //north
						xmod = 0;
						ymod = +1;
					}
					else if (GetCellType(newx+1, newy) == Tile.DirtFloor || GetCellType(newx+1, newy) == Tile.Corridor){
						validTile = 3; //west
						xmod = -1;
						ymod = 0;
					}
 
					//check that we haven't got another door nearby, so we won't get alot of openings besides
					//each other
					if (validTile > -1){
						if (GetCellType(newx, newy+1) == Tile.Door) //north
							validTile = -1;
						else if (GetCellType(newx-1, newy) == Tile.Door)//east
							validTile = -1;
						else if (GetCellType(newx, newy-1) == Tile.Door)//south
							validTile = -1;
						else if (GetCellType(newx+1, newy) == Tile.Door)//west
							validTile = -1;
					}
 
					//if we can, jump out of the loop and continue with the rest
					if (validTile > -1) break;
				}
			}
			if (validTile > -1){
				//choose what to build now at our newly found place, and at what direction
				int feature = getRand(0, 100);
				if (feature <= chanceRoom){ //a new room
					var v=new Vector2d{ x=newx, xMod=(xmod==0?ymod:xmod)*8, y=newy, yMod=(ymod==0? xmod:ymod)*8};
					v.Dump("Making room");
					if (MakeRoom(v,true)){
						currentFeatures++; //add to our quota
 
						//then we mark the wall opening with a door
						setCell(newx, newy, Tile.Door);
 
						//clean up infront of the door so we can reach it
						setCell((newx+xmod), (newy+ymod), Tile.DirtFloor);
					} else{
						v.Dump("Failed to make room");
					}
				}
				else if (feature >= chanceRoom){ //new corridor
					if (MakeCorridor(new Vector2d{ x=newx, y=newy,xMod=xmod*corridorMaxLength, yMod=ymod*corridorMaxLength},true)){
						//same thing here, add to the quota and a door
						currentFeatures++;
 
						setCell(newx, newy, Tile.Door);
					}
				}
			}
		}
 
 
		/*******************************************************************************
		All done with the building, let's finish this one off
		*******************************************************************************/
 		//AddSprinkles();
		
 
 
		//all done with the map generation, tell the user about it and finish
		Console.WriteLine(msgNumObjects + currentFeatures);
 
		return true;
	}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	void AddSprinkles()
	{
	//sprinkle out the bonusstuff (stairs, chests etc.) over the map
		var newx = 0;
		var newy = 0;
		int ways = 0; //from how many directions we can reach the random spot from
		int state = 0; //the state the loop is in, start with the stairs
		while (state != 10){
			for (int testing = 0; testing < 1000; testing++){
				newx = getRand(1, xsize-1);
				newy = getRand(1, ysize-2); //cheap bugfix, pulls down newy to 0<y<24, from 0<y<25
 
				//Console.WriteLine("x: " + newx + "\ty: " + newy);
				ways = 4; //the lower the better
 
				//check if we can reach the spot
				if (GetCellType(newx, newy+1) == Tile.DirtFloor || GetCellType(newx, newy+1) == Tile.Corridor){
				//north
					if (GetCellType(newx, newy+1) != Tile.Door)
					ways--;
				}
				if (GetCellType(newx-1, newy) == Tile.DirtFloor || GetCellType(newx-1, newy) == Tile.Corridor){
				//east
					if (GetCellType(newx-1, newy) != Tile.Door)
					ways--;
				}
				if (GetCellType(newx, newy-1) == Tile.DirtFloor || GetCellType(newx, newy-1) == Tile.Corridor){
				//south
					if (GetCellType(newx, newy-1) != Tile.Door)
					ways--;
				}
				if (GetCellType(newx+1, newy) == Tile.DirtFloor || GetCellType(newx+1, newy) == Tile.Corridor){
				//west
					if (GetCellType(newx+1, newy) != Tile.Door)
					ways--;
				}
 
				if (state == 0){
					if (ways == 0){
					//we're in state 0, let's place a "upstairs" thing
						setCell(newx, newy, Tile.Upstairs);
						state = 1;
						break;
					}
				}
				else if (state == 1){
					if (ways == 0){
					//state 1, place a "downstairs"
						setCell(newx, newy, Tile.Downstairs);
						state = 10;
						break;
					}
				}
			}
		}
	}
}