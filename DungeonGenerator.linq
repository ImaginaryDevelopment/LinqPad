<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.Annotations.dll</Reference>
  <Namespace>System.ComponentModel</Namespace>
</Query>

//http://roguebasin.roguelikedevelopment.org/index.php?title=Java_Example_of_Dungeon-Building_Algorithm
void Main()
{
	//initial stuff used in making the map
		int x = 30; int y = 25; int dungeon_objects = 2;

		var rand=new JavaRandomizer();
		var myRand=new TestRandomizer(0);
		var generator = new Dungeon(myRand);
 
		//then we create a new dungeon map
		if (generator.createDungeon(x, y, dungeon_objects)){
			//always good to be able to see the results..
			var dungeonArray=generator.GetDungeon();
			
			char[,] dungeon= new char[y,x];
			for (int j = 0; j < y; j++)
			for (int i = 0; i < x; i++)
			{
				try
				{	        
					dungeon[j,i]=generator.GetCellTile(i,j);
				}
				catch (IndexOutOfRangeException ex)
				{
					new{i,j}.Dump("exceptional");
					throw;
				}
				
			}
			dungeon.Dump("java dungeon");
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
 

 
	//misc. messages to print
	private String msgXSize = "X size of dungeon: \t";
	private String msgYSize = "Y size of dungeon: \t";
	private String msgMaxObjects = "max # of objects: \t";
	private String msgNumObjects = "# of objects made: \t";
 
 
	//setting a tile's type
	private void setCell(int x, int y, Tile celltype){
		dungeon_map[x + xsize * y] = celltype;
	}
 	IRandomize _rnd;
 	public Dungeon(IRandomize rnd)
	{
		_rnd=rnd;
	}
	//returns the type of a tile
 	public Tile GetCellType(int x, int y){
		try
		{	        
			return dungeon_map[x + xsize * y];
		}
		catch (IndexOutOfRangeException ex)
		{
			new{x,y}.Dump("exceptional");
			throw;
		}
		
	}
	private int getRand(int min, int max){
		return _rnd.Next(min,max);
	}
	private bool makeCorridor(int x, int y, int length, int direction){
		//define the dimensions of the corridor (er.. only the width and height..)
		int len = getRand(2, length);
		Tile floor = Tile.Corridor;
		int dir = 0;
		if (direction > 0 && direction < 4) dir = direction;
 
		int xtemp = 0;
		int ytemp = 0;
 
		switch(dir){
		case 0:
		//north
			//check if there's enough space for the corridor
			//start with checking it's not out of the boundaries
			if (x < 0 || x > xsize) return false;
			else xtemp = x;
 
			//same thing here, to make sure it's not out of the boundaries
			for (ytemp = y; ytemp > (y-len); ytemp--){
				if (ytemp < 0 || ytemp > ysize) return false; //oh boho, it was!
				if (GetCellType(xtemp, ytemp) != Tile.Unused) return false;
			}
 
			//if we're still here, let's start building
			for (ytemp = y; ytemp > (y-len); ytemp--){
				setCell(xtemp, ytemp, floor);
			}
			break;
		case 1:
		//east
				if (y < 0 || y > ysize) return false;
				else ytemp = y;
 
				for (xtemp = x; xtemp < (x+len); xtemp++){
					if (xtemp < 0 || xtemp > xsize) return false;
					if (GetCellType(xtemp, ytemp) != Tile.Unused) return false;
				}
 
				for (xtemp = x; xtemp < (x+len); xtemp++){
					setCell(xtemp, ytemp, floor);
				}
			break;
		case 2:
		//south
			if (x < 0 || x > xsize) return false;
			else xtemp = x;
 
			for (ytemp = y; ytemp < (y+len); ytemp++){
				if (ytemp < 0 || ytemp > ysize) return false;
				if (GetCellType(xtemp, ytemp) != Tile.Unused) return false;
			}
 
			for (ytemp = y; ytemp < (y+len); ytemp++){
				setCell(xtemp, ytemp, floor);
			}
			break;
		case 3:
		//west
			if (ytemp < 0 || ytemp > ysize) return false;
			else ytemp = y;
 
			for (xtemp = x; xtemp > (x-len); xtemp--){
				if (xtemp < 0 || xtemp > xsize) return false;
				if (GetCellType(xtemp, ytemp) != Tile.Unused) return false; 
			}
 
			for (xtemp = x; xtemp > (x-len); xtemp--){
				setCell(xtemp, ytemp, floor);
			}
			break;
		}
 
		//woot, we're still here! let's tell the other guys we're done!!
		return true;
	}
 
	private bool makeRoom(int x, int y, int xlength, int ylength, int direction){
		//define the dimensions of the room, it should be at least 4x4 tiles (2x2 for walking on, the rest is walls)
		int xlen = getRand(4, xlength);
		int ylen = getRand(4, ylength);
		//the tile type it's going to be filled with
		Tile floor = Tile.DirtFloor;
		Tile wall = Tile.DirtWall;
		//choose the way it's pointing at
		int dir = 0;
		if (direction > 0 && direction < 4) dir = direction;
 
		switch(dir){
		case 0:
		//north
			//Check if there's enough space left for it
			for (int ytemp = y; ytemp > (y-ylen); ytemp--){
				if (ytemp < 0 || ytemp > ysize) return false;
				for (int xtemp = (x-xlen/2); xtemp < (x+(xlen+1)/2); xtemp++){
					if (xtemp < 0 || xtemp > xsize) return false;
					if (GetCellType(xtemp, ytemp) != Tile.Unused) return false; //no space left...
				}
			}
 
			//we're still here, build
			for (int ytemp = y; ytemp > (y-ylen); ytemp--){
				for (int xtemp = (x-xlen/2); xtemp < (x+(xlen+1)/2); xtemp++){
					//start with the walls
					if (xtemp == (x-xlen/2)) setCell(xtemp, ytemp, wall);
					else if (xtemp == (x+(xlen-1)/2)) setCell(xtemp, ytemp, wall);
					else if (ytemp == y) setCell(xtemp, ytemp, wall);
					else if (ytemp == (y-ylen+1)) setCell(xtemp, ytemp, wall);
					//and then fill with the floor
					else setCell(xtemp, ytemp, floor);
				}
			}
			break;
		case 1:
		//east
			for (int ytemp = (y-ylen/2); ytemp < (y+(ylen+1)/2); ytemp++){
				if (ytemp < 0 || ytemp > ysize) return false;
				for (int xtemp = x; xtemp < (x+xlen); xtemp++){
					if (xtemp < 0 || xtemp > xsize) return false;
					if (GetCellType(xtemp, ytemp) != Tile.Unused) return false;
				}
			}
 
			for (int ytemp = (y-ylen/2); ytemp < (y+(ylen+1)/2); ytemp++){
				for (int xtemp = x; xtemp < (x+xlen); xtemp++){
 
					if (xtemp == x) setCell(xtemp, ytemp, wall);
					else if (xtemp == (x+xlen-1)) setCell(xtemp, ytemp, wall);
					else if (ytemp == (y-ylen/2)) setCell(xtemp, ytemp, wall);
					else if (ytemp == (y+(ylen-1)/2)) setCell(xtemp, ytemp, wall);
 
					else setCell(xtemp, ytemp, floor);
				}
			}
			break;
		case 2:
		//south
			for (int ytemp = y; ytemp < (y+ylen); ytemp++){
				if (ytemp < 0 || ytemp > ysize) return false;
				for (int xtemp = (x-xlen/2); xtemp < (x+(xlen+1)/2); xtemp++){
					if (xtemp < 0 || xtemp > xsize) return false;
					if (GetCellType(xtemp, ytemp) != Tile.Unused) return false;
				}
			}
 
			for (int ytemp = y; ytemp < (y+ylen); ytemp++){
				for (int xtemp = (x-xlen/2); xtemp < (x+(xlen+1)/2); xtemp++){
 
					if (xtemp == (x-xlen/2)) setCell(xtemp, ytemp, wall);
					else if (xtemp == (x+(xlen-1)/2)) setCell(xtemp, ytemp, wall);
					else if (ytemp == y) setCell(xtemp, ytemp, wall);
					else if (ytemp == (y+ylen-1)) setCell(xtemp, ytemp, wall);
 
					else setCell(xtemp, ytemp, floor);
				}
			}
			break;
		case 3:
		//west
			for (int ytemp = (y-ylen/2); ytemp < (y+(ylen+1)/2); ytemp++){
				if (ytemp < 0 || ytemp > ysize) return false;
				for (int xtemp = x; xtemp > (x-xlen); xtemp--){
					if (xtemp < 0 || xtemp > xsize) return false;
					if (GetCellType(xtemp, ytemp) != Tile.Unused) return false; 
				}
			}
 
			for (int ytemp = (y-ylen/2); ytemp < (y+(ylen+1)/2); ytemp++){
				for (int xtemp = x; xtemp > (x-xlen); xtemp--){
 
					if (xtemp == x) setCell(xtemp, ytemp, wall);
					else if (xtemp == (x-xlen+1)) setCell(xtemp, ytemp, wall);
					else if (ytemp == (y-ylen/2)) setCell(xtemp, ytemp, wall);
					else if (ytemp == (y+(ylen-1)/2)) setCell(xtemp, ytemp, wall);
 
					else setCell(xtemp, ytemp, floor);
				}
			}
			break;
		}
 
		//yay, all done
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
		makeRoom(xsize/2, ysize/2, 8, 6, getRand(0,3)); //getrand saken f????r att slumpa fram riktning p?? rummet
 
		//keep count of the number of "objects" we've made
		int currentFeatures = 1; //+1 for the first room we just made
 
		//then we sart the main loop
		for (int countingTries = 0; countingTries < 1000; countingTries++){
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
			for (int testing = 0; testing < 1000; testing++){
				newx = getRand(1, xsize-1);
				newy = getRand(1, ysize-1);
				validTile = -1;
				//Console.WriteLine("tempx: " + newx + "\ttempy: " + newy);
				if (GetCellType(newx, newy) == Tile.DirtWall || GetCellType(newx, newy) == Tile.Corridor){
					//check if we can reach the place
					if (GetCellType(newx, newy+1) == Tile.DirtFloor || GetCellType(newx, newy+1) == Tile.Corridor){
						validTile = 0; //
						xmod = 0;
						ymod = -1;
					}
					else if (GetCellType(newx-1, newy) == Tile.DirtFloor || GetCellType(newx-1, newy) == Tile.Corridor){
						validTile = 1; //
						xmod = +1;
						ymod = 0;
					}
					else if (GetCellType(newx, newy-1) == Tile.DirtFloor || GetCellType(newx, newy-1) == Tile.Corridor){
						validTile = 2; //
						xmod = 0;
						ymod = +1;
					}
					else if (GetCellType(newx+1, newy) == Tile.DirtFloor || GetCellType(newx+1, newy) == Tile.Corridor){
						validTile = 3; //
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
					if (makeRoom((newx+xmod), (newy+ymod), 8, 6, validTile)){
						currentFeatures++; //add to our quota
 
						//then we mark the wall opening with a door
						setCell(newx, newy, Tile.Door);
 
						//clean up infront of the door so we can reach it
						setCell((newx+xmod), (newy+ymod), Tile.DirtFloor);
					}
				}
				else if (feature >= chanceRoom){ //new corridor
					if (makeCorridor((newx+xmod), (newy+ymod), 6, validTile)){
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
 		AddSprinkles();
		
 
 
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

   public interface IRandomize
    {
        bool NextBool();
        int Next(int bound1, int bound2); //should accept min/max in any order
        int NextSign();
    }
	
	public class JavaRandomizer:IRandomize{
	  //the old seed from the RNG is saved in this one
        private long oldseed = 0;

        //The RNG. the seed is based on seconds from the "java epoch" ( I think..)
        //perhaps it's the same date as the unix epoch
        private int getRand(int min, int max)
        {
            //the seed is based on current date and the old, already used seed
            var now = DateTime.Now;
            long seed = now.Ticks + oldseed;
            oldseed = seed;

            var randomizer = new Random((int)seed);
            int n = max - min + 1;
            int i = randomizer.Next(n);
            if (i < 0)
                i = -i;
            //Console.WriteLine("seed: " + seed + "\tnum:  " + (min + i));
            return min + i;
        }

        #region IRandomize Members

        public bool NextBool()
        {
            throw new NotImplementedException();
        }

        public int Next(int bound1, int bound2)
        {
            return this.getRand(bound1, bound2);
        }

        public int NextSign()
        {
            throw new NotImplementedException();
        }

        #endregion
	}
	
	 public class TestRandomizer : IRandomize
    {
        readonly Random _rnd;

        public TestRandomizer(int seed)
        {
            _rnd = new Random(seed);
        }

        public bool NextBool()
        {
            //http://stackoverflow.com/a/1493094/57883
            //upperbound is exclusive
            return _rnd.Next(0, 2) ==0;
        }
        public int NextInclusive(int bound1, int bound2)
        {
            var min = Math.Min(bound1, bound2);
            var max = Math.Max(bound1, bound2);
            return _rnd.Next(min, max+1);
        }
        public int Next(int bound1, int bound2)
        {
            var min = Math.Min(bound1, bound2);
            var max = Math.Max(bound1, bound2);
            return _rnd.Next(min, max);
        }
        public int NextSign()
        {
            var negative = NextBool();
            return negative ? -1 : 1;
        }
    }