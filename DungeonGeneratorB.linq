<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Accessibility.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.Windows</Namespace>
</Query>

void Main()
{
	var d=new Dungeon(20,20,5,0,75,new TestRandomizer(0));
	d.GetTiles().Dump();
}

public enum TileType{
Unused,// uninitialized
Floor, //walkable
Wall, //feature wall
Border //map bounds
}

/// does not allow for diagonal walking
public class Dungeon{
	//max size of the map
	readonly int _xmax = 80; //80 columns
	readonly int _ymax = 25; //25 rows
 
	//size of the map after generation
	Rect _dungeonSize;
 	const int minRoomSize=3; //3x3 accounting for walls
	
	public int Features {get;private set;}
 	//number of "objects" to generate on the map
	public int FeaturesDesired{get;private set;}
 
 	public int Sprinkles{get;private set;}
	public int SprinklesDesired{get;private set;}
	//define the %chance to generate either a room or a corridor on the map
	//BTW, rooms are 1st priority so actually it's enough to just define the chance
	//of generating a room
	readonly int _chanceRoom = 75; 
	readonly IRandomize _rnd;
	readonly TileType [,] _tiles; //not accounting for sprinkles yet
	
 	public TileType[,] GetTiles(){
		return _tiles;
	}
	///only works for inner cells, not the border cells
	public bool InBounds(int x, int y){
		return x>0 && x<_xmax && y>0 && y<_ymax;
	}
	public bool InBounds(Vector2 v){
		return InBounds(v.X,v.Y);
	}
	/// only searches in bounds surroundings
	public IEnumerable<Vector2> GetSurroundingPoints(Vector2 v){
		var points=new []{new Vector2{ X=v.X, Y=v.Y+1},new Vector2{ X=v.X, Y=v.Y-1},
			new Vector2{X=v.X-1,Y=v.Y}, new Vector2{X=v.X+1,Y=v.Y}};
		return points.Where (p => InBounds(p));
	}
	public IEnumerable<Tuple<Vector2,TileType>> GetSurroundings(Vector2 v){
		return GetSurroundingPoints(v).Select (r => Tuple.Create(r,_tiles[r.X,r.Y]));
	}
	public Dungeon(int xmax, int ymax, int featuresDesired, int sprinklesDesired, int chanceRoom=75,IRandomize r=null){
		_xmax=xmax;
		_ymax=ymax;
		if(xmax<10 || ymax<10)
			throw new ArgumentOutOfRangeException();
		_rnd=r??new TestRandomizer(1);
		FeaturesDesired=featuresDesired;
		SprinklesDesired=sprinklesDesired;
		_chanceRoom=chanceRoom;
		_tiles=new TileType[xmax,ymax];
		Generate(maxFeatureAttempts:20);
	}
	void Generate(int maxFeatureAttempts){
		if(Features>0 || _dungeonSize.X>0 || _dungeonSize.Y>0)
		throw new InvalidOperationException();
		//make starting room
		var v=new Vector2d{ x=_xmax/2, y=_ymax/2, xMod=8, yMod=8};
		v.Dump("making room");
		MakeRoom(v,randomizeLength:true,force:true);
		Features++;
		int attempts=0;
		while(attempts<maxFeatureAttempts && Features<FeaturesDesired){
			attempts++;
			
		}
	}
	
	Vector2d MakeRoom(Vector2d v, bool randomizeLength=true,bool force=false){
		if(!InBounds(v.GetOrigin()))
			throw new ArgumentOutOfRangeException("v",Newtonsoft.Json.JsonConvert.SerializeObject(v));
		var targetV=randomizeLength? v.RandomizeWithinMods(minRoomSize,_rnd): v;
		
		//we might not use this but we might use it in both scopes
		
		var points=new System.Lazy<IEnumerable<Vector2>>(()=> targetV.Walk().ToArray());
		if(!force){
			//check we are connected to a valid room or corridor, or that we are close enough to one
			//if one tile in the opposite direction we are going is room or corridor or door? we are close enough
			
		
		}
		
		//build
		foreach (var p in points.Value){
			var wall= p.X==v.x || p.X == v.xMod+v.x-1 || p.Y==v.y || p.Y==v.yMod+v.y-1;
			//new{ p, wall}.Dump("room builder");
			_tiles[p.X,p.Y]= wall? TileType.Wall : TileType.Floor;
		}
		//TODO: increase dungeon size based on new room
		//_dungeonSize=Rect.Union(new Rect(targetV.x,targetV.y,Math.Abs(targetV.
		return true;
	}
}
public class TestRandomizer:IRandomize{
	readonly Random _rnd;
	public TestRandomizer(int seed){
		_rnd=new Random(seed);
	}
	public bool NextBool(){
		return _rnd.NextBool();
	}
	public int NextInt(int bound1, int bound2){
		var min=Math.Min(bound1,bound2);
		var max=Math.Max(bound1,bound2);
		return _rnd.Next(min,max);
	}
	public int NextSign(){
		var negative=_rnd.NextBool();
		return negative? -1:1;
	}
}
public interface IRandomize{
	bool NextBool();
	int NextInt(int bound1,int bound2); //should accept min/max in any order
	int NextSign();
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
	public Vector2d RandomizeWithinMods(int minMod, IRandomize rnd){
		var nextXmod= xMod>0? rnd.Next(minMod,Math.Max(minMod,xMod)): rnd.Next(minMod*-1,Math.Min(xMod,minMod*-1));
		var nextYMod= yMod>0? rnd.Next(minMod,Math.Max(minMod,yMod)): rnd.Next(minMod*-1,Math.Min(yMod,minMod*-1));
		return new Vector2d{ x=this.x, y=this.y, xMod=nextXmod, yMod=nextYMod};
	}
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
	public Vector2 GetOrigin(){
		return new Vector2{ X=this.x, Y=this.y};
	}
	
}