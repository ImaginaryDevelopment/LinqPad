<Query Kind="Statements">
  <Reference Relative="..\Visual Studio 11\Projects\DungeonGenerator\DungeonGenerator\bin\Debug\DungeonGenerator.exe">&lt;MyDocuments&gt;\Visual Studio 11\Projects\DungeonGenerator\DungeonGenerator\bin\Debug\DungeonGenerator.exe</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>DungeonGenerator</Namespace>
</Query>


int seed=1;
int x=30;
int y=25;
int objects=20;
var tr=new TestRandomizer(seed);
var d=new DungeonGenerator.Java.Generator(tr,s=>s.Dump());
d.CreateDungeon(x,y,objects);
tr.RandomsCount.Dump("randoms");
"java".Dump("start");

char[,] dungeon= new char[y,x];
			for (int j = 0; j < y; j++)
			for (int i = 0; i < x; i++)
			{
				try
				{	        
					//switch axes as c# has rows first then columns
					dungeon[j,i]=d.GetCellTile(i,j);
				}
				catch (IndexOutOfRangeException ex)
				{
					new{i,j}.Dump("exceptional");
					throw;
				}
				
			}
dungeon.Dump();

var rnd=new TestRandomizer(seed);
var jd=new DungeonGenerator.Java.JDungeon(rnd,s=>s.Dump());
jd.CreateDungeon(x,y,objects);
var dungeonArray=jd.GetDungeon();
rnd.RandomsCount.Dump("java randoms");
jd.Corridors.Dump("java corridors");
			
			
			for (int j = 0; j < y; j++)
			for (int i = 0; i < x; i++)
			{
				try
				{	        
					//switch axes as c# has rows first then columns
					dungeon[j,i]=jd.GetCellTile(i,j);
				}
				catch (IndexOutOfRangeException ex)
				{
					new{i,j}.Dump("exceptional");
					throw;
				}
				
			}
			dungeon.Dump();