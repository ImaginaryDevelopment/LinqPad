<Query Kind="Program" />

void Main()
{	
var	targetTree=System.IO.File.ReadAllText(@"C:\Users\bcort\Downloads\triangle.txt");
//targetTree.Dump();
	var trees=BuildTree(null,0,0, targetTree.SplitLines());
	var paths=trees.Where(a=>a.Left!=null && a.Right !=null && a.Value==3); //.Dump("tree");
	var max=trees.Select(m=>m.SumDescending).Max();
	var highestPath = trees.Where(a=>a.SumDescending==max).Dump();  //|| (a.Left!=null && a.Left.SumDescending==max) || a.Right!=null && a.Right.SumDescending==max).Dump();
		
}

IEnumerable<BTree> BuildTree(BTree parent,int depth,int column, IEnumerable<string> leaves){
	
	if(leaves.Any()==false)
		yield break;
		
	var siblings= leaves.First().Split(new[]{' '},  StringSplitOptions.RemoveEmptyEntries).Select((a,i)=>new{Column=i,Value=int.Parse( a.ToString())});
	var children=leaves.Skip(1);
	
	BTree result;
	if(siblings.Count()==1 && parent==null){
		//top of tree
		var node=siblings.Single();
		result=new BTree(node.Value);

		foreach(var bt in BuildTree(result,depth+1,node.Column,children))
		{	

			yield return bt;
		}

		yield return result;
	} else {
		//leaves
		var leaflets=siblings.Where(s=>s.Column==column || s.Column==column+1).ToArray();
		
		foreach(var l in leaflets){
			var leaf= new BTree(l.Value);
			leaf.SumDescending=checked(leaf.SumDescending+parent.SumDescending);
			if(parent.Left==null)
				parent.Left=leaf;
			else
				parent.Right=leaf;
			foreach(var bt in BuildTree(leaf,depth+1,l.Column,children))
			{

				yield return bt;
			}

			
			yield return leaf;
		}
		}
		
	}

// Define other methods and classes here

class BTree{

	public int Value{get;private set;}
	public BTree Left{get;set;}
	public BTree Right{get;set;}
	public int SumDescending{get;set;}
	//public int Sum{get;set;}
	//public string Path{get;set;}
	
	public BTree(int value){
	Value=value;
	SumDescending=value;
	//Sum=value;
	//Path+=value.ToString();
	}
}
const string testTree=@"3
7 4
2 4 6
8 5 9 3";
const string tree=@"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23";