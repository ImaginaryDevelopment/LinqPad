<Query Kind="Program">
  <Namespace>System.Collections.ObjectModel</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>System.Reflection.Emit</Namespace>
</Query>

void Main()
{
	var x=this.Quota_group_logs.ToArray();
	var y=x.AsEnumerable();
	//x.GetType().Dump();
	//y.GetType().Dump();
	//object.ReferenceEquals(x,y).Dump();
	//---------------------------------------------------
	
	
	//var rth= dictionaryX.GetType().TypeHandle;
	//unsafe
	//{
	//int size = *(*(int**)&rth +1);
	//size.Dump();
	//}
	
	foreach(var n in new []{1,2,3,4}){
		var dictionaryX=new Dictionary<int,List<Object>>();
		var sampleObj=new Object();
		List<long> memorySizes= new List<long>(5);
		memorySizes.Add(GC.GetTotalMemory(false));
		dictionaryX.Add(1,new List<Object>(){ sampleObj});
		Enumerable.Range(1,5000).ToList().ForEach(i=>dictionaryX[1].Add(new object()));
		memorySizes.Add(GC.GetTotalMemory(false));
		var dictionaryY=new ReadOnlyDictionary<int,List<Object>>(dictionaryX);
		GetMemoryUsageDifference<IDictionary<int,List<Object>>>(dictionaryX,dictionaryY).Dump("memory Usage diff");
		var dictionaryZ= dictionaryX.ToReadOnlyDictionary();
		memorySizes.Add(GC.GetTotalMemory(false));
		memorySizes.Add(GC.GetTotalMemory(false));
		if(object.ReferenceEquals(dictionaryX,dictionaryZ)==false)
			GetMemoryUsageDifference<object>(dictionaryX,dictionaryZ).Dump("memory usage diff x to z");
		//object.ReferenceEquals( dictionaryX[1],dictionaryY[1]).Dump();
		//object.ReferenceEquals( dictionaryX[1].First(),dictionaryZ[1].First()).Dump();
		
		dictionaryX.GetType().TypeHandle.Value.Dump();
		memorySizes.Dump();
		for (int i = 1; i < memorySizes.Count; i++)
		{
		GC.Collect();
				var change=(memorySizes[i]- memorySizes[i-1]);
				//change.ToString().Dump("delta");
				var totalValues = dictionaryX.AsEnumerable().SelectMany(a=>a.Value).Count();
				var changePerListValue=change/(dictionaryX.Count+totalValues);
				//changePerListValue.Dump("change per key");
				new{ change=change.ToString("N0"),changePerListValue}.Dump();
				
				
		}
	}
}

// Define other methods and classes here

//Assumes type are EQUAL just possibly different instances
int GetMemoryUsageDifference<T>(T a, T b){
	if(object.ReferenceEquals(a,b))
		return 0;
	var aSize=Utils.SizeOf(a).Dump("a size");
	var bSize=Utils.SizeOf(b).Dump("b size");//Marshal.SizeOf(b);
	if(a.GetType()==b.GetType() && a.GetType() as IEnumerable !=null){ //assumes counts are EQUAL since they should be the same data
		(a as IEnumerable).Cast<object>().Zip((b as IEnumerable).Cast<object>(),(d,e)=>new{a=d,b=e}).Dump("zipped");
	}
	if(a.
	
	return aSize-bSize;
	
	
}
public static class Utils
{
 public static int SizeOf<T>(T obj)
    {
        return SizeOfCache<T>.SizeOf;
    }

    private static class SizeOfCache<T>
    {
        public static readonly int SizeOf;

        static SizeOfCache()
        {
            var dm = new DynamicMethod("func", typeof(int),
                                       Type.EmptyTypes, typeof(Utils));

            ILGenerator il = dm.GetILGenerator();
            il.Emit(OpCodes.Sizeof, typeof(T));
            il.Emit(OpCodes.Ret);

            var func = (Func<int>)dm.CreateDelegate(typeof(Func<int>));
            SizeOf = func();
        }
    }
}