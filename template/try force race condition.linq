<Query Kind="Program">
</Query>

void Main()
{
	
	// race testing harness
	var useParallelInvoke = false;
	
	var sw = new System.Diagnostics.Stopwatch();
	sw.Start();
	
	var items = Enumerable.Range(1,10000).Select(i=> new{Index=i, Thread = new System.Threading.Thread((System.Threading.ParameterizedThreadStart)TryGet)}).ToList();
	if(useParallelInvoke)
	ParallelInvokeTest();
	else
	ForEachAsParallelTest();

	// // // items.Select(i=> i.Thread.ThreadState).Dump("thread states");
	while(items.All(i=> i.Thread.ThreadState != System.Threading.ThreadState.Stopped)){
		System.Threading.Thread.Sleep(1);
		"waiting".Dump();
	}
	
	sw.Stop();
	sw.Elapsed.Dump();
	CountryBO.Instance.GetAll().Dump();
}

void ForEachAsParallelTest(IDictionary<int,Thread> items){
	foreach(var i in items.AsParallel())
	{
		i.Thread.Start(i.Index);
	}
}

void ParallelInvokeTest(IDictionary<int,Thread> items){
	var invokeArray = items.Select( i =>(Action)(()=> i.Value.Start(i.Key))).ToArray();
	Parallel.Invoke(invokeArray);
}

Object lockObject = new Object();
// Define other methods and classes here
void TryGet(object data){
	var id = (int)data;
	//Console.WriteLine(id + " started" );
	var target = CountryBO.Instance.GetAll();
	//Console.WriteLine(id +" finished");
}