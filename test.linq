<Query Kind="Program">
  <Connection>
    <ID>282ab884-278b-4d31-a865-51e85288681f</ID>
    <Persist>true</Persist>
    <Server>SVRRBIDEVDB04</Server>
    <Database>CVS</Database>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

void Main()
{
	var data = new[]{
		
		RunDelegate(()=>TryCatchWrapper()),
		RunDelegate(()=>TryCatchWrapperStackDepth(10)),
		RunDelegate(()=>GoToDb(true)),
		RunDelegate(()=>GoToDb(false)),
		RunDelegate(()=>GoToFreshDb()),
		RunDelegate(()=>TryCatchWithThrow())
		};
	
	data[0].Dump("trycatchwith slightly deeper stack");
	data[1].Dump("try catch with deeper stack");
	data[2].Dump("go to db, closing connection every time");
	data[3].Dump("go to db, without closing every iteration");
	data[4].Dump("ado go to db");
	data[5].Dump("trycatchwithThrow");
	
}
public Stopwatch RunDelegate(Action toRun){
	var sw= new Stopwatch();
	toRun();
	sw.Start();
	for(var i=0;i<1000;i++){
		toRun();
	}
	sw.Stop();
	GC.Collect();
	return sw;
}
public void TryCatchWrapperStackDepth(int recursionsRemaining){
	if(recursionsRemaining<1)
		TryCatchWithThrow();
		else 
		TryCatchWrapperStackDepth(recursionsRemaining-1);
}
public void TryCatchWrapper(){
	TryCatchWithThrow();
}
// Define other methods and classes here
public void TryCatchWithThrow(){
	try{
		throw new NullReferenceException();
	}
	catch(Exception){
	
	}
}

public void GoToDb(bool closeEvery){
	var m = Member_members.First();
	if(closeEvery)
		this.Connection.Close();
	if(m.Member_external_id==null)
	{
	}
}

public void GoToFreshDb(){
	using(var cn = new System.Data.SqlClient.SqlConnection(this.Connection.ConnectionString)){
		cn.Open();
		using(var cmd = cn.CreateCommand()){
			cmd.CommandText="select top 10 * from member.member";
			using(var r=cmd.ExecuteReader()){
			r.Read();
			}
		}
	}
}