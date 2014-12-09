<Query Kind="Statements">
  <Connection>
    <ID>55a5055d-fa01-41fc-a146-32cd08598235</ID>
    <Persist>true</Persist>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

var query = 
	from o in  sys.Objects.Where(o => !o.Is_ms_shipped && o.Type=="P")
	join oSchema in sys.Schemas on o.Schema_id equals oSchema.Schema_id
	join mod in sys.Sql_modules on o.Object_id equals mod.Object_id
	select new {SchemaName = oSchema.Name,ProcName = o.Name, mod.Definition};
var sql = query.ToString().Dump();
query.GetType().GenericTypeArguments[0].Dump();
sql.Dump();
var dev = query.Take(1).ToList();
using(var devcn= new System.Data.SqlClient.SqlConnection(this.Connection.ConnectionString.Replace("\\QA",string.Empty)))
using(var devcmd = devcn.CreateCommand())
{
		devcmd.CommandText = sql;
		devcmd.AddInputParameter("@p0","P");
		devcn.Open();
		using(var r = devcmd.ExecuteReader())
		dev = r.Select(rec => new {SchemaName = r["SchemaName"].ToString(), ProcName = r["ProcName"] is DBNull ? null : r["ProcName"].ToString(), Definition = r["Definition"].ToString()}
		).ToList();
//			while(r.NextResult()){

}

var missingFromAOrDifferent = 
	from devSproc in dev
	join qaL in query.ToArray() on new{ devSproc.SchemaName, devSproc.ProcName} equals new{qaL.SchemaName, qaL.ProcName}  into qaLeft
	from qaSproc in qaLeft.DefaultIfEmpty()
	select new{devSproc.SchemaName, devSproc.ProcName,
		compare = qaSproc!=null ? Util.OnDemand ("generateComparisonFiles",() => {
		var args = new[] {
			devSproc.Definition.ToTempFile().RawPath +" "+ qaSproc.Definition.ToTempFile().RawPath,
			"--L1 dev", "--L2 QA"
		};
		return My.ProcessStartLink( @"C:\Program Files (x86)\KDiff3\kdiff3.exe",string.Join(" ",args),"kdiff");}):null, 
		devSproc = devSproc.Definition, 
		qaSproc = qaSproc != null? qaSproc.Definition : null
		};
	
var missingOrDifferent = missingFromAOrDifferent.ToArray();

missingFromAOrDifferent.Where( m => m.qaSproc == null).Select(m => m.SchemaName+"."+m.ProcName).Dump("missing from ");
missingFromAOrDifferent.Where( m => m.qaSproc != null).Dump("different");
 