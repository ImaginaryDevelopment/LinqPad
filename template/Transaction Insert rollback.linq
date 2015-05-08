<Query Kind="Statements">
  <Namespace>System.Globalization</Namespace>
</Query>

var catalog= new Catalog() { Add_dt=DateTime.Now, Update_dt=DateTime.Now, Reward_name=string.Empty, Reward_image=string.Empty,Add_by=string.Empty,Update_by=string.Empty, Redemption_type_id=1 };
var doesTrip= '\u0FFE';

using(var db = new UserQuery(this.Connection)){
db.Catalogs.Max (c => c.Reward_desc.Length).Dump("max before starting");
this.Connection.Open();
using(var tran = new TransactionScope())
{
	var source=Enumerable.Range(1,500).Select (e =>doesTrip.ToString()).Delimit(string.Empty);
	
	catalog.Reward_desc=source;
	catalog.Reward_desc.Length.Dump("item to submit length");
	new StringInfo(catalog.Reward_desc).LengthInTextElements.Dump("length in text");
	db.Catalogs.InsertOnSubmit(catalog);
	
	
	db.SubmitChanges();
	db.Catalogs.Max (c => c.Reward_desc.Length).Dump("max after submit");
	var desc=db.Catalogs.Where(c=>c.Reward_catalog_id == catalog.Reward_catalog_id).Select (c => c.Reward_desc).First().Dump();
	if(!((desc==source).Dump("result equals input?"))){
		checked{
			Util.HorizontalRun(false, (new {Name="result",Char=(char)desc[0], Int= (int)desc[0],Length=new StringInfo(desc).LengthInTextElements}),
				(new {Char=(char)source[0], Int= (int)source[0],Length=new StringInfo(source).LengthInTextElements})).Dump();
			
		}
		desc.Dump(desc.Length.ToString()+" instead of " + source.Length);
	}
}
	db.Catalogs.Max (c => c.Reward_desc.Length).Dump();
}