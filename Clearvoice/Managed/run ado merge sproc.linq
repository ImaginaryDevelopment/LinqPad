<Query Kind="Program">
</Query>

void Main()
{
	//table valued sproc call
	Project_suppliers.Count().Dump("table count before merging");
	
	var projectId= 29;
	var orgs =new[] {0,1};
	 RunMerge(projectId,orgs).Dump("1st merge");
				
		Project_suppliers.Count().Dump("table now has");
		RunMerge(38,365,2).Dump("2nd merge");
		RunMerge(projectId,1,2).Dump("2nd merge");
		Project_suppliers.Dump("after 2nd");
		RunMerge(projectId).Dump("clearing project merge");
		Project_suppliers.Dump("after clearing");
}

// Define other methods and classes here
int RunMerge(int projectId, params int[] orgs){
  using (SqlCommand cmd =(SqlCommand) this.Connection.CreateCommand())
	            {
	                //cmd.Transaction = trans;
	                DataTable dt = new DataTable();
	
	                dt.Columns.Add(new DataColumn("int", typeof(int)));
	
	                foreach (var i in orgs)
	                {
	                    DataRow dr = dt.NewRow();
	
	                    dr[0] = i;
	                    
	                    dt.Rows.Add(dr);
	                }
	
	                cmd.CommandText = "[project].[ProjectSupplier_Merge]";
					
	                cmd.CommandType = CommandType.StoredProcedure;
					cmd.Parameters.AddWithValue("@projectId",projectId);
					cmd.Parameters.AddWithValue("@orgs",dt);
					
					Debug.Assert(cmd.Parameters.Count==2);
					if(this.Connection.State!= ConnectionState.Open)
	                this.Connection.Open();
	                return cmd.ExecuteNonQuery();
	            }
}