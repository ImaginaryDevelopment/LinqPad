<Query Kind="Statements">
  <Connection>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
  <Namespace>System.Data.Linq.Mapping</Namespace>
</Query>

// generate merge from table
var rowType = table.GetType().GetGenericArguments()[0];
var tableName = rowType.GetCustomAttribute<TableAttribute>().Name;

var tableColumns = rowType
	.GetFields(BindingFlags.Instance | BindingFlags.Public )
	.Select(p=>new{p.Name,Getter= ((Func<object,object>) (r => p.GetValue(r))),ColumnAttr= p.GetCustomAttribute<ColumnAttribute>()});
	
string.Format("--merge statement for table {0}",tableName).Dump();
string.Format("merge into {0} as target{1}using (values",tableName,Environment.NewLine).Dump();
var identity = tableColumns.FirstOrDefault(c => c.ColumnAttr !=null && c.ColumnAttr.IsDbGenerated && c.ColumnAttr.CanBeNull == false);
if(identity !=null)
	string.Format("set identity_insert {0} ON{1}go",tableName,Environment.NewLine).Dump();
	
var values = table.ToArray().Select(r=> string.Format("\t({0})",string.Join(",", 
	tableColumns
		.Select(tc => tc.Getter(r))
		.Select(v => v != null ? v.ToString():(string)null)
	)));
string.Join(","+Environment.NewLine,values).Dump();
var columnNames = tableColumns.Select(tc =>tc.ColumnAttr.Name).ToArray();
var columnList = string.Join(",", columnNames);
string.Format("){0}\tas source({1}){0}\ton target.{2} = source.{2}{0}",Environment.NewLine,columnList,tableColumns.First(tc => tc.ColumnAttr.IsPrimaryKey).ColumnAttr.Name).Dump();
string.Format("\twhen not matched by target then{0}\t\tinsert({1}){0}\t\tvalues({2})",Environment.NewLine, columnList, string.Join(",",columnNames.Select(cn => "source."+cn))).Dump();
string.Format("\twhen not matched by source then DELETE{0}{0}\t--what comes out of the merge console{0}OUTPUT $action, inserted.*, deleted.*;{0}go{0}",Environment.NewLine).Dump();
if(identity !=null)
	string.Format("set identity_insert {0} OFF{1}go",tableName, Environment.NewLine).Dump();
