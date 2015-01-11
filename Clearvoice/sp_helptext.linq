<Query Kind="Program">
  <Connection>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

#if CMD
void Main(string[] args)
#else
static string[] args = new[] {
	};
void Main()
#endif
{
	sys.sp_helptext(args[0]).Dump();
	
	
	//sys.sp_help("admin.org_2_ipaddress")
}

// Define other methods and classes here
