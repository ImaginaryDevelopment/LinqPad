<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <Namespace>Microsoft.Win32</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>Microsoft.Win32.SafeHandles</Namespace>
</Query>

void Main()
{
	bool debug=false;
	
	
	HandleEnvironmentVariables(debug);
	
	HandleApps(debug);
	HandleGitBash(debug);
	
	HandleSites(_payspanRoot, HealthMode.Interactive,debug);
//	Util.RawHtml("<script src='http://cdnjs.cloudflare.com/ajax/libs/knockout/2.1.0/knockout-min.js'></script>").Dump();
//	Util.RawHtml(@"<script type='text/javascript'>var healthModel= function HealthModel(){
//		this.env=ko.observable(false);
//		this.apps=ko.observable(false);
//		this.bash=ko.observable(false);
//	};
//	ko.applyBindings(new healthModel());</script>").Dump();
}
public enum HealthMode{
Interactive,
True,
False,
}

#region EnvironmentVars
string _payspanRoot=@"C:\Projects\psh\hpx";
string _payspanEndpoint;
const string PayspanEnv="PSH_ENVIRONMENT";
const string PayspanConfigEndpoint="PSH_CONFIG_ENDPOINT";
const string PayspanConfigDb="PSH_CONFIG_DB_CONNECTION";

// Define other methods and classes here
public  void HandleEnvironmentVariables(bool debug)
{
	var paths=Environment.GetEnvironmentVariable("PATH").Split(new[]{';'}, StringSplitOptions.RemoveEmptyEntries);
	if(paths.Any (p => p.Contains("\"")))
		Util.Highlight("Quotes cause errors for VS command prompt in Windows 7").Dump();
	if(paths.Any (p => p.Contains("SysInternals"))==false)
		Util.Highlight("SysInternals not in path");
	foreach(var path in paths)
	{
		if(System.IO.Directory.Exists(path)==false)
			Util.Highlight(path).Dump("path entry not found");
		
	}
	this._payspanEndpoint=Environment.GetEnvironmentVariable(PayspanConfigEndpoint);
	HandlePshConfigEp(_payspanEndpoint);
	
	
}

public static void HandlePshConfigEp(string endpoint)
{
	if(endpoint.IsNullOrEmpty()){
		"endpoint not found".Dump(PayspanConfigEndpoint);
		return;
	}
	var epUri=new Hyperlinq(endpoint);
	epUri.Dump("Config Endpoint");
}


#endregion

public static void HandleSites(string payspanRoot,HealthMode mode,bool debug)
{
	//http://stackoverflow.com/questions/1619308/how-do-i-change-the-physical-path-of-web-site-in-iis7-with-appcmd
	const string path=@"C:\Windows\System32\inetsrv\appcmd.exe"; //http://blogs.msdn.com/b/sukeshak/archive/2006/05/24/605911.aspx
	if(System.IO.File.Exists(path)==false)
	{
		Util.Highlight(path).Dump("Not found");
		return;
	}
	using(var ps=new Process())
	{
	ps.StartInfo.UseShellExecute=false;
		ps.StartInfo.RedirectStandardOutput=true;
		ps.StartInfo.RedirectStandardError=true;
		ps.StartInfo.FileName=path; //appcmd list app /config
		var outputs=ps.RunProcessRedirected("list apps /config /xml"); // for debug consider adding /path:/jms
		if(outputs.Errors.IsNullOrEmpty()==false)
		{
			Util.Highlight(outputs.Errors).Dump("Error running appcmd");
		}
		
		var config=XDocument.Parse(outputs.Output);
		if(config.Root.IsEmpty)
			throw new InvalidOperationException();
		if(debug) config.DumpFormatted();
		var isJunction=payspanRoot.IsNullOrEmpty()==false && PInvokeWrapper. JunctionExists(payspanRoot);
		
		var q= from siteApp in config.XPathSelectElements("appcmd/APP")
			let appName=siteApp.Attribute(XNamespace.None+"APP.NAME").Value
				from app in siteApp.XPathSelectElements("application")
			let appPath=app.Attribute(XNamespace.None+"path").Value
			let pool=app.GetAttribValOrNull("applicationPool")
			let vd=app.XPathSelectElements("virtualDirectory[@path]")
			let virtuals=vd.Select (v => new{VirDir=v.Attribute(XNamespace.None+"path").Value,PhysicalPath=v.Attribute(XNamespace.None+"physicalPath").Value})
			let xvirtuals=virtuals.Select (v => new{ VirDir=v.VirDir,
				PhysicalPath=v.PhysicalPath,
				DisplayPhysicalPath=Util.HighlightIf(v.PhysicalPath,f=>f.StartsWith(payspanRoot)==false),
				ValidRoot=(isJunction && v.PhysicalPath.StartsWith(payspanRoot)),
				Exists=Directory.Exists(v.PhysicalPath)}) //valid meaning consistent with team standards
			select new{AppName=appName,AppPath=appPath, Pool=pool,Virtuals=xvirtuals};
		var allEntries=q.ToList();
		if(debug) allEntries.Dump("all entries");
		
		var suspects=allEntries.Where (e => e.Virtuals.Any (v =>v.ValidRoot==false));
		
		if(payspanRoot.IsNullOrEmpty()){
			suspects.Dump("suspects");
			"unable to process app paths, no payspanRoot variable found".Dump();
			return;
		}
		//items.Dump();
		var payspanUri= new Uri(payspanRoot);
		if(suspects.Any()){	
				foreach(var badItem in suspects)
				foreach(var virt in badItem.Virtuals.Where (v =>v.ValidRoot==false))
				{
					if(virt.PhysicalPath.ToLower().Contains("projects")==false)
					{
						Util.Highlight(new{badItem.AppPath,badItem.Pool,virt.DisplayPhysicalPath,virt.VirDir}).Dump("unable to determine adjustment for path");
						continue;
					}
					//all items before projects
					var payspanBase= payspanRoot.Before("projects", StringComparison.CurrentCultureIgnoreCase);
					var payspanSuffix=payspanRoot.After("projects", StringComparison.CurrentCultureIgnoreCase);
					var proposedUri=payspanUri.MakeRelativeUri(new Uri(virt.PhysicalPath));
					var segments=SegmentPath(proposedUri.ToString()).ToArray();
					
					if(System.IO.Path.GetDirectoryName(proposedUri.ToString())!=System.IO.Path.GetDirectoryName(payspanRoot))
					segments=segments.Skip(1).ToArray();
					var basedProposed=segments.Prepend(payspanRoot).ToArray();
					
					var proposedPath=System.IO.Path.Combine(basedProposed);
					
					var display=new{badItem.AppName,AppPath=badItem.AppPath,ProposedPath=proposedPath,Proposed=segments.Aggregate (System.IO.Path.Combine ).ToString(),virt.VirDir,
						virt.PhysicalPath,ProposedExists=System.IO.Directory.Exists(proposedPath)};
					display.Dump();
					var relPath=segments.Aggregate (System.IO.Path.Combine);
					
					var args="set vdir \""+badItem.AppName+virt.VirDir
						+"\" -physicalPath:\""+ System.IO.Path.Combine(payspanRoot,relPath)+"\"";
							
					
					if(display.ProposedExists)
					{
							StreamOuts outs;
							if (System.IO.Directory.Exists(virt.PhysicalPath)==false)
							{
								args.Dump("running appcmd");
								outs=ps.RunProcessRedirected(args);
								outs.Dump("ran appcmd");
							}
							else{
								
								args.Dump("proposed appcmd");
								if(mode== HealthMode.True || (mode== HealthMode.Interactive && Util.ReadLine<bool>("Run update?"))){
									outs=ps.RunProcessRedirected(args);
									outs.Dump("ran appcmd");
								}
							}
							
						
					} else{
						Util.Highlight(new{badItem.AppPath,badItem.Pool,virt.DisplayPhysicalPath,virt.VirDir}).Dump("proposed does not exist");
					}
					
				
				}
		
		}
	}
	
}

public static IEnumerable<string> SegmentPath(string input)
{
	return input.Split(new[]{System.IO.Path.AltDirectorySeparatorChar,System.IO.Path.DirectorySeparatorChar });
}

public static void HandleGitBash(bool debug)
{
	var profilePath=System.Environment.GetFolderPath( System.Environment.SpecialFolder.UserProfile);
	var gitrc=System.IO.Path.Combine(profilePath,".bashrc");
	if(File.Exists(gitrc)==false)
	{
		gitrc.Dump("Not found");
		return;
	}
	var rcLines=File.ReadAllLines(gitrc);
	if(rcLines.Any (l => l.StartsWith("alias "))==false)
		("Suggested aliases:"+Environment.NewLine+
		"alias dir='ls -l'"+Environment.NewLine+
		"alias gr='git remote -v'").Dump();
	
}

public static void HandleApps(bool debug){

	var uninstallKey=Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall");
	var installed=uninstallKey.GetSubKeyNames()
	.Select (k => uninstallKey.OpenSubKey(k))
	.Where (appKey=>appKey.GetValueNames().Contains("DisplayName"))
	.Select (appKey => appKey.GetValue("DisplayName").ToString());
	var vs2012=installed.FirstOrDefault (i =>Regex.IsMatch(i,"Microsoft Visual Studio \\w+ 2012" ) );
	if(vs2012==null)
		"Visual Studio 2012 not found".Dump();
		else if (debug)
		vs2012.Dump();
		if(installed.FirstOrDefault (i => i.StartsWith("MagicDisc "))==null)
			"MagicDisc not found".Dump();
			if(debug)
	installed.Dump();
}

