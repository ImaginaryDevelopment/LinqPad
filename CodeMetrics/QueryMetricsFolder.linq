<Query Kind="Statements">
  <Reference Relative="..\Visual Studio 2012\Projects\CodeMetricsAdapter\CodeMetricsAdapter\bin\Release\CodeMetricsAdapter.exe">&lt;MyDocuments&gt;\Visual Studio 2012\Projects\CodeMetricsAdapter\CodeMetricsAdapter\bin\Release\CodeMetricsAdapter.exe</Reference>
</Query>

 var sourceDirectory = System.IO.Path.Combine(
			 System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop), "measures");
var mfw= new CodeMetricsAdapter.MetricFilesWalker(sourceDirectory);
 var targets = mfw.WalkFiles();
 var notOursPath=new[]{"ICSharpCode.SharpZipLib.dll",
 	"EntityFramework.dll",
 	"LumenWorks.Framework.IO.dll",
	"itextsharp.dll",
	"eWorld.UI.dll",
	"SevenZip.dll","dotless.Core.dll",
	"ActiveReports.HtmlExport.dll",	"ActiveReports.PdfExport.dll","ActiveReports.Viewer.dll","activereports.web.dll","ActiveReports.Viewer3.dll","ActiveReports.Viewer3.dll",
	"log4net.dll",
	};
var notOursNs= new[]{string.Empty
//"SevenZip.Compression.LZMA", "eWorld.UI","System.util.zlib",
//"Org.BouncyCastle.Security",
//"Org.BouncyCastle.Crypto.Engines",
//"Org.BouncyCastle.Utilities.Zlib",
//"Org.BouncyCastle.Crypto.Digests",
//"Org.BouncyCastle.Asn1.Utilities",
};
var notOursC=new[]{
"Microsoft",
"EldoS Corporation",
"Enterprise Distributed Technologies",
};
var filtered= from t in targets
					where t!=null
					from m in t.Modules
					where notOursPath.Contains(m.Path)==false
					from n in m.Namespaces
					where notOursNs.Contains(n.Name)==false
					from ty in n.Types
					from mem in ty.Members
					let cName=t.GetCompanyName().Value
					where cName==null ||notOursC.Contains(cName)==false
					//where mem.Metrics.Mi < 30
					select new{mem.Name,mem.Metrics,CompanyName=t.GetCompanyName().Value,t.FullPath,m.Path,NamespaceName=n.Name,TypeName=ty.Name};
					
			var q = from f in filtered
					where f.Metrics.Mi < 30
					select f;



					var outQ=q.GroupBy (x => x.CompanyName).OrderBy (x => x.Key).Dump();
					var ccIssues = from f in filtered
					where f.Metrics.Cc>50
					select f;
					ccIssues.GroupBy (x => x.CompanyName).OrderBy (x => x.Key).Dump();
		q.Count ( ).Dump();
		//q.Dump();

