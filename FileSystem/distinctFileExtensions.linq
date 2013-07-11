<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

string path;
using (var fbd=new System.Windows.Forms.FolderBrowserDialog())
{
	if(fbd.ShowDialog()!= System.Windows.Forms.DialogResult.OK)
		return;
	path=fbd.SelectedPath;
}

//a GUESS only, on what is on the real blacklist that pertains to you
var blacklist= new[]{".exe",".bat",".dll"};

var allextensions=System.IO.Directory.GetFiles(path,"*.*", SearchOption.AllDirectories)
	.Select(f=>System.IO.Path.GetExtension(f)).Distinct().ToArray();
	Func<string,bool> predicate= ext=>blacklist.Any(bl=>ext.EndsWith(bl, StringComparison.CurrentCultureIgnoreCase));
var inblacklist= allextensions.Where(predicate).OrderBy(e=>e).Dump("on blacklist");

var unknown=allextensions.Except(inblacklist).OrderBy(e=>e).Dump("other");

