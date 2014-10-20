<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>EnvDTE80, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>EnvDTE90, Version=9.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.TeamFoundation.Client, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.OLE.Interop, Version=7.1.40304.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Platform.WindowManagement, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Shell.12.0, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Shell.Interop, Version=7.1.40304.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Shell.Interop.12.0, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Com.Helper</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>EnvDTE80</Namespace>
  <Namespace>EnvDTE90</Namespace>
  <Namespace>Microsoft.VisualStudio</Namespace>
  <Namespace>Microsoft.VisualStudio.OLE.Interop</Namespace>
  <Namespace>Microsoft.VisualStudio.Platform.WindowManagement.DTE</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell.Interop</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

const string SolutionFolder="{66A26720-8FB5-11D2-AA7E-00C04F688DDE}";
const string ProjectGuid="{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";

void Main()
{

	const string TeamExplorerWindow ="{131369F2-062D-44A2-8671-91FF31EFB4F4}";
	// http://www.mztools.com/articles/2006/MZ2006015.aspx
	const string SolutionExplorerWindow="{3AE79031-E1BC-11D0-8F78-00A0C9110057}";
	
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.12.0");
	Microsoft.VisualStudio.Shell.Interop.IVsUIHierarchyWindow hierarchy;
	ServiceProvider sp = new ServiceProvider((Microsoft.VisualStudio.OLE.Interop.IServiceProvider)dte);
	dte.Windows.Item(TeamExplorerWindow).Activate();
	//var uih=dte.ActiveWindow.Object as UIHierarchy;
	
	hierarchy=(IVsUIHierarchyWindow) sp.GetService(typeof(IVsUIHierarchyWindow));
	//hierarchy.Dump("hier");
	
	//dte.Windows.Cast<dynamic>().Select(d=>d.ToString()).Dump();
	//	uih.GetItem(".Dump();
	//dte.Windows.Item(SolutionExplorerWindow).Activate();
	//while(true){
	var activeWindow = dte.ActiveWindow;
	ComHelper.GetTypeName(activeWindow).Dump();;
	var windowBase = dte.ActiveWindow as EnvDTE.Window;
	var assemblies = System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies();
	assemblies.Count().Dump("searching assemblies");
	var totalTypesChecked = 0;
	
	foreach(var asm in assemblies){
	
	foreach(var type in asm.GetType().Assembly.GetExportedTypes()){
		totalTypesChecked++;
		try
		{	        
			Convert.ChangeType(windowBase,type).Dump("Convert success");
		}
		catch {}
	}
	}
	
	var persist = windowBase as IPersist;
	if (persist!=null) persist.Dump("yay persist!");
	
	totalTypesChecked.Dump("tried types");
	new {
	
		windowBase.Caption,
		windowBase.Object, 
		windowBase.Type, 
		windowBase.Kind, 
		windowBase.ObjectKind, 
		windowBase.Selection,
		windowBase.Project,
		windowBase.Document,
		windowBase.Linkable,
		Ca=windowBase.ContextAttributes.Cast<ContextAttribute>().Select(ca=> ca.Values),
		linkedWindowFrameCaption = ((EnvDTE.Window)windowBase.LinkedWindowFrame).Caption,
		windowBase.LinkedWindows,
		windowBase.DocumentData,
		windowBase.ProjectItem,
		@string=windowBase.ToString(),
		CollectionCaption = windowBase.Collection.Cast<EnvDTE.Window>().Select(w=>w.Caption)
		}.Dump();
		
	assemblies.Select(a=>a.CodeBase).Dump();
	dynamic windowDyn = windowBase;
	//windowDyn.Dump();
	//windowDyn..Dump();
	dte.ActiveWindow.Document.Dump();
		dte.ActiveWindow.ContextAttributes.Dump("attributes");
	//System.Threading.Thread.Sleep(1000);
	//}
	////Microsoft.VisualStudio.Shell.Interop.IVsUIHierarchyWindow hierarchy;
	
	//var slnUI=uih.GetItem(slnName).Dump("uih getItem");
	//var slnFolderUI=slnUI.UIHierarchyItems.Item("1. Web Sites").Dump("child ui item");
	
	
		
}

 