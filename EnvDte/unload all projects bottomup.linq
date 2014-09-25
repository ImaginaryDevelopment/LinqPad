<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

void Main()
{
	var skips=new string[]{};
	bool topdown=false;
	const string SolutionExplorerWindow="{3AE79031-E1BC-11D0-8F78-00A0C9110057}";
	const string SolutionFolder="{66A26720-8FB5-11D2-AA7E-00C04F688DDE}";
	//EnvDTE.DTE
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.12.0");
	dte.Dump();
	dte.FileName.Dump();
	dte.Solution.FullName.Dump();
	dte.Windows.Item(SolutionExplorerWindow).Activate();
	var uih=dte.ActiveWindow.Object as UIHierarchy;
	var failures=new List<object>();
	var unloaded=new List<object>();
	var solutionItem=uih.UIHierarchyItems.Cast<UIHierarchyItem>().Single ();
	ComHelper.GetTypeName(solutionItem).Dump("solutionItem Type");
	foreach(var uiProject in GetChildItems(solutionItem,topdown))
	{
		object display;
		try
		{	        
			display=GetDisplay(uiProject);
		}
		catch (InvalidCastException ex)
		{
			continue;
		
		}
		
		var comType=ComHelper.GetTypeName(uiProject.Object);
		
		uiProject.Select(vsUISelectionType.vsUISelectionTypeSelect);
		
		try
		{	        
			dte.ExecuteCommand("Project.UnloadProject");
			unloaded.Add(display);
		}
		catch (COMException ex)
		{
			if (object.ReferenceEquals( uiProject.Object,uiProject)==false) //assume project is already unloaded
			{
				failures.Add(Util.HorizontalRun(false, display,ex));			
			}
			//object display=new{Object= uiProject.Object.ToString(),ComTypeName=comType};
		}
	}
	
	if(failures.Any ( ))
			failures.Dump("failures");
	unloaded.Dump("unloaded");
	
}

public object GetDisplay(UIHierarchyItem item)
{

	object display;
	var itemObject=item.Object;
	var comType=ComHelper.GetTypeName(itemObject);
	display=new{ComType=comType,Display=item.ToString()};
			if(comType=="Project")
			{
				var p = item.Object as Project;
				
				if(p!=null)
					display=new{p.Kind, p.UniqueName,p.FileName,p.FullName,};
				
				
			} else if(comType=="_UIHierarchyItemMarshaler")
			{
				if(object.ReferenceEquals(item.Object,item))
					return display;
				var uihi=(UIHierarchyItem) item.Object;
				ComHelper.GetTypeName(uihi.Object).Dump("Found child of uihi");
			} else {
				comType.Dump("Failed to unload");
			}
	return display;
}

public IEnumerable<UIHierarchyItem> GetChildItems(UIHierarchyItem parent,bool topdown)
{
	
	if(parent.Collection.Count>0)
		foreach(var item in parent.UIHierarchyItems.Cast<UIHierarchyItem>())
		{//.Where (uihi => uihi.Object is Project))
			if(topdown)
				yield return item;
			foreach(var ic in GetChildItems(item,topdown))
				yield return ic;
			if(!topdown)
				yield return item;
		}
	yield break;
}
 
    public class ComHelper 
    { 
        /// <summary> 
        /// Returns a string value representing the type name of the specified COM object. 
        /// </summary> 
        /// <param name="comObj">A COM object the type name of which to return.</param> 
        /// <returns>A string containing the type name.</returns> 
        public static string GetTypeName(object comObj) 
        { 
 
            if (comObj == null) 
                return String.Empty; 
 
            if (!Marshal.IsComObject(comObj)) 
                //The specified object is not a COM object 
                return String.Empty; 
 
            IDispatch dispatch = comObj as IDispatch; 
            if (dispatch == null) 
                //The specified COM object doesn't support getting type information 
                return String.Empty; 
 
            System.Runtime.InteropServices.ComTypes.ITypeInfo typeInfo = null; 
            try 
            { 
                try 
                { 
                    // obtain the ITypeInfo interface from the object 
                    dispatch.GetTypeInfo(0, 0, out typeInfo); 
                } 
                catch (Exception ex) 
                { 
				ex.Dump();
                    //Cannot get the ITypeInfo interface for the specified COM object 
                    return String.Empty; 
                } 
 
                string typeName = ""; 
                string documentation, helpFile; 
                int helpContext = -1; 
 
                try 
                { 
                    //retrieves the documentation string for the specified type description 
                    typeInfo.GetDocumentation(-1, out typeName, out documentation, 
                        out helpContext, out helpFile); 
                } 
                catch (Exception ex) 
                { 
                    // Cannot extract ITypeInfo information 
                    return String.Empty; 
                } 
                return typeName; 
            } 
            catch (Exception ex) 
            { 
                // Unexpected error 
                return String.Empty; 
            } 
            finally 
            { 
                if (typeInfo != null) Marshal.ReleaseComObject(typeInfo); 
            } 
        } 
    } 
 
    /// <summary> 
    /// Exposes objects, methods and properties to programming tools and other 
    /// applications that support Automation. 
    /// </summary> 
    [ComImport()] 
    [Guid("00020400-0000-0000-C000-000000000046")] 
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)] 
    interface IDispatch 
    { 
        [PreserveSig] 
        int GetTypeInfoCount(out int Count); 
 
        [PreserveSig] 
        int GetTypeInfo( 
            [MarshalAs(UnmanagedType.U4)] int iTInfo, 
            [MarshalAs(UnmanagedType.U4)] int lcid, 
            out System.Runtime.InteropServices.ComTypes.ITypeInfo typeInfo); 
 
        [PreserveSig] 
        int GetIDsOfNames( 
            ref Guid riid, 
            [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPWStr)] 
            string[] rgsNames, 
            int cNames, 
            int lcid, 
            [MarshalAs(UnmanagedType.LPArray)] int[] rgDispId); 
 
        [PreserveSig] 
        int Invoke( 
            int dispIdMember, 
            ref Guid riid, 
            uint lcid, 
            ushort wFlags, 
            ref System.Runtime.InteropServices.ComTypes.DISPPARAMS pDispParams, 
            out object pVarResult, 
            ref System.Runtime.InteropServices.ComTypes.EXCEPINFO pExcepInfo, 
            IntPtr[] pArgErr); 
    }