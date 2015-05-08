<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>EnvDTE80, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>EnvDTE90, Version=9.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.TeamFoundation.Controls, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.OLE.Interop, Version=7.1.40304.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Shell.12.0, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Shell.Interop, Version=7.1.40304.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.Shell.Interop.12.0, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.TeamFoundation, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.TeamFoundation.Client, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <GACReference>Microsoft.VisualStudio.TeamFoundation.PCW, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>EnvDTE80</Namespace>
  <Namespace>EnvDTE90</Namespace>
  <Namespace>Microsoft.TeamFoundation.Controls</Namespace>
  <Namespace>Microsoft.VisualStudio</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell.Interop</Namespace>
  <Namespace>Microsoft.VisualStudio.TeamFoundation</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

const string SolutionFolder="{66A26720-8FB5-11D2-AA7E-00C04F688DDE}";
const string ProjectGuid="{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";

void Main()
{
//projects to keep loaded

	const string SolutionExplorerWindow="{3AE79031-E1BC-11D0-8F78-00A0C9110057}";
	
	var pcId = TeamExplorerPageIds.PendingChanges;
	
	//EnvDTE.DTE
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.12.0");
	var sp = new ServiceProvider((Microsoft.VisualStudio.OLE.Interop.IServiceProvider)dte);
	
	
	
	IEnumerable<Type> types = typeof(Microsoft.VisualStudio.TeamFoundation.Catalogs).Assembly.GetExportedTypes();
	types = types.Concat(typeof(Microsoft.TeamFoundation.Controls.AdminAreaIterationHelper).Assembly.GetExportedTypes());
	
	foreach(var type in types)
	{
	try
	{	        
	
		var obj = dte.GetObject(type.FullName).Dump();	
		type.FullName.Dump();
		ComHelper.GetTypeName(obj).Dump("comhelper helped");
	}
	catch (Exception ex)
	{
		
	}
	try
	{	        
		var obj = sp.GetService(type);
		if(obj!=null){
		new{ Object = obj, Name = type.Name, type.FullName, TypeName = ComHelper.GetTypeName(obj),Type=type}.Dump("Sp!");
		}
	}
	catch (Exception ex)
	{
		
		throw;
	}
	}
	
	//dte.Windows.Item(SolutionExplorerWindow).Activate();
	
	types.GroupBy(t=>t.Assembly.Location).Dump("types found");
	types.Dump("types found");
		
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