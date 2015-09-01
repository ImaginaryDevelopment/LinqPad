<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.AccountManagement.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Namespace>System.DirectoryServices.AccountManagement</Namespace>
</Query>

using (var context = new PrincipalContext(ContextType.Domain))

using (var groupPrincipal = new GroupPrincipal(context))

using (var principalSearcher = new PrincipalSearcher(groupPrincipal))

using (var result = principalSearcher.FindAll())

{
result.Dump(1);

}