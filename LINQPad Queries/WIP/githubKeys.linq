<Query Kind="Statements">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

var oAuth = Util.GetPassword("GitHubOAuth");
var clientId = Util.GetPassword("GitHubClientId");
var clientSecret = Util.GetPassword("GitHubSecret");

//var gitHubCn = new CSharp.GitHub.Connect.GitHubServiceProvider(clientId, clientSecret);
//var gitHubClient = gitHubCn.GetApi(oAuth);
//var result = gitHubClient.RestOperations.GetForObject<JObject>("https://api.github.com/user/repos");
//result.Dump();