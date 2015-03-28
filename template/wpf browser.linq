<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

var control = new System.Windows.Controls.WebBrowser();
var html = File.ReadAllText(targetDoc);
control.MinHeight= 600;
control.Dump();

control.NavigateToString(html);

//control.Navigate(targetDoc); // security issues