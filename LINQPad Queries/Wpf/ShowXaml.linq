<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Windows.Presentation.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
</Query>

// see also http://forum.linqpad.net/discussion/555/xaml-support

let label = System.Windows.Controls.Label()
let binding = System.Windows.Data.Binding()
//binding.StringFormat <- "g"

label.DataContext <- 103.10m
System.Windows.Data.BindingOperations.SetBinding(label, System.Windows.Controls.Label.ContentProperty, binding)
//label.Content <- binding
label.ContentStringFormat <- "N2"
PanelManager.DisplayWpfElement(label :> Windows.UIElement, "PanelTitle")
//PanelManager.StackWpfElement(label :> Windows.UIElement).Dump()
//label.Dump()
