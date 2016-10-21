<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\PresentationCore.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\PresentationFramework.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Windows.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Xaml.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\WindowsBase.dll</Reference>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Data</Namespace>
</Query>

// come back and add some other content, that tries to bind to ancestor of type Window, is that null?


type Window2() = 
    inherit Window()
let x = Window2()
let sp = StackPanel()
let label = Label()

// does this button live on the visual or logical tree of its parent?
let bCancel = System.Windows.Controls.Button()

bCancel.Content <- "Cancel"
bCancel.IsCancel <- true
let bd = Binding() // some more info on binding in code: http://stackoverflow.com/questions/7525185/how-to-set-a-binding-in-code
bd.Source <- bCancel.Parent


sp.Children.Add label |> ignore<int>
sp.Children.Add bCancel |> ignore<int>
label.Content <- sprintf "button parent is null? %A" (isNull b.Parent)
x.Content <- sp
x.Dump()