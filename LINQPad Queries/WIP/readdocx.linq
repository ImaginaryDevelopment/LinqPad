<Query Kind="FSharpProgram">
  <NuGetReference>DocX</NuGetReference>
  <Namespace>Novacode</Namespace>
</Query>

@"C:\Users\Brandon\Downloads\face_sample.docx"
|> DocX.Load
|> Dump
