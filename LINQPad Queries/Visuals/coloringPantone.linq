<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll</Reference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
</Query>

// https://www.labelpartners.com/pantone_coated_table.html

type ColorsTable = HtmlProvider<"https://www.labelpartners.com/pantone_coated_table.html">

let colorsData = ColorsTable.Load("https://www.labelpartners.com/pantone_coated_table.html")
type ColorMap = { Pantone:string; Hex:string; R:byte; G:byte; B:byte}
colorsData.Tables.``All Pantone C colors with HEX and RGB codes``
|> fun x -> 
    x.Rows.Length.Dump()
    x.Rows
    |> Seq.map(fun r -> {Pantone=r.``PANTONE Coated``; Hex=r.HEX; R=byte r.R; G = byte r.G; B= byte r.B})
    

|> Dump
|> ignore
