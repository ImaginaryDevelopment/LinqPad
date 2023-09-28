<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

// generate constraint name
let read prompt =
    Util.ReadLine(prompt)
// fkey table is is the one that is the primary key table
let fKeyTable,fKeyColumn = read "FKey from table?", read "FKey column?"
let refTable,refColumn = read "Reference Table?", read "Reference column?"

//TODO: account for non-dbo schema tables
sprintf "CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [dbo].[%s] ([%s])" fKeyTable fKeyColumn refTable refColumn fKeyColumn refTable refColumn
|> fun s-> s.Dump("constraint")

sprintf "alter table %s add CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [dbo].[%s] ([%s])" fKeyTable fKeyTable fKeyColumn refTable refColumn fKeyColumn refTable refColumn
|> fun s -> s.Dump("add")



// Users_UserID_Providers_AddedBy] FOREIGN KEY ([UserID]) REFERENCES [dbo].[Providers] ([AddedBy])