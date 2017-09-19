<Query Kind="FSharpProgram" />

dc.Users.Dump()
let uId = 3
let upas = dc.UserPasswordArchives.Where(fun upa -> upa.UserId = uId).ToList()
upas.Dump()
dc.UserPasswordArchives.DeleteAllOnSubmit(upas)

dc.SubmitChanges()

let u = dc.Users.First(fun x -> x.UserID = uId)
u.Dump()
