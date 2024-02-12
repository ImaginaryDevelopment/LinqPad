<Query Kind="FSharpExpression" />

//gen1DayPwd

let forDate = DateTime.Today
let m = forDate.Month * 2

sprintf "%i%i" (forDate.Day + 7) (m)