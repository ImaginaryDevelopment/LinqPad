


type Person = {PersonId:int; FirstName :string; LastName:string}


type Employee = {Person:Person; Employees: Person list}

type PersonObj () = 
    member val PersonId= 0 with get,set
    member val FirstName = "" with get,set
    member val LastName = "" with get, set
    member x.FullName = x.FirstName + x.LastName

let john = {PersonId=1; FirstName="John";LastName="Doe"}
type EmployeeObj (person:Person) =
    member val Person = person with get,set
    member val Employees: Person list = List.empty with get,set
    //no idea where this syntax works outside of inheritance, if anywhere
    //new (x) = {inherit EmployeeObj(x); Employees = list.Empty}
    new () = 
        EmployeeObj(john)
            then
                System.Diagnostics.Debug.WriteLine("constructor alternate!")


let johnMgr = {Person=john; Employees = list.Empty}
let johnObj = EmployeeObj(john,Employees= list.Empty)

open System
open System.Linq
let getValidFileName (filepath:string) = 
    let invalids = System.IO.Path.GetInvalidFileNameChars()// |> Seq.map string |> Array.ofSeq
    let result = System.String.Join("_", filepath.Split(invalids,StringSplitOptions.RemoveEmptyEntries)).TrimEnd()
    result
