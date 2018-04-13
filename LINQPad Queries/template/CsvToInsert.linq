<Query Kind="FSharpExpression" />

let split (d:string list) (x:string) = x.Split(Array.ofList d, StringSplitOptions.None)
let delimit d (x:string seq) = String.Join(d,x)
let trim (x:string) = x.Trim()
let replace (d:string) r (x:string) = x.Replace(d,r)
let tableName = "DCReferralPhysicians"
let text = """



ReferringPhysicianID,SpecialtyID,FirstName,MiddleName,LastName,Title,Phone,Fax,Address,Address2,City,State,Zip,Email,Note
661,54,William,,Mendenhall,MD,(352) 265-0287,(352) 265-0759,P.O. Box 100385,2000 SW Archer Road,Gainesville,FL,32610,,Radiation Oncology
932,13,Kent,,Wehmeier,MD,633-0411,244-5650,655 West 8th Street,"ACC Building, Lower Level",Jacksonville,FL,32209,,"Endocrinology, Diabetes & Metabolism"
661,5,William,,Mendenhall,MD,(352) 265-0287,(352) 265-0759,P.O. Box 100385,2000 SW Archer Road,Gainesville,FL,32610,,






"""
let trim (x:string) = x.Trim()
let walkIt (s:string) =
    Seq.unfold(
        function
        | null -> None
        | "" -> Some(String.Empty,null)
        | x -> 
            try
                let x = trim x
                if x.[0] = '"' then
                    let quoted = x |> Seq.skip 1 |> Seq.takeWhile(fun x -> x <> '"') |> Array.ofSeq |> String
                    // 2 = skip the starting and closing "
                    let remainder = trim x.[quoted.Length + 2 ..]
                    if remainder.Length > 0 then 
                        if remainder.[0] <> ',' then
                            (x,quoted,remainder).Dump("is bad mkay?")
                            failwith "bad quoted input"
                        Some(quoted,trim remainder.[1..])
                        
                    else Some(quoted,null)
                    
                elif x.[0] = ',' then
                    if x.Length > 1 && x.[1] = ',' then
                        Some(String.Empty,x.[2..])
                    else Some("",x.[1..])
                else 
                    let unquoted = x |> Seq.takeWhile(fun x -> x <> ',') |> Array.ofSeq |> String               
                    let r = 
                        if x.Length > unquoted.Length && x.[unquoted.Length] = ',' then
                            x.[unquoted.Length + 1 ..]
                        else
                            null
                         
                    (x,unquoted,r).Dump("remainder")
                    Some(unquoted,r) 
                |> Option.map (fun (u,r) ->  
    //                (x,u,r).Dump("remainder")
                    if x = r then failwithf "did not eat anything, starving"
                    u,r
                    )
            with ex -> 
                ex.Dump("bad")
                reraise()
            
    ) s

//let textComma = "&comma;"
//let substitute = sprintf "%sMD" textComma
let lines = 
    text |> trim |> split [ "\r\n"; "\n";"\r"] (* |> Seq.map (replace ",MD" substitute) *) |> Seq.map trim |> Array.ofSeq
let headers = lines.[0] |> split [","] |> Array.skip 1 |> Array.map trim

let data = lines.[1..]
let valueMap = 
    function
    | null 
    | ""
    | "NULL" -> "null"
    | x when x |> Seq.forall System.Char.IsNumber -> x
    | x -> x |> replace "'" "''" (* |> replace textComma "," *) |> sprintf "'%s'"
let expectedCount = headers.Length

let assertLength expected (a:_[]) =
    if expected <> a.Length then
        Util.HorizontalRun(false,headers,a).Dump("failing")
        failwithf "bad data found expected %i fields, but row contained %i" expected a.Length
    a
    
let headerText = headers |> delimit ", "
let f = Array.map (trim >> valueMap)
data
|> Seq.map(walkIt >> Seq.skip 1 >> Array.ofSeq >> f >> assertLength expectedCount >> delimit ",")
|> Seq.map(fun x -> sprintf "insert into %s (%s) values(%s)" tableName headerText x)
|> delimit "\r\n"