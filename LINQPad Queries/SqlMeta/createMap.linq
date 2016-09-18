<Query Kind="FSharpExpression">
</Query>

let stringJoin delimiter (items:#seq<String>) = String.Join(delimiter,values = (Array.ofSeq items))
let isTableType (t:Type) = 
    let isFKey =
        t.GetCustomAttribute<System.Data.Linq.Mapping.TableAttribute>()
        |> isNull
        |> not
    let isPKey = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<System.Data.Linq.EntitySet<_>>
    isFKey || isPKey
    
let getLinqTypeMembers (t:Type) = 
    let fields = 
        t.GetFields()
        |> Seq.map (fun f -> "Field",f.Name, f.FieldType)
        |> List.ofSeq
    let props = 
        t.GetProperties()
        |> Seq.map (fun p -> "Property",p.Name, p.PropertyType)
        |> List.ofSeq
        
    fields@props
    //|> List.filter(fun (desc, name, mt) -> mt.Name.StartsWith "Entity" |> not)
    |> List.filter(fun (_,_,mt) -> isTableType mt |> not)
    //|> List.map(fun (desc, name, mt) -> desc,name,mt,mt.GetCustomAttributes())
    
let t = this.PaymentItems.GetType().GenericTypeArguments.[0]

let m = getLinqTypeMembers t
m
|> Seq.map(fun (_desc,name,_mt) -> sprintf "%s=x.%s" name name)
|> Seq.sort
|> stringJoin ","
