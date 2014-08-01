<Query Kind="FSharpExpression">
  <Namespace>Microsoft.FSharp.Quotations</Namespace>
  <Namespace>Microsoft.FSharp.Quotations.Patterns</Namespace>
</Query>

// property name fetcher!
//open Microsoft.FSharp.Quotations
//open Microsoft.FSharp.Quotations.Patterns
let rec fieldName quot = 
	match quot with
	|FieldGet (_,fieldInfo) -> fieldInfo.Name
	
let rec propName quot = 
	match quot with
	|PropertyGet (_, propInfo, _) -> propInfo.Name
	|Lambda(param, body) -> propName body
	| _ -> failwithf "not a property get quotation"
let rec methodName quot = 
	match quot with
	|Application (expr1,expr2) -> sprintf "%A |> %A" expr1 expr2
	|Call (exprOpt, methodInfo, exprList) -> 
		//printfn "Call %A | %A | %A" exprOpt methodInfo exprList
		methodInfo.Name
	|PropertyGet (expr1,propInfo,args) -> 
		match expr1 with
		|Some(x) -> methodName x
		|None -> failwithf "Not a method call"
	|Lambda (param, body) -> methodName body //failwithf "not accounted for lambda %A %A" param body
	| _ as x -> failwithf "not accounted for quot %A" x
let assertion(f:'a->'a->bool) (expected:'a) (actual:'a) =
	let result = f expected actual
	if result=false then failwithf "expected %A to %A (%A)" expected f actual else printfn "passed %A" actual
let isEqual expected actual =
	assertion (=) expected actual
	
// instance closure property access
isEqual "Length" <| propName <@ "".Length @>
// no-instance closure property access
isEqual "Length" <| propName <@ fun (x:string) -> x.Length @>
// static property access
isEqual "CurrentCulture" <| propName <@ System.StringComparer.CurrentCulture @>

// static field name String.Empty
isEqual "Empty" <| fieldName <@ String.Empty@>
isEqual "IsNullOrEmpty" <| methodName <@ String.IsNullOrEmpty("") @>
isEqual "IsNullOrEmpty" <| methodName <@ String.IsNullOrEmpty @>
isEqual "ToCharArray" <| methodName <@ fun (x:string)-> x.ToCharArray() @>
isEqual "Compare" <| methodName <@ fun (x:string) (y:string) -> System.StringComparer.CurrentCulture.Compare(x,y) @>