<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Quotations</Namespace>
  <Namespace>Microsoft.FSharp.Quotations.DerivedPatterns</Namespace>
  <Namespace>Microsoft.FSharp.Quotations.Patterns</Namespace>
</Query>

//let expr : Expr<int> = <@ 1 + 1 @>
//let (?) (this:'Source) (prop:string) : 'Result =
//	let p = this.GetType().GetProperty(prop)
//	p.GetValue(this,null) :?> 'Result
	
type thing = {First:int; Second: int}
// let test = <@ typeof<BlockSyntax> @>
let typeToString= function
        |FSharp.Quotations.Patterns.Call (e,r,children) -> printfn "Call %A,%A,%A" e (r.GetGenericArguments().[0].Name) children
        |_ as x -> failwithf "call must match typeof<Foo> %A" x
		
let something = Some({thing.First = 1; Second = 2})
let nothing = Option<thing>.None
let maybe (expr:Expr<_>) : unit = 
	let rec visit expr = 
		match expr with
		| Application(expr1,expr2) ->
			printfn "application"
			//let val1 = 
			visit expr1
			printfn " "
			//let val2 = 
			visit expr2
			
		//|SpecificCall <@@ (+) @@> (_, _, exprList) ->
		//	printfn "%A"
		| Call (exprOpt, methodInfo, exprList) ->
			// method or module function call
			printfn "Call me maybe"
			match exprOpt with
			|Some expr -> visit expr
			|None -> printfn "%s" methodInfo.DeclaringType.Name
			printfn ".%s(" methodInfo.Name
			if exprList.IsEmpty then printf ")" else
			visit exprList.Head
			//printfn "Call %A %A %A" exprOpt methodInfo exprList
			for expr in exprList.Tail do
				printf ","
				visit expr
			printf ")"
			()
		|Int32(n) ->
			printfn "Int32 %d" n
			
		|Lambda(param, body) ->
			printfn "fun (%s:%s) -> " param.Name (param.Type.ToString())
			visit body
		|Let(var, expr1, expr2) ->
			if var.IsMutable then
				printf "let mutable %s = " var.Name
			else
				printf "let %s = " var.Name
			visit expr1
			printf " in "
			visit expr2
			
//		|PropertyGet(Some(instance), pi, args ) ->  //http://stackoverflow.com/questions/6403600/evaluate-function-inside-quotation
//			printfn "Some!"
		|PropertyGet(expr1Opt, propOrValInfo, args ) ->
			match expr1Opt with
			| Some expr2 ->
				//printfn "visiting an expr inside a propertyGet %A %A" expr1Opt args
				//expr2.GetType().GetMethods().Dump("methods")
				visit expr2
				//let value = propOrValInfo.GetValue(expr2,null)
			| None -> 
				//printfn "PropertyGet None"
				//handle var access?
				let value = propOrValInfo.GetValue(null,null)
				match value with 
				| null -> printfn "Null!"  //None
				//| :? None as n -> printfn "None"
				| _ as x -> printfn "%A" x //Some(x)
				//(propOrValInfo,propOrValInfo.GetType(),args).Dump("pi")
			
		| String(str) ->
			printf "string %s" str
		| Var(var) ->
			printf "var %s" var.Name
		| _ -> printf "unmatched %s" (expr.ToString())
			
	visit expr

maybe <@ something.Value.First @>
printfn "\r\n and now nothing \r\n"
maybe <@ nothing.Value.First @>
//printfn "\r\n\r\n%A" <@ something.Value.First @>
//printfn "\r\n\r\n%A" <@ nothing.Value.First @>