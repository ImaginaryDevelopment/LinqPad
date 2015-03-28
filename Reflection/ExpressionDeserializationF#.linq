<Query Kind="FSharpProgram">
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>



let inline toJObj (x:JToken) = 
	x :?> JObject


let rec createNode<'t> (entity:ParameterExpression option) (serializedNode:JObject) :Expression = 
	let nodeType = serializedNode.["NodeType"].ToString();
	let targetEntityType = typeof<'t>;
	//nodeType.Dump("expr Type simple");
	let exprType = Enum.Parse(typeof<System.Linq.Expressions.ExpressionType>,nodeType) :?> ExpressionType
	let entity = if entity.IsSome then entity else Some <| Expression.Parameter(targetEntityType,"f")
	let getSides () = 
		let left = createNode <| entity <| toJObj serializedNode.["Left"]
		let right = createNode <| entity <| toJObj serializedNode.["Right"]
		left,right
	match exprType with
	| ExpressionType.Lambda -> 
		let bodyNode = serializedNode.["Body"] :?> JObject
		let body = createNode entity <| bodyNode
		upcast Expression.Lambda<Func<'t,bool>>(body,entity.Value)
	| ExpressionType.Equal ->
		let left = 
			toJObj serializedNode.["Left"]
			|> createNode entity
		let right = 
			toJObj serializedNode.["Right"]
			|> createNode entity
		upcast Expression.Equal(left,right)
	| ExpressionType.MemberAccess ->
		let memberN = toJObj serializedNode.["Member"]
		let name = memberN.["Name"].ToString()
		let className = memberN.["ClassName"].ToString()
		if className = targetEntityType.FullName then
			let property:PropertyInfo = typeof<'t>.GetProperties().First( fun info -> 
				info.Name = name)
			upcast Expression.MakeMemberAccess(entity.Value,property)
		else
			let expression = toJObj serializedNode.["Expression"]
			upcast Expression.Constant(expression.["Value"].[name].ToString())
	| ExpressionType.Constant ->
		let cType = serializedNode.["Type"].ToString()
		let value = 
			match serializedNode.["Value"] with
			| :? JObject as valueJ -> valueJ.ToString()
			| _ as x -> x.ToString()
		upcast Expression.Constant(value)
	| ExpressionType.AndAlso ->
		let left,right = getSides()
		upcast Expression.AndAlso(left,right)
	| ExpressionType.And ->
		let left,right = getSides()
		upcast Expression.And(left,right)
	| ExpressionType.Or ->
		let left,right = getSides()
		upcast Expression.Or(left,right)
	| ExpressionType.OrElse ->
		let left,right = getSides()
		upcast Expression.OrElse(left,right)
	| ExpressionType.Not ->
		let operand = createNode entity <| toJObj serializedNode.["Operand"]
		upcast Expression.Not(operand)
	| ExpressionType.Call //TODO
	| _ ->
		serializedNode.ToString().Dump("failing to deserialize")
		raise <| new NotImplementedException(exprType.ToString())
		
type PropertyExpr = {(* Index:int; *) Name:string; Info:PropertyInfo; PropExpression: ParameterExpression}
let deserializeLambdaPredicate<'t> 	(entity:ParameterExpression) (serializedNode:JObject) :Expression<Func<'t,bool>> =
	let body = createNode <| Some entity <| toJObj serializedNode.["Body"]
	Expression.Lambda<Func<'t,bool>>(body,entity)
type Foo = { Hello:string;IsSet:bool}

let x = "Hello"
//let expr :Expression<Func<Foo,bool>> = Expression.Lambda


//void Main()
//{
//	var x = "Hello";
//	Expression<Func<Foo,bool>> expr = f => f.Hellop == x;
//	//expr = f=> f.IsSet | f.Hellop=="Hello" && f.Hellop.Equals("Hello");
//	var manualExpr = CreateManualCompareExpression<Foo,string>("Hellop",x);
//	var serialized = Newtonsoft.Json.JsonConvert.SerializeObject(expr)
//		//.Dump("serialized")
//		;
//	
//	expr.ToString().Dump("expr to string");
//	
//	var deserializedJ = (JObject) Newtonsoft.Json.JsonConvert.DeserializeObject(serialized)
//	//.Dump("yay deserializedJ")
//	;
//	//deserializedJ.ToString().Dump();
//	var deserialized=(Expression<Func<Foo,bool>>) CreateNode<Foo>(deserializedJ);
//	deserialized.Dump("deserialized!");
//	Util.HorizontalRun(false, deserialized,expr,manualExpr).Dump("deserialized,full auto, manual");
//	deserialized.Compile().Dump("full auto func");
//	var func = deserialized.Compile();
//	func.Dump();
//	//var searchExpr = SearchExpression<Foo>(deserialized).Dump("whut whut!");
//	new{ OriginalExpression = expr.ToString(), Deserialized = deserialized.ToString()}.Dump();
//	//Util.HorizontalRun(true, expr.ToString(), deserialized.ToString()).Dump("expression,deserializedExpression");
//	
//	func(new Foo(){ Hellop = "Hello", IsSet=true}.Dump("foo")).Dump("should be true");
//	func(new Foo(){ Hellop = "Helloa"}.Dump("foo")).Dump("should be false");
//	
//}
//
//// Define other methods and classes here

////public MethodInfo GetMethodInfo(JObject serializedMethodNode){
////	return null;
////}
//public Expression<Func<T,Boolean>> CreateManualCompareExpression<T,TValue>(string propName, TValue compareValue){
//var entity = Expression.Parameter(typeof(T),"f");
//	
//	var valueExpr = Expression.Constant(compareValue);
//	
//	var properties = typeof(Foo).GetProperties().Select((info,i) => new {Index = i, Name = info.Name, PropExpression = Expression.Property(entity,info)}).ToArray();
//	//var meth = MethodCallExpression.Call(typeof(string).GetMethod("op_Equality"),properties.First(p=> p.Name == "Hellop").PropExpression,value);
//	var meth = Expression.Equal(properties.First(p=>p.Name == propName).PropExpression,valueExpr);
//	var lam = Expression.Lambda<Func<T,bool>>(meth,entity);
//	return lam;
//}
//
//
//public LambdaExpression DeserializeLambda<T>(JObject n, ParameterExpression entity)
//{
//	/* kwilla's option for orderby serialization */
//	var targetEntityType = typeof(T);
//	var body = CreateNode<T>((JObject) n["Body"],entity);
//	var returnTypeName = n["ReturnType"].ToString();
//	var returnType = Type.GetType(returnTypeName);
//	var funcDelegateType = typeof (Func<,>).MakeGenericType(targetEntityType, returnType);
//
//	return Expression.Lambda(funcDelegateType, body, entity);
//}
//
//