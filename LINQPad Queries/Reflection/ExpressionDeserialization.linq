<Query Kind="Program">
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

void Main()
{
	var x = "Hello";
	Expression<Func<Foo,bool>> expr = f => f.Hellop == x;
	//expr = f=> f.IsSet | f.Hellop=="Hello" && f.Hellop.Equals("Hello");
	var manualExpr = CreateManualCompareExpression<Foo,string>("Hellop",x);
	var serialized = Newtonsoft.Json.JsonConvert.SerializeObject(expr)
		//.Dump("serialized")
		;
	
	expr.ToString().Dump("expr to string");
	
	var deserializedJ = (JObject) Newtonsoft.Json.JsonConvert.DeserializeObject(serialized)
	//.Dump("yay deserializedJ")
	;
	//deserializedJ.ToString().Dump();
	var deserialized=(Expression<Func<Foo,bool>>) CreateNode<Foo>(deserializedJ);
	deserialized.Dump("deserialized!");
	Util.HorizontalRun(false, deserialized,expr,manualExpr).Dump("deserialized,full auto, manual");
	deserialized.Compile().Dump("full auto func");
	var func = deserialized.Compile();
	func.Dump();
	//var searchExpr = SearchExpression<Foo>(deserialized).Dump("whut whut!");
	new{ OriginalExpression = expr.ToString(), Deserialized = deserialized.ToString()}.Dump();
	//Util.HorizontalRun(true, expr.ToString(), deserialized.ToString()).Dump("expression,deserializedExpression");
	
	func(new Foo(){ Hellop = "Hello", IsSet=true}.Dump("foo")).Dump("should be true");
	func(new Foo(){ Hellop = "Helloa"}.Dump("foo")).Dump("should be false");
	
}

// Define other methods and classes here
public class Foo{
	public string Hellop{get;set;}
	public bool IsSet {get;set;}
}
//public MethodInfo GetMethodInfo(JObject serializedMethodNode){
//	return null;
//}
public Expression<Func<T,Boolean>> CreateManualCompareExpression<T,TValue>(string propName, TValue compareValue){
var entity = Expression.Parameter(typeof(T),"f");
	
	var valueExpr = Expression.Constant(compareValue);
	
	var properties = typeof(T).GetProperties().Select((info,i) => new {Index = i, Name = info.Name, PropExpression = Expression.Property(entity,info)}).ToArray();
	//var meth = MethodCallExpression.Call(typeof(string).GetMethod("op_Equality"),properties.First(p=> p.Name == "Hellop").PropExpression,value);
	var meth = Expression.Equal(properties.First(p=>p.Name == propName).PropExpression,valueExpr);
	var lam = Expression.Lambda<Func<T,bool>>(meth,entity);
	return lam;
}

public ConstantExpression DeserializeConstant(JObject n)
{
	var type = (string)n["Type"];
	object value = null;
	var valueJ = n["Value"] as JObject;
	if(valueJ !=null)
		value = valueJ.ToString();
	else 
		value = n["Value"].ToString();
	new{ type,value}.Dump("making constant");
	return Expression.Constant(value);
}

public LambdaExpression DeserializeLambda<T>(JObject n, ParameterExpression entity)
{
	/* kwilla's option for orderby serialization */
	var targetEntityType = typeof(T);
	var body = CreateNode<T>((JObject) n["Body"],entity);
	var returnTypeName = n["ReturnType"].ToString();
	var returnType = Type.GetType(returnTypeName);
	var funcDelegateType = typeof (Func<,>).MakeGenericType(targetEntityType, returnType);

	return Expression.Lambda(funcDelegateType, body, entity);
}

public LambdaExpression DeserializeLambdaPredicate<T>( JObject n, ParameterExpression entity){
	var body = CreateNode<T>((JObject) n["Body"],entity);
	return Expression.Lambda<Func<T,bool>>(body,entity);
}

public Expression CreateNode<T>(JObject serializedNode,ParameterExpression entity = null) {
	
	var nodeType = serializedNode["NodeType"].ToString();
	var targetEntityType = typeof(T);
	//nodeType.Dump("expr Type simple");
	var exprType = (ExpressionType) Enum.Parse(typeof(System.Linq.Expressions.ExpressionType),nodeType);
	entity = entity ?? Expression.Parameter(targetEntityType,"f");
	switch(exprType)
	{
		case ExpressionType.Lambda:
			return DeserializeLambdaPredicate<T>(serializedNode,entity);
		case ExpressionType.Equal:
			var left = CreateNode<T>((JObject)serializedNode["Left"],entity);
			var right = CreateNode<T>((JObject)serializedNode["Right"],entity);
			//TODO: consider method path:
/*			var methodNode = serializedNode["Method"] as JObject;
//			if(methodNode !=null){
//				var liftToNull = (bool)serializedNode["IsLiftedToNull"];
//				var method = GetMethodInfo(methodNode);
//				return Expression.Equal(left,right,liftToNull, method);
//			} */
			return Expression.Equal(left,right);
			
		case ExpressionType.MemberAccess:
			var member = (JObject)serializedNode["Member"];
			var name = (string)member["Name"];
			var className = member["ClassName"].ToString();
			if(className == targetEntityType.FullName){
				var properties = typeof(T).GetProperties().Select((info,i) => new {Index = i, Name = info.Name,Info=info, PropExpression = Expression.Property(entity,info)}).ToArray();
				
				return Expression.MakeMemberAccess(entity,properties.First(p=>p.Name==name).Info);
			}
			serializedNode.ToString().Dump("deserializing non-entity member access");
			var expression = (JObject)serializedNode["Expression"];
			return Expression.Constant(expression["Value"][name].ToString());
			
		case ExpressionType.Constant:
			return DeserializeConstant(serializedNode);
			
		case ExpressionType.AndAlso:
			left = CreateNode<T>((JObject)serializedNode["Left"],entity);
			right = CreateNode<T>((JObject) serializedNode["Right"],entity);
			return Expression.AndAlso(left,right);
			
		case ExpressionType.And:
			left = CreateNode<T>((JObject)serializedNode["Left"],entity);
			right = CreateNode<T>((JObject) serializedNode["Right"],entity);
			return Expression.And(left,right);
			
		case ExpressionType.Or:
			left = CreateNode<T>((JObject)serializedNode["Left"],entity);
			right = CreateNode<T>((JObject) serializedNode["Right"],entity);
			return Expression.Or(left,right);
			
		case ExpressionType.OrElse:
			left = CreateNode<T>((JObject)serializedNode["Left"],entity);
			right = CreateNode<T>((JObject) serializedNode["Right"],entity);
			return Expression.OrElse(left,right);
			
		case ExpressionType.Not:
			var operand = CreateNode<T>((JObject)serializedNode["Operand"],entity);
			return Expression.Not(operand);
			
		case ExpressionType.Call:
			//TODO
		default:
			serializedNode.ToString().Dump("failing to deserialize");
		throw new NotImplementedException(exprType.ToString());
	}
}