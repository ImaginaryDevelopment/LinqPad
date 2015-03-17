<Query Kind="Program">
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

void Main()
{
	
	Expression<Func<Foo,bool>> expr = f => f.Hellop == "Hello";
	expr = f=> f.IsSet | f.Hellop=="Hello" && f.Hellop.Equals("Hello");
	var manualExpr = CreateManualCompareExpression<Foo,string>("Hellop","Hello");
	//lam.Dump("manual");
	//var exprCast = expr as Expression;
	//exprCast.Dump("full auto");
	//expr.Dump();
	var serialized = Newtonsoft.Json.JsonConvert.SerializeObject(expr)
		//.Dump("serialized")
		;
		
	//((int)System.Linq.Expressions.ExpressionType.MemberAccess).Dump("member access");
	expr.ToString()
		//.Dump("expr to string")
		;
	
	var deserializedJ = (JObject) Newtonsoft.Json.JsonConvert.DeserializeObject(serialized)
	//.Dump("yay deserializedJ")
	;
	//deserializedJ.ToString().Dump();
	var deserialized=(Expression<Func<Foo,bool>>) CreateNode(deserializedJ);
	deserialized.Dump("deserialized!");
	Util.HorizontalRun(false, deserialized,expr,manualExpr).Dump("deserialized,full auto, manual");
	expr.Compile().Dump("full auto func");
	var func = deserialized.Compile();
	func.Dump();
	//var searchExpr = SearchExpression<Foo>(deserialized).Dump("whut whut!");
	expr.ToString().Dump("expression");
	
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
	
	var properties = typeof(Foo).GetProperties().Select((info,i) => new {Index = i, Name = info.Name, PropExpression = Expression.Property(entity,info)}).ToArray();
	//var meth = MethodCallExpression.Call(typeof(string).GetMethod("op_Equality"),properties.First(p=> p.Name == "Hellop").PropExpression,value);
	var meth = Expression.Equal(properties.First(p=>p.Name == propName).PropExpression,valueExpr);
	var lam = Expression.Lambda<Func<T,bool>>(meth,entity);
	return lam;
}
public ConstantExpression DeserializeConstant(JObject n)
{
	var type = n["Type"].ToString();
	var value = n["Value"].ToString();
	return Expression.Constant(value);
}

public Expression CreateNode(JObject serializedNode,ParameterExpression entity = null) {
	
	var nodeType = serializedNode["NodeType"].ToString();
	//nodeType.Dump("expr Type simple");
	var exprType = (ExpressionType) Enum.Parse(typeof(System.Linq.Expressions.ExpressionType),nodeType);
	entity = entity ?? Expression.Parameter(typeof(Foo),"f");
	switch(exprType)
	{
		case ExpressionType.Lambda:
			
			var body = CreateNode((JObject)serializedNode["Body"],entity);
			return Expression.Lambda<Func<Foo,bool>>(body,entity);
		case ExpressionType.Equal:
			var left = CreateNode((JObject)serializedNode["Left"],entity);
			var right = CreateNode((JObject)serializedNode["Right"],entity);
//			var methodNode = serializedNode["Method"] as JObject;
//			if(methodNode !=null){
//				var liftToNull = (bool)serializedNode["IsLiftedToNull"];
//				var method = GetMethodInfo(methodNode);
//				return Expression.Equal(left,right,liftToNull, method);
//			}
			return Expression.Equal(left,right);
		case ExpressionType.MemberAccess:
			var member = (JObject)serializedNode["Member"];
			var name = (string)member["Name"];
			//serializedNode["Member"].GetType().Dump("member type");
			//serializedNode.ToString().Dump("member access");
			//member.ToString().Dump("name access");
			//var entity = Expression.Parameter(typeof(Foo),"f");
			var properties = typeof(Foo).GetProperties().Select((info,i) => new {Index = i, Name = info.Name,Info=info, PropExpression = Expression.Property(entity,info)}).ToArray();
			return Expression.MakeMemberAccess(entity,properties.First(p=>p.Name==name).Info);
		case ExpressionType.Constant:
			return DeserializeConstant(serializedNode);
		case ExpressionType.AndAlso:
			left = CreateNode((JObject)serializedNode["Left"],entity);
			right = CreateNode((JObject) serializedNode["Right"],entity);
			return Expression.AndAlso(left,right);
		case ExpressionType.And:
			left = CreateNode((JObject)serializedNode["Left"],entity);
			right = CreateNode((JObject) serializedNode["Right"],entity);
			return Expression.And(left,right);
		case ExpressionType.Or:
			left = CreateNode((JObject)serializedNode["Left"],entity);
			right = CreateNode((JObject) serializedNode["Right"],entity);
			return Expression.Or(left,right);
		case ExpressionType.OrElse:
			left = CreateNode((JObject)serializedNode["Left"],entity);
			right = CreateNode((JObject) serializedNode["Right"],entity);
			return Expression.OrElse(left,right);
		case ExpressionType.Not:
			var operand = CreateNode((JObject)serializedNode["Operand"],entity);
			return Expression.Not(operand);
		case ExpressionType.Call:
			//TODO
		default:
			serializedNode.ToString().Dump("failing to deserialize");
		throw new NotImplementedException(exprType.ToString());
	}
}
public Expression<Func<T,bool>> SearchExpression<T>(JObject deserialization) {
	var entity = Expression.Parameter(typeof(T));
	var body =  deserialization["Body"] as JObject;
	var type =  body["Type"].Dump();
	
	body.ToString().Dump("to string body");
	
	return null;
	//var expectedValues = body[
	//var comparisonExpression = typeof(T).GetProperties().Select((info, i) => Expression.Equal(Expression.Property(entity,info),expectedValues));
}