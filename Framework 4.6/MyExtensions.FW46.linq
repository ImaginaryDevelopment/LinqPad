<Query Kind="Program">
  <Namespace>Microsoft.Win32.SafeHandles</Namespace>
  <Namespace>System.Collections.ObjectModel</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

#define LinqPad
//http://higherlogics.blogspot.com/2013/03/sasastrings-general-string-extensions.html
// see also: http://www.linqpad.net/CustomizingDump.aspx
void Main()
{
	// Write code to test your extensions here. Press F5 to compile and run.
	var dtStart = new DateTime(2014, 5, 22);
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Monday) == new DateTime(2014, 5, 19));
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Tuesday) == new DateTime(2014, 5, 20));
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Wednesday) == new DateTime(2014, 5, 21));
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Thursday) == new DateTime(2014, 5, 22));
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Friday) == new DateTime(2014, 5, 16));
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Saturday) == new DateTime(2014, 5, 17));
	Debug.Assert(dtStart.StartOfWeek(DayOfWeek.Sunday) == new DateTime(2014, 5, 18));

	var child = new XElement("child");
	var xmlWithAttrib = new System.Xml.Linq.XElement("element", new System.Xml.Linq.XAttribute("test", "1"), child);

	Debug.Assert(xmlWithAttrib.GetAbsoluteXPath() == "/element");
	Debug.Assert(child.GetAbsoluteXPath() == "/element/child[1]", child.GetAbsoluteXPath());

	// int.To

	Debug.Assert(1.To(10).Aggregate((x, y) => x + y) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
	Debug.Assert(2.To(10).Aggregate((x, y) => x + y) == 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);

	//SplitWords
	string stringInstance = null;
	stringInstance = "a bowl of cherries";
	var expected = new[] { "a", "bowl", "of", "cherries" };
	var actual = stringInstance.SplitWords().ToArray();
	Debug.Assert(actual.Count() == 4, "SplitWords failed:" + actual.Delimit(" "));
	for (int i = 0; i < expected.Length; i++)
	{
		Debug.Assert(actual[i] == expected[i]);
	}
	var so = AttributeTargets.Assembly | AttributeTargets.Class;
	//so.Dump();
	Debug.Assert(so.Has(AttributeTargets.Assembly), "Enum Has failed to find Assembly");
	Debug.Assert(so.ContainedValues<AttributeTargets>().Count() == 2, "Enum iteration of flags failed to enumerate properly");

	//Tokenize - idea from http://higherlogics.blogspot.com/2013/03/sasastrings-general-string-extensions.html
	var operators = "4 + 5 - 6^2 == x".Tokenize("+", "-", "^", "==").ToArray();
	Debug.Assert(operators.Count() == 4);
	Debug.Assert(operators[0].Item1 == "+" && operators[0].Item2 == 2);
	Debug.Assert(operators[1].Item1 == "-" && operators[1].Item2 == 6);
	Debug.Assert(operators[2].Item1 == "^" && operators[2].Item2 == 9);
	Debug.Assert(operators[3].Item1 == "==" && operators[3].Item2 == 12);
}

public static class MyExtensions
{
	public static string Before(this string text, string delimiter, StringComparison? comparison = null)
	{
		return text.Substring(0, text.IndexOf(delimiter, comparison ?? StringComparison.CurrentCulture));
	}

	public static string After(this string text, string delimiter, StringComparison? comparison = null)
	{
		return text.Substring(text.IndexOf(delimiter, comparison ?? StringComparison.CurrentCulture) + delimiter.Length);
	}

	public static string BeforeLast(this string text, string delimiter, StringComparison? comparison = null)
	{

		return text.Substring(0, text.LastIndexOf(delimiter, comparison ?? StringComparison.CurrentCulture));
	}

	public static string BeforeOrSelf(this string text, string delimiter)
	{
		if (text.Contains(delimiter) == false)
			return text;
		return text.Before(delimiter);
	}

	public static string AfterLast(this string text, string delimiter, StringComparison? comparison = null)
	{
		return text.Substring(text.LastIndexOf(delimiter, comparison ?? StringComparison.CurrentCulture) + delimiter.Length);
	}

	public static string AfterLastOrSelf(this string text, string delimiter)
	{
		if (text.Contains(delimiter) == false)
			return text;
		return text.AfterLast(delimiter);
	}

	public static string AfterOrSelf(this string text, string delimiter)
	{
		if (text.Contains(delimiter) == false)
			return text;
		return text.After(delimiter);
	}

	// Write custom extension methods here. They will be available to all queries.
	public static bool IsNullOrEmpty(this string s)
	{
		return string.IsNullOrEmpty(s);
	}

	public static bool HasValue(this string s)
	{
		return !s.IsNullOrEmpty();
	}

	public static string SurroundWith(this string s, string wrapper)
	{
		return wrapper + s + wrapper;
	}

	public static string SurroundWith(this string s, string opener, string closer)
	{
		return opener + s + closer;
	}

	public static int StrComp(this String str1, String str2, bool ignoreCase)
	{
		return string.Compare(str1, str2, ignoreCase);
	}

	public static bool IsIgnoreCaseMatch(this string s, string comparisonText)
	{
		return s.StrComp(comparisonText, true) == 0;
	}

	//http://stackoverflow.com/a/1464929/57883
	public static IEnumerable<T> Select<T>(this IDataReader reader,
									   Func<IDataReader, T> projection)
	{
		while (reader.Read())
		{
			yield return projection(reader);
		}
	}

	public static TimeSpan Minutes(this int m)
	{
		return TimeSpan.FromMinutes(m);
	}

	#region DateTimeExtensions
	public static string ToTime(this DateTime dt)
	{
		return dt.ToString().After(" ");
	}

	/// <summary>
	/// Returns the number of milliseconds since Jan 1, 1970 (useful for converting C# dates to JS dates)
	/// http://stackoverflow.com/a/9191958/57883
	/// </summary>
	/// <param name="dt">Date Time</param>
	/// <returns>Returns the number of milliseconds since Jan 1, 1970 (useful for converting C# dates to JS dates)</returns>
	public static double UnixTicks(this DateTime dt)
	{
		DateTime d1 = new DateTime(1970, 1, 1);
		DateTime d2 = dt.ToUniversalTime();
		TimeSpan ts = new TimeSpan(d2.Ticks - d1.Ticks);
		return ts.TotalMilliseconds;
	}

	//http://stackoverflow.com/questions/38039/how-can-i-get-the-datetime-for-the-start-of-the-week
	public static DateTime StartOfWeek(this DateTime dt, DayOfWeek startOfWeek)
	{
		int diff = dt.DayOfWeek - startOfWeek;
		if (diff < 0)
		{
			diff += 7;
		}

		return dt.AddDays(-1 * diff).Date;
	}

	#endregion

	public static bool NextBool(this Random rnd)
	{
		return rnd.NextDouble() > 0.5;
	}
	//
	//    public static My.StreamOuts RunProcessRedirected(this Process ps, string arguments)
	//    {
	//        ps.StartInfo.Arguments = arguments;
	//        ps.Start();
	//        ps.Id.Dump("processId started");
	//        var output = ps.StandardOutput.ReadtoEndAndDispose();
	//        var errors = ps.StandardError.ReadtoEndAndDispose();
	//
	//        ps.WaitForExit(2000);
	//        if (errors.Length > 0) Util.Highlight(errors).Dump("errors");
	//        return new My.StreamOuts() { Errors = errors, Output = output };
	//    }

	#region xml related

	public static string GetAttribValOrNull(this XElement node, XName name)
	{
		var xa = node.Attribute(name);
		if (xa == null)
			return null;
		return xa.Value;
	}

	public static XElement GetXElement(this XmlNode node)
	{
		XDocument xDoc = new XDocument();
		using (XmlWriter xmlWriter = xDoc.CreateWriter())
			node.WriteTo(xmlWriter);
		return xDoc.Root;
	}

	public static XmlNode GetXmlNode(this XElement element)
	{
		using (XmlReader xmlReader = element.CreateReader())
		{
			XmlDocument xmlDoc = new XmlDocument();
			xmlDoc.Load(xmlReader);
			return xmlDoc;
		}
	}
	/// <summary>
	/// Get the absolute XPath to a given XElement
	/// (e.g. "/people/person[6]/name[1]/last[1]").
	/// </summary>
	public static string GetAbsoluteXPath(this XElement element)
	{
		if (element == null)
		{
			throw new ArgumentNullException("element");
		}

		Func<XElement, string> relativeXPath = e =>
		{
			int index = e.IndexPosition();
			string name = e.Name.LocalName;

			// If the element is the root, no index is required

			return (index == -1) ? "/" + name : string.Format
			(
				"/{0}[{1}]",
				name,
				index.ToString()
			);
		};

		var ancestors = from e in element.Ancestors()
						select relativeXPath(e);

		return string.Concat(ancestors.Reverse().ToArray()) +
			   relativeXPath(element);
	}

	/// <summary>
	/// Get the index of the given XElement relative to its
	/// siblings with identical names. If the given element is
	/// the root, -1 is returned.
	/// </summary>
	/// <param name="element">
	/// The element to get the index of.
	/// </param>
	public static int IndexPosition(this XElement element)
	{
		if (element == null)
		{
			throw new ArgumentNullException("element");
		}

		if (element.Parent == null)
		{
			return -1;
		}

		int i = 1; // Indexes for nodes start at 1, not 0

		foreach (var sibling in element.Parent.Elements(element.Name))
		{
			if (sibling == element)
			{
				return i;
			}

			i++;
		}

		throw new InvalidOperationException
			("element has been removed from its parent.");
	}

	public static string ReadtoEndAndDispose(this StreamReader reader)
	{
		using (System.IO.StreamReader r = reader)
		{
			return r.ReadToEnd();
		}
	}

	#endregion

	///returns all tokens with the index of that token
	public static IEnumerable<Tuple<string, int>> Tokenize(this string input, params string[] tokens)
	{
		var matches = new Regex(tokens.Select(t => My.StringUtilities.RegexEncode(t)).Delimit("|")).Matches(input);
		//matches.Dump();
		foreach (Match m in matches)
		{
			yield return new Tuple<string, int>(m.Value, m.Index);
		}
	}

	public static bool IsMatch(this string text, string pattern, bool ignoreCase)
	{
		return ignoreCase ? Regex.IsMatch(text, pattern, RegexOptions.IgnoreCase) :
		Regex.IsMatch(text, pattern);
	}

	public static byte[] ToByteArrayFromAscii(this string text)
	{
		var encoding = new ASCIIEncoding();
		return encoding.GetBytes(text);
	}

	///<summary>
	///Assumes Ascii!
	///</summary>
	public static String ToStringFromAsciiBytes(this byte[] buffer)
	{
		var encoding = new ASCIIEncoding();
		return encoding.GetString(buffer);
	}

	public static string WrapWith(this string txt, string delimiter)
	{
		if (txt == null)
			return txt;
		return delimiter + txt + delimiter;
	}
	public static bool Contains(this string text, string value, StringComparison comparison)
	{
		return text.IndexOf(value, comparison) >= 0;
	}

	public static string[] SplitLines(this string text)
	{
		return text.Split(new string[] { "\r\n", "\n" }, StringSplitOptions.None);
	}

	public static T? ParseEnum<T>(this String enumString) where T : struct
	{
		if (Enum.IsDefined(typeof(T), enumString))
			return (T)Enum.Parse(typeof(T), enumString);
		return new T?();
	}

	public static IEnumerable<string> RegexSplit(this string text, string pattern)
	{
		return Regex.Split(text, pattern);
	}

	public static IEnumerable<string> SplitWords(this string text)
	{
		return text.RegexSplit("\\W+");
	}

	public static string RemoveMultipleWhitespaces(this string text)
	{
		return Regex.Replace(text, "\\s\\s+", " ");
	}

	/// <summary>
	/// Join a list of strings with a separator
	/// From BReusable
	/// </summary>
	/// <param name="l"></param>
	/// <param name="seperator"></param>
	/// <returns></returns>
	public static String DelimitLarge(this IEnumerable<string> l, string separator)
	{
		var counter = 0;

		var result = new StringBuilder();
		foreach (var item in l)
		{
			if (counter != 0) result.Append(separator);
			result.Append(item);
			counter++;
		}
		return result.ToString();
	}

	//    
	//    // from http://forum.linqpad.net/discussion/13/caching-things-between-execution-runs-but-same-linqpad-process-same-open-tab
	//    public static IEnumerable<T> Cache<T>(this IEnumerable<T> o, string key = "default")
	//    {
	//        string slot = "Cache." + key;
	//        object existing = AppDomain.CurrentDomain.GetData(slot);
	//
	//        if (existing is Array && typeof(T).IsAssignableFrom(existing.GetType().GetElementType()))
	//            return (IEnumerable<T>)existing;
	//
	//        if (existing is Array && CanShredAnonymousObject(typeof(T), existing.GetType().GetElementType()))
	//            return ShredEnumerable<T>((IEnumerable)existing, existing.GetType().GetElementType()).ToArray();
	//
	//        var result = o.ToArray();
	//        AppDomain.CurrentDomain.SetData(slot, result);
	//        return result;
	//    }
	//    
	//    static IEnumerable<TTarget> ShredEnumerable<TTarget>(IEnumerable source, Type sourceElementType)
	//    {
	//        foreach (var element in source)
	//            yield return (TTarget)ShredAnonymousObject(element, sourceElementType, typeof(TTarget));
	//    }
	//
	//    static bool CanShredAnonymousObject(Type sourceType, Type targetType)
	//    {
	//        return
	//            sourceType.Name.StartsWith("<") &&
	//            targetType.Name.StartsWith("<") &&
	//            sourceType.GetProperties().Select(p => p.Name).OrderBy(p => p).SequenceEqual(
	//            targetType.GetProperties().Select(p => p.Name).OrderBy(p => p));
	//    }
	//
	//    static object ShredAnonymousObject(object source, Type sourceType, Type targetType)
	//    {
	//        object[] args = targetType.GetConstructors().Single()
	//            .GetParameters()
	//            .Select(p => ShredValue(sourceType.GetProperty(p.Name).GetValue(source, null), p.ParameterType))
	//            .ToArray();
	//
	//        return Activator.CreateInstance(targetType, args);
	//    }
	//
	//    static object ShredValue(object source, Type targetType)
	//    {
	//        if (source == null) return null;
	//        Type sourceType = source.GetType();
	//
	//        if (targetType.IsAssignableFrom(sourceType)) return source;
	//
	//        if (targetType.IsArray && source is Array && CanShredAnonymousObject(sourceType.GetElementType(), targetType.GetElementType()))
	//        {
	//            var sourceElementType = sourceType.GetElementType();
	//            var targetElementType = targetType.GetElementType();
	//            var sourceArray = (Array)source;
	//            var targetArray = Array.CreateInstance(targetElementType, sourceArray.Length);
	//            for (int i = 0; i < sourceArray.Length; i++)
	//                targetArray.SetValue(ShredAnonymousObject(sourceArray.GetValue(i), sourceElementType, targetElementType), i);
	//            return targetArray;
	//        }
	//
	//        if (targetType.IsGenericType &&
	//            (targetType.GetGenericTypeDefinition() == typeof(IEnumerable<>) || targetType.GetGenericTypeDefinition() == typeof(List<>)) &&
	//            sourceType.GetInterface("System.Collections.Generic.IEnumerable`1") != null &&
	//            CanShredAnonymousObject(
	//                targetType.GetGenericArguments()[0],
	//                sourceType.GetInterface("System.Collections.Generic.IEnumerable`1").GetGenericArguments()[0]))
	//        {
	//            var sourceElementType = sourceType.GetInterface("System.Collections.Generic.IEnumerable`1").GetGenericArguments()[0];
	//            var targetElementType = targetType.GetGenericArguments()[0];
	//            var target = (IList)Activator.CreateInstance(typeof(List<>).MakeGenericType(targetElementType));
	//            foreach (var sourceElement in (IEnumerable)source)
	//                target.Add(ShredAnonymousObject(sourceElement, sourceElementType, targetElementType));
	//            return target;
	//        }
	//
	//        throw new NotSupportedException("Unrecognized type: " + targetType.FullName);
	//    }

	//public static class EnumerableExtensions
	#region EnumerableExtensions

	public static void CheckDuplicates<T>(this IEnumerable<T> items, IEqualityComparer<T> comparer, string message, bool throwOnDupes = true)
	{
		if (items.Count() != items.Distinct(comparer).Count())
		{
			(from i in items.Distinct(comparer)
			 let count = items.Count(s => comparer.Equals(s, i))
			 where count > 1
			 select i).Dump(message);
			if (throwOnDupes)
				throw new InvalidOperationException(message);
		}
	}

	public static void CheckDuplicates<T>(this IEnumerable<T> items, string message, bool throwOnDupes = true)
		where T : IEquatable<T>
	{
		if (items.Count() != items.Distinct().Count())
		{
			(from i in items.Distinct()
			 let count = items.Count(s => s.Equals(i))
			 where count > 1
			 select i).Dump(message);
			if (throwOnDupes)
				throw new InvalidOperationException(message);
		}
	}

	//	public static void CheckDuplicates<T>(this IEnumerable<T> items,string message,bool throwOnDupes = true)
	//		where T:IEquatable<T>
	//	{
	//		CheckDuplicates(items, (x,y) => x.Equals(y),message,throwOnDupes);
	//	}

	//	public static IEnumerable<TSource> Distinct<TSource>(this IEnumerable<TSource> items, Func<TSource,TSource,bool> comparer){
	//		return items.Distinct( new FuncEqualityComparer<TSource>(comparer));
	//	}
	//	
	//	private class FuncEqualityComparer<T> : IEqualityComparer<T>{
	//		readonly Func<T,T,bool> _comparer;
	//		public FuncEqualityComparer(Func<T,T,bool> comparer){
	//			_comparer = comparer;
	//		}
	//		public bool Equals(T b1, T b2){
	//			return _comparer(b1,b2);
	//		}
	//		
	//		public int GetHashCode(T item){
	//			return item.GetHashCode();
	//		}
	//		
	//		
	//	}
	//https://code.google.com/p/morelinq/source/browse/MoreLinq/DistinctBy.cs
	public static IEnumerable<TSource> DistinctBy<TSource, TKey>(this IEnumerable<TSource> source,
			Func<TSource, TKey> keySelector)
	{
		return source.DistinctBy(keySelector, null);
	}

	public static IEnumerable<T> SortBy<T>(this IQueryable<T> source, IEnumerable<KeyValuePair<string, bool>> orderings, Func<string, string> selectionToOutputMapper)
	{
		var inThenBy = false;
		var sortable = source;

		foreach (var ordering in orderings)
		{
			var targetProp = selectionToOutputMapper(ordering.Key);

			if (targetProp.IsNullOrEmpty() == false)
			{
				sortable = SortBy(sortable, targetProp, inThenBy, ordering.Value);
				inThenBy = true;
			}
		}
		var sorted = sortable.ToList();
		return sorted;
	}
	// David Fowler - http://weblogs.asp.net/davidfowler/dynamic-sorting-with-linq
	public static IQueryable<T> SortBy<T>(this IQueryable<T> source, string propertyName, bool thenBy, bool desc)
	{
		if (source == null)
			throw new ArgumentNullException("source");
		if (string.IsNullOrEmpty(propertyName))
			return source;

		ParameterExpression par = Expression.Parameter(source.ElementType, String.Empty);
		MemberExpression prop = Expression.Property(par, propertyName);
		LambdaExpression lambda = Expression.Lambda(prop, par);
		string methodName = (thenBy ? "ThenBy" : "OrderBy") + (desc ? "Descending" : string.Empty);
		Expression methodCallExpression = Expression.Call(typeof(Queryable), methodName, new Type[] { source.ElementType, prop.Type },
			source.Expression, Expression.Quote(lambda));
		return source.Provider.CreateQuery<T>(methodCallExpression);
	}

	public static IEnumerable<TSource> DistinctBy<TSource, TKey>(this IEnumerable<TSource> source,
			Func<TSource, TKey> keySelector, IEqualityComparer<TKey> comparer)
	{
		if (source == null) throw new ArgumentNullException("source");
		if (keySelector == null) throw new ArgumentNullException("keySelector");
		return DistinctByImpl(source, keySelector, comparer);
	}

	static IEnumerable<TSource> DistinctByImpl<TSource, TKey>(IEnumerable<TSource> source,
			Func<TSource, TKey> keySelector, IEqualityComparer<TKey> comparer)
	{
#if !NO_HASHSET
		var knownKeys = new HashSet<TKey>(comparer);
		foreach (var element in source)
		{
			if (knownKeys.Add(keySelector(element)))
			{
				yield return element;
			}
		}
#else
            //
            // On platforms where LINQ is available but no HashSet<T>
            // (like on Silverlight), implement this operator using 
            // existing LINQ operators. Using GroupBy is slightly less
            // efficient since it has do all the grouping work before
            // it can start to yield any one element from the source.
            //

            return source.GroupBy(keySelector, comparer).Select(g => g.First());
#endif
	}

	public static IEnumerable<int> CumulativeSum(this IEnumerable<int> source)
	{
		//http://stackoverflow.com/a/4831908/57883
		int sum = 0;
		foreach (var item in source)
		{
			sum += item;
			yield return sum;
		}
	}

	///<summary>
	/// MyExtensions! Delimit aggregate a list of strings
	///</summary>
	public static string Delimit(this IEnumerable<string> values, string delimiter)
	{
		return string.Join(delimiter, values);
	}

	public static IEnumerable<T> Materialize<T>(this IEnumerable<T> set)
	{
		return set.ToArray();
	}

	public static IEnumerable<T> Prepend<T>(this IEnumerable<T> values, T head)
	{
		return new[] { head }.Concat(values);
	}

	public static IEnumerable<int> To(this int i, int exclusiveEnd)
	{
		return Enumerable.Range(i, exclusiveEnd - i);
	}

	#endregion

	#region DictionaryExtensions

	public static IReadOnlyDictionary<TKey, IEnumerable<TValue>> ToReadOnlyDictionary<TKey, TValue>(
		   this IDictionary<TKey, List<TValue>> toWrap)
	{
		IDictionary<TKey, IEnumerable<TValue>> intermediate = toWrap.ToDictionary(a => a.Key, a => a.Value.ToArray().AsEnumerable());

		IReadOnlyDictionary<TKey, IEnumerable<TValue>> wrapper = new ReadOnlyDictionary<TKey, IEnumerable<TValue>>(intermediate);
		return wrapper;
	}

	#endregion

	#region EnumExtensions
	//http://stackoverflow.com/a/417217/57883

	public static bool Has<T>(this System.Enum type, T value)
	{
		return (((int)(object)type & (int)(object)value) == (int)(object)value);
	}

	//needs work, requires the type of T to be specified even though it's kinda there in the params
	public static IEnumerable<T> ContainedValues<T>(this Enum flagEnum)
		where T : struct
	{
		//if(Enum.IsDefined(typeof(T),
		return from v in Enum.GetValues(typeof(T)).Cast<T>()
			   where flagEnum.Has(v)
			   select (T)v;
	}

	#endregion

	#region Linqpad

	public static T DumpIf<T>(this T val, Func<T, bool> predicate, string header = null)
	{
		if (predicate(val))
			val.Dump(header);
		return val;
	}

	public static T DumpProp<T>(this T val, Func<T, object> accessor, string header = null)
	{
		accessor(val).Dump(header);
		return val;
	}

	#endregion

	#region Serialization
#if ASP
    public static string Serialize<T>(this T value, MediaTypeFormatter formatter)
    {
        // http://www.asp.net/web-api/overview/formats-and-model-binding/json-and-xml-serialization

        string serialized;
        using (var stream = new MemoryStream())
        using (var content = new StreamContent(stream))
        {
            formatter.WriteToStreamAsync(
                typeof(T),
                value,
                stream,
                content,
                null).Wait();
            stream.Position = 0;
            serialized = content.ReadAsStringAsync().Result;
        }

        Trace.WriteLine("Serialized to " + serialized);
        return serialized;
    }

    public static IDictionary<string, object> DeserializeToDictionary(string serialized, MediaTypeFormatter formatter)
    {
        using (var stream = new MemoryStream())
        using (var writer = new StreamWriter(stream))
        {
            writer.Write(serialized);
            writer.Flush();
            stream.Position = 0;
            return formatter.ReadFromStreamAsync(typeof(IDictionary<string, object>), stream, null, null).Result as IDictionary<string, object>;
        }
    }

    public static T Deserialize<T>(string serialized, MediaTypeFormatter formatter) where T : class
    {
        using (var stream = new MemoryStream())
        using (var writer = new StreamWriter(stream))
        {
            writer.Write(serialized);
            writer.Flush();
            stream.Position = 0;
            return formatter.ReadFromStreamAsync(typeof(T), stream, null, null).Result as T;
        }
    }
#endif
	#endregion

	#region DataExtensions
	// http://madprops.org/blog/addwithvalue-via-extension-methods/

	public static int AddInputParameter<T>(this IDbCommand cmd,
		string name, T value)
	{
		var p = cmd.CreateParameter();
		p.ParameterName = name;
		p.Value = value;
		return cmd.Parameters.Add(p);
	}

	public static int AddInputParameter<T>(this IDbCommand cmd,
		string name, Nullable<T> value) where T : struct
	{
		var p = cmd.CreateParameter();
		p.ParameterName = name;
		p.Value = value.HasValue ? (object)value : DBNull.Value;
		return cmd.Parameters.Add(p);
	}

	public static int AddInputParameter(this IDbCommand cmd,
		string name, string value)
	{
		var p = cmd.CreateParameter();
		p.ParameterName = name;
		p.Value = string.IsNullOrEmpty(value) ? DBNull.Value : (object)value;
		return cmd.Parameters.Add(p);
	}

	public static IDbDataParameter AddOutputParameter(this IDbCommand cmd,
		string name, DbType dbType)
	{
		var p = cmd.CreateParameter();
		p.ParameterName = name;
		p.DbType = dbType;
		p.Direction = ParameterDirection.Output;
		cmd.Parameters.Add(p);
		return p;
	}

	#endregion
}


///bucket for generic stuff that wasn't an extension method
public static class My
{

	public static Hyperlinq ProcessStartLink(string fileName, string formattedArguments, string linkText)
	{
		linkText = linkText ?? fileName;
		var safeFileName = (fileName.Contains("\\") ? "@" : string.Empty) + "\"" + fileName + "\"";
		return new Hyperlinq(QueryLanguage.Expression, "System.Diagnostics.Process.Start(" + safeFileName + ",@\"" + formattedArguments + "\")", linkText);
	}

	public class LinqpadStorage
	{

		public string Path { get; private set; }
		public string Value { get; set; }

		public LinqpadStorage(string storageName)
		{
			var directory = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);

			System.IO.Directory.CreateDirectory(directory);
			Path = System.IO.Path.Combine(directory, storageName + ".json");
			if (System.IO.File.Exists(Path))
				Value = System.IO.File.ReadAllText(Value);
		}
		public void Save()
		{
			System.IO.File.WriteAllText(Path, Value);
		}


	}

	public static class StringUtilities
	{
		public static string RegexEncode(string input)
		{
			var tokens = new[] { "(", ")", "+", ".", "[", "^", "$", "*", "\\", "|", "?" }; //simple, ignores 2 character tokens
			var ouput = new StringBuilder();
			for (int i = 0; i < input.Length; i++)
			{
				if (tokens.Any(t => t == input[i].ToString()))
				{
					ouput.Append("\\" + input[i]);
				}
				else
				{
					ouput.Append(input[i]);
				}

			}
			return ouput.ToString();
		}
	}

	static IEnumerable<string> GetFiles(string startDir, string wildcard = null)
	{ //recursive
		IEnumerable<string> files = null;
		try
		{
			if (wildcard.IsNullOrEmpty() == false)
				files = System.IO.Directory.GetFiles(startDir, wildcard);
			else
				files = System.IO.Directory.GetFiles(startDir);
		}
		catch (Exception ex)
		{
			System.Diagnostics.Debug.WriteLine("Could not read files from " + startDir, ex);
		}
		if (files != null)
			foreach (var i in files)
			{
				yield return i;
			}
		IEnumerable<string> directories = null;
		try
		{
			directories = System.IO.Directory.GetDirectories(startDir);
		}
		catch (Exception ex)
		{
			System.Diagnostics.Debug.WriteLine("Could not directories read from " + startDir, ex);
		}
		if (directories != null)
			foreach (var dir in directories)
			{
				foreach (var f in GetFiles(dir, wildcard))
					yield return f;

			}
	}
	#region PInvoke //http://www.codeproject.com/script/Articles/ViewDownloads.aspx?aid=15633
	public static class PInvokeWrapper
	{


		[Flags]
		private enum EFileAccess : uint
		{
			GenericRead = 0x80000000,
			GenericWrite = 0x40000000,
			GenericExecute = 0x20000000,
			GenericAll = 0x10000000,
		}

		[Flags]
		private enum EFileAttributes : uint
		{
			Readonly = 0x00000001,
			Hidden = 0x00000002,
			System = 0x00000004,
			Directory = 0x00000010,
			Archive = 0x00000020,
			Device = 0x00000040,
			Normal = 0x00000080,
			Temporary = 0x00000100,
			SparseFile = 0x00000200,
			ReparsePoint = 0x00000400,
			Compressed = 0x00000800,
			Offline = 0x00001000,
			NotContentIndexed = 0x00002000,
			Encrypted = 0x00004000,
			Write_Through = 0x80000000,
			Overlapped = 0x40000000,
			NoBuffering = 0x20000000,
			RandomAccess = 0x10000000,
			SequentialScan = 0x08000000,
			DeleteOnClose = 0x04000000,
			BackupSemantics = 0x02000000,
			PosixSemantics = 0x01000000,
			OpenReparsePoint = 0x00200000,
			OpenNoRecall = 0x00100000,
			FirstPipeInstance = 0x00080000
		}
		[Flags]
		private enum EFileShare : uint
		{
			None = 0x00000000,
			Read = 0x00000001,
			Write = 0x00000002,
			Delete = 0x00000004,
		}
		private enum ECreationDisposition : uint
		{
			New = 1,
			CreateAlways = 2,
			OpenExisting = 3,
			OpenAlways = 4,
			TruncateExisting = 5,
		}
		/// <summary>
		/// Determines whether the specified path exists and refers to a junction point.
		/// </summary>
		/// <param name="path">The junction point path</param>
		/// <returns>True if the specified path represents a junction point</returns>
		/// <exception cref="IOException">Thrown if the specified path is invalid
		/// or some other error occurs</exception>
		public static bool JunctionExists(string path)
		{
			if (!Directory.Exists(path))
				return false;

			using (SafeFileHandle handle = OpenReparsePoint(path, EFileAccess.GenericRead))
			{
				string target = InternalGetTarget(handle);
				return target != null;
			}
		}
		/// <summary>
		/// The file or directory is not a reparse point.
		/// </summary>
		private const int ERROR_NOT_A_REPARSE_POINT = 4390;
		/// <summary>
		/// Command to get the reparse point data block.
		/// </summary>
		private const int FSCTL_GET_REPARSE_POINT = 0x000900A8;

		/// <summary>
		/// Reparse point tag used to identify mount points and junction points.
		/// </summary>
		private const uint IO_REPARSE_TAG_MOUNT_POINT = 0xA0000003;
		/// This prefix indicates to NTFS that the path is to be treated as a non-interpreted
		/// path in the virtual file system.
		/// </summary>
		private const string NonInterpretedPathPrefix = @"\??\";
		[StructLayout(LayoutKind.Sequential)]
		private struct REPARSE_DATA_BUFFER
		{
			/// <summary>
			/// Reparse point tag. Must be a Microsoft reparse point tag.
			/// </summary>
			public uint ReparseTag;

			/// <summary>
			/// Size, in bytes, of the data after the Reserved member. This can be calculated by:
			/// (4 * sizeof(ushort)) + SubstituteNameLength + PrintNameLength + 
			/// (namesAreNullTerminated ? 2 * sizeof(char) : 0);
			/// </summary>
			public ushort ReparseDataLength;

			/// <summary>
			/// Reserved; do not use. 
			/// </summary>
			public ushort Reserved;

			/// <summary>
			/// Offset, in bytes, of the substitute name string in the PathBuffer array.
			/// </summary>
			public ushort SubstituteNameOffset;

			/// <summary>
			/// Length, in bytes, of the substitute name string. If this string is null-terminated,
			/// SubstituteNameLength does not include space for the null character.
			/// </summary>
			public ushort SubstituteNameLength;

			/// <summary>
			/// Offset, in bytes, of the print name string in the PathBuffer array.
			/// </summary>
			public ushort PrintNameOffset;

			/// <summary>
			/// Length, in bytes, of the print name string. If this string is null-terminated,
			/// PrintNameLength does not include space for the null character. 
			/// </summary>
			public ushort PrintNameLength;

			/// <summary>
			/// A buffer containing the unicode-encoded path string. The path string contains
			/// the substitute name string and print name string.
			/// </summary>
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 0x3FF0)]
			public byte[] PathBuffer;
		}

		private static void ThrowLastWin32Error(string message)
		{
			throw new IOException(message, Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error()));
		}
		private static string InternalGetTarget(SafeFileHandle handle)
		{
			int outBufferSize = Marshal.SizeOf(typeof(REPARSE_DATA_BUFFER));
			IntPtr outBuffer = Marshal.AllocHGlobal(outBufferSize);

			try
			{
				int bytesReturned;
				bool result = DeviceIoControl(handle.DangerousGetHandle(), FSCTL_GET_REPARSE_POINT,
					IntPtr.Zero, 0, outBuffer, outBufferSize, out bytesReturned, IntPtr.Zero);

				if (!result)
				{
					int error = Marshal.GetLastWin32Error();
					if (error == ERROR_NOT_A_REPARSE_POINT)
						return null;

					ThrowLastWin32Error("Unable to get information about junction point.");
				}

				REPARSE_DATA_BUFFER reparseDataBuffer = (REPARSE_DATA_BUFFER)
					Marshal.PtrToStructure(outBuffer, typeof(REPARSE_DATA_BUFFER));

				if (reparseDataBuffer.ReparseTag != IO_REPARSE_TAG_MOUNT_POINT)
					return null;

				string targetDir = Encoding.Unicode.GetString(reparseDataBuffer.PathBuffer,
					reparseDataBuffer.SubstituteNameOffset, reparseDataBuffer.SubstituteNameLength);

				if (targetDir.StartsWith(NonInterpretedPathPrefix))
					targetDir = targetDir.Substring(NonInterpretedPathPrefix.Length);

				return targetDir;
			}
			finally
			{
				Marshal.FreeHGlobal(outBuffer);
			}
		}

		private static SafeFileHandle OpenReparsePoint(string reparsePoint, EFileAccess accessMode)
		{
			SafeFileHandle reparsePointHandle = new SafeFileHandle(CreateFile(reparsePoint, accessMode,
				EFileShare.Read | EFileShare.Write | EFileShare.Delete,
				IntPtr.Zero, ECreationDisposition.OpenExisting,
				EFileAttributes.BackupSemantics | EFileAttributes.OpenReparsePoint, IntPtr.Zero), true);

			if (Marshal.GetLastWin32Error() != 0)
				ThrowLastWin32Error("Unable to open reparse point.");

			return reparsePointHandle;
		}

		[DllImport("kernel32.dll", SetLastError = true)]
		private static extern IntPtr CreateFile(
		 string lpFileName,
		 EFileAccess dwDesiredAccess,
		 EFileShare dwShareMode,
		 IntPtr lpSecurityAttributes,
		 ECreationDisposition dwCreationDisposition,
		 EFileAttributes dwFlagsAndAttributes,
		 IntPtr hTemplateFile);
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		private static extern bool DeviceIoControl(IntPtr hDevice, uint dwIoControlCode,
	IntPtr InBuffer, int nInBufferSize,
	IntPtr OutBuffer, int nOutBufferSize,
	 out int pBytesReturned, IntPtr lpOverlapped);
	}
	#endregion
}