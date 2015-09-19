<Query Kind="Program" />

void Main()
{
	
}
public static class AdoHelper{
// type TryParseDelegate<'t> = delegate of string*('t byref) -> bool
public delegate bool TryParseDelegate<T>(string s, out T result);

        /// <summary>
        /// will use casting if converter is null
        /// </summary>
        public static T? ReadValueMap<T>(this IDataRecord rec, string name, Func<object, T> converter = null)
            where T : struct
        {
            if (converter != null)
                return rec.ReadOr<T?>(name, o => converter(o));
            return rec.ReadOr(name, o => (T?)o);
        }

        /// <summary>
        /// used when the conversion has additional default cases
        /// will use casting if converter is null
        /// </summary>
        public static T? ReadValueMapC<T>(this IDataRecord rec, string name, Func<object, T?> converter = null)
            where T : struct
        {
            if (converter != null)
                return rec.ReadOr<T?>(name, o => converter(o));
            return rec.ReadOr(name, o => (T?)o);
        }

        public static T? ReadValueMapIf<T>(this IDataRecord rec, bool condition, string name, Func<object, T> converter = null)
            where T : struct
        {
            if (!condition)
                return default(T?);
            return rec.ReadValueMap(name, converter);
        }

        /// <summary>
        /// will use casting if converter is null
        /// </summary>
        public static T ReadMap<T>(this IDataRecord rec, string name, Func<object, T> converter = null)
            where T : class
        {
            if (converter != null)
                return rec.ReadOr(name, converter);
            return rec.ReadOr(name, o => (T)o);
        }

        public static string ReadToString(this IDataRecord rec, string name)
        {
            return rec.ReadMap(name, o => o.ToString());
        }

        public static string ReadToStringIf(this IDataRecord rec, bool condition, string name)
        {
            if (!condition)
                return null;
            return ReadToString(rec, name);
        }

        public static int? ReadInt(this IDataRecord rec, string name)
        {
            return rec.ReadValueMap(name, o => (int)o);
        }

        public static bool? ReadBool(this IDataRecord rec, string name)
        {
            return rec.ReadValueMap(name, o => (bool)o);
        }

        public static DateTime? ReadDateTime(this IDataRecord rec, string name)
        {
            return rec.ReadValueMap(name, o => (DateTime)o);
        }

        public static string ReadString(this IDataRecord rec, string name)
        {
            return rec.ReadMap(name, o => (string)o);
        }

        public static Guid? ReadGuid(this IDataRecord rec, string name)
        {
            return rec.ReadValueMap(name, o => (Guid)o);
        }

        public static T? TryReadValueMap<T>(this IDataRecord rec, string name, TryParseDelegate<T> tryParser)
            where T : struct
        {
            var value = rec.ReadToString(name);
            if (value == null)
                return null;

            T result;
            if (!tryParser(value, out result))
                return null;

            return new T?();
        }
		
		//let readOr<'t> (r:IDataRecord) (name:string) convertValue =
		//    try
		//        let value = r.[name]
		//        match value,convertValue with
		//        | empty, _ when empty = upcast dbNull -> Unchecked.defaultof<'t>
		//        | v, None -> v :?> 't
		//        | v, Some f ->  f v
		//    with
		//    |ex -> 
		//        ex.Data.Add("Read failure",name)
		//        reraise()
        static T ReadOr<T>(this IDataRecord rec, string name, Func<object, T> convertValue)
        {
            try
            {
                var value = rec[name];
                if (value == DBNull.Value)
                    return default(T);

                if (convertValue == null)
                    return (T)value;

                return convertValue(value);
            }
            catch (Exception ex)
            {
                ex.Data.Add("Read failure", name);
                throw;
            }
        }

}