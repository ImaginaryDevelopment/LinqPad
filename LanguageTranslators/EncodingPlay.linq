<Query Kind="Statements" />

//http://csharp.2000things.com/2014/01/09/1007-getting-length-of-string-that-contains-surrogate-pairs/
//http://csharp.2000things.com/tag/unicode/

//Note that because a surrogate pair requires more then 2 bytes, you cannot represent a surrogate pair within a single character (System.Char) literal.

var trippers = new []{ "\u0FFE","\u00e9","\u0FEFC","𠈓","\u0100","\uD840\uDE13","\U00020213","\uD840\uDE13","Ā丁"};

var coders = new System.Text.Encoding[]{ new ASCIIEncoding(), new UTF7Encoding(),new UTF8Encoding(), new UnicodeEncoding(), new UTF32Encoding()};
var q = from t in trippers
	from encoding in coders
	let input = Encoding.Unicode.GetBytes(t)
	let output = System.Text.Encoding.Convert(Encoding.Unicode, encoding,input)
	let reassembled = encoding.GetString(output)
	let poorlyReassembled = Encoding.Unicode.GetString(output)
	//orderby encoding.BodyName
	
	select new{ t,reassembled,asUni=poorlyReassembled, Encoding=encoding.BodyName,
		AreEqual =Util.HighlightIf( string.Equals(t,reassembled),v=>!v), input,output,encoding.IsSingleByte
		};
//q.Dump();

q.GroupBy (x => new{x.Encoding,x.IsSingleByte},x=>new{x.t,x.reassembled,x.asUni, x.AreEqual, x.input,x.output}).Dump();

//if you try to dump a char that is invalid linqpad vomits =)
// some characters are invalid chars in some encodings.
// and there are character elements of a language that are valid in a string, that are too big for a char to hold


Encoding.Default.Dump("system encoding");