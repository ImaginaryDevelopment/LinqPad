<Query Kind="Statements" />

var path=System.Environment.ExpandEnvironmentVariables("%path%");
var semiCount=path.ToCharArray().Count(c=>c==';').Dump("semicolons");
var paths=path.Split(";".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
Func<string,string> RemoveQuotes= s=> s.AfterOrSelf("\"").BeforeOrSelf("\"");
var invalidPathSyntax=paths.Where(s=>s.Contains("\""));
var doesntExist=paths.Where(s=>System.IO.Directory.Exists(RemoveQuotes(s))==false );
var warningNoTrail=paths.Where(s=>RemoveQuotes(s).EndsWith("\\")==false);
invalidPathSyntax.Dump("invalid syntax");
doesntExist.Dump("path does not exist");
warningNoTrail.Dump("does not end with \\, might be invalid?");
path.Length.Dump("path length");
paths.Dump();