<Query Kind="Statements" />

var file= @"C:\projects\Products\CVS\CVS.Member.Web4\ViewModels\TranslationServices.cs";
var lines= System.IO.File.ReadAllLines(file);
var q= from l in lines.Where(dline=>dline.Contains("public const string "))
		let declaration = l.After("public const string ").Before(" =")
		join frL in lines
			.SkipWhile(l=>l.Contains("fr-CA")==false)
			.TakeWhile(l=>l.Contains("}")==false)
			.Where(l=>l.Contains(","))
			.Select(l=>new{line=l,declaration=l.After("Strings.").Before(","),
				translation = l.After("\"").Before("\"")})
		on declaration equals frL.declaration into frLeft
		from fr in frLeft.DefaultIfEmpty()
		
		join enL in lines.SkipWhile(l=>l.Contains("fr-CA")==false)
			.SkipWhile(l=>l.Contains("}")==false)
			.SkipWhile(l=>l.Contains("{")==false).Skip(1)
			.TakeWhile(l=>l.Contains("}")==false)
			.Where(l=>l.Contains(","))
			.Select(l=>new{ line=l, declaration =l.After("Strings.").Before(","),
				translation = l.After("\"").Before("\"")})
			on declaration equals enL.declaration into enLeft
		from en in enLeft.DefaultIfEmpty()
		orderby declaration
		select new{ declaration, fr=fr!=null?fr.translation:null, en=en!=null?en.translation:null};
		new{ Total=q.Count(),
			French= q.Where(x=>x.fr.IsNullOrEmpty()==false).Count(),
			English= q.Where(x=>x.en.IsNullOrEmpty()==false).Count()
			}.Dump("count");
		q.Select(l=>"new Tr(\""+l.declaration+string.Format("\",\"{0}\",\"{1}\"",l.en,l.fr)+")").Delimit(","+Environment.NewLine).Dump();