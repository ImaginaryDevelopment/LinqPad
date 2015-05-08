<Query Kind="Expression">
</Query>

//WorkItemLongTexts.OrderByDescending(w=>w.AddedDate).Take (100)
//Util.RawHtml(WorkItemLongTexts.OrderByDescending(w=>w.AddedDate).First().Words.Replace("<br>",string.Empty))
//WorkItemLongTexts.Where(w=>w.FldID==54).OrderByDescending(w=>w.AddedDate)
WorkItemLongTexts
	.Where(w=>w.ID==4082 /* ticket number! */)
	.OrderByDescending(w=>w.AddedDate)
	.ToArray()
	.Select(i=>new{ i.AddedDate,i.FldID,i.ID,Words=Util.RawHtml(i.Words.Replace("<br>","<br />").Replace("&nbsp;","_"))})