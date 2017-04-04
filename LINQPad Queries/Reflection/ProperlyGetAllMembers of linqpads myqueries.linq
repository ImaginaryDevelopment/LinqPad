<Query Kind="Expression" />

// per http://forum.linqpad.net/discussion/1115/specify-linqpad-connection-programatically#latest
// linqpad Query objects implement a dump customization, so to see all the properties you do this
Util.GetMyQueries().Select(x => Util.ToExpando(x))