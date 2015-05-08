<Query Kind="Program" />

void Main()
{
	var w=new System.Net.WebClient();
	w.BaseAddress="http://www.google.com";
	var my = new MyWebRequest(w);
	my.BaseAddress.Dump();
	Debug.Assert(my.BaseAddress==w.BaseAddress);
}

// Define other methods and classes here
public class MyWebRequest:System.Net.WebClient{
	public MyWebRequest(System.Net.WebClient original){
		PopulateFrom(original);
	}
	void PopulateFrom(System.Net.WebClient source)
            {
                var properties = source.GetType().GetProperties();
                foreach (var pi in properties)
                    if (pi.CanRead && pi.CanWrite)
                        pi.SetValue(this, pi.GetValue(source, null), null); //this will work only for classes inherited from BaseClass
            }
}
