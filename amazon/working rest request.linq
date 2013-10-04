<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Tasks.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.EnterpriseServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Caching.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ServiceProcess.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.ApplicationServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.RegularExpressions.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Services.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Globalization</Namespace>
  <Namespace>System.Security.Cryptography</Namespace>
  <Namespace>System.Web</Namespace>
</Query>

void Main()
{
	//http://docs.aws.amazon.com/AWSECommerceService/latest/DG/rest-signature.html
	var accessKey=Util.GetPassword("AmazonAccessKeyId");
	var secretKey=Util.GetPassword("AmazonSecretKey");
	
	//needs signing
	var requestUri="http://webservices.amazon.com/onca/xml";
	var request=requestUri+"?Service=AWSECommerceService&AWSAccessKeyId="+accessKey+
		"&Operation=ItemLookup&ItemId=0679722769&ResponseGroup=ItemAttributes,Offers,Images,Reviews&Version=2009-01-06";
	var timeStamp=GetEC2Date();//"2009-01-01T12:00:00Z";
	
	var requestWithTime= request+"&Timestamp="+timeStamp;
	//2.
	var encoded=requestWithTime.Replace(",",Uri.HexEscape(',')).Replace(":",Uri.HexEscape(':')).Dump("step 2 done");
	//3.
	var split= encoded.After("?").Split('&')//.Select(s=>s+"\n")
		.Dump("step 3 done");
	//4.
	var sorted= split.OrderBy(a=>a.Before("="),new CanonicalizedDictCompare()).Dump("step 4 done");
	//5.
	var rejoined= sorted.Delimit("&").Dump("step 5 done");
	//6.
	var method="GET";
	var uri=new Uri(request);
	var host= uri.Host;
	var path =uri.AbsolutePath;
	var canonicalParts= new []{
		method,host,path,rejoined
	};
	var canonical= canonicalParts.Delimit("\n").Dump("step 6-7 done");
	
	//8.
	var auth=GetAWS3_SHA256AuthorizationValue(accessKey,secretKey,canonical).Dump("step 8 done");
	//9.
	var encAuth=Uri.EscapeDataString(auth).Dump("step 9 done");
	//10.
	var signedRequestUri= requestUri+"?"+rejoined+"&Signature="+encAuth;
	signedRequestUri.Dump("step 10 done");
}

static public string GetEC2Date() 
{ 
    //string httpDate = DateTime.UtcNow.ToString("s") + "Z"; 
    string httpDate = DateTime.UtcNow.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'Z'", 
                      DateTimeFormatInfo.InvariantInfo); 
    return httpDate; 
}

public static string GetAWS3_SHA256AuthorizationValue(string accessKeyId, 
              string secretAccessKey, string input) 
{ 
    var  signer = new System.Security.Cryptography.HMACSHA256(System.Text.Encoding.UTF8.GetBytes(secretAccessKey)); 

    var  signatureValue = Convert.ToBase64String(
           signer.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input))); 
 
    return signatureValue; 
}


// Define other methods and classes here
 internal class CanonicalizedDictCompare : IComparer<string>
  {
    #region IComparer<string> Members

    public int Compare(string x, string y)
    {
      return string.CompareOrdinal(x, y);
    }

    #endregion
  }