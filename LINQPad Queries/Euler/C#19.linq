<Query Kind="Program">
  <Connection>
    <ID>c9b76ea2-c7e7-4a9e-a336-ec4241c6dac5</ID>
    <Persist>true</Persist>
    <Server>rpsql2008r2dev</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>NOVA08RLS</Database>
    <UserName>WINRLS</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAA1WLKs9qc4USFiwcJ5tmkhgAAAAACAAAAAAADZgAAwAAAABAAAACrzDqjgelbVgZelHzxoUCkAAAAAASAAACgAAAAEAAAANaxhqC+PlSpfWS3MUIfULoIAAAAIqZ2P2kltAwUAAAAsZVmuOxGnoVr0wOGD/We2Jqm8Z4=</Password>
  </Connection>
</Query>

void Main()
{
	"one, two, three, four, five".ToCharArray().Where(a=>Char.IsLetter(a)).Count().Dump();
	"three hundred and forty-two".ToCharArray().Where(a=>Char.IsLetter(a)).Count().Dump();
	CountUp(1,99).Select(a=>a.ToCharArray().Where(Char.IsLetter).Count()).Sum().Dump();
}

// Define other methods and classes here
IEnumerable<string> CountUp(int start,int stop){
	
	var ones="one, two, three, four, five,six,seven,eight,nine".Split(',').Select(a=>a.Trim());
	ones.Select(a=>a.ToCharArray().Where(c=>Char.IsLetter(c)).Count()).Sum().Dump("ones");
	var tens="ten,twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety".Split(',').Select(a=>a.Trim());
	var hundreds=ones.Select(a=>a.Trim()+" hundred");
	
	foreach(var hundred in new[]{""}.Concat(hundreds)){
		if(hundred.IsNullOrEmpty()==false)
			yield return hundred;
		foreach(var ten in new[]{""}.Concat(tens)){
		
		var ht= (hundred.IsNullOrEmpty()?string.Empty:hundred+" and ")+ ten;
		if(ten.IsNullOrEmpty()==false)
				yield return ht;	
			foreach(var one in ones){
				
				var ho= (hundred.IsNullOrEmpty()?string.Empty:hundred+" and ")+ ten;
				if(ht.EndsWith("ten") && one.IsNullOrEmpty()==false)
				{
				var special=new[]{"eleven","twelve","thirteen","fourteen","fifteen",
					"sixteen","seventeen","eighteen","nineteen"};
					var safeHt=ht.Substring(0,ht.Length-3);
					switch(one){
						case "one":
							yield return safeHt+special[0];
							break;
						case "two":
							yield return safeHt+special[1];
							break;
							case "three":
							yield return safeHt+special[2];
							break;
						case "four":
							yield return safeHt+special[3];
							break;
							case "five":
							yield return safeHt+special[4];
							break;
							case "six":
							yield return safeHt+special[5];
							break;
							case "seven":
							yield return safeHt+special[6];
							break;
							case "eight":
							yield return safeHt+special[7];
							break;
							case "nine":
							yield return safeHt+special[8];
							break;
					}
					
					
				} else {
				yield return ht+ one;
				
				}
				
			}
		}
	}
	yield return "one thousand";
}