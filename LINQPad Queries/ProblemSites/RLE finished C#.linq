<Query Kind="Program" />

public static class Program
{
	/// <summary>
	/// Encode the input data with basic RLE; each block of
	/// contiguous bytes should be represented by the length
	/// (one byte, maximum block-length 255), followed by the
	/// value (one byte). An input with a contiguous sequence
	/// more than 255 will be represented as multiple blocks.
	/// </summary>
	/// <remarks>
	/// It is not expected that any solution is super-awesome optimized,
	/// or even very pretty; the idea is simply to look at how you approach
	/// the problem
	/// </remarks>
	static byte[] RunLengthEncode(byte[] input)
	{
		//your code here.
		var output = new List<byte>();
		var current = new {Current = (byte?) null, Count = 0};
		Action<byte> fChange = b =>
		{
			var x = current.Count;
			while (x > 255)
			{
				output.Add((byte)255);
				output.Add(b);
				x = x - 255;
			}
			output.Add((byte)x);
			output.Add(current.Current.Value);
		};
		foreach(var b in input)
		{
			if(!current.Current.HasValue)
			{
				current = new {Current=(byte?)b, Count = 1};
			} else if(current.Current == b) {
				current = new {Current=current.Current, Count = current.Count + 1};
			} else {
				fChange(current.Current.Value);
				current = new { Current = (byte?)b, Count = 1 };
			}
		}
		if(current.Count > 0 && current.Current.HasValue) {
			fChange(current.Current.Value);
		}
		return output.ToArray();
	}
		

	/// <summary>Basic console runner</summary>
	static void Main()
	{
		// some arbitrary tests of sample input / output, but please feel free to
		// use your own approach for validation etc if you prefer
		bool allGood = true;
		allGood &= Test(new byte[] { 1, 1, 1, 2, 2, 2 }, new byte[] { 3, 1, 3, 2 });
		allGood &= Test(new byte[] { 0, 1, 2, 3 }, new byte[] { 1, 0, 1, 1, 1, 2, 1, 3 });
		allGood &= Test(new byte[] { }, new byte[] { });
		var raw = new byte[300];
		for (int i = 0; i < 270; i++)
			raw[i] = 12;
		for (int i = 270; i < 300; i++)
			raw[i] = 3;
		allGood &= Test(raw, new byte[] { 255, 12, 15, 12, 30, 3 });

		Console.WriteLine();
		Console.WriteLine(allGood ? "all good!" : "sorry, something isn't right");
		Console.ReadLine();
	}
	
	/// <summary>Really low grade unit tester, to avoid any dependencies</summary>
	static bool Test(byte[] input, byte[] expectedOutput)
	{
		bool pass;
		try
		{
			var actualOutput = RunLengthEncode(input);
			pass = actualOutput.SequenceEqual(expectedOutput);
			if (!pass)
			{
				(actualOutput.Select(x => (int)x),expectedOutput.Select(x => (int)x)).Dump("Failing");
				Console.Error.WriteLine("Expected: {0}", BitConverter.ToString(expectedOutput));
				Console.Error.WriteLine("Actual: {0}", BitConverter.ToString(actualOutput));
			}
		}
		catch (Exception ex)
		{
			Console.Error.WriteLine(ex.Message);
			pass = false;
		}
		Console.WriteLine(pass ? "pass" : "fail");
		return pass;
	}
}