<Query Kind="Program">
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Collections.Generic</Namespace>
  <Namespace>System.IO</Namespace>
  <Namespace>System.Net.Sockets</Namespace>
  <Namespace>System.Text</Namespace>
  <Namespace>System.Threading</Namespace>
</Query>

//Talk to minecraft C# - https://gist.github.com/Fireflies/2480d14fbbb33b4bbae3

//#if DEBUG
//using System.Diagnostics;
//#endif

class ServerPing
{
	private static readonly Dictionary<char, ConsoleColor> Colours = new Dictionary<char, ConsoleColor>
		{
			 { '0', ConsoleColor.Black       },
			 { '1', ConsoleColor.DarkBlue    },
			 { '2', ConsoleColor.DarkGreen   },
			 { '3', ConsoleColor.DarkCyan    },
			 { '4', ConsoleColor.DarkRed     },
			 { '5', ConsoleColor.DarkMagenta },
			 { '6', ConsoleColor.Yellow      },
			 { '7', ConsoleColor.Gray        },
			 { '8', ConsoleColor.DarkGray    },
			 { '9', ConsoleColor.Blue        },
			 { 'a', ConsoleColor.Green       },
			 { 'b', ConsoleColor.Cyan        },
			 { 'c', ConsoleColor.Red         },
			 { 'd', ConsoleColor.Magenta     },
			 { 'e', ConsoleColor.Yellow      },
			 { 'f', ConsoleColor.White       },
			 { 'k', Console.ForegroundColor  },
			 { 'l', Console.ForegroundColor  },
			 { 'm', Console.ForegroundColor  },
			 { 'n', Console.ForegroundColor  },
			 { 'o', Console.ForegroundColor  },
			 { 'r', ConsoleColor.White       }
		};

	private static NetworkStream _stream;
	private static List<byte> _buffer;
	private static int _offset;

	private static void Main(string[] args)
	{
		//Console.Title = "Minecraft Server Ping";

		var client = new TcpClient();
		var task = client.ConnectAsync("192.168.0.117", 25565);
		Console.WriteLine("Connecting to Minecraft server..");

		while (!task.IsCompleted)
		{
#if DEBUG
                Debug.WriteLine("Connecting..");
#endif
			Thread.Sleep(250);
		}

		if (!client.Connected)
		{
			Console.ForegroundColor = ConsoleColor.Red;
			Console.WriteLine("Unable to connect to the server");
			Console.ResetColor();
			Console.ReadKey(true);
			Environment.Exit(1);
		}

		_buffer = new List<byte>();
		_stream = client.GetStream();
		Console.WriteLine("Sending handshake request");


		/*
		 * Send a "Handshake" packet
		 * http://wiki.vg/Server_List_Ping#Ping_Process
		 */
		WriteVarInt(47);
		WriteString("localhost");
		WriteShort(25565);
		WriteVarInt(1);
		Flush(0);
		"handshake finished".Dump();
		/*
		 * Send a "Status Request" packet
		 * http://wiki.vg/Server_List_Ping#Ping_Process
		 */
		Flush(0);

		var buffer = new byte[4096];
		_stream.Read(buffer, 0, buffer.Length);

		try
		{
			var length = ReadVarInt(buffer);
			var packet = ReadVarInt(buffer);
			var jsonLength = ReadVarInt(buffer);
#if DEBUG
                Console.WriteLine("Received packet 0x{0} with a length of {1}", packet.ToString("X2"), length);
				
#endif
			var json = ReadString(buffer, jsonLength);
			json.Dump("json");
			var ping = JsonConvert.DeserializeObject<PingPayload>(json);

			Console.WriteLine("Software: {0}", ping.Version.Name);
			Console.WriteLine("Protocol: {0}", ping.Version.Protocol);
			Console.WriteLine("Players Online: {0}/{1}", ping.Players.Online, ping.Players.Max);
			WriteMotd(ping);

		}
		catch (IOException ex)
		{
			/*
			 * If an IOException is thrown then the server didn't 
			 * send us a VarInt or sent us an invalid one.
			 */
			Console.ForegroundColor = ConsoleColor.Red;
			Console.WriteLine("Unable to read packet length from server,");
			Console.WriteLine("are you sure it's a Minecraft server?");
#if DEBUG
                Console.WriteLine("Here are the details:");
                Console.WriteLine(ex.ToString());
#endif
			Console.ResetColor();
		}
	}

	private static void WriteMotd(PingPayload ping)
	{
		Console.Write("Motd: ");
		var chars = ping.Motd.ToCharArray();
		for (var i = 0; i < ping.Motd.Length; i++)
		{
			try
			{
				if (chars[i] == '\u00A7' && Colours.ContainsKey(chars[i + 1]))
				{
					Console.ForegroundColor = Colours[chars[i + 1]];
					continue;
				}
				if (chars[i - 1] == '\u00A7' && Colours.ContainsKey(chars[i]))
				{
					continue;
				}
			}
			catch (IndexOutOfRangeException)
			{
				// End of string
			}
			Console.Write(chars[i]);
		}
		Console.WriteLine();
		Console.ResetColor();
	}

	#region Read/Write methods
	internal static byte ReadByte(byte[] buffer)
	{
		var b = buffer[_offset];
		_offset += 1;
		return b;
	}

	internal static byte[] Read(byte[] buffer, int length)
	{
		var data = new byte[length];
		Array.Copy(buffer, _offset, data, 0, length);
		_offset += length;
		return data;
	}

	internal static int ReadVarInt(byte[] buffer)
	{
		var value = 0;
		var size = 0;
		int b;
		while (((b = ReadByte(buffer)) & 0x80) == 0x80)
		{
			value |= (b & 0x7F) << (size++ * 7);
			if (size > 5)
			{
				throw new IOException("This VarInt is an imposter!");
			}
		}
		return value | ((b & 0x7F) << (size * 7));
	}

	internal static string ReadString(byte[] buffer, int length)
	{
		var data = Read(buffer, length);
		return Encoding.UTF8.GetString(data);
	}

	internal static void WriteVarInt(int value)
	{

		while ((value & 128) != 0)
		{
			_buffer.Add((byte)(value & 127 | 128));
			value = (int)((uint)value) >> 7;
		}
		_buffer.Add((byte)value);
		new { value = value, _buffer = _buffer }.Dump("after writeVarInt");
	}

	internal static void WriteShort(short value)
	{
		_buffer.AddRange(BitConverter.GetBytes(value));
	}

	internal static void WriteString(string data)
	{
		var buffer = Encoding.UTF8.GetBytes(data);
		WriteVarInt(buffer.Length);
		_buffer.AddRange(buffer);
	}

	internal static void Write(byte b)
	{
		_stream.WriteByte(b);
	}

	internal static void Flush(int id = -1)
	{
		var buffer = _buffer.ToArray();
		_buffer.Clear();

		var add = 0;
		var packetData = new[] { (byte)0x00 };
		if (id >= 0)
		{
			WriteVarInt(id);
			packetData = _buffer.ToArray();
			add = packetData.Length;
			_buffer.Clear();
		}

		WriteVarInt(buffer.Length + add);
		var bufferLength = _buffer.ToArray();
		_buffer.Clear();

		_stream.Write(bufferLength, 0, bufferLength.Length);
		_stream.Write(packetData, 0, packetData.Length);
		_stream.Write(buffer, 0, buffer.Length);
		_offset.Dump("offset after flush");
	}
	#endregion
}

#region Server ping 
/// <summary>
/// C# represenation of the following JSON file
/// https://gist.github.com/thinkofdeath/6927216
/// </summary>
class PingPayload
{
	/// <summary>
	/// Protocol that the server is using and the given name
	/// </summary>
	[JsonProperty(PropertyName = "version")]
	public VersionPayload Version { get; set; }

	[JsonProperty(PropertyName = "players")]
	public PlayersPayload Players { get; set; }

	[JsonProperty(PropertyName = "description")]
	public string Motd { get; set; }

	/// <summary>
	/// Server icon, important to note that it's encoded in base 64
	/// </summary>
	[JsonProperty(PropertyName = "favicon")]
	public string Icon { get; set; }
}

class VersionPayload
{
	[JsonProperty(PropertyName = "protocol")]
	public int Protocol { get; set; }

	[JsonProperty(PropertyName = "name")]
	public string Name { get; set; }
}

class PlayersPayload
{
	[JsonProperty(PropertyName = "max")]
	public int Max { get; set; }

	[JsonProperty(PropertyName = "online")]
	public int Online { get; set; }

	[JsonProperty(PropertyName = "sample")]
	public List<Player> Sample { get; set; }
}

class Player
{
	[JsonProperty(PropertyName = "name")]
	public string Name { get; set; }

	[JsonProperty(PropertyName = "id")]
	public string Id { get; set; }
}
#endregion