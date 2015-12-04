<Query Kind="Statements" />

// enlarge VM
//untested
var cmd = Util.Cmd(@"C:\program files\oracle\virtualbox\vboxmanage.exe", @"modifyhd ""C:\Users\PROG7\VirtualBox VMs\CitrixEmulator Clone\CitrixEmulator Clone.vdi"" --resize 42786");
cmd.Dump();