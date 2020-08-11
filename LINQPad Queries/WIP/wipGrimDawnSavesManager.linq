<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.ZipFile.dll</Reference>
</Query>

// manage Grim dawn files
// https://stackoverflow.com/questions/51688525/how-to-upload-files-to-one-drive-from-net
module Paths =
    let saveFolder = @"D:\Users\DBee\My Documents\My Games\Grim Dawn\save"
    let oneDriveApiRoot = "https://api.onedrive.com/v1.0/"
    


let zip target src = System.IO.Compression.ZipFile.CreateFromDirectory(src,target)

let backupSaves () =
    saveFolder
    |> zip (sprintf "