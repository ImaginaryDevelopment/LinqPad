<Query Kind="FSharpExpression" />

let basePath = """ "C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe" """.Trim()
[
    "--allow-file-access-from-files"
    "--bwsi" 
    //"--disable-features=IsolateOrigins,site-per-process"
    "--guest"
    "--disable-site-isolation-trials"
    "--disable-web-security"
    """--user-data-dir="C:\temp" """.TrimEnd() // it did not seem to work without this
]
|> List.sort
|> fun x -> basePath::x
|> String.concat " "
