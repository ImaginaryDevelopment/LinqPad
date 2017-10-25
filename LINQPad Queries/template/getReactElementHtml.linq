<Query Kind="FSharpProgram">
  <NuGetReference>Selenium.WebDriver</NuGetReference>
</Query>

open OpenQA.Selenium

open OpenQA.Selenium.Chrome
let dumpt t x = x.Dump(description=t); x
Environment.CurrentDirectory
|> printfn "%A"
// set it for current process only
Environment.SetEnvironmentVariable("path",Environment.CurrentDirectory)
let findAndCopyIfMissing () = 
    let fn = "chromedriver.exe"
    let sourceFile = Path.Combine(Environment.ExpandEnvironmentVariables("%userprofile%"),"downloads","chromedriver_win32", fn)
    let targetFile = Path.Combine(Environment.CurrentDirectory,fn)
    if File.Exists sourceFile && (not <| File.Exists(Path.Combine(Environment.CurrentDirectory,fn)) || FileInfo(sourceFile).LastWriteTimeUtc > FileInfo(targetFile).LastWriteTimeUtc) then
        File.Copy(sourceFile, targetFile, true)
findAndCopyIfMissing()
Environment.ExpandEnvironmentVariables("%path%").Split(';').Dump()

let getPage (uri:string) wait elemByCssOpt = 
    printfn "getting page %s" uri
    try
        let driver = new ChromeDriver()
        driver.Navigate().GoToUrl(uri)
        let captureOpt = 
            elemByCssOpt
            |> Option.map (fun css ->
                driver.FindElement(By.CssSelector css).GetAttribute("innerHTML"))
        printfn "Captured?"
//        if wait then Util.ReadLine("") |> ignore
//        driver.FindElement(By.CssSelector("table.currency-table")) |> Dump |> ignore
        driver.Quit()
        captureOpt
    with 
        | :? DriverServiceNotFoundException as ex ->
            // in case whatever version we are using doesn't include a detailed error message
            printfn "The chromedriver.exe file does not exist in the current directory or in a directory on the PATH environment variable. The driver can be downloaded at http://chromedriver.storage.googleapis.com/index.html."
            reraise()

let simpleTest() = 
    getPage "http://www.google.com/" false
    
let poeNinja () = 
    getPage "http://poe.ninja/challenge/currency" false (Some "table.currency-table")

poeNinja()
|> dumpt "result"
|> ignore