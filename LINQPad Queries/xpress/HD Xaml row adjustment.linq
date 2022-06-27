<Query Kind="FSharpProgram" />

let path = @"C:\projects\HealthDesigns\Client\HD.Client.WPF\Pages\EventTSummary.xaml"

let updated = 
    File.ReadAllLines path
    |> Array.map(fun line ->
        let del = "Grid.Row="
        let ptrn = "Grid.Row=\"(-?\d+)\""
        match line.IndexOf del with
        | i when i < 0 -> line
        | _ ->
            Regex.Replace(line, ptrn, (fun m -> m.Groups.[1].Value |> int |> (+) 1 |> sprintf "Grid.Row=\"%i\"" ))
    )
    
File.WriteAllLines(path,updated)
|> Dump
|> ignore