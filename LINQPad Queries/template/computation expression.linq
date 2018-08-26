<Query Kind="FSharpProgram" />

let delimit (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)
let tag name content (attr:Map<string,string>) =
    let attrs = 
        attr |> Map.toSeq |> Seq.map (fun (k,v) -> sprintf "%s=\"%s\"" k v) |> delimit " "
        |> function
            |null
            |"" -> ""
            | x -> " " + x
    sprintf "<%s%s>%s</%s>" name attrs content name
    
type ADict = Map<string,string>
type TagDelegate = ADict*(ADict -> string)
    
module Map = 
    let singleton x y = Map[x,y]
let gridRow (i:int) = 
    i
    |> string
    |> Map.add "Grid.Row" 
    
let gridColumn (i:int) = 
    i
    |> string
    |> Map.add "Grid.Column"

let binding = sprintf "{Binding %s}"
let textBox binder attr =
    attr
    |> Map.add "Text" (binding binder)
    |> tag "TextBox" null
let textBlock content attr =
    attr
    |> tag "TextBlock" content
    
let label content attr =
    attr
    |> tag "Label" content
    
let wordWrap = Map["TextWrapping","Wrap"]

type RowBuilder (i:int) =
    let mutable j = 0
    member self.Bind(x:ADict,f) = 
        let result = x |> gridColumn j |> gridRow i
        j <- j + 1
        f result
//    member __.Return x = x |> delimit "\r\n"
    member x.Yield a = [x.Bind a]
    member __.Delay f = f()
    member x.Combine (a,b) = List.concat [a;b]
    
    
let maxLength i = "MaxLength",sprintf "%i" i
//typeof<RowBuilder>.GetMethods().Dump()
let row i = RowBuilder(i)
seq{
    yield
        row 1 {
                yield (Map.empty,label "First")
                yield (Map[maxLength 100; "MinWidth","200"],textBox "FirstName")
                yield (Map.empty, label (textBlock "Middle Initial" wordWrap))
                yield (Map["MinWidth","40";maxLength 1],textBox "Middle")
                yield (Map.empty,label "Last")
                yield (Map[maxLength 100; "MinWidth","200"],textBox "LastName")
            }
    yield
        row 2 {
            yield (Map.empty,label "MemberNumber")
            yield (Map[maxLength 50], textBox "MemberNumber")
            yield (Map.empty,label "Gender(m/f)")
            yield (Map.empty,textBox "Gender")
            yield (Map.empty,label "DOB")
            yield (Map.empty, textBox "Dob, StringFormat='yyyy-MM-dd'")
        }
}
|> Seq.map(List.map(sprintf "        %s")>> delimit "\r\n")
|> delimit "\r\n\r\n"
|> Dump
|> ignore
