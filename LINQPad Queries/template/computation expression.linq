<Query Kind="FSharpProgram">
  <Connection>
    <ID>01133a54-bf1f-423a-b144-12acb2f6c661</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>hd</Database>
    <UserName>sa</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAAry/3yE9NCE6DnvfcIFtbCgAAAAACAAAAAAAQZgAAAAEAACAAAAAzy5gld67tCJJ0CzChoWaRaEonLQEL3WErGtHeNySfwwAAAAAOgAAAAAIAACAAAAC9pt88JJEbzEjGDn6avASLQDKHkH/DveHuj8xkoqzkWBAAAACYic7/FZCjyP1xQd6Og7HzQAAAALpwyi8RHLgOTRA+RpUCxw5uvupEffoF2gS5rVFbIY5AWvTq2iAcmNgU69C3fHSUC3Anh6wKyNK8QOTsxR/UHE4=</Password>
  </Connection>
</Query>

let dc = new TypedDataContext()

module Helpers =
    let flip f x y = f y x
    let curry f x y = f (x, y) 
    let uncurry f (x, y) = f x y
    let delimit (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)
    let split (d:string) (x:string) = x.Split([| d|],StringSplitOptions.None)
    
    let (|ValueString|NonValueString|) =
        function
        | null |"" -> NonValueString
        | x when String.IsNullOrWhiteSpace x -> NonValueString()
        | x -> ValueString x
        
        
    let (|Contains|_|) (d:string) =
        function
        |ValueString x when x.Contains d -> Some x
        | _-> None
        
    module Option =
        let getOrDefault y = function | Some x -> x | None -> y
        let add x map = match x with | Some (x,y) -> Map.add x y map | None -> map
        let ofValueString = function |ValueString x -> Some x | _ -> None
    module Map =
        let merge<'K ,'V when 'K : comparison> = Map.fold(fun acc (key:'K) (value:'V) -> Map.add key value acc)

    
    type ValueString (s:string) =
        let s = match s with |ValueString s -> s | _ -> invalidOp "String must be a valueString"
        static member CreateOpt x = 
            Option.ofValueString x
            |> Option.map ValueString
        member __.Value = s
            
    let rec indent indentation level (s:string) =  
        let indentation = List.replicate level indentation |> delimit ""
        if s.Contains "\r\n" then 
            s |> split "\r\n" |> Seq.map (indent indentation 1) |> delimit "\r\n"
        else 
            sprintf "%s%s" indentation s
            
    type IndentBuilder (level:int,indentation:string) =
        member __.Bind(x,f) =
            let result = f x |> indent
            result
        member x.Yield a = indent indentation level a
        member __.Delay f = f()
        member x.Combine (a,b) = sprintf "%s\r\n%s" a b
()    
open Helpers

let indent level = indent " " (4*level)
module Map = 
    let singleton x y = Map[x,y]
    
module Wpf =
    type ADict = Map<string,string>
    type TagDelegate = ADict*(ADict -> string)
    let tag name content (attr:Map<string,string>) =
        let attrs = 
            attr |> Map.toSeq |> Seq.map (fun (k,v) -> sprintf "%s=\"%s\"" k v) |> delimit " "
            |> function
                |null
                |"" -> ""
                | x -> " " + x
        match content with
        | NonValueString ->
            sprintf "<%s%s />" name attrs
        | Contains "\r\n" s ->
            [
                sprintf "<%s%s>" name attrs
                indent 1 <| sprintf "%s" s
                sprintf "</%s>"name
            ]
            |> delimit "\r\n"
        | ValueString s ->
                sprintf "<%s%s>%s</%s>" name attrs s name
            
    // error template info from https://blog.magnusmontin.net/2013/08/26/data-validation-in-wpf/
    let errorTemplate msg = 
        
        [
            "<Validation.ErrorTemplate>"
            indent 1 "<ControlTemplate>"
            indent 2 "<StackPanel>"
            indent 3 """<AdornedElementPlaceholder x:Name="textBox"/>"""
            indent 3 """<TextBlock Text="{Binding [0].ErrorContent}" Foreground="Red"/>"""
            indent 3 <| sprintf """<TextBlock Text="%s" Foreground="Red"/>""" msg
            indent 2 "</StackPanel>"
            indent 1 "</ControlTemplate>"
            "</Validation.ErrorTemplate>"
        ]
        |> delimit "\r\n"
        
        
    let gridRow (i:int) = 
        i
        |> string
        |> Map.add "Grid.Row" 
        
    let gridColumn (i:int) = 
        i
        |> string
        |> Map.add "Grid.Column"
    
    let binding = sprintf "{Binding %s}"
    let comboBox binder children attr = 
        tag "ComboBox" children attr
        
        
    type ComboBox = {DisplayMemberPath:string;SelectedValue:ValueString;SelectedValueMemberPath:string; ItemsSourceBinding:string;OtherAttrs:ADict}
        with override x.ToString() =
                let attrOpt n v = v |> Option.ofValueString |> Option.map(fun x -> n,x)
                let items = ("ItemsSource" ,binding x.ItemsSourceBinding)
                let displayMem = attrOpt "DisplayMemberPath" x.DisplayMemberPath
                let selVal = attrOpt "SelectedValueMemberPath" x.SelectedValueMemberPath
                
                let a = 
                    x.OtherAttrs
                    |> Map.add "SelectedValue" x.SelectedValue.Value
                    |> uncurry Map.add items
                    |> Option.add displayMem 
                    |> Option.add selVal
                
                tag "ComboBox" null a
            
    
    let textBox binder children attr =
        attr
        |> Map.add "Text" (binding binder)
        |> tag "TextBox" children
    let textBlock content attr =
        attr
        |> tag "TextBlock" content
        
    let label content attr =
        attr
        |> tag "Label" content
    let button content attr =
        attr
        |> tag "Button" content
        
    let wordWrap = Map["TextWrapping","Wrap"]

    type RowBuilder (i:int,startColumn:int) =
        let mutable j = startColumn
        let inc () = 
            j <- j + 1
        let addColumn (x:ADict,f) : string = 
            let result = x |> gridColumn j |> gridRow i
            inc()
            f result
        member self.Bind x = inc();x
        member x.YieldFrom a = a |> List.map addColumn
        member x.Yield a = [addColumn a]
        member __.Delay f = f()
        member x.Combine (a,b) : _ list = List.concat [a;b]
        
    let maxLength i = "MaxLength",sprintf "%i" i
    
open Wpf

//typeof<RowBuilder>.GetMethods().Dump()
let row i = RowBuilder(i)
let dobError = errorTemplate "The expected format is yyyy/mm/dd" |> sprintf "%s"
let plainTextBox name limit = (Map[maxLength limit], textBox name null)
let plainLabel (content:string) = (Map.empty,label content)
let plainBoth name humanized limit = [plainLabel humanized; plainTextBox name limit]
let genderCB = {DisplayMemberPath=null;SelectedValue=ValueString <|binding "Gender";SelectedValueMemberPath=null;ItemsSourceBinding="Genders";OtherAttrs=Map.empty}
let empOrCB = {DisplayMemberPath=null;SelectedValue=ValueString <|binding "EmployeeOrSpouse";SelectedValueMemberPath=null;ItemsSourceBinding="EmployeeOrSpouseOpts";OtherAttrs=Map.empty}
let unOrCB = {DisplayMemberPath=null;SelectedValue=ValueString <|binding "UnionOrNonUnion";SelectedValueMemberPath=null;ItemsSourceBinding="UnionOrNonUnionOpts";OtherAttrs=Map.empty}
let makeCb x = (Map.empty,(fun y -> {x with OtherAttrs = Map.merge x.OtherAttrs y} |> string))
let wrapLabel text = (Map.empty, label (textBlock text wordWrap))
let multiTextBox name humanized limit (cSpan:int) =  
            [   plainLabel humanized
                Map[    "Grid.ColumnSpan",string cSpan
                        "TextWrapping","Wrap"
                        "AcceptsReturn","True"
                        maxLength limit
                        "VerticalScrollBarVisibility","Visible"],textBox name null
                    ]
seq {
    yield
        row (1,0){
                yield (Map["Style","{StaticResource LabelWithRedAsterisk}"],label "First")
                yield (Map[maxLength 100; "MinWidth","200"],textBox "FirstName" null)
                yield wrapLabel "Middle Initial"
                yield (Map["MinWidth","40";maxLength 1],textBox "MiddleInitial" null)
                yield (Map["Style","{StaticResource LabelWithRedAsterisk}"],label "Last")
                yield (Map[maxLength 100; "MinWidth","200"],textBox "LastName" null)
            }
    yield
        row (2,0){
            yield plainLabel "MemberNumber"
            yield plainTextBox "MemberNumber" 50
            yield (Map.empty,label "Gender(m/f)")
            yield makeCb genderCB
            yield (Map["Style","{StaticResource LabelWithRedAsterisk}"],label "DOB")
            yield (Map[maxLength 10], textBox "Dob, StringFormat='yyyy-MM-dd'" dobError)
        }
    yield
        row (3,0){
            yield! plainBoth "EmployeeNumber" "EmployeeNumber" 50 
            yield wrapLabel "Employee Or Spouse"
            yield makeCb empOrCB
            yield plainLabel "Coverage Start"
            yield (Map[maxLength 10], textBox "CoverageStartDate, StringFormat='yyyy-MM-dd'" dobError)
        }
    yield row (4,2) { yield wrapLabel "Union Or Non-Union";yield makeCb unOrCB}
    yield row (5,0){
            yield! multiTextBox "EmailAddress" "Email" 255 3
        }
    yield row (5,4){
            yield! multiTextBox "OtherIdentifier" "Other Identifier" 255 3
        }
    yield row (6,0){
            yield! multiTextBox "ClientReportedLocation" "Client Reported Location" 255 3
        }
    yield row (6,4){
            yield! multiTextBox "ForeignSystemIdentifier" "ForeignSystemIdentifier" 255 3
        }
    yield row (7,0){
            yield! multiTextBox "CorehealthId" "CorehealthId" 255 3
        }
    yield row (7,4){
            yield! multiTextBox "PhysicalAddress" "Physical Address" 255 3
        }
    yield row (8,0){
            yield! multiTextBox "RelationshipLink" "RelationshipLink" 255 3
        }
    yield row (8,4){
            yield! multiTextBox "Notes" "Notes" 255 3
        }
    
    yield
        row (10,0) {
            yield (Map[ "Grid.ColumnSpan","9"
                        "VerticalAlignment","Bottom"
                        "Margin","0,5,0,0"
                        "Content","  Save  >>"
                        "HorizontalAlignment","Right"
                        "IsDefault","True"
                        "Command",binding "SaveCommand"], button null)
        }
}
|> Seq.map (Seq.map (indent 2) >> delimit "\r\n")
|> delimit "\r\n\r\n"
|> Dump
|> ignore