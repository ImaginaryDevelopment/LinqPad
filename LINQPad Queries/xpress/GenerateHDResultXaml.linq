<Query Kind="FSharpProgram" />

let delimit (d:string) (x: string seq) = String.Join(d, x)
let delimit13 = delimit Environment.NewLine
module Option =
    let getOrDefault y =
        function
        | Some x -> x
        | None -> y
let indent (i,t) = sprintf "%s%s" ([0..i-1] |> Seq.map(ignore>>fun () -> "    ") |> delimit null) t

type MathyType = | Floaty | Inty |Decy
type NumericEntryRow = {LabelOpt:string; BindingName:string;FormatStringOpt:string;Editable:bool; Mathy:MathyType; ValidatorOpt: string}
let previousRows = 1
let generateRow i x =
    let i = i + previousRows
    printfn "Generating Row %i <| %s" i x.BindingName
    let label =
        x.LabelOpt |> Option.ofObj |> Option.getOrDefault x.BindingName
        |> sprintf """<Label Grid.Row="%i" Margin="0,0,50,0" FontWeight="Bold">%s</Label>""" i
    let userInput =
        [
            0, sprintf """<Border Grid.Row="%i" Grid.Column="1" BorderThickness="1" >""" i
            1, sprintf """<TextBox Text="{Binding %s.User, UpdateSourceTrigger=PropertyChanged}" />""" x.BindingName
            2, "<Border.Style>"
            3, """<Style TargetType="Border">"""
            4, """<Style.Triggers>"""
            5, sprintf """<DataTrigger Binding="{Binding %s.Error, Mode=OneWay, Converter={StaticResource hasValueConverter}, ConverterParameter={StaticResource True}}" Value="True">"""
                x.BindingName
            5, """<Setter Property="BorderBrush" Value="Red" />"""
            5, """<Setter Property="Background" Value="Red"/>"""
            5, """<Setter Property="ToolTip" Value="{Binding Height.Error}"/>"""
            4, "</DataTrigger>"
            3, "</Style.Triggers>"
            2, "</Style>"
            1, "</Border.Style>"
            0, "</Border>"

        ]
        |> Seq.map indent
        |> delimit13

    let makeDisplay j subField =
        let adds = x.FormatStringOpt |> Option.ofObj |> Option.map(fun fs -> sprintf ", StringFormat=%s%s" (if fs.StartsWith"{" || fs.Contains(",") then "{}" else null) fs) |> Option.getOrDefault null
        sprintf """<TextBlock Grid.Row="%i" Grid.Column="%i" Text="{Binding %s.%s%s%s}" Margin="10,0,0,0" Visibility="{Binding %s.%s, Converter={StaticResource hasValueConverter}}" VerticalAlignment="Center" />"""
            i j x.BindingName subField (if x.Editable then null else ".Value") adds x.BindingName subField
    [
        yield sprintf "<!-- %s -->" x.BindingName
        yield label
        if x.Editable then
            yield userInput
        yield makeDisplay 2 "Next"
        yield makeDisplay 3 "Prev"
        yield sprintf "<!-- /%s -->" x.BindingName
    ]
    |> delimit13
let input =
    [
            {LabelOpt=null; BindingName="SBP"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="DBP"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="TC"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="HDL"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="LDL"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt="Triglycerides"; BindingName="Trig"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="TCLDLRatio"; FormatStringOpt = "N1"; Editable = true;Mathy=Decy;ValidatorOpt=null}
            {LabelOpt="Glucose"; BindingName="Gluc"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="A1C"; FormatStringOpt = "N1"; Editable = true;Mathy=Floaty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="Cotinine"; FormatStringOpt = null; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt="Waist Circumference"; BindingName="Waist"; FormatStringOpt = "N1"; Editable = true;Mathy=Decy;ValidatorOpt=null}
            {LabelOpt=null; BindingName="Height"; FormatStringOpt = "{0} inches"; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="Weight"; FormatStringOpt = "{0} lbs."; Editable = true;Mathy=Inty;ValidatorOpt=null}
            {LabelOpt=null; BindingName="BMI"; FormatStringOpt = "N1"; Editable = false;Mathy=Inty;ValidatorOpt=null}
    ]
let generateValidator x =
    let gen parser = sprintf "function | %s x -> next.%s <- Nullable x; Choice1Of2 x | _ -> Choice2Of2 \"Invalid number\"" parser x.BindingName
    match x.ValidatorOpt |> Option.ofObj, x.Mathy with
    | None, Floaty -> gen "ParseFloat"
    | None, Inty -> gen "ParseInt"
    | None, Decy -> gen "ParseDecimal"
    | Some t,_ -> t
let toPascal (x:string) =
    x.[..0].ToLower() + x.[1..]

(input |> Seq.filter(fun x -> x.BindingName <> "Height" && x.BindingName <> "Weight" && x.BindingName <> "BMI")
    |> Seq.map (fun x ->
        sprintf "let %sComponent = Result%s(rv.Result |> Option.bind (fun x -> x.%s |> Option.ofNullable),%s)" (toPascal x.BindingName) (match x.Mathy with |Floaty -> "Float" | Inty -> "Int" | Decy -> "Decimal") x.BindingName (generateValidator x)
    )).Dump("comps")
(input |> Seq.filter(fun x -> x.BindingName <> "Height" && x.BindingName <> "Weight" && x.BindingName <> "BMI")
    |> Seq.map (fun x ->
        sprintf "member __.%s = %sComponent" x.BindingName (toPascal x.BindingName)
    )).Dump("props")
// there is an extra row margin between each row
(input |> Seq.mapi (fun i -> generateRow (i * 2))).Dump("gridText")
(input |> Seq.map(fun x ->
        sprintf """
            <!-- %s Row -->
            <RowDefinition Height="Auto"/>
            <RowDefinition MinHeight="10" Height="Auto"/>
        """ x.BindingName
) ).Dump("known row definitions")
