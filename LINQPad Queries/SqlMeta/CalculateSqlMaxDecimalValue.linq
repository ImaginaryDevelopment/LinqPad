<Query Kind="FSharpProgram" />


module SqlDecimal = 
    let calcSqlMaxPrecision precision scale= 
        if precision > 38 then raise <| ArgumentOutOfRangeException("precision",precision, "Out of range")
        if scale > precision then raise <| InvalidOperationException("scale must be less than or equal to precision")
        if scale < 0 then raise <| ArgumentOutOfRangeException("scale",scale, "Out of range")
        Convert.ToDecimal(Math.Pow(10., float precision - float scale)) - Convert .ToDecimal(Math.Pow(10., float scale * -1.))
    let getSqlType precision scale = 
        sprintf "decimal(%i,%i)" precision scale
    
    // create query that will fail if our calculated maximum doesn't fit within it's declared space
    let getMaxValueString p s :string= 
        let tryCalc name f = 
            try
                f()
            with ex -> 
                ex.Data.Add("subCalc",name)
                ex.Data.Add("p", p)
                ex.Data.Add("s",s)
                reraise()
        let intPart = 
            tryCalc "intPart" (fun () ->
                [0.. p-s] |> Seq.map (fun _ -> "9") |> Seq.reduce (fun a b -> a + b)
            )
            
                
        let decPart = 
            tryCalc "decPart" (fun () -> 
                if s>=1 then  [1..s] |> Seq.map (fun _ -> "9") |> Seq.reduce (fun a b -> a + b) else String.Empty
            )
            
        sprintf "%s%s" intPart (if String.IsNullOrWhiteSpace decPart then String.Empty else sprintf ".%s" decPart)
        
    let getFailScaleString p s : string = 
        let result = getMaxValueString p s |> string |> fun x -> (if x.IndexOf "." = 1 then "0" else String.Empty) + x.[1..] + (if x.IndexOf "." < 0 then "." else String.Empty) + "0"
        
        result

open SqlDecimal
type MetaData = {
    Precision:int
    Scale:int
} with 
    static member FormatCurrency (x:decimal) scale = x.ToString(sprintf "C%i" scale)
    
    static member GenerateTruncateClause x = sprintf "select '%s' as expected, @test as actual1, Convert(varchar(max),@test) as actual2, case when Convert(varchar(max),@test) = '%s' then 'pass' else 'truncated' end as valueCheck" x x
    static member GenerateDeclaration (value:string) p s = 
        let scaleCount = if value.IndexOf '.' >=0 then value.Length - value.IndexOf '.' - 1 else s
        printfn "Generating declaration for %s with p=%A s = %A scaleCount = %A" value p s scaleCount
        let printScale = Math.Max(s,scaleCount)
        let printVisual:string = sprintf "print 'checking %s'" (MetaData.FormatCurrency (decimal value) printScale).[1..]
        let declaration :string= sprintf "declare @test %s = %s" (getSqlType p s) value
        sprintf "%s\r\n%s" printVisual declaration
    static member GenerateOverflowTest p s : string = 
        let maxClause :string= getMaxValueString p s
        MetaData.GenerateDeclaration maxClause p s
        |> sprintf "%s\r\nprint 'test failed, it should have thrown an overflow'"
    static member GenerateTruncationTest p s :string=
        let value :string= getFailScaleString p s
        let d = MetaData.GenerateDeclaration value p s
        let truncationClause :string= MetaData.GenerateTruncateClause value
        sprintf "%s\r\n%s" d truncationClause
        
    member x.MaxValue = calcSqlMaxPrecision x.Precision x.Scale
    member x.Currency = MetaData.FormatCurrency x.MaxValue x.Scale
    member x.FailScale = getFailScaleString x.Precision x.Scale
    // unimplemented:
    //member x.FailCurrency = 
    
    // checksums/visual inspections of work:
    //member x.DigitsBeforeDecimalInMax = Math.Truncate(x.MaxValue) |> int64 |> string |> fun x -> x.Length
    //member x.MaxDigitsBeforeDecimal = int64 x.MaxValue |> string |> fun x -> x.Length
    //member x.SqlType = getSqlType x.Precision x.Scale
    member x.ShouldOverflow =         MetaData.GenerateOverflowTest x.Precision x.Scale
    // any number of digits longer in the decimal than scale should fail, right?
    
    member x.ShouldTruncate = MetaData.GenerateTruncationTest x.Precision x.Scale
//    member x.ShouldTruncate = 
        
        
[ 
    18,6 //(reference value) (verified overflow: 9999999999991) (verified truncate: 
    //declare @Dec decimal(1,0) = 0.112
    //select @dec, case when Convert(varchar(max),@dec) = '0.112' then 'pass' else 'truncated' end as valueCheck
    1,1 // 0.1 - 0.9  (verified overflow values: 1.0) (verified truncated: 0.01)
    18,0
    9,2
    19,2
    9,9
]
|> Seq.map (fun (p,s) ->
    let maxValue = calcSqlMaxPrecision 9 2
    let maxBeforeDecimal = int maxValue |> string |> fun x -> x.Length
    let result = 
        {   Precision = p
            Scale = s }
//    printfn "Created one!"
    result
)
|> Dump
|> ignore
//sample from msdn:
//Convert.ToDecimal(Math.Pow(10.,18.-6.)) - Convert.ToDecimal(Math.Pow(10., -6.))