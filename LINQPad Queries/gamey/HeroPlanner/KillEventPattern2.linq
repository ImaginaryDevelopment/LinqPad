<Query Kind="FSharpProgram" />

let getGroupValue(x:int) (m:Match) = m.Groups.[x].Value
let getGroupNValue(x:string) (m:Match) = m.Groups.[x].Value
let trimEnd(x:string) = x.TrimEnd()
let replace d r (x:string) =
    x.Replace(d,newValue=r)
let remove d x =
    replace d String.Empty x
  
let replace1 (d:string) r (x:string) =
    let start = x.IndexOf(d)
    let result = String.concat "" [x.[0..start - 1];r;x.[start+d.Length..] ]
//    (d,r,x,start,result).Dump("replaced 1")
    result
    
let rMatch p x =
    Regex.Match(input=x,pattern=p)
let rReplace p r x =
    Regex.Replace(x,pattern=p,replacement=r)
let killWs =
    String.collect(string>> fun c -> if String.IsNullOrWhiteSpace c then " " else Regex.Escape c)
    >> rReplace @"\s+" @"\s+"
let matchesM p x = Regex.Matches(x,pattern=p,options=RegexOptions.Multiline)
let targetField =
     """                [AccessedThroughProperty("ibTotals")]
        ImageButton _ibTotals;"""
            
let targetTemplate= 
    """ImageButton ibTotals
        {
            get
            {
                return this._ibTotals;
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                ImageButton.ButtonClickedEventHandler clickedEventHandler = new ImageButton.ButtonClickedEventHandler(this.ibTotals_ButtonClicked);
                if (this._ibTotals != null)
                    this._ibTotals.ButtonClicked -= clickedEventHandler;
                this._ibTotals = value;
                if (this._ibTotals == null)
                    return;
                this._ibTotals.ButtonClicked += clickedEventHandler;
            }
        }"""
type ReplInfo =  {Type:string;PropName:string;Handler:string;Event:string;Field:string} // ;FieldP:string}    
type Patterning = {ReplInfo:ReplInfo;PropP:string} // ;FieldP:string}    
// ok so frequently the field names and prop names contain their conterparts, escape counterparts in descending size, then replace escapes in ascending size
let makeFieldPattern {Type=typeName;PropName=propName;Field=fieldName}=
    targetField
    |> killWs
    |> replace "_ibTotals" "$$fname$$"
    |> replace "ibTotals" "$$pname$$"
    |> replace "ImageButton" typeName
    |> replace "$$pname$$" propName
    |> replace "$$fname$$" fieldName
let makePropPattern {Type=typeName;PropName=prop;Handler=h;Event=e;Field=f} =
    let failWord ="ButtonButtonClickeded"
    let replace x y z =
        let result = replace x y z
        if result.Contains(failWord) then
            (x,y).Dump("failing")
            failwithf "found failword"
        result
    let result =
        targetTemplate
        |> killWs
        |> replace "ibTotals_ButtonClicked" "$$h$$"
        |> replace "_ibTotals" "$$fname$$"
        |> replace "ibTotals" "$$pname$$"
        |> replace1 "ImageButton" typeName
        |> replace "$$pname$$" prop
        |> replace "$$fname$$" f
        |> replace "$$h$$" h
        |> replace "ButtonClicked" e
    if result.Contains(failWord) then
        failwithf "bad prop pattern"
    result
    
let a1p=
    let typeName,propName =
        let m = Regex.Match(targetTemplate,@"^\s*(\w+)\s+(\w+)")
        m |> getGroupValue 1, m |> getGroupValue 2
    let ri = {  Type =typeName;PropName=propName
                Handler=
                    let m = Regex.Match(targetTemplate,@"EventHandler\(this\.(\w+_\w+)")
                    m |> getGroupValue 1
                Event=
                    let m = Regex.Match(targetTemplate,@"\s*this._\w+\.(\w+) \+=")
                    m |> getGroupValue 1
                Field=
                    let m = Regex.Match(targetField, @"\w+ (_\w+);")
                    m |> getGroupValue 1
                }
            
    let singleName mp name tp input =
        input
        |> replace1 mp (sprintf "(?<%s>%s)" name tp)
    
    let namedRepeat mp name tp input =
        singleName mp name tp input
        |> replace mp (sprintf "\k<%s>" name)
    let wordP ="[_\w]+"
    let propPattern =
        targetTemplate
        |> killWs
        |> singleName ri.Handler "h" wordP
        |> namedRepeat ri.Field "field" wordP
        |> namedRepeat ri.PropName "prop" wordP
        |> namedRepeat ri.Type "type" wordP
        |> namedRepeat ri.Event "event" wordP
    {ReplInfo=ri;PropP=propPattern} //;FieldP=fieldPattern}
let targetFile = @"C:\projects\CoHDesigner2\Hero Designer\frmMain.cs"
let text =  File.ReadAllText targetFile
let names = ResizeArray<string>()
let toRip =
    text
    |> matchesM a1p.PropP
    |> Seq.cast<Match>
    |> Seq.choose(fun m -> 
        let gnv x = m |> getGroupNValue x
        let propInfo ={PropName=gnv "prop";Type=gnv "type"; Event=gnv "event";Field=gnv "field";Handler=gnv "h"}
        let fieldMatch = text |> rMatch (makeFieldPattern propInfo)
        let propMatch =
            let p = makePropPattern propInfo
            let m = text |> rMatch p
            if not m.Success || String.IsNullOrWhiteSpace m.Value then
                (p |> replace @"\s+" " ",propInfo,fieldMatch.Value).Dump()
                failwithf "bad prop match"
            m
        if fieldMatch.Success then
            names.Add propInfo.PropName
            Some (propInfo,fieldMatch.Value, propMatch.Value)
        else propInfo.Dump("unmatched");None
    )
    |> List.ofSeq
let runIt() =
    names.Dump("names")
    if names.Count > 0 then
        (text,toRip)
        ||>Seq.fold(fun text (pi,fm,pm) ->
            let removeAssert n (x:string) =
                let next = remove n x
                if x.Length = next.Length then (n,x,next).Dump(); failwithf "Bad field remove"
                else next
            text
            |> removeAssert fm
            |> replace pm (sprintf "%s %s; // this.%s.%s += %s\r\n" pi.Type pi.PropName pi.PropName pi.Event pi.Handler)
        )
    else null
let diag() =
    toRip
    |> Seq.map(fun (x,fn,pn) ->
        x,fn,text.IndexOf fn, pn, text.IndexOf pn
    )
//    |> Seq.groupBy(fun (x,_,_) -> x.PropName)
//    |> Seq.map fst
runIt()
//diag()
|> Dump
|> ignore