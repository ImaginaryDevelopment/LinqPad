<Query Kind="FSharpProgram" />

let getGroupValue(x:int) (m:Match) = m.Groups.[x].Value
let getGroupNValue(x:string) (m:Match) = m.Groups.[x].Value
let trimEnd(x:string) = x.TrimEnd()
let replace d r (x:string) =
    try
        x.Replace(d,newValue=r)
    with _ ->
        (d,r).Dump("failing")
        reraise()
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
     """        [AccessedThroughProperty("AccoladesWindowToolStripMenuItem")]
            ToolStripMenuItem _AccoladesWindowToolStripMenuItem;"""
            
let targetTemplate= 
    """ToolStripMenuItem AccoladesWindowToolStripMenuItem
        {
            get
            {
                return this._AccoladesWindowToolStripMenuItem;
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                EventHandler eventHandler = new EventHandler(this.AccoladesWindowToolStripMenuItem_Click);
                if (this._AccoladesWindowToolStripMenuItem != null)
                    this._AccoladesWindowToolStripMenuItem.Click -= eventHandler;
                this._AccoladesWindowToolStripMenuItem = value;
                if (this._AccoladesWindowToolStripMenuItem == null)
                    return;
                this._AccoladesWindowToolStripMenuItem.Click += eventHandler;
            }
        }"""
type ReplInfo =  {Type:string;PropName:string;Handler:string;Event:string;Field:string} // ;FieldP:string}    
type Patterning = {ReplInfo:ReplInfo;PropP:string} // ;FieldP:string}    
// ok so frequently the field names and prop names contain their conterparts, escape counterparts in descending size, then replace escapes in ascending size
let makeFieldPattern {Type=typeName;PropName=propName;Field=fieldName}=
    targetField
    |> killWs
    |> replace "_AccoladesWindowToolStripMenuItem" "$$fname$$"
    |> replace "AccoladesWindowToolStripMenuItem" "$$pname$$"
    |> replace "ToolStripMenuItem" typeName
    |> replace "$$pname$$" propName
    |> replace "$$fname$$" fieldName
let makePropPattern {Type=typeName;PropName=prop;Handler=h;Event=e;Field=f} =
    targetTemplate
    |> killWs
    |> replace "AccoladesWindowToolStripMenuItem_Click" "$$h$$"
    |> replace "_AccoladesWindowToolStripMenuItem" "$$fname$$"
    |> replace "AccoladesWindowToolStripMenuItem" "$$pname$$"
    |> replace "ToolStripMenuItem" typeName
    |> replace "$$pname$$" prop
    |> replace "$$fname$$" f
    |> replace "$$h$$" h
    |> replace "Click" e
    
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
            
    //    let fName = Regex.Match(
    
    let namedRepeat mp name tp input =
        input
        |> replace1 mp (sprintf "(?<%s>%s)" name tp)
        |> replace mp (sprintf "\k<%s>" name)
    let wordP ="[_\w]+"
    let propPattern =
        targetTemplate
        |> killWs
        |> namedRepeat ri.Handler "h" wordP
        |> namedRepeat ri.Field "field" wordP
        |> namedRepeat ri.PropName "prop" wordP
        |> namedRepeat ri.Type "type" wordP
        |> namedRepeat ri.Event "event" wordP
    {ReplInfo=ri;PropP=propPattern}
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
                (p,propInfo,fieldMatch.Value).Dump()
                failwithf "bad prop match"
            m
        if fieldMatch.Success then
            if String.IsNullOrWhiteSpace propMatch.Value then
                (propInfo,fieldMatch.Value).Dump()
                failwithf "bad prop match"
            Some (propInfo,fieldMatch.Value, propMatch.Value)
        else propInfo.Dump("unmatched");None
    )
    |> List.ofSeq
let runIt() =
    if toRip.Length < 1 then
        failwithf "Did not match anything to rip"
    (text,toRip)
    ||>Seq.fold(fun text (pi,fm,pm) ->
        try
            text
            |> remove fm
            |> replace pm (sprintf "%s %s; // this.%s.%s += %s\r\n" pi.Type pi.PropName pi.PropName pi.Event pi.Handler)
        with _ ->
            (pi,fm,pm).Dump("failing")
            reraise()
    )
let diag() =
    toRip
    |> Seq.groupBy(fun (x,_,_) -> x.PropName)
    |> Seq.map fst
//diag()
runIt()
|> Dump
|> ignore