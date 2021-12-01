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
     """        [AccessedThroughProperty("accoladeButton")]
            ImageButton _accoladeButton;"""
            
let targetTemplate= 
    """ ImageButton accoladeButton
        {
            get
            {
                return this._accoladeButton;
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                MouseEventHandler mouseEventHandler = new MouseEventHandler(this.accoladeButton_MouseDown);
                ImageButton.ButtonClickedEventHandler clickedEventHandler = new ImageButton.ButtonClickedEventHandler(this.accoladeButton_ButtonClicked);
                if (this._accoladeButton != null)
                {
                    this._accoladeButton.MouseDown -= mouseEventHandler;
                    this._accoladeButton.ButtonClicked -= clickedEventHandler;
                }
                this._accoladeButton = value;
                if (this._accoladeButton == null)
                    return;
                this._accoladeButton.MouseDown += mouseEventHandler;
                this._accoladeButton.ButtonClicked += clickedEventHandler;
            }
        }"""
type Eventing = {Event:string;Handler:string}
type ReplInfo =  {Type:string;PropName:string;HandledEvents:Eventing list;Field:string} // ;FieldP:string}    
type Patterning = {ReplInfo:ReplInfo;PropP:string} // ;FieldP:string}    
// ok so frequently the field names and prop names contain their conterparts, escape counterparts in descending size, then replace escapes in ascending size
let makeFieldPattern {Type=typeName;PropName=propName;Field=fieldName}=
    targetField
    |> killWs
    |> replace "_accoladeButton" "$$fname$$"
    |> replace "accoladeButton" "$$pname$$"
    |> replace "ImageButton" typeName
    |> replace "$$pname$$" propName
    |> replace "$$fname$$" fieldName
let makePropPattern {Type=typeName;PropName=prop;HandledEvents=he;Field=f} =
    targetTemplate
    |> killWs
    |> replace "accoladeButton_MouseDown" "$$h1$$"
    |> replace "accoladeButton_ButtonClicked" "$$h2$$"
    |> replace "_accoladeButton" "$$fname$$"
    |> replace "accoladeButton" "$$pname$$"
    |> replace "ImageButton" typeName
    |> replace "$$pname$$" prop
    |> replace "$$fname$$" f
    |> replace "$$h1$$" he.[0].Handler
    |> replace "MouseDown" he.[0].Event
    |> replace "$$h2$$" he.[1].Handler
    |> replace "ButtonClicked" he.[1].Event
    
// the template match info if this fails, we aren't even processing our sample input text properly
let a1p=
    
    
    let typeName,propName =
        let m = Regex.Match(targetTemplate,@"^\s*(\w+)\s+(\w+)")
        m |> getGroupValue 1, m |> getGroupValue 2
    let ri = {  Type = typeName
                PropName=propName
                HandledEvents=
                    let handlers =
                        let m = Regex.Matches(targetTemplate,@"EventHandler\(this\.(\w+_\w+)") |> Seq.cast<Match>
                        m |> Seq.map(getGroupValue 1) |> List.ofSeq
                    let events = 
                        let m = Regex.Matches(targetTemplate,@"\s*this._\w+\.(\w+) \+=") |> Seq.cast<Match>
                        m |> Seq.map(getGroupValue 1) |> List.ofSeq
                    if handlers.Length <> events.Length then (typeName,propName,handlers,events).Dump("failing match");failwithf "bad he"
                    let he =
                        handlers
                        |> List.zip events
                        |> List.map(fun (e,h) -> {Handler=h;Event=e})
                    he
                Field=
                    let m = Regex.Match(targetField, @"\w+ (_\w+);")
                    m |> getGroupValue 1
                }
            
    //    let fName = Regex.Match(
    
    let namedRepeat mp name tp input =
        let inline failMe name = (ri,mp,name,tp,input).Dump(sprintf "failing namedRepeat for %s" name)
        let inline failNE name value = if String.IsNullOrWhiteSpace value then failMe name
        failNE "mp" mp
        failNE "name" name
        failNE "tp" tp
        failNE "input" input
        input
        |> replace1 mp (sprintf "(?<%s>%s)" name tp)
        |> replace mp (sprintf "\k<%s>" name)
    let wordP ="[_\w]+"
    let propPattern =
        targetTemplate
        |> killWs
        |> namedRepeat ri.HandledEvents.[0].Handler "h1" wordP
        |> namedRepeat ri.HandledEvents.[1].Handler "h2" wordP
        |> namedRepeat ri.Field "field" wordP
        |> namedRepeat ri.PropName "prop" wordP
        |> namedRepeat ri.Type "type" wordP
        |> namedRepeat ri.HandledEvents.[0].Event "event1" wordP
        |> namedRepeat ri.HandledEvents.[1].Event "event2" wordP
    {ReplInfo=ri;PropP=propPattern} //;FieldP=fieldPattern}
    
// now starts the real file processing
//a1p.Dump("all prop info")

let targetFile = @"C:\projects\CoHDesigner2\Hero Designer\frmMain.cs"

let text =  File.ReadAllText targetFile
let names = ResizeArray<string>()
//a1p.PropP.Dump("eh?")
let toRip =
    text
    |> matchesM a1p.PropP
    |> Seq.cast<Match>
    |> Seq.choose(fun m -> 
        let gnv x = m |> getGroupNValue x
        let propInfo ={ PropName=gnv "prop";Type=gnv "type"
                        HandledEvents=
                            [{Event=gnv "event1";Handler=gnv "h1"};{Event=gnv "event2";Handler=gnv"h2"}]
                        Field=gnv "field"}
        let fieldMatch = text |> rMatch (makeFieldPattern propInfo)
        let propMatch =text |> rMatch(makePropPattern propInfo)
        if fieldMatch.Success then
            names.Add propInfo.PropName
            Some (propInfo,fieldMatch.Value, propMatch.Value)
        else propInfo.Dump("unmatched");None
    )
    |> List.ofSeq
let runIt() =
//    names.Dump("names")
    if names.Count > 0 then
        (text,toRip)
        ||>Seq.fold(fun text (pi,fm,pm) ->
            let removeAssert n (x:string) =
                let next = remove n x
                if x.Length = next.Length then (n,x,next).Dump(); failwithf "Bad field remove"
                else next
            let propReplace =
                [
                    yield! pi.HandledEvents |> List.map(fun he -> sprintf "//this.%s.%s += %s;" pi.PropName he.Event he.Handler)
                    yield sprintf "%s %s;" pi.Type pi.PropName
                ]
                |> String.concat"\r\n"
            text
            |> removeAssert fm
            |> replace pm propReplace
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