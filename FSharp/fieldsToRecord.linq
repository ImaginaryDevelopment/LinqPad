<Query Kind="FSharpProgram" />

// fold f# fields into record
let append (sb:StringBuilder) (text:string) = sb.Append(text) |> ignore
let appendL (sb:StringBuilder) (text:string) = sb.AppendLine(text) |> ignore
let rReplace (pattern:string) (replace:string) (text:string) = Regex.Replace(text,pattern,replace)
type System.String with
	static member join (delimiter:string) (items:string seq) = String.Join(delimiter,items |> Array.ofSeq)
	static member pascalize (item:String) = item.[0..0].ToLower() + item.[1..]
let sample = """


type AppointmentTypeDataModel() = 
  inherit FSharp.ViewModule.ViewModelBase()

  let mutable _AppointmentTypeId : int = 0//(int _AppointmentTypeID = 0)
  let mutable _DisplayName : string = String.Empty//(string _DisplayName = string.Empty)
  let mutable _EMBillableLevels : ObservableCollection<AppointmentTypeMappingDataModel> = new ObservableCollection<AppointmentTypeMappingDataModel>()
  let mutable _FacilityId : int = 0//(int _FacilityID = 0)
  let mutable _IsEMBillable : bool = false//(bool _IsEMBillable = false)

  override x.ToString() =  x.DisplayName
  member x.AppointmentTypeId //SimpleINotify
    with get() = _AppointmentTypeId
    and set v = _AppointmentTypeId <- v;x.RaisePropertyChanged(<@ x.AppointmentTypeId @>)

  member x.DisplayName //SimpleINotify
    with get() = _DisplayName
    and set v = _DisplayName <- v;x.RaisePropertyChanged(<@ x.DisplayName @>)

  member x.EMBillableLevels //SimpleINotify
    with get() = _EMBillableLevels
    and set v = _EMBillableLevels <- v;x.RaisePropertyChanged(<@ x.EMBillableLevels @>)

  member x.FacilityId //SimpleINotify
    with get() = _FacilityId
    and set v = _FacilityId <- v;x.RaisePropertyChanged(<@ x.FacilityId @>)

  member x.IsEMBillable //SimpleINotify
    with get() = _IsEMBillable
    and set v = _IsEMBillable <- v;x.RaisePropertyChanged(<@ x.IsEMBillable @>)
"""

let regex = System.Text.RegularExpressions.Regex("let mutable _(\w+) : ([\w<>]+) = ([^/\r\n]+)")
let matches = regex.Matches(sample)
let model,args = 
	Regex.Match(sample,"type (\w+)DataModel\((.*)\) =")
	|> (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
(model,args).Dump("model with args")
let vModelName = sprintf "%sDataModel" model
let recordType = sprintf "%sRecord" model
let args' = sprintf "dm:%s%s" recordType (if String.IsNullOrEmpty(args) then String.Empty else sprintf ",%s" args)
let sb = new StringBuilder(sprintf "type %sRecord = {" model)
for m in matches do
	append sb (sprintf "%s:%s;" (m.Groups.[1].Value) (m.Groups.[2].Value))
appendL sb "}"
appendL sb (sprintf "type %s(%s) =" vModelName args')
if Regex.IsMatch(sample,"\s+inherit FSharp\.ViewModule\.ViewModelBase\(") then 
	appendL sb "  inherit FSharp.ViewModule.ViewModelBase()"
	appendL sb String.Empty
appendL sb "  let mutable record = dm"
appendL sb String.Empty
let propNames = [ for m in matches do yield m.Groups.[1].Value]
let initializersBare =
	[ for m in matches do
		yield m.Groups.[1].Value, m.Groups.[3].Value]
initializersBare.Dump("bare")
let initializers = 
	initializersBare |> Seq.map (fun (name,v) -> sprintf "%s=%s" name v)
			
let pascalizedPropNames = propNames |> Seq.map String.pascalize
appendL sb (sprintf "  new () = %s({%s.%s})" vModelName recordType (String.join ";" initializers))

appendL sb String.Empty

let zipInit = 
	Seq.zip initializersBare pascalizedPropNames
	|> Seq.map (fun ((name,_),pascalized) -> sprintf "%s=%s" name pascalized)
//zipInit.Dump()
appendL sb (sprintf "  new (%s) = %s( {%s.%s})" (pascalizedPropNames |> String.join ",") vModelName recordType (String.join ";" zipInit))
//appendL sb "}"

appendL sb String.Empty
appendL sb "  member x.GetDataModel() = record"
appendL sb String.Empty
let afterLets = 
	let last:Match = matches |> Seq.cast<Match> |> Seq.rev |> Seq.head
	//last.Dump("last")
	let lastIndex = last.Index + last.Length
	let afterLets = sample.[lastIndex ..]
	//afterLets.Dump("afterLets")
	afterLets
let transform text = 
	text
	|> rReplace "with get\(\) = _(\w+)" "with get() = record.$1"
	|> rReplace "and set v = _(?<name>\w+) <- v;x\.RaisePropertyChanged\(<@ x\.\k<name> @>\)" "and set v=record <- {record with $1 = v};x.RaisePropertyChanged(<@ x.$1 @>)"
append sb (transform afterLets)		
	
sb.ToString().Dump()