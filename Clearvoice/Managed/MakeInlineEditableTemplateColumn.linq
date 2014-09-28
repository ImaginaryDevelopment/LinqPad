<Query Kind="FSharpProgram" />

// let alwaysEditable = false // not implemented



type Keys = {Header:string;Pascal:string;Camel:string;Field:string;DisplayAccessor:string;HiddenName:string;HiddenDom:string;AddlDisplayAttrs:(string->string) option}

let toPascalCase s = 
	s
	|> Seq.mapi (fun i l -> if i=0 && Char.IsLower(l) then Char.ToUpper(l) else l)
	|> String.Concat

let humanize camel :string = 
	
	seq {
		let pascalCased = toPascalCase camel
		yield pascalCased.[0]
		for l in  pascalCased |> Seq.skip(1) do
			if System.Char.IsUpper(l) then 
				yield ' '
				yield l
			else
				yield l
	}
	|> String.Concat
	
//type Accessor =
//	|Default 
//	|FormattedPositiveInt 
//	|RequestedMembers

let createKeys (camel:string) field header accessor addlDisplayAttrs=
	if(Char.IsUpper(camel.[0])) then failwithf "camel was not camel %s" camel
	let hiddenName = sprintf "%sEdits" camel
	let hidden = sprintf """<input type="hidden" name="%s" />"""
	{
		Keys.Header=header
		Field=field
		DisplayAccessor = accessor field
		Camel=camel 
		HiddenName = hiddenName
		HiddenDom= hidden hiddenName
		Pascal= toPascalCase camel //FromCamel
		AddlDisplayAttrs= addlDisplayAttrs
	}

let AllKeys = 
	let formattedPositiveIntAccessor = sprintf """GetFormattedPositiveInt(Eval("%s"))"""
	
	let defaultAccessor = sprintf """Eval("%s")"""	
	let reqMembersAccessor = sprintf """GetRequestedMembers(Eval("%s")) + (Convert.ToBoolean(Eval("IsQuotaCap")) ? "*" : "")"""	
	let priceAccessor = sprintf """GetPrice(Eval("FinalPrice"), Eval("%s"))"""
	let priceAttrs (s:string) = sprintf """data-overrideprice="<%%#Eval("%s")%%>" data-finalprice="<%%#Eval("FinalPrice")%%>%s""" s ("\"")
	seq {
		
		yield(createKeys "reqCompletes" "RequestedMembers" "Requested Completes" reqMembersAccessor None)
		yield(createKeys "price" "OverridePrice" "Price"  priceAccessor (Some(priceAttrs)))
		yield(createKeys "overQuotaCap" "OverQuotaCap" "OverQuota Cap" formattedPositiveIntAccessor None)
		yield(createKeys "completedCap" "CompletedCap" "Completed Cap" formattedPositiveIntAccessor None)
		yield(createKeys "terminatedCap" "TerminatedCap" "Terminated Cap" formattedPositiveIntAccessor None)
		yield(createKeys "quotaName" "QuotaName" "Name" defaultAccessor None)
		yield(createKeys "startedCap" "StartedCap" "Started Cap" formattedPositiveIntAccessor None)
	}

type InlineEditorComponent = 
	|Display
	|Editor
	|EditLink
	|ItemChild


type ClientSelector(inlineComponent:InlineEditorComponent,keys) =
	//let roles = { InlineEditorRole.Display = sprintf "%sDisplay" keys.Pascal; Editor=sprintf "%sEditor" keys.Pascal;EditLink=sprintf "%sEditLink" keys.Pascal;ItemChild=sprintf "%sItemChild" keys.Pascal}
	static member private makeJsRole c = match c with 
											|Display -> "display"
											|Editor -> "editor"
											|EditLink -> "editLink"
											|ItemChild -> "itemChild"
	
	
	member x.InlineComponent = inlineComponent
	member private x.jsRoleRaw = ClientSelector.makeJsRole inlineComponent
	member x.JsRole = sprintf "'%s'" x.jsRoleRaw
	member x.JsColumn = sprintf "'%s'" keys.Camel
	member x.Name = sprintf "%s%s" <|| (keys.Camel, (toPascalCase <| ClientSelector.makeJsRole inlineComponent ))
	
	member x.Role = sprintf "%s%s" <|| (keys.Pascal, (toPascalCase <| ClientSelector.makeJsRole inlineComponent ))
	
	//member x.DataRole = ClientSelector.dataRoleSelector x.Name x.Role
	
	
	
	member x.Dom = match inlineComponent with
					|Display -> 
						let displayAttrs =if keys.AddlDisplayAttrs.IsSome then " "+(keys.AddlDisplayAttrs.Value keys.Field) else ""
						sprintf """<span data-role="%s" class="fieldDisplay"%s><%%#%s%%></span>""" x.Role displayAttrs keys.DisplayAccessor
					|EditLink -> sprintf """<i data-role="%s" class="onHover fa fa-pencil-square-o" title="edit"></i>""" x.Role
					|Editor -> sprintf """<input data-role="%s" type="text" disabled="disabled" name="%s<%%# Eval("ProjectQuotaId") %%>" style="display: none" value="" data-original-value="" data-quotagroupid="<%%# Eval("ProjectQuotaId") %%>" />""" x.Role keys.HiddenName 
					|ItemChild-> sprintf"""<span data-role="%s">""" x.Role  //failwithf "Can't generate dom for itemChild directly" 
					
type HandlerArgs = {HiddenNameAndStorageKey:string; EditorSelector:string}
type InitializeHandlerArgs = {EditLinkSelector:string;ChildSelector:string;}
type ClientSelectorCollection(keys) =
	
	let createClient com = ClientSelector(com,keys)
	member x.Keys = keys
	member x.Display = createClient InlineEditorComponent.Display
	member x.Editor = createClient InlineEditorComponent.Editor
	member x.EditLink = createClient InlineEditorComponent.EditLink
	member x.ItemChild = createClient InlineEditorComponent.ItemChild
	member x.Asp = sprintf """ <asp:TemplateColumn HeaderText="%s">
                            <ItemTemplate>
                                 <span data-role="%s">
                                        %s
                                        %s
                                    	%s
                                    </span>
                                
                            </ItemTemplate>
                        </asp:TemplateColumn>""" keys.Header x.ItemChild.Role x.EditLink.Dom x.Display.Dom x.Editor.Dom
	member x.Selectors = [
			x.Display
			x.Editor
			x.EditLink
			x.ItemChild
		]
	
	member x.JsRolesStart = "var roles = ["
	
	member x.JsColumnStart = sprintf "var columns = ["
	 
	member x.HandlerArgs = {HandlerArgs.HiddenNameAndStorageKey=keys.Camel+"Edits";EditorSelector = sprintf "%s.editor" keys.Camel}
	member x.JsCleanup = //before save or updatePanel request starts
		sprintf """beforeSavePostHandleClosure('%s', clientSelectors.%s);""" x.HandlerArgs.HiddenNameAndStorageKey x.HandlerArgs.EditorSelector
		
	member x.JsInitialize = sprintf """initializer(clientSelectors.%s, '%s');""" x.Keys.Camel x.HandlerArgs.HiddenNameAndStorageKey
	
	member x.AscxCs = sprintf """IDictionary<int, string> %s = GetInlineEdits("%s", Request.Form);""" keys.HiddenName keys.HiddenName
	member x.ValidateLine = sprintf """ValidateInlineEditInput("%s", "%s",""" keys.HiddenName keys.Header
	member x.SaveIfLine = sprintf """if (%s != null && %s.ContainsKey(qgId))""" keys.HiddenName keys.HiddenName
type InlineEditDiagnostic = { Keys:Keys;Asp:string;Selectors:ClientSelector list; SampleSelector: ClientSelector; JsCleanup:string;JsInit:string}	
for k in AllKeys do // TODO: account for tests that really should ensure items occur more than once
	let csColl = ClientSelectorCollection(k)
	
	{InlineEditDiagnostic.Keys=k;Asp=csColl.Asp;Selectors=csColl.Selectors;SampleSelector=csColl.Display;JsCleanup=csColl.JsCleanup;JsInit=csColl.JsInitialize} |> Dump

	
	printfn "Validating %s" k.Pascal
//validation
 
	
	let targetJsTextValidation = 
		
		let text = System.IO.File.ReadAllText(targetJs)
		let roleSection = text.After(csColl.JsRolesStart).Before("]")
		let missingRoles = 
			csColl.Selectors 
			|> Seq.map (fun x-> x.JsRole)
			|> Seq.filter ( fun x->roleSection.Contains(x)=false)
		if Seq.isEmpty(missingRoles)=false then failwithf ".js: missing %A" missingRoles
		
		let columnSection = text.After(csColl.JsColumnStart).Before("]")
		let missingColumns = 
			csColl.Selectors
			|>Seq.map (fun x-> x.JsColumn)
			|>Seq.filter(fun x->columnSection.Contains(x)=false)
		if Seq.isEmpty(missingColumns)=false then failwithf ".js: missing %A" missingColumns

		if text.Contains(csColl.JsInitialize)= false then failwithf ".js: missing call(s) to InitializeHandler\r\n%A" csColl.JsInitialize
		if text.Contains(csColl.JsCleanup)=false then failwithf ".js: missing call %A" csColl.JsCleanup
		printfn "js tests passed"
	let targetAspxValidation =
		let text = System.IO.File.ReadAllText(targetAspx)
		if text.Contains(csColl.Keys.HiddenDom)=false then failwithf ".aspx: missing %A" csColl.Keys.HiddenDom
		printfn "aspx tests passed"
	let targetAscxCsValidation = 
		let text = System.IO.File.ReadAllText(targetAscx+".cs") 
		if text.Contains(csColl.ValidateLine) = false then failwithf ".ascx.cs Validate missing call %A" csColl.ValidateLine
		if text.Contains(csColl.AscxCs)=false then failwithf ".ascx.cs Save missing call %A" csColl.AscxCs
		if text.Contains(csColl.SaveIfLine) = false then failwithf ".ascx.cs: Save missing if block %A" csColl.SaveIfLine
		
		printfn "ascxCs tests passed"
	let targetAscxValidation = 
		let text = System.IO.File.ReadAllText(targetAscx)
		csColl.Selectors
		//|> Seq.filter (fun s-> s.InlineComponent <> InlineEditorComponent.Editor)
		|> Seq.iter (fun s ->
			match s.InlineComponent with
				|Editor -> 
					let delimiter = "display: none"
					let containsFirstHalf = text.Contains(s.Dom.Before(delimiter))
					let containsSecondHalf = text.Contains(s.Dom.After(delimiter))
					if containsFirstHalf = false && containsSecondHalf=false 
						then failwithf ".ascx: missing %A" s.Dom
					elif not containsFirstHalf 
						then failwithf ".ascx: missing %A" (s.Dom.Before(delimiter))
					elif not containsSecondHalf
						then failwithf ".ascx: missing %A" (s.Dom.After(delimiter))
					else ()
				|_ -> if text.Contains(s.Dom) = false then failwithf ".ascx: missing %A" s.Dom
			)
		printfn "ascx tests passed"
	()