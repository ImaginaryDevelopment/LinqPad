<Query Kind="FSharpProgram" />

// let alwaysEditable = false // not implemented



type Keys = {Header:string;Pascal:string;Camel:string;Field:string;DisplayAccessor:string;HiddenDom:string}

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

let createKeys camel field header accessor =
	let hidden = sprintf """<input type="hidden" name="%s"/>"""
	{
		Keys.Header=header
		Field=field
		DisplayAccessor = accessor
		Camel=camel 
		HiddenDom= hidden field
		Pascal= toPascalCase camel //FromCamel
	}
let keys = 
	let camel="overQuotaCap"
	let field="OverQuotaCap"
	let header = "OverQuota Cap"
	let defaultAccessor = sprintf """Eval("%s")"""
	let formattedPositiveIntAccessor = sprintf """GetFormattedPositiveInt(Eval("%s"))"""
	let reqMembersAccessor = sprintf """GetRequestedMembers(Eval("%s")) + (Convert.ToBoolean(Eval("IsQuotaCap")) ? "*" : "")"""
	let accessor = formattedPositiveIntAccessor field
	createKeys camel field header accessor
	


//						
type InlineEditorComponent = 
	|Display
	|Editor
	|EditLink
	|ItemChild

type ClientSelector(nameFun,inlineComponent:InlineEditorComponent) =
	//let roles = { InlineEditorRole.Display = sprintf "%sDisplay" keys.Pascal; Editor=sprintf "%sEditor" keys.Pascal;EditLink=sprintf "%sEditLink" keys.Pascal;ItemChild=sprintf "%sItemChild" keys.Pascal}
	static member private makeRole c = match c with 
										|Display -> sprintf "%sDisplay"
										|Editor -> sprintf "%sEditor"
										|EditLink -> sprintf "%sEditLink"
										|ItemChild -> sprintf "%sItemChild"
	static member private dataRoleSelector = sprintf "%s: '[data-role=%s]'"
	
	member x.InlineComponent = inlineComponent
	member x.Name = nameFun keys.Camel
	member x.Role = ClientSelector.makeRole inlineComponent keys.Pascal
	member x.DataRole = ClientSelector.dataRoleSelector x.Name x.Role
	
	member x.Dom = match inlineComponent with
					|Display -> sprintf """<span data-role="%s" class="fieldDisplay"><%%#%s%%></span>""" x.Role keys.DisplayAccessor
					|EditLink -> sprintf """<i  data-role="%s" class="onHover fa fa-pencil-square-o" title="edit"></i>""" x.Role
					|Editor -> sprintf """<input data-role="%s" type="text" disabled="disabled" name="%s<%%# Eval("ProjectQuotaId") %%>" style="display:none;width:90px" value="" data-original-value="" data-quotaGroupId="<%%# Eval("ProjectQuotaId") %%>"/>""" x.Role keys.Pascal 
					|ItemChild-> failwithf "Can't generate dom for itemChild directly" 

module ClientEditable =
	let display = ClientSelector(sprintf "%sDisplay",InlineEditorComponent.Display)
	let editor = ClientSelector(sprintf "%sEditor",InlineEditorComponent.Editor)
	let editLink = ClientSelector(sprintf "%sEditLink",InlineEditorComponent.EditLink)
	let child = ClientSelector(sprintf "%sItemChild", InlineEditorComponent.ItemChild)
	let asp = sprintf """ <asp:TemplateColumn HeaderText="%s">
                            <ItemTemplate>
                                 <span data-role="%s">
                                        %s
                                        %s
                                    	%s
                                    </span>
                                
                            </ItemTemplate>
                        </asp:TemplateColumn>""" keys.Header child.Role editLink.Dom display.Dom editor.Dom
	let selectors = [
			display
			editor
			editLink
			child
		]
	
let jsClientSelectors= 
	sprintf """		%s, 
			%s,
			%s,
			%s""" ClientEditable.display.DataRole ClientEditable.editor.DataRole ClientEditable.editLink.DataRole ClientEditable.child.DataRole
	
(* var createClient = function(target,camel, pascal) {
        target[camel + 'Editor'] = "'[data-role=" + camel + "Editor]'";
        target[camel+'Display'] = "'[data-role="
    };
	*)
type HandlerArgs = {StorageKey:string; HiddenName:string; EditorSelector:string}
let handlerArgs = {HandlerArgs.StorageKey=keys.Camel+"Edits"; HiddenName=keys.Camel+"Edits";EditorSelector = sprintf "%sEditor" keys.Camel}

let jsBeforeReq=  //before save or updatePanel request starts
	
	sprintf """beforeSavePostHandler(currentTabIsQuotaGroups, quotaGroupTabStorage, '%s', '%s', clientSelectors.%s, $qgGrid);""" handlerArgs.StorageKey handlerArgs.HiddenName handlerArgs.EditorSelector
type InitializeHandlerArgs = {EditLinkSelector:string;ChildSelector:string;}
let jsInitialize = sprintf """quotaGridInlineEditsInitializeHandler(clientSelectors.%s, clientSelectors.%s, clientSelectors.%s, clientSelectors.quotaGroupIdAttr, clientSelectors.%s, quotaGroupTabStorage, '%s');""" ClientEditable.editLink.Name ClientEditable.child.Name ClientEditable.editor.Name ClientEditable.display.Name handlerArgs.StorageKey

(keys,ClientEditable.asp,ClientEditable.selectors,jsClientSelectors,jsBeforeReq,jsInitialize) |> Dump

