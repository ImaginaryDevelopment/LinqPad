<Query Kind="FSharpProgram" />

// map class to F#

let toLower (s:string) = s.ToLower()
let toUpper (s:string) = s.ToUpper()
let splitLines (s:string) = s.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
let delimit (d:string) (s:string seq) = String.Join(d,values=s)
let trimStart (s:string) = s.TrimStart()

// helpful msdn regex documentation: https://msdn.microsoft.com/en-us/library/az24scfc(v=vs.110).aspx
let replace (s:string) (r:string) (t:string) = t.Replace(s,r)
let regexIt (p:string) (r:string) (t:string) = Regex.Replace(t,p,r,RegexOptions.Multiline)
let regexItD (p:string) (r:string) (t:string) = 
    Regex.Matches(t,p,RegexOptions.Multiline)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> fun x -> x.Dump(p)
    Regex.Replace(t,p,r,RegexOptions.Multiline)
let isMatch (p:string) (t:string) = Regex.IsMatch(t,p)
let matches (p:string) (t:string) = Regex.Matches(t, p,RegexOptions.Multiline) |> Seq.cast<Match>
let getGroupVal (i:int) (m:Match) = m.Groups.[i] |> string

let tee f x = x,f x

let fMe p f t = 
    let me:MatchEvaluator = MatchEvaluator(f)    
    Regex.Replace(t,p, me, RegexOptions.Multiline)


let regexRemove p t = regexIt p String.Empty t

module Types = 
    let fixTypeName (x:string) = 
        //x.Dump("TypeName")
        match x with
        | "DateTime?" -> "DateTime Nullable"
        | "int?" -> "int Nullable"
        | "bool?" -> "bool Nullable"
        | "return" -> failwithf "return is not a valid type name"
        | _ -> x
        
    let fixLiteral typeName value = 
        //(typeName,value).Dump("Fixing literal")
        match typeName,value with
        | "DateTime Nullable", "null" 
        | "int Nullable", "null"
        | "bool Nullable", "null"
            -> "Nullable()"
        | _,_ -> value


module Fields = 
    let fixFieldName (fieldName:string):string= 
        let fieldName:string = if fieldName.[0..0] = "_" then fieldName.[1..] else fieldName
        if toUpper fieldName = fieldName then toLower fieldName else (sprintf "%s%s" (toLower fieldName.[0..0]) ( fieldName.[1..]))
    
    let fieldRegexp = @"^[ \t]*(?:(?:public|private)[ \t]*)?(\w+(?:\?|\[\])*)\s(_?\w+)(\s?=\s?(.*))?;"
    let fieldRegex t = 
        
        let f (m:Match)= 
            let firstWord = getGroupVal 1 m 
            if firstWord = "return" then
                m.Value
            else
                let varName = getGroupVal 2 m |> fixFieldName
                let typeName = firstWord |> Types.fixTypeName
                let setter = getGroupVal 3 m
                let setter = if not <| isNull setter && setter.Length > 0 then setter.Replace(getGroupVal 4 m, getGroupVal 4 m |> Types.fixLiteral typeName) else setter
                let result = sprintf "\r\n    let mutable %s:%s%s // %s" varName typeName setter (m.Value.Trim())
                result
        fMe fieldRegexp f t
        //let me:MatchEvaluator = MatchEvaluator(fMe)
        
        //Regex.Replace(t,fieldRegexp, me, RegexOptions.Multiline)
    
module Props = 
    let fixPropDeclaration = regexIt @"\s*^[ \t]*(public (?!override )(\w+(?:\?|\[\])?) (\w+))\s*" "\r\n\r\n    member x.$3 // $2 (*$1*)"
    let private getDeclarationRegexp = @"^[ \t]+get[\n\s]*{[\n\s]*(?:return[ \t]+(.*)\s*;[\n\s]*})?" 
    let fixGetDeclaration = 
        let defaultGet = "        with get() = "  
        let f (m:Match) = 
            //m.Value.Dump("matched!")
            let result = 
                if m.Groups.[1].Value <> String.Empty then // simple return //  && m.Groups.[2].Value <> String.Empty then //&& m.Groups.[2].Value.[0..0] = "_"  then
                    let v = getGroupVal 1 m
                    //v.Dump("replacing")
                    Fields.fixFieldName v //getGroupVal 1 m |> replace v v.[1..]
                    //|> replace "return " String.Empty
                    |> sprintf "%s%s" defaultGet
                else
                    defaultGet
            //(m.Value,result,m.Index).Dump("fixGetReplaced")
            result
        fMe getDeclarationRegexp f
        
    let private setDeclarationRegexp = @"^[ \t]+set[\n\s]*{[\n\s]*"
    let fixSetDeclaration = 
        let defaultSet = "        and set v = "
        let f (m:Match) = 
            let result = defaultSet
            //(m.Value,result,m.Index).Dump("fixSetReplaced")
            result
        fMe setDeclarationRegexp f
   
module DataModelBase =

    let notifyFieldsAndMethods = """

    let propertyChangedEvent = new DelegateEvent<PropertyChangedEventHandler>()
    let toPropName (query: Expr) = 
        match query with 
        |Microsoft.FSharp.Quotations.Patterns.PropertyGet(_,i,_) -> i.Name
        | _ -> String.Empty
"""
    let notifyMembers = """
    member x.OnPropertyChangedExpr expr = // via http://www.fssnip.net/4Q
        let propName = toPropName expr
        x.OnPropertyChanged propName

    member x.OnPropertyChanged propertyName = 
        propertyChangedEvent.Trigger([| x; new PropertyChangedEventArgs(propertyName) |])

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish"""
        
let toF (t:string) = 
    let transformed = 
        t
        |> regexRemove "#region.*"
        |> regexRemove "#endregion.*"
        |> regexRemove  "namespace .*" 
        |> regexRemove  "using .*" 
        |> regexIt @"^[ \t]*public class (\w+)\s*(:\s*?[\w.]+)?[\n\s]*" "type $1() =\r\n    inherit $2\r\n"
        |> Fields.fieldRegex
        |> regexIt @"^([^/]+[ \t]*)string.Empty" "$1String.Empty"
        |> regexIt @"this\.(?!_)" "x."
        |> Props.fixPropDeclaration
        |> Props.fixGetDeclaration
        |> Props.fixSetDeclaration
        |> regexRemove  @"^[ \t]*[{}][ \t]*"
        |> regexIt @"\(\s*\)\s*=>\s*" "fun () -> "
        |> regexIt @"\s*=>\s*" " -> "
        |> regexIt @"(?<=\s)return\s" " "
        |> regexIt @"if \(([^\r\n]+)\)\s*" "if $1 then "
        |> regexIt @"x.SetAndNotify\(fun \(\) -> x.(\w+), ref _\1, value(\.\w+\(\))?\)" "$1 <- v$2; x.OnPropertyChanged \"$1\"" // \\\\$0"
        |> regexRemove @";\s*}\s*$"
        |> regexRemove "^\s*$" // remove pure-whitespace lines
        |> regexIt "(\r\n|\r|\n){3,}" "\r\n\r\n"
        
    let sortClass (l:string) = 
        not <| l.StartsWith "type",
            //&& not <| String.IsNullOrWhiteSpace l,
        not <| l.Trim().StartsWith "inherit",
            //&& not <| String.IsNullOrWhiteSpace l,
        not <| l.StartsWith "    let "
            //&& not <| String.IsNullOrWhiteSpace l
            
    let ordered = splitLines transformed |> Seq.sortBy sortClass
    let result =
        ordered 
        |> delimit Environment.NewLine
        |> fun t -> if isMatch "inherit\s*:\s*DataModelBase" t then t + DataModelBase.notifyMembers else t
        |> regexIt "\s*inherit\s*:\s*DataModelBase[ \t]*" DataModelBase.notifyFieldsAndMethods
        |> trimStart
    //transformed.Dump("before ordering")
    (t.Length,result.Length).Dump("before,after")
    result

let ``class`` = """
using System;
using System.Collections.Generic;
using System.Data;

using PracticeManagement.Foundation.Helpers;
using Pm.Schema.DataModels;
using Pm.Schema.DataModels.ClaimFilingIndicators;

using Connector = Pm.Dal.AdoHelper.Connections.Connector;
using AppDb = Pm.Dal.AppDal.AppTypeHost.ServiceTypes.SimpleDataContextTypes.ApplicationDatabase;
using Pm.Dal.DataModels;

namespace PracticeManagement.Foundation.DataModels
{
    public static class ClaimFilingIndicatorDataAccess
    {
        public static void SaveClaimFilingIndicator(SpecialProgramDataModel specProg, Connector cn)
        {
            var cmdText = nameof(AppDb.UspClaimFilingIndicatorInsUpd);
            var @params = new Dictionary<string, object>
            {
                ["@CodeID"] = specProg.CodeID,
                ["@Code"] = specProg.Code,
                ["@Description"] = specProg.Description
            };
            Pm.Dal.AdoHelper.ExecuteNonQuery(cmdText, CommandType.StoredProcedure, cn, @params);
        }

        public static IEnumerable<ClaimFilingIndicatorN> LoadClaimFilingIndicator(Connector cn)
        {
            var cmdText = nameof(AppDb.UspClaimFilingIndicatorGet);
            return Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.StoredProcedure, cn, null, r =>
                 new ClaimFilingIndicatorN(new ClaimFilingIndicatorRecord(
                     r.ReadToString("Code") ?? string.Empty,
                     r.ReadValueMap("CodeID", Convert.ToInt32) ?? 0,
                     r.ReadToString("Description") ?? string.Empty))
            );
        }

        public static void LoadClaimFilingIndicatorByID(SpecialProgramDataModel specProg, Connector cn, Action<Exception> onException)
        {
            try
            {
                var cmdText = nameof(AppDb.UspSpecialProgramByIDGet);
                var @params = new Dictionary<string, object>
                {
                    ["@CodeID"] = specProg.CodeID
                };
                Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.StoredProcedure, cn, null, r => { FillDataModelFromReader(specProg, r); return 0; });
            }
            catch (Exception ex)
            {
                onException(ex);
            }
        }

        static void FillDataModelFromReader(SpecialProgramDataModel specProg, IDataRecord reader)
        {
            specProg.CodeID = reader.ReadValueMap("CodeID", Convert.ToInt32) ?? 0;
            specProg.Code = reader.ReadToString("Code") ?? string.Empty;
            specProg.Description = reader.ReadToString("Description") ?? string.Empty;
        }
    }
}

"""

toF ``class``
|> Dump

``class``.Dump("original")