<Query Kind="FSharpProgram">
  <NuGetReference>Fantomas.Core</NuGetReference>
  <Namespace>FSharp.Compiler.Syntax</Namespace>
  <Namespace>FSharp.Compiler.SyntaxTrivia</Namespace>
  <Namespace>FSharp.Compiler.Text</Namespace>
  <Namespace>FSharp.Compiler.Xml</Namespace>
</Query>

// https://fsprojects.github.io/fantomas/docs/end-users/GeneratingCode.html

// online ast tool - https://fsprojects.github.io/fantomas-tools/#/ast
let implementationSyntaxTree =
    ParsedInput.ImplFile(
        ParsedImplFileInput(
            "filename.fsx",
            true,
            QualifiedNameOfFile(Ident("", Range.Zero)),
            [],
            [],
            [ SynModuleOrNamespace(
                  [],
                  false,
                  SynModuleOrNamespaceKind.AnonModule,
                  [ SynModuleDecl.Let(
                        false,
                        [ SynBinding(
                              None,
                              SynBindingKind.Normal,
                              false,
                              false,
                              [],
                              PreXmlDoc.Empty,
                              SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
                              SynPat.Named(SynIdent(Ident("a", Range.Zero), None), false, None, Range.Zero),
                              None,
                              SynExpr.Const(SynConst.Int32(0), Range.Zero),
                              Range.Zero,
                              DebugPointAtBinding.Yes Range.Zero,
                              { EqualsRange = Some Range.Zero
                                LetKeyword = Some Range.Zero }
                          ) ],
                        Range.Zero
                    ) ],
                  PreXmlDoc.Empty,
                  [],
                  None,
                  Range.Zero,
                  { ModuleKeyword = None
                    NamespaceKeyword = None }
              ) ],
            (false, false),
            { ConditionalDirectives = []
              CodeComments = [] }
        )
    )

open Fantomas.Core

CodeFormatter.FormatASTAsync(implementationSyntaxTree) |> Async.RunSynchronously
|> Dump
|> ignore