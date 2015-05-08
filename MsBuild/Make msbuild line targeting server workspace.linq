<Query Kind="FSharpProgram" />

//form server build string
open System.IO;
let sourceServer="devbuild01"
let msbuild="""C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"""
let sourceBaseDir = """\\"""+sourceServer+"""\c$\Builds\3\Development\MO.Web Dev Deploy\"""
let sourceDir=sourceBaseDir+"src\\"
let solutionDir = sourceDir+"""Products\MarketOnce\"""
let proj= sourceDir+"""OceansideTen\OceansideTen.Common\OceansideTen.Common.csproj"""

//assumes you are deploying the project you are currently building
let projectFileName = Path.GetFileNameWithoutExtension(proj)

let targets = "Clean,Build"
let outServer="""\\svrrbidev03\"""
let webOutDir =outServer+ "dev.marketonce.com"
let buildDir="""C:\Development\Products\Build\"""
let logTarget=buildDir+projectFileName+".log"
let outDir= """\\devbuild01\c$\Builds\3\Development\MO.Web Dev Deploy\bin\\""" //this should have 2 slashes at the end
let customTargets="""C:\Development\Products\CVS\BuildDefinitions\CustomNuGetRestore.targets"""
//var deployProject ="MarketOnce.Web.csproj"
//var webprojectoutputArg=@";WebProjectOutputDir=";
//var weboutputTarget=@"\\svrrbidev03\dev.marketonce.com";
//var args3=@";MSBuildTargetsVerbose=true /p:OutDir=""\\devbuild01\c$\Builds\3\Development\MO.Web Dev Deploy\bin\\"" /p:RunCodeAnalysis=""False"";"
//	+@"/p:VCBuildOverride=""\\devbuild01\c$\Builds\3\De
//var result= msbuild+proj+projectFileName+args+"\""+logTarget+"\""

printfn "cd %s" buildDir
let printMsBuildCommandLine =printfn "%s /nologo \"%s\" /nr:False /t:%s \
	/p:SkipInvalidConfigurations=true /fl /flp:\"%s\" \
	/p:%s=%s;WebProjectOutputDir=\"%s\";MSBuildTargetsVerbose=true \
	/p:OutDir=\"%s\" \
	/p:RunCodeAnalysis=\"False\" \
	/p:VCBuildOverride=\"%s.vsprops\" \
	/p:CustomAfterMicrosoftCommonTargets=\"%s\";\
	SolutionDir=\"%s\""

printMsBuildCommandLine msbuild proj targets logTarget "DeployWeb=" projectFileName webOutDir outDir proj customTargets solutionDir