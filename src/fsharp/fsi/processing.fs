// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.Interactive.Processing

#nowarn "55"

open Internal.Utilities

module Tc = Microsoft.FSharp.Compiler.TypeChecker

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.IO
open System.Text
open System.Threading
open System.Reflection
open System.Windows.Forms

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler.AbstractIL.ILRuntimeWriter 
open Microsoft.FSharp.Compiler.Interactive.Settings
open Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Fscopts
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Ilxgen
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.TypeChecker
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Opt
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.PostTypecheckSemanticChecks

open Internal.Utilities.Collections
open Internal.Utilities.StructuredFormat
open Internal.Utilities.FileSystem

open Microsoft.FSharp.Compiler.Interactive.Utilities
open Microsoft.FSharp.Compiler.Interactive.Frontend


//----------------------------------------------------------------------------
// FsiDynamicCompilerState
//----------------------------------------------------------------------------

[<AutoSerializable(false)>]
[<NoEquality; NoComparison>]
type internal FsiDynamicCompilerState = {
    optEnv    : Opt.IncrementalOptimizationEnv;
    emEnv     : ILRuntimeWriter.emEnv;
    tcGlobals : Env.TcGlobals;
    tcState   : Build.TcState;
    ilxGenerator : Ilxgen.IlxAssemblyGenerator;
    // Why is this not in FsiOptions?
    timing    : bool;
}

let internal WithImplicitHome (tcConfigB, dir) f = 
    let old = tcConfigB.implicitIncludeDir 
    tcConfigB.implicitIncludeDir <- dir
    try f() 
    finally tcConfigB.implicitIncludeDir <- old


/// <summary>
/// Encapsulates the coordination of the typechecking, optimization and code generation
/// components of the F# compiler for interactively executed fragments of code.
/// </summary>
/// <remarks>
/// A single instance of this object is created per interactive session.
/// </remarks>
type internal FsiDynamicCompiler
                       (timeReporter : FsiTimeReporter, 
                        tcConfigB, 
                        tcLockObject : obj, 
                        errorLogger: ErrorLoggerThatStopsOnFirstError, 
                        outWriter: TextWriter,
                        tcImports: TcImports, 
                        tcGlobals: TcGlobals, 
                        ilGlobals: ILGlobals, 
                        fsiOptions : FsiCommandLineOptions,
                        fsiConsoleOutput : FsiConsoleOutput,
                        niceNameGen,
                        resolvePath) = 

    let outfile = "TMPFSCI.exe"
    let assemblyName = "FSI-ASSEMBLY"

    let mutable fragmentId = 0
    let mutable prevIt : ValRef option = None

    let generateDebugInfo = tcConfigB.debuginfo

    let valuePrinter = FsiValuePrinter(ilGlobals, generateDebugInfo, resolvePath, outWriter)

    let assemblyBuilder,moduleBuilder = ILRuntimeWriter.mkDynamicAssemblyAndModule (assemblyName, tcConfigB.optSettings.localOpt(), generateDebugInfo)

    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

    let _writer = moduleBuilder.GetSymWriter()

    let infoReader = InfoReader(tcGlobals,tcImports.GetImportMap())    

    /// Add attributes 
    let CreateModuleFragment (tcConfigB, assemblyName, codegenResults) =
        if !progress then fprintfn fsiConsoleOutput.Out "Creating main module...";
        let mainModule = mkILSimpleModule assemblyName (fsharpModuleName tcConfigB.target assemblyName) (tcConfigB.target = Dll) tcConfigB.subsystemVersion tcConfigB.useHighEntropyVA (mkILTypeDefs codegenResults.ilTypeDefs) None None 0x0 (mkILExportedTypes []) ""
        { mainModule 
          with Manifest = 
                (let man = mainModule.ManifestOfAssembly
                 Some { man with  CustomAttrs = mkILCustomAttrs codegenResults.ilAssemAttrs }); }

    let ProcessInputs(istate: FsiDynamicCompilerState, inputs: ParsedInput list, showTypes: bool, isIncrementalFragment: bool, isInteractiveItExpr: bool, prefixPath: LongIdent) =
        let optEnv    = istate.optEnv
        let emEnv     = istate.emEnv
        let tcState   = istate.tcState
        let ilxGenerator = istate.ilxGenerator
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)

        // Typecheck. The lock stops the type checker running at the same time as the 
        // server intellisense implementation (which is currently incomplete and #if disabled)
        let (tcState:TcState),topCustomAttrs,declaredImpls,tcEnvAtEndOfLastInput =
            lock tcLockObject (fun _ -> TypecheckClosedInputSet(errorLogger.CheckForErrors,tcConfig,tcImports,tcGlobals, Some prefixPath,tcState,inputs))

#if DEBUG
        // Logging/debugging
        if tcConfig.printAst then
            let (TAssembly(declaredImpls)) = declaredImpls
            for input in declaredImpls do 
                fprintfn fsiConsoleOutput.Out "AST:" 
                fprintfn fsiConsoleOutput.Out "%+A" input
#endif

        errorLogger.AbortOnError();
         
        let importMap = tcImports.GetImportMap()

        // optimize: note we collect the incremental optimization environment 
        let optimizedImpls, _optData, optEnv = ApplyAllOptimizations (tcConfig, tcGlobals, (LightweightTcValForUsingInBuildMethodCall tcGlobals), outfile, importMap, isIncrementalFragment, optEnv, tcState.Ccu, declaredImpls)
        errorLogger.AbortOnError();
            
        let fragName = textOfLid prefixPath 
        let codegenResults = GenerateIlxCode (IlReflectBackend, isInteractiveItExpr, runningOnMono, tcConfig, topCustomAttrs, optimizedImpls, fragName, true, ilxGenerator)
        errorLogger.AbortOnError();

        // Each input is like a small separately compiled extension to a single source file. 
        // The incremental extension to the environment is dictated by the "signature" of the values as they come out 
        // of the type checker. Hence we add the declaredImpls (unoptimized) to the environment, rather than the 
        // optimizedImpls. 
        ilxGenerator.AddIncrementalLocalAssemblyFragment (isIncrementalFragment, fragName, declaredImpls)

        ReportTime tcConfig "TAST -> ILX";
        errorLogger.AbortOnError();
            
        ReportTime tcConfig "Linking";
        let ilxMainModule = CreateModuleFragment (tcConfigB, assemblyName, codegenResults)

        errorLogger.AbortOnError();
            
        ReportTime tcConfig "ILX -> IL (Unions)"; 
        let ilxMainModule = EraseIlxUnions.ConvModule ilGlobals ilxMainModule
        ReportTime tcConfig "ILX -> IL (Funcs)"; 
        let ilxMainModule = EraseIlxFuncs.ConvModule ilGlobals ilxMainModule 

        errorLogger.AbortOnError();   
              
        ReportTime tcConfig "Assembly refs Normalised"; 
        let mainmod3 = Morphs.morphILScopeRefsInILModuleMemoized ilGlobals (NormalizeAssemblyRefs tcImports) ilxMainModule
        errorLogger.AbortOnError();

#if DEBUG
        if fsiOptions.ShowILCode then 
            fsiConsoleOutput.uprintnfn "--------------------";
            ILAsciiWriter.output_module outWriter mainmod3;
            fsiConsoleOutput.uprintnfn "--------------------"
#else
        ignore(fsiOptions)
#endif

        ReportTime tcConfig "Reflection.Emit";
        let emEnv,execs = ILRuntimeWriter.emitModuleFragment(ilGlobals, emEnv, assemblyBuilder, moduleBuilder, mainmod3, generateDebugInfo, resolvePath)

        errorLogger.AbortOnError();

        // Explicitly register the resources with the QuotationPickler module 
        // We would save them as resources into the dynamic assembly but there is missing 
        // functionality System.Reflection for dynamic modules that means they can't be read back out 
        for bytes in codegenResults.quotationResourceBytes do 
            Microsoft.FSharp.Quotations.Expr.RegisterReflectedDefinitions (assemblyBuilder, fragName, bytes);
            

        ReportTime tcConfig "Run Bindings";
        timeReporter.TimeOpIf istate.timing (fun () -> 
          execs |> List.iter (fun exec -> 
            match exec() with 
            | Some err ->         
                fprintfn fsiConsoleOutput.Error "%s" (err.ToString())
                errorLogger.SetError()
                errorLogger.AbortOnError()

            | None -> ())) ;

        errorLogger.AbortOnError();

        // Echo the decls (reach inside wrapping)
        // This code occurs AFTER the execution of the declarations.
        // So stored values will have been initialised, modified etc.
        if showTypes && not tcConfig.noFeedback then  
            let denv = tcState.TcEnvFromImpls.DisplayEnv
            let denv = 
                if isIncrementalFragment then
                  // Extend denv with a (Val -> layout option) function for printing of val bindings.
                  {denv with generatedValueLayout = (fun v -> valuePrinter.InvokeDeclLayout (emEnv, ilxGenerator, v)) }
                else
                  // With #load items, the vals in the inferred signature do not tie up with those generated. Disable printing.
                  denv 

            // 'Open' the path for the fragment we just compiled for any future printing.
            let denv = denv.AddOpenPath (pathOfLid prefixPath) 

            let (TAssembly(declaredImpls)) = declaredImpls
            for (TImplFile(_qname,_,mexpr,_,_)) in declaredImpls do
                let responseL = NicePrint.layoutInferredSigOfModuleExpr false denv infoReader AccessibleFromSomewhere rangeStdin mexpr 
                if not (Layout.isEmptyL responseL) then      
                    fsiConsoleOutput.uprintfn "";
                    let opts = valuePrinter.GetFsiPrintOptions()
                    let responseL = Internal.Utilities.StructuredFormat.Display.squash_layout opts responseL
                    Layout.renderL (Layout.channelR outWriter) responseL |> ignore
                    fsiConsoleOutput.uprintfnn ""

        // Build the new incremental state.
        let istate = {istate with  optEnv    = optEnv;
                                   emEnv     = emEnv;
                                   ilxGenerator = ilxGenerator;
                                   tcState   = tcState  }
        
        // Return the new state and the environment at the end of the last input, ready for further inputs.
        (istate,tcEnvAtEndOfLastInput)

    let nextFragmentId() = fragmentId <- fragmentId + 1; fragmentId

    let mkFragmentPath  i = 
        // NOTE: this text shows in exn traces and type names. Make it clear and fixed width 
        [mkSynId rangeStdin (FsiDynamicModulePrefix + sprintf "%04d" i)]

    member __.DynamicAssemblyName = assemblyName
    member __.DynamicAssembly = (assemblyBuilder :> Assembly)

    member __.EvalParsedSourceFiles (istate, inputs) =
        let i = nextFragmentId()
        let prefix = mkFragmentPath i 
        // Ensure the path includes the qualifying name 
        let inputs = inputs |> List.map (PrependPathToInput prefix) 
        let istate,_ = ProcessInputs (istate, inputs, true, false, false, prefix)
        istate

    /// Evaluate the given definitions and produce a new interactive state.
    member __.EvalParsedDefinitions (istate, showTypes, isInteractiveItExpr, defs: SynModuleDecls) =
        let filename = Lexhelp.stdinMockFilename
        let i = nextFragmentId()
        let prefix = mkFragmentPath i
        let prefixPath = pathOfLid prefix
        let impl = SynModuleOrNamespace(prefix,(* isModule: *) true,defs,PreXmlDoc.Empty,[],None,rangeStdin)
        let input = ParsedInput.ImplFile(ParsedImplFileInput(filename,true, QualFileNameOfUniquePath (rangeStdin,prefixPath),[],[],[impl],true (* isLastCompiland *) ))
        let istate,tcEnvAtEndOfLastInput = ProcessInputs (istate, [input], showTypes, true, isInteractiveItExpr, prefix)
        let tcState = istate.tcState 
        { istate with tcState = tcState.NextStateAfterIncrementalFragment(tcEnvAtEndOfLastInput) }
      
     
    /// Evaluate the given expression and produce a new interactive state.
    member fsiDynamicCompiler.EvalParsedExpression (istate, expr: SynExpr) =
        let tcConfig = TcConfig.Create (tcConfigB, validate=false)
        let itName = "it" 

        // Construct the code that saves the 'it' value into the 'SaveIt' register.
        let defs = fsiDynamicCompiler.BuildItBinding expr

        // Evaluate the overall definitions.
        let istate = fsiDynamicCompiler.EvalParsedDefinitions (istate, false, true, defs)
        // Snarf the type for 'it' via the binding
        match istate.tcState.TcEnvFromImpls.NameEnv.FindUnqualifiedItem itName with 
        | Nameres.Item.Value vref -> 
             if not tcConfig.noFeedback then 
                 valuePrinter.InvokeExprPrinter (istate.tcState.TcEnvFromImpls.DisplayEnv, vref.Deref)
             
             /// Clear the value held in the previous "it" binding, if any, as long as it has never been referenced.
             match prevIt with
             | Some prevVal when not prevVal.Deref.HasBeenReferenced -> 
                 istate.ilxGenerator.ClearGeneratedValue (valuePrinter.GetEvaluationContext istate.emEnv, prevVal.Deref)
             | _ -> ()
             prevIt <- Some vref
        | _ -> ()

        // Return the interactive state.
        istate

    // Construct the code that saves the 'it' value into the 'SaveIt' register.
    member __.BuildItBinding (expr: SynExpr) =
        let m = expr.Range
        let itName = "it" 

        let itID  = mkSynId m itName
        let itExp = SynExpr.Ident itID
        let mkBind pat expr = Binding (None, DoBinding, false, (*mutable*)false, [], PreXmlDoc.Empty, SynInfo.emptySynValData, pat, None, expr, m, NoSequencePointAtInvisibleBinding)
        let bindingA = mkBind (mkSynPatVar None itID) expr (* let it = <expr> *)  // NOTE: the generalizability of 'expr' must not be damaged, e.g. this can't be an application 
        let saverPath  = ["Microsoft";"FSharp";"Compiler";"Interactive";"RuntimeHelpers";"SaveIt"]
        let dots = List.replicate (saverPath.Length - 1) rangeStdin
        let bindingB = mkBind (SynPat.Wild m) (SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent(false, LongIdentWithDots(List.map (mkSynId rangeStdin) saverPath,dots),None,m), itExp,m)) (* let _  = saverPath it *)
        let defA = SynModuleDecl.Let (false, [bindingA], m)
        let defB = SynModuleDecl.Let (false, [bindingB], m)
        
        [defA; defB]

    member __.EvalRequireReference istate m path = 
        if Path.IsInvalidPath(path) then
            error(Error(FSIstrings.SR.fsiInvalidAssembly(path),m))
        // Check the file can be resolved before calling requireDLLReference 
        let resolutions = tcImports.ResolveAssemblyReference(AssemblyReference(m,path),ResolveAssemblyReferenceMode.ReportErrors)
        tcConfigB.AddReferencedAssemblyByPath(m,path)
        let tcState = istate.tcState 
        let tcEnv,(_dllinfos,ccuinfos) = 
            try
                RequireDLL tcImports tcState.TcEnvFromImpls m path 
            with e ->
                tcConfigB.RemoveReferencedAssemblyByPath(m,path)
                reraise()
        let optEnv = List.fold (AddExternalCcuToOpimizationEnv tcGlobals) istate.optEnv ccuinfos
        istate.ilxGenerator.AddExternalCcus (ccuinfos |> List.map (fun ccuinfo -> ccuinfo.FSharpViewOfMetadata)) 
        resolutions,
        { istate with tcState = tcState.NextStateAfterIncrementalFragment(tcEnv); optEnv = optEnv }

    member fsiDynamicCompiler.ProcessMetaCommandsFromInputAsInteractiveCommands istate sourceFile inp =
        WithImplicitHome
           (tcConfigB, directoryName sourceFile) 
           (fun () ->
               ProcessMetaCommandsFromInput 
                   ((fun st (m,nm) -> tcConfigB.TurnWarningOff(m,nm); st),
                    (fun st (m,nm) -> snd (fsiDynamicCompiler.EvalRequireReference st m nm)),
                    (fun _ _ -> ()))  
                   tcConfigB 
                   inp 
                   (Path.GetDirectoryName sourceFile)
                   istate)
      
    member fsiDynamicCompiler.EvalSourceFiles(istate, m, sourceFiles, lexResourceManager) =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        match sourceFiles with 
        | [] -> istate
        | _ -> 
          // use a set of source files as though they were command line inputs
          let sourceFiles = sourceFiles |> List.map (fun nm -> tcConfig.ResolveSourceFile(m,nm,tcConfig.implicitIncludeDir),m)
         
          // Close the #load graph on each file and gather the inputs from the scripts.
          let closure = LoadClosure.ComputeClosureOfSourceFiles(TcConfig.Create(tcConfigB,validate=false),sourceFiles,CodeContext.Evaluation,lexResourceManager=lexResourceManager,useDefaultScriptingReferences=true)
          
          // Intent "[Loading %s]\n" (String.concat "\n     and " sourceFiles)
          fsiConsoleOutput.uprintf "[%s " (FSIstrings.SR.fsiLoadingFilesPrefixText())
          closure.Inputs  |> List.iteri (fun i (sourceFile,_) ->
              if i=0 then fsiConsoleOutput.uprintf  "%s" sourceFile
              else fsiConsoleOutput.uprintnf " %s %s" (FSIstrings.SR.fsiLoadingFilesPrefixText()) sourceFile)
          fsiConsoleOutput.uprintfn "]"

          // Play errors and warnings from closures of the surface (root) script files.
          closure.RootErrors |> List.iter errorSink
          closure.RootWarnings |> List.iter warnSink
                
          // Non-scripts will not have been parsed during #load closure so parse them now
          let sourceFiles,inputs =
              closure.Inputs
              |> List.map (fun (filename, input)->
                    let parsedInput =
                        match input with
                        | None -> ParseOneInputFile(tcConfig,lexResourceManager,["INTERACTIVE"],filename,true,errorLogger,(*retryLocked*)false)
                        | _-> input
                    filename, parsedInput)
              |> List.unzip
          
          errorLogger.AbortOnError()
          if inputs |> List.exists isNone then failwith "parse error"
          let inputs = List.map Option.get inputs
          let istate = List.fold2 fsiDynamicCompiler.ProcessMetaCommandsFromInputAsInteractiveCommands istate sourceFiles inputs
          fsiDynamicCompiler.EvalParsedSourceFiles (istate, inputs)

    
    member __.GetInitialInteractiveState () =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        let optEnv0 = InitialOptimizationEnv tcImports tcGlobals
        let emEnv = ILRuntimeWriter.emEnv0
        let tcEnv = GetInitialTypecheckerEnv None rangeStdin tcConfig tcImports tcGlobals
        let ccuName = assemblyName 

        let tcState = TypecheckInitialState (rangeStdin,ccuName,tcConfig,tcGlobals,tcImports,niceNameGen,tcEnv)

        let ilxGenerator = CreateIlxAssemblyGenerator(tcConfig,tcImports,tcGlobals, (LightweightTcValForUsingInBuildMethodCall tcGlobals), tcState.Ccu )
        {optEnv    = optEnv0;
         emEnv     = emEnv;
         tcGlobals = tcGlobals;
         tcState   = tcState;
         ilxGenerator = ilxGenerator;
         timing    = false;
        } 


[<Sealed>]
type internal FsiIntellisenseProvider(tcGlobals, tcImports: TcImports) = 

    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

    //----------------------------------------------------------------------------
    // FsiIntellisense - v1 - identifier completion - namedItemInEnvL
    //----------------------------------------------------------------------------

    member __.CompletionsForPartialLID istate (prefix:string) =
        let lid,stem =
            if prefix.IndexOf(".",StringComparison.Ordinal) >= 0 then
                let parts = prefix.Split(Array.ofList ['.'])
                let n = parts.Length
                Array.sub parts 0 (n-1) |> Array.toList,parts.[n-1]
            else
                [],prefix   
        let tcState = istate.tcState (* folded through now? *)

        let amap = tcImports.GetImportMap()
        let infoReader = new Infos.InfoReader(tcGlobals,amap)
        let ncenv = new Nameres.NameResolver(tcGlobals,amap,infoReader,Nameres.FakeInstantiationGenerator)
        // Note: for the accessor domain we should use (AccessRightsOfEnv tcState.TcEnvFromImpls)
        let ad = Infos.AccessibleFromSomeFSharpCode
        let nItems = Nameres.ResolvePartialLongIdent ncenv tcState.TcEnvFromImpls.NameEnv (ConstraintSolver.IsApplicableMethApprox tcGlobals amap rangeStdin) rangeStdin ad lid false
        let names  = nItems |> List.map (fun d -> d.DisplayName tcGlobals) 
        let names  = names |> List.filter (fun (name:string) -> name.StartsWith(stem,StringComparison.Ordinal)) 
        names

#if FSI_SERVER_INTELLISENSE
    //----------------------------------------------------------------------------
    // FsiIntellisense (posible feature for v2) - GetDeclarations
    //----------------------------------------------------------------------------

    member __.FsiGetDeclarations istate (text:string) (names:string[]) =
        try
          let tcConfig = TcConfig.Create(tcConfigB,validate=false)
          Microsoft.FSharp.Compiler.SourceCodeServices.FsiIntelisense.getDeclarations
            (tcConfig,
             tcGlobals,
             tcImports,
             istate.tcState) 
            text 
            names
        with
          e ->
            System.Windows.Forms.MessageBox.Show("FsiGetDeclarations: throws:\n" ^ e.ToString()) |> ignore;
            [| |]

#endif

//----------------------------------------------------------------------------
// Reading stdin 
//----------------------------------------------------------------------------

type internal FsiStdinLexerProvider
                          (tcConfigB, fsiStdinSyphon, 
                           fsiConsoleInput : FsiConsoleInput, 
                           fsiConsoleOutput : FsiConsoleOutput, 
                           fsiOptions : FsiCommandLineOptions,
                           lexResourceManager : LexResourceManager,
                           errorLogger) = 

    // #light is the default for FSI
    let interactiveInputLightSyntaxStatus = 
        let initialLightSyntaxStatus = tcConfigB.light <> Some false
        LightSyntaxStatus (initialLightSyntaxStatus, false (* no warnings *))

    let LexbufFromLineReader (fsiStdinSyphon: FsiStdinSyphon) readf = 
        UnicodeLexing.FunctionAsLexbuf 
          (fun (buf: char[], start, len) -> 
            //fprintf fsiConsoleOutput.Out "Calling ReadLine\n";
            let inputOption = try Some(readf()) with :? EndOfStreamException -> None
            inputOption |> Option.iter (fun t -> fsiStdinSyphon.Add (t + "\n"));
            match inputOption with 
            |  Some(null) | None -> 
                 if !progress then fsiConsoleOutput.Out.WriteLine "End of file from TextReader.ReadLine";
                 0
            | Some (input:string) ->
                let input  = input + "\n" 
                let ninput = input.Length 
                if ninput > len then fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiLineTooLong());
                let ntrimmed = min len ninput 
                for i = 0 to ntrimmed-1 do
                    buf.[i+start] <- input.[i]
                ntrimmed
        )

    //----------------------------------------------------------------------------
    // Reading stdin as a lex stream
    //----------------------------------------------------------------------------

    let removeZeroCharsFromString (str:string) = (* bug://4466 *)
        if str<>null && str.Contains("\000") then
          System.String(str |> Seq.filter (fun c -> c<>'\000') |> Seq.toArray)
        else
          str

    let CreateLexerForLexBuffer (sourceFileName, lexbuf) =

        Lexhelp.resetLexbufPos sourceFileName lexbuf;
        let skip = true  // don't report whitespace from lexer 
        let defines = "INTERACTIVE"::tcConfigB.conditionalCompilationDefines
        let lexargs = mkLexargs (sourceFileName,defines, interactiveInputLightSyntaxStatus, lexResourceManager, ref [], errorLogger) 
        let tokenizer = Lexfilter.LexFilter(interactiveInputLightSyntaxStatus, tcConfigB.compilingFslib, Lexer.token lexargs skip, lexbuf)
        tokenizer


    // Create a new lexer to read stdin 
    member __.CreateStdinLexer () =
        let lexbuf = 
            match fsiConsoleInput.TryGetConsole() with 
            | Some console when fsiOptions.EnableConsoleKeyProcessing && not fsiOptions.IsInteractiveServer -> 
                LexbufFromLineReader fsiStdinSyphon (fun () -> 
                    match fsiConsoleInput.TryGetFirstLine() with 
                    | Some firstLine -> firstLine
                    | None -> console.ReadLine())
            | _ -> 
                LexbufFromLineReader fsiStdinSyphon (fun () -> fsiConsoleInput.In.ReadLine() |> removeZeroCharsFromString)

        fsiStdinSyphon.Reset();
        CreateLexerForLexBuffer (Lexhelp.stdinMockFilename, lexbuf)

    // Create a new lexer to read an "included" script file
    member __.CreateIncludedScriptLexer sourceFileName =
        let lexbuf = UnicodeLexing.UnicodeFileAsLexbuf(sourceFileName,tcConfigB.inputCodePage,(*retryLocked*)false)  
        CreateLexerForLexBuffer (sourceFileName, lexbuf)
