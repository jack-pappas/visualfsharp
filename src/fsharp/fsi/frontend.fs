// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.Interactive.Frontend

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


/// Used to make a copy of input in order to include the input when displaying the error text.
[<Sealed>]
type internal FsiStdinSyphon(errorWriter: TextWriter) = 
    let syphonText = new StringBuilder()

    /// Clears the syphon text.
    member __.Reset () = 
        syphonText.Clear() |> ignore

    /// Adds a new line to the syphon text.
    member __.Add (str:string) = 
        syphonText.Append str |> ignore

    // In Visual Studio, when sending a block of text, it  prefixes  with '# <line> "filename"\n'
    // and postfixes with '# 1 "stdin"\n'. To first, get errors filename context,
    // and second to get them back into stdin context (no position stack...).
    // To find an error line, trim upto the last stdinReset string the syphoned text.
    static member Prune (text : string) =
        let stdinReset = "# 1 \"stdin\"\n"
        let idx = text.IndexOf(stdinReset,StringComparison.Ordinal)
        if idx <> -1 then
            FsiStdinSyphon.Prune (text.Substring(idx + stdinReset.Length))
        else
            text

    /// Gets the indicated line in the syphon text.
    member __.GetLine filename i =
        if filename <> Lexhelp.stdinMockFilename then
            ""
        else
            // In Visual Studio, when sending a block of text, it  prefixes  with '# <line> "filename"\n'
            // and postfixes with '# 1 "stdin"\n'. To first, get errors filename context,
            // and second to get them back into stdin context (no position stack...).
            // To find an error line, trim upto the last stdinReset string the syphoned text.
            let lines =
                let text =
                    let text = syphonText.ToString()
                    //printf "PrePrune:-->%s<--\n\n" text;
                    FsiStdinSyphon.Prune text
                text.Split '\n'
            if 0 < i && i <= lines.Length then lines.[i-1] else ""

    /// Display the given error.
    member syphon.PrintError (tcConfig:TcConfigBuilder, isWarn, err) =
        Utilities.ignoreAllErrors <| fun () -> 
            DoWithErrorColor isWarn <| fun () ->
                errorWriter.WriteLine ()
                writeViaBufferWithEnvironmentNewLines errorWriter (OutputErrorOrWarningContext "  " syphon.GetLine) err
                writeViaBufferWithEnvironmentNewLines errorWriter (OutputErrorOrWarning (tcConfig.implicitIncludeDir,tcConfig.showFullPaths,tcConfig.flatErrors,tcConfig.errorStyle,false)) err
                errorWriter.WriteLine ()

/// <summary>
/// Encapsulates functions for writing 'usual' responses to output and error streams.
/// Messages written to the output stream can be suppressed by specifying the
/// <c>--quiet</c> switch when starting F# interactive.
/// </summary>
[<Sealed>]
type internal FsiConsoleOutput (tcConfigB, outWriter : TextWriter, errorWriter : TextWriter) =
    /// TextWriter instance which discards anything written to it.
    // REVIEW : Why is this necessary? In the 'uprintf' methods below, why not just short-circuit
    //          when tcConfigB.noFeedback is set, instead of formatting the input then discarding it?
    let nullOut = new StreamWriter(Stream.Null) :> TextWriter

    // Closure to use with kprintf when 'fprintfnn' is called with 'outWriter'.
    // Creating the closure up-front avoids re-allocating it each time the function is called.
    let fprintfnnContOut = fun () -> outWriter.WriteLine(); outWriter.WriteLine ()

    let fprintfnn (os : TextWriter) fmt =
        let cont =
            // If the TextWriter is 'outWriter', use the pre-allocated closure.
            if Object.ReferenceEquals (os, outWriter) then fprintfnnContOut
            else
                // Shouldn't ever reach this point, but in case the code changes later...
                fun () -> os.WriteLine(); os.WriteLine()

        Printf.kfprintf cont os fmt

    member __.Out = outWriter
    member __.Error = errorWriter

    /// Print to the output stream using the given format.
    member __.uprintf    fmt = fprintf   (if tcConfigB.noFeedback then nullOut else outWriter) fmt
    /// Print to the output stream using the given format, and add a newline.
    member __.uprintfn   fmt = fprintfn  (if tcConfigB.noFeedback then nullOut else outWriter) fmt
    /// Print to the output stream using the given format, and add two newlines.
    member __.uprintfnn  fmt = fprintfnn (if tcConfigB.noFeedback then nullOut else outWriter) fmt

    member out.uprintnf   fmt = out.uprintfn ""; out.uprintf   fmt
    member out.uprintnfn  fmt = out.uprintfn ""; out.uprintfn  fmt
    member out.uprintnfnn fmt = out.uprintfn ""; out.uprintfnn fmt


/// This ErrorLogger reports all warnings, but raises StopProcessing on first error or early exit
[<Sealed>]
type internal ErrorLoggerThatStopsOnFirstError(tcConfigB:TcConfigBuilder, fsiStdinSyphon:FsiStdinSyphon, fsiConsoleOutput: FsiConsoleOutput) = 
    inherit ErrorLogger("ErrorLoggerThatStopsOnFirstError")
    let mutable errors = 0

    member x.SetError () =
        errors <- 1
    member x.ErrorSinkHelper err = 
        fsiStdinSyphon.PrintError(tcConfigB,false,err)
        errors <- errors + 1
        if tcConfigB.abortOnError then exit 1 (* non-zero exit code *)
        // STOP ON FIRST ERROR (AVOIDS PARSER ERROR RECOVERY)
        raise StopProcessing 
    
    member x.CheckForErrors() = errors > 0
    member x.ResetErrorCount() = errors <- 0
    
    override x.WarnSinkImpl err =
        DoWithErrorColor true (fun () -> 
            if ReportWarningAsError tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn tcConfigB.specificWarnAsError tcConfigB.specificWarnAsWarn tcConfigB.globalWarnAsError err then 
                x.ErrorSinkHelper err 
            elif ReportWarning tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn err then 
                fsiConsoleOutput.Error.WriteLine()
                writeViaBufferWithEnvironmentNewLines fsiConsoleOutput.Error (OutputErrorOrWarningContext "  " fsiStdinSyphon.GetLine) err
                writeViaBufferWithEnvironmentNewLines fsiConsoleOutput.Error (OutputErrorOrWarning (tcConfigB.implicitIncludeDir,tcConfigB.showFullPaths,tcConfigB.flatErrors,tcConfigB.errorStyle,true)) err
                fsiConsoleOutput.Error.WriteLine())

    override x.ErrorSinkImpl err = x.ErrorSinkHelper err
    override x.ErrorCount = errors

    /// A helper function to check if its time to abort
    member x.AbortOnError() =
        if errors > 0 then
            fsiConsoleOutput.Error.Write (FSIstrings.SR.stoppedDueToError())
            fsiConsoleOutput.Error.Flush()
            raise StopProcessing 


//----------------------------------------------------------------------------
// cmd line - state for options
//----------------------------------------------------------------------------

/// Process the command line options
[<Sealed>]
type internal FsiCommandLineOptions(argv: string[], tcConfigB, fsiConsoleOutput: FsiConsoleOutput) as this =
    let mutable enableConsoleKeyProcessing =
       // Mono on Win32 doesn't implement correct console processing
       not (runningOnMono && System.Environment.OSVersion.Platform = System.PlatformID.Win32NT)

    let mutable gui        = true // override via "--gui", on by default

    let mutable fsiServerName = ""
    let mutable interact = true
    let mutable explicitArgs = []

    let mutable inputFilesAcc   = []

    let mutable fsiServerInputCodePage = None
    let mutable fsiServerOutputCodePage = None

    // internal options
    let mutable probeToSeeIfConsoleWorks         = true
    let mutable peekAheadOnConsoleToPermitTyping = true

    let isInteractiveServer() = fsiServerName <> ""
    let recordExplicitArg arg = explicitArgs <- explicitArgs @ [arg]

    let executableFileName = 
        lazy
            let currentProcess = System.Diagnostics.Process.GetCurrentProcess()
            Path.GetFileName(currentProcess.MainModule.FileName)


    // Additional fsi options are list below.
    // In the "--help", these options can be printed either before (fsiUsagePrefix) or after (fsiUsageSuffix) the core options.

    let displayHelpFsi tcConfigB (blocks:CompilerOptionBlock list) =
        DisplayBannerText tcConfigB
        fsiConsoleOutput.Out.WriteLine ()
        fsiConsoleOutput.Out.WriteLine (FSIstrings.SR.fsiUsage(executableFileName.Value))
        printCompilerOptionBlocks blocks
        exit 0

    // option tags
    let [<Literal>] tagFile        = "<file>"
    let [<Literal>] tagNone        = ""
  
    /// These options preceed the FsiCoreCompilerOptions in the help blocks
    let fsiUsagePrefix tcConfigB =
      [PublicOptions(FSIstrings.SR.fsiInputFiles(),
        [CompilerOption("use",tagFile, OptionString (fun s -> inputFilesAcc <- inputFilesAcc @ [(s,true)]), None,
                                 Some (FSIstrings.SR.fsiUse()));
         CompilerOption("load",tagFile, OptionString (fun s -> inputFilesAcc <- inputFilesAcc @ [(s,false)]), None,
                                 Some (FSIstrings.SR.fsiLoad()));
        ]);
       PublicOptions(FSIstrings.SR.fsiCodeGeneration(),[]);
       PublicOptions(FSIstrings.SR.fsiErrorsAndWarnings(),[]);
       PublicOptions(FSIstrings.SR.fsiLanguage(),[]);
       PublicOptions(FSIstrings.SR.fsiMiscellaneous(),[]);
       PublicOptions(FSIstrings.SR.fsiAdvanced(),[]);
       PrivateOptions(
        [// Make internal fsi-server* options. Do not print in the help. They are used by VFSI. 
         CompilerOption("fsi-server","", OptionString (fun s -> fsiServerName <- s), None, None); // "FSI server mode on given named channel");
         CompilerOption("fsi-server-input-codepage","",OptionInt (fun n -> fsiServerInputCodePage <- Some(n)), None, None); // " Set the input codepage for the console"); 
         CompilerOption("fsi-server-output-codepage","",OptionInt (fun n -> fsiServerOutputCodePage <- Some(n)), None, None); // " Set the output codepage for the console"); 
         CompilerOption("fsi-server-no-unicode","", OptionUnit (fun () -> fsiServerOutputCodePage <- None;  fsiServerInputCodePage <- None), None, None); // "Do not set the codepages for the console");
         CompilerOption("fsi-server-lcid","", OptionInt (fun n -> this.FsiLCID <- Some(n)), None, None); // "LCID from Visual Studio"

         // We do not want to print the "script.fsx arg2..." as part of the options 
         CompilerOption("script.fsx arg1 arg2 ...","",
                                 OptionGeneral((fun args -> args.Length > 0 && IsScript args.[0]),
                                               (fun args -> let scriptFile = args.[0]
                                                            let scriptArgs = List.tail args
                                                            inputFilesAcc <- inputFilesAcc @ [(scriptFile,true)]   (* record script.fsx for evaluation *)
                                                            List.iter recordExplicitArg scriptArgs            (* record rest of line as explicit arguments *)
                                                            tcConfigB.noFeedback <- true                      (* "quiet", no banners responses etc *)
                                                            interact <- false                                 (* --exec, exit after eval *)
                                                            [] (* no arguments passed on, all consumed here *)

                                               )),None,None); // "Run script.fsx with the follow command line arguments: arg1 arg2 ...");
        ]);
       PrivateOptions(
        [
         // Private options, related to diagnostics around console probing 
         CompilerOption("probeconsole","", OptionSwitch (fun flag -> probeToSeeIfConsoleWorks <- flag=On), None, None); // "Probe to see if Console looks functional");
         CompilerOption("peekahead","", OptionSwitch (fun flag -> peekAheadOnConsoleToPermitTyping <- flag=On), None, None); // "Probe to see if Console looks functional");
        ])
      ]

    /// These options follow the FsiCoreCompilerOptions in the help blocks
    let fsiUsageSuffix tcConfigB =
      [PublicOptions(FSComp.SR.optsHelpBannerInputFiles(),
        [CompilerOption("--","", OptionRest recordExplicitArg, None,
                                 Some (FSIstrings.SR.fsiRemaining()));
        ]);
       PublicOptions(FSComp.SR.optsHelpBannerMisc(),    
        [   CompilerOption("help", tagNone,                      
                                 OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks),None,
                                 Some (FSIstrings.SR.fsiHelp()))
        ]);
       PrivateOptions(
        [   CompilerOption("?"        , tagNone, OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks), None, None); // "Short form of --help");
            CompilerOption("help"     , tagNone, OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks), None, None); // "Short form of --help");
            CompilerOption("full-help", tagNone, OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks), None, None); // "Short form of --help");
        ]);
       PublicOptions(FSComp.SR.optsHelpBannerAdvanced(),
        [CompilerOption("exec",                 "", OptionUnit (fun () -> interact <- false), None, Some (FSIstrings.SR.fsiExec()));
         CompilerOption("gui",                  tagNone, OptionSwitch(fun flag -> gui <- (flag = On)),None,Some (FSIstrings.SR.fsiGui()));
         CompilerOption("quiet",                "", OptionUnit (fun () -> tcConfigB.noFeedback <- true), None,Some (FSIstrings.SR.fsiQuiet()));     
         (* Renamed --readline and --no-readline to --tabcompletion:+|- *)
         CompilerOption("readline",             tagNone, OptionSwitch(fun flag -> enableConsoleKeyProcessing <- (flag = On)),           None, Some(FSIstrings.SR.fsiReadline()));
         CompilerOption("quotations-debug",     tagNone, OptionSwitch(fun switch -> tcConfigB.emitDebugInfoInQuotations <- switch = On),None, Some(FSIstrings.SR.fsiEmitDebugInfoInQuotations()));
         CompilerOption("shadowcopyreferences", tagNone, OptionSwitch(fun flag -> tcConfigB.shadowCopyReferences <- flag = On),         None, Some(FSIstrings.SR.shadowCopyReferences()));
        ]);
      ]


    /// Process command line, flags and collect filenames.
    /// The ParseCompilerOptions function calls imperative function to process "real" args
    /// Rather than start processing, just collect names, then process them.
    let sourceFiles =
        let collect name =
            let fsx = Build.IsScript name
            inputFilesAcc <- inputFilesAcc @ [(name,fsx)] // O(n^2), but n small...
        try 
           let fsiCompilerOptions = fsiUsagePrefix tcConfigB @ GetCoreFsiCompilerOptions tcConfigB @ fsiUsageSuffix tcConfigB
           let abbrevArgs = abbrevFlagSet tcConfigB false
           ParseCompilerOptions collect fsiCompilerOptions (List.tail (PostProcessCompilerArgs abbrevArgs argv))
        with e ->
            stopProcessingRecovery e range0
            exit 1
        inputFilesAcc

    do 
        if tcConfigB.utf8output then
            let prev = Console.OutputEncoding
            Console.OutputEncoding <- System.Text.Encoding.UTF8
            System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> Console.OutputEncoding <- prev)

    do 
        let firstArg =
            match List.tryLast sourceFiles with
            | None -> argv.[0]
            | Some lastFile ->
                fst lastFile
        let args = Array.ofList (firstArg :: explicitArgs) 
        fsi.CommandLineArgs <- args


    //----------------------------------------------------------------------------
    // Banner
    //----------------------------------------------------------------------------

    member __.ShowBanner() =
        fsiConsoleOutput.uprintnfn "%s" (tcConfigB.productNameForBannerText)
        fsiConsoleOutput.uprintfnn "%s" (FSComp.SR.optsCopyright())
        fsiConsoleOutput.uprintfn  "%s" (FSIstrings.SR.fsiBanner3())
     
    member __.ShowHelp() =
        let helpLine = sprintf "%s --help" (Path.GetFileNameWithoutExtension executableFileName.Value)

        fsiConsoleOutput.uprintfn  ""
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader1directives());
        fsiConsoleOutput.uprintfn  "    #r \"file.dll\";;        %s" (FSIstrings.SR.fsiIntroTextHashrInfo());
        fsiConsoleOutput.uprintfn  "    #I \"path\";;            %s" (FSIstrings.SR.fsiIntroTextHashIInfo());
        fsiConsoleOutput.uprintfn  "    #load \"file.fs\" ...;;  %s" (FSIstrings.SR.fsiIntroTextHashloadInfo());
        fsiConsoleOutput.uprintfn  "    #time [\"on\"|\"off\"];;   %s" (FSIstrings.SR.fsiIntroTextHashtimeInfo());
        fsiConsoleOutput.uprintfn  "    #help;;                %s" (FSIstrings.SR.fsiIntroTextHashhelpInfo());
        fsiConsoleOutput.uprintfn  "    #quit;;                %s" (FSIstrings.SR.fsiIntroTextHashquitInfo()); (* last thing you want to do, last thing in the list - stands out more *)
        fsiConsoleOutput.uprintfn  "";
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader2commandLine());
        fsiConsoleOutput.uprintfn  "%s" (FSIstrings.SR.fsiIntroTextHeader3(helpLine));
        fsiConsoleOutput.uprintfn  "";
        fsiConsoleOutput.uprintfn "";

#if DEBUG
    // show module IL code
    member val ShowILCode = false with get, set
#endif
    // show types after each interaction?
    member val ShowTypes = true with get, set
    member __.FsiServerName = fsiServerName
    member __.FsiServerInputCodePage = fsiServerInputCodePage
    member __.FsiServerOutputCodePage = fsiServerOutputCodePage
    member val FsiLCID = None with get, set
    member __.IsInteractiveServer = isInteractiveServer()
    member __.ProbeToSeeIfConsoleWorks = probeToSeeIfConsoleWorks
    member __.EnableConsoleKeyProcessing = enableConsoleKeyProcessing

    member __.Interact = interact
    member __.PeekAheadOnConsoleToPermitTyping = peekAheadOnConsoleToPermitTyping
    member __.SourceFiles = sourceFiles
    member __.Gui = gui

/// Set the current UI culture for the current thread.
let internal SetCurrentUICultureForThread (lcid : int option) =
    match lcid with
    | Some n -> Thread.CurrentThread.CurrentUICulture <- new CultureInfo(n)
    | None -> ()


//----------------------------------------------------------------------------
// Reporting - warnings, errors
//----------------------------------------------------------------------------

let internal InstallErrorLoggingOnThisThread errorLogger =
    if !progress then dprintfn "Installing logger on id=%d name=%s" Thread.CurrentThread.ManagedThreadId Thread.CurrentThread.Name
    SetThreadErrorLoggerNoUnwind(errorLogger)
    SetThreadBuildPhaseNoUnwind(BuildPhase.Interactive)

/// Set the input/output encoding. The use of a thread is due to a known bug on
/// on Vista where calls to Console.InputEncoding can block the process.
let internal SetServerCodePages(fsiOptions: FsiCommandLineOptions) =
    match fsiOptions.FsiServerInputCodePage, fsiOptions.FsiServerOutputCodePage with 
    | None,None -> ()
    | inputCodePageOpt,outputCodePageOpt ->
        let successful = ref false 
        async {
        do
            match inputCodePageOpt with
            | None -> ()
            | Some n ->
                let encoding = System.Text.Encoding.GetEncoding(n)
                // Note this modifies the real honest-to-goodness settings for the current shell.
                // and the modifications hang around even after the process has exited.
                Console.InputEncoding <- encoding
        
        do
            match outputCodePageOpt with
            | None -> ()
            | Some n ->
                let encoding = System.Text.Encoding.GetEncoding n
                // Note this modifies the real honest-to-goodness settings for the current shell.
                // and the modifications hang around even after the process has exited.
                Console.OutputEncoding <- encoding
        
        do successful := true
        } |> Async.Start

        for pause in [10;50;100;1000;2000;10000] do
            if not !successful then
                Thread.Sleep pause
        if not !successful then
            FSIstrings.SR.fsiConsoleProblem()
            |> System.Windows.Forms.MessageBox.Show
            |> ignore


//----------------------------------------------------------------------------
// Prompt printing
//----------------------------------------------------------------------------
[<Sealed>]
type internal FsiConsolePrompt(fsiOptions: FsiCommandLineOptions, fsiConsoleOutput: FsiConsoleOutput) =
    // A prompt gets "printed ahead" at start up. Tells users to start type while initialisation completes.
    // A prompt can be skipped by "silent directives", e.g. ones sent to FSI by VS.
    let mutable dropPrompt = 0
    // NOTE: SERVER-PROMPT is not user displayed, rather it's a prefix that code elsewhere 
    // uses to identify the prompt, see vs\FsPkgs\FSharp.VS.FSI\fsiSessionToolWindow.fs
    let prompt = if fsiOptions.IsInteractiveServer then "SERVER-PROMPT>\n" else "> "  

    member __.Print()      = if dropPrompt = 0 then fsiConsoleOutput.uprintf "%s" prompt else dropPrompt <- dropPrompt - 1
    member __.PrintAhead() = dropPrompt <- dropPrompt + 1; fsiConsoleOutput.uprintf "%s" prompt
    member __.SkipNext()   = dropPrompt <- dropPrompt + 1    
    member __.FsiOptions = fsiOptions


//----------------------------------------------------------------------------
// Startup processing
//----------------------------------------------------------------------------
[<Sealed>]
type internal FsiConsoleInput(fsiOptions: FsiCommandLineOptions, inReader: TextReader, outWriter: TextWriter) =

    let consoleLooksOperational() =
        if fsiOptions.ProbeToSeeIfConsoleWorks then 
            try
                // Probe to see if the console looks functional on this version of .NET
                let _ = Console.KeyAvailable 
                let _ = Console.ForegroundColor
                let _ = Console.CursorLeft <- Console.CursorLeft
                true
            with _ -> 
                (* warning(Failure("Note: there was a problem setting up custom readline console support. Consider starting fsi.exe with the --no-readline option")); *)
                false
        else
            true 

    let consoleOpt =
        // The "console.fs" code does a limited form of "TAB-completion".
        // Currently, it turns on if it looks like we have a console.
        if fsiOptions.EnableConsoleKeyProcessing && consoleLooksOperational() then
            Some(new Microsoft.FSharp.Compiler.Interactive.ReadLineConsole())
        else
            None

    // When VFSI is running, there should be no "console", and in particular the console.fs readline code should not to run.
    do  if fsiOptions.IsInteractiveServer then assert(consoleOpt = None)

    /// This threading event gets set after the first-line-reader has finished its work
    let consoleReaderStartupDone = new ManualResetEvent(false)

    /// When using a key-reading console this holds the first line after it is read
    let mutable firstLine = None

    /// Peek on the standard input so that the user can type into it from a console window.
    do if fsiOptions.Interact then
         if fsiOptions.PeekAheadOnConsoleToPermitTyping then 
          (new Thread(fun () -> 
              match consoleOpt with 
              | Some console when fsiOptions.EnableConsoleKeyProcessing && not fsiOptions.IsInteractiveServer ->
                  if isNil fsiOptions.SourceFiles then 
                      if !progress then fprintfn outWriter "first-line-reader-thread reading first line...";
                      firstLine <- Some(console.ReadLine()); 
                      if !progress then fprintfn outWriter "first-line-reader-thread got first line = %A..." firstLine;
                  consoleReaderStartupDone.Set() |> ignore 
                  if !progress then fprintfn outWriter "first-line-reader-thread has set signal and exited." ;
              | _ -> 
                  ignore(inReader.Peek());
                  consoleReaderStartupDone.Set() |> ignore 
            )).Start()
         else
           consoleReaderStartupDone.Set() |> ignore

    /// Try to get the first line, if we snarfed it while probing.
    member __.TryGetFirstLine() = let r = firstLine in firstLine <- None; r

    /// Try to get the console, if it appears operational.
    member __.TryGetConsole() = consoleOpt

    member __.In = inReader

    member __.WaitForInitialConsoleInput() = WaitHandle.WaitAll [| consoleReaderStartupDone  |] |> ignore;


type internal FsiValuePrinterMode = 
    | PrintExpr 
    | PrintDecl

/// Used to print value signatures along with their values, according to the current
/// set of pretty printers installed in the system, and default printing rules.
[<Sealed>]
type internal FsiValuePrinter(ilGlobals, generateDebugInfo, resolvePath, outWriter) = 

    /// This printer is used by F# Interactive if no other printers apply.
    let DefaultPrintingIntercept (ienv: Internal.Utilities.StructuredFormat.IEnvironment) (obj:obj) = 
       match obj with 
       | null -> None 
       | :? System.Collections.IDictionary as ie ->
          let it = ie.GetEnumerator() 
          try 
              let itemLs = 
                  Internal.Utilities.StructuredFormat.LayoutOps.unfoldL // the function to layout each object in the unfold
                          (fun obj -> ienv.GetLayout obj) 
                          // the function to call at each step of the unfold
                          (fun () -> 
                              if it.MoveNext() then 
                                 Some((it.Key, it.Value),()) 
                              else None) () 
                          // the maximum length
                          (1+fsi.PrintLength/3) 
              let makeListL itemLs =
                (leftL "[") ^^
                sepListL (rightL ";") itemLs ^^
                (rightL "]")
              Some(wordL "dict" --- makeListL itemLs)
          finally
             match it with 
             | :? System.IDisposable as d -> d.Dispose()
             | _ -> ()
             
       | _ -> None 


    /// Get the print options used when formatting output using the structured printer.
    member __.GetFsiPrintOptions() = 
        { Internal.Utilities.StructuredFormat.FormatOptions.Default with 
              FormatProvider = fsi.FormatProvider;
              PrintIntercepts = 
                  // The fsi object supports the addition of two kinds of printers, one which converts to a string
                  // and one which converts to another object that is recursively formatted.
                  // The internal AddedPrinters reports these to FSI.EXE and we pick them up here to produce a layout
                  [ for x in fsi.AddedPrinters do 
                         match x with 
                         | Choice1Of2 (aty: System.Type, printer) -> 
                                yield (fun _ienv (obj:obj) ->
                                   match obj with 
                                   | null -> None 
                                   | _ when aty.IsAssignableFrom(obj.GetType())  ->  
                                       match printer obj with 
                                       | null -> None
                                       | s -> Some (wordL s) 
                                   | _ -> None)
                                   
                         | Choice2Of2 (aty: System.Type, converter) -> 
                                yield (fun ienv (obj:obj) ->
                                   match obj with 
                                   | null -> None 
                                   | _ when aty.IsAssignableFrom(obj.GetType())  -> 
                                       match converter obj with 
                                       | null -> None
                                       | res -> Some (ienv.GetLayout res)
                                   | _ -> None)
                    yield DefaultPrintingIntercept];
              FloatingPointFormat = fsi.FloatingPointFormat;
              PrintWidth = fsi.PrintWidth; 
              PrintDepth = fsi.PrintDepth; 
              PrintLength = fsi.PrintLength;
              PrintSize = fsi.PrintSize;
              ShowProperties = fsi.ShowProperties;
              ShowIEnumerable = fsi.ShowIEnumerable; }

    /// Get the evaluation context used when inverting the storage mapping of the ILRuntimeWriter.
    member __.GetEvaluationContext emEnv = 
        { LookupFieldRef = ILRuntimeWriter.LookupFieldRef emEnv >> Option.get
          LookupMethodRef = ILRuntimeWriter.LookupMethodRef emEnv >> Option.get
          LookupTypeRef = ILRuntimeWriter.LookupTypeRef emEnv >> Option.get
          LookupType = ILRuntimeWriter.LookupType { ilg = ilGlobals ; generatePdb = generateDebugInfo; resolvePath=resolvePath } emEnv }

    /// Generate a layout for an actual F# value, where we know the value has the given static type.
    member __.PrintValue (printMode, opts:FormatOptions, x:obj, ty:System.Type) = 
        // We do a dynamic invoke of any_to_layout with the right System.Type parameter for the static type of the saved value.
        // In principle this helps any_to_layout do the right thing as it descends through terms. In practice it means
        // it at least does the right thing for top level 'null' list and option values (but not for nested ones).
        //
        // The static type was saved into the location used by RuntimeHelpers.GetSavedItType when RuntimeHelpers.SaveIt was called.
        // RuntimeHelpers.SaveIt has type ('a -> unit), and fetches the System.Type for 'a by using a typeof<'a> call.
        // The funny thing here is that you might think that the driver (this file) knows more about the static types
        // than the compiled code does. But it doesn't! In particular, it's not that easy to get a System.Type value based on the
        // static type information we do have: we have no direct way to bind a F# TAST type or even an AbstractIL type to 
        // a System.Type value (I guess that functionality should be in ilreflect.fs).
        //
        // This will be more significant when we print values other then 'it'
        //
        try 
            let anyToLayoutCall = Utilities.getAnyToLayoutCall ty
            match printMode with
              | PrintDecl ->
                  // When printing rhs of fsi declarations, use "fsi_any_to_layout".
                  // This will suppress some less informative values, by returning an empty layout. [fix 4343].
                  anyToLayoutCall.FsiAnyToLayout(opts, x)
              | PrintExpr -> 
                  anyToLayoutCall.AnyToLayout(opts, x)
        with 
        | :? ThreadAbortException -> Layout.wordL ""
        | e ->
#if DEBUG
          printf "\n\nPrintValue: x = %+A and ty=%s\n" x (ty.FullName)
#endif
          printf "%s" (FSIstrings.SR.fsiExceptionDuringPrettyPrinting(e.ToString())); 
          Layout.wordL ""
            
    /// Display the signature of an F# value declaration, along with its actual value.
    member valuePrinter.InvokeDeclLayout (emEnv, ilxGenerator: Ilxgen.IlxAssemblyGenerator, v:Val) =
        // Implemented via a lookup from v to a concrete (System.Object,System.Type).
        // This (obj,objTy) pair can then be fed to the fsi value printer.
        // Note: The value may be (null:Object).
        // Note: A System.Type allows the value printer guide printing of nulls, e.g. as None or [].
        //-------
        // Ilxgen knows what the v:Val was converted to w.r.t. AbsIL datastructures.
        // Ilreflect knows what the AbsIL was generated to.
        // Combining these allows for obtaining the (obj,objTy) by reflection where possible.
        // This assumes the v:Val was given appropriate storage, e.g. StaticField.
        if fsi.ShowDeclarationValues then 
            // Adjust "opts" for printing for "declared-values":
            // - No sequences, because they may have effects or time cost.
            // - No properties, since they may have unexpected effects.
            // - Limit strings to roughly one line, since huge strings (e.g. 1 million chars without \n are slow in vfsi).
            // - Limit PrintSize which is a count on nodes.
            let declaredValueReductionFactor = 10 (* reduce PrintSize for declared values, e.g. see less of large terms *)
            let opts   = valuePrinter.GetFsiPrintOptions()
            let opts   = {opts with ShowProperties  = false // properties off, motivated by Form props 
                                    ShowIEnumerable = false // seq off, motivated by db query concerns 
                                    StringLimit = max 0 (opts.PrintWidth-4) // 4 allows for an indent of 2 and 2 quotes (rough) 
                                    PrintSize = opts.PrintSize / declaredValueReductionFactor } // print less 
            let res    = 
                try  ilxGenerator.LookupGeneratedValue (valuePrinter.GetEvaluationContext emEnv, v)
                with e -> 
                    assert false
#if DEBUG
                    //fprintfn fsiConsoleOutput.Out "lookGenerateVal: failed on v=%+A v.Name=%s" v v.LogicalName
#endif
                    None // lookup may fail 
            match res with
              | None             -> None
              | Some (obj,objTy) -> 
                  let lay = valuePrinter.PrintValue (FsiValuePrinterMode.PrintDecl, opts, obj, objTy)
                  if isEmptyL lay then None else Some lay // suppress empty layout 
                                    
        else
            None
    
    /// Fetch the saved value of an expression out of the 'it' register and show it.
    member valuePrinter.InvokeExprPrinter (denv, vref) = 
        let opts        = valuePrinter.GetFsiPrintOptions()
        let savedIt     = Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSavedIt()
        let savedItType = Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSavedItType()
        let rhsL  = valuePrinter.PrintValue (FsiValuePrinterMode.PrintExpr, opts, savedIt, savedItType)
        let denv = { denv with suppressMutableKeyword = true } // suppress 'mutable' in 'val mutable it = ...'
        let fullL = if isEmptyL rhsL then
                      NicePrint.layoutValOrMember denv vref (* the rhs was suppressed by the printer, so no value to print *)
                    else
                      (NicePrint.layoutValOrMember denv vref ++ wordL "=") --- rhsL
        Internal.Utilities.StructuredFormat.Display.output_layout opts outWriter fullL;  
        outWriter.WriteLine()
