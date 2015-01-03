// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// Server mode for F# interactive.
module internal Microsoft.FSharp.Compiler.Interactive.Server

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
open Microsoft.FSharp.Compiler.Interactive.Processing
open Microsoft.FSharp.Compiler.Interactive.UserInterface


let internal SpawnThread name f =
    let th = new Thread(new ThreadStart(f), Name = name, IsBackground = true)
    th.Start()

let internal SpawnInteractiveServer 
                           (fsiOptions : FsiCommandLineOptions, 
                            fsiConsoleOutput:  FsiConsoleOutput,
                            fsiInterruptController : FsiInterruptController) =   
    //printf "Spawning fsi server on channel '%s'" !fsiServerName
    SpawnThread ThreadNames.serverThread (fun () ->
         SetCurrentUICultureForThread fsiOptions.FsiLCID
         try
             let server =
                 {new Server.Shared.FSharpInteractiveServer() with
                    member this.Interrupt() = 
                        //printf "FSI-SERVER: received CTRL-C request...\n";
                        try 
                            fsiInterruptController.Interrupt()
                        with e -> 
                            // Final sanity check! - catch all exns - but not expected 
                            assert false
                            ()    

#if FSI_SERVER_INTELLISENSE
                    member this.Completions(prefix) = 
                        try 
                            fsiIntellisenseProvider.CompletionsForPartialLID !istateRef prefix  |> List.toArray
                        with e -> 
                            // Final sanity check! - catch all exns - but not expected
                            assert false
                            [| |] 

                    member this.GetDeclarations(text,names) = 
                        try 
                            // Stop the type checker running at the same time as the intellisense provider.
                            lock tcLockObject (fun () -> fsiIntellisenseProvider.FsiGetDeclarations !istateRef text names)
                        with e -> 
                            // Final sanity check! - catch all exns - but not expected 
                            assert false
                            [| |] 
#endif
                 }

             Server.Shared.FSharpInteractiveServer.StartServer(fsiOptions.FsiServerName,server)
         with e ->
             fsiConsoleOutput.Error.WriteLine (FSIstrings.SR.fsiExceptionRaisedStartingServer(e.ToString())))
  

let internal StartStdinReadAndProcessThread
                                  (lcid, istateRef, errorLogger,
                                   fsiConsoleInput: FsiConsoleInput,
                                   fsiConsoleOutput: FsiConsoleOutput,
                                   fsiStdinLexerProvider: FsiStdinLexerProvider,
                                   fsiInteractionProcessor : FsiInteractionProcessor,
                                   exitViaKillThread) =
    if !progress then fsiConsoleOutput.Out.WriteLine ("creating " + ThreadNames.stdinReaderThread)
    let cont = ref Completed
    let tokenizerRef = ref (fsiStdinLexerProvider.CreateStdinLexer())
    let culture = Thread.CurrentThread.CurrentUICulture

    let stdinReaderThread =
        let threadStart = new ThreadStart(fun () ->
            InstallErrorLoggingOnThisThread errorLogger // FSI error logging on stdinReaderThread, e.g. parse errors.
            SetCurrentUICultureForThread lcid
            try
               try 
                  if !progress then fsiConsoleOutput.Out.WriteLine "READER: stdin thread started..."

                  // Delay until we've peeked the input or read the entire first line
                  fsiConsoleInput.WaitForInitialConsoleInput()
                  
                  if !progress then fsiConsoleOutput.Out.WriteLine "READER: stdin thread got first line..."

                  // The main stdin loop, running on the stdinReaderThread.
                  // 
                  // The function 'ParseAndProcessAndEvalOneInteractionFromLexbuf' is blocking: it reads stdin 
                  // until one or more real chunks of input have been received. 
                  //
                  // We run the actual computations for each action on the main GUI thread by using
                  // mainForm.Invoke to pipe a message back through the form's main event loop. (The message 
                  // is a delegate to execute on the main Thread)
                  //
                  while !cont = CompletedWithReportedError || !cont = Completed || !cont = CtrlC do
                      if !cont = CtrlC then 
                          tokenizerRef := fsiStdinLexerProvider.CreateStdinLexer()

                      let runCodeOnMainThread f istate =
                          try 
                              fsi.EventLoop.Invoke (fun () ->
                                  InstallErrorLoggingOnThisThread errorLogger
                                  SetCurrentUICultureForThread lcid
                                  f istate) // FSI error logging on switched to thread
                          with _ -> 
                              (istate,Completed)
                              
                      let istateNew,contNew =
                          fsiInteractionProcessor.ParseAndProcessAndEvalOneInteractionFromLexbuf (exitViaKillThread, runCodeOnMainThread, !istateRef, !tokenizerRef)   

                      istateRef := istateNew
                      cont := contNew
                      if !progress then fprintfn fsiConsoleOutput.Out "READER: cont = %O" !cont

                  if !progress then fsiConsoleOutput.Out.WriteLine ("- READER: Exiting " + ThreadNames.stdinReaderThread)

                with e ->
                    stopProcessingRecovery e range0

            finally 
                // Reset the Culture code
                Thread.CurrentThread.CurrentCulture <- culture
                if !progress then fsiConsoleOutput.Out.WriteLine ("- READER: Exiting process because of failure/exit on " + ThreadNames.stdinReaderThread)
                // REVIEW: On some flavors of Mono, calling exit may freeze the process if we're using the WinForms event handler
                // Basically, on Mono 2.6.3, the GUI thread may be left dangling on exit.  At that point:
                //   -- System.Environment.Exit will cause the process to stop responding
                //   -- Calling Application.Exit() will leave the GUI thread up and running, creating a Zombie process
                //   -- Calling Abort() on the Main thread or the GUI thread will have no effect, and the process will remain unresponsive
                // Also, even the the GUI thread is up and running, the WinForms event loop will be listed as closed
                // In this case, killing the process is harmless, since we've already cleaned up after ourselves and FSI is responding
                // to an error.  (CTRL-C is handled elsewhere.) 
                // We'll only do this if we're running on Mono, "--gui" is specified and our input is piped in from stdin, so it's still
                // fairly constrained.
                if runningOnMono && fsiInteractionProcessor.FsiOptions.Gui then
                    System.Environment.ExitCode <- 1
                    Process.GetCurrentProcess().Kill()
                else
                    exit 1
            )
        new Thread(threadStart, Name = ThreadNames.stdinReaderThread)
    // stdinReaderThread.IsBackground <- true
    if !progress then fsiConsoleOutput.Out.WriteLine "MAIN: starting stdin thread..."
    stdinReaderThread.Start()


let internal DriveFsiEventLoop (fsiConsoleOutput: FsiConsoleOutput) =
    let rec runLoop() = 
        if !progress then fsiConsoleOutput.Out.WriteLine "GUI thread runLoop"
        let restart = 
            try 
              // BLOCKING POINT: The GUI Thread spends most (all) of its time this event loop
              if !progress then fsiConsoleOutput.Out.WriteLine "MAIN:  entering event loop..."
              fsi.EventLoop.Run()
            with
            |  :? ThreadAbortException ->
              // If this TAE handler kicks it's almost certainly too late to save the
              // state of the process - the state of the message loop may have been corrupted 
              fsiConsoleOutput.uprintnfn "%s" (FSIstrings.SR.fsiUnexpectedThreadAbortException())
              (try Thread.ResetAbort() with _ -> ())
              true
              // Try again, just case we can restart
            | e -> 
              stopProcessingRecovery e range0
              true
              // Try again, just case we can restart
        if !progress then fsiConsoleOutput.Out.WriteLine "MAIN:  exited event loop..."
        if restart then runLoop() 

    runLoop()

/// The primary type, representing a full F# Interactive session, reading from the given
/// text input, writing to the given text output and error writers.
type internal FsiEvaluationSession (argv:string[], inReader:TextReader, outWriter:TextWriter, errorWriter: TextWriter) =
    static let win8OSVersion = Version(6, 2, 0, 0)

    (* SDL recommendation *)
    do if not runningOnMono then Lib.UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption()
    
    // See Bug 735819
    let lcidFromCodePage =
        let outputCodePage = Console.OutputEncoding.CodePage
        let currentUICulture = Thread.CurrentThread.CurrentUICulture

        // Windows Code Page 65001 = "utf-8"
        if outputCodePage <> 65001 &&
           outputCodePage <> currentUICulture.TextInfo.OEMCodePage &&
           outputCodePage <> currentUICulture.TextInfo.ANSICodePage then
                Thread.CurrentThread.CurrentUICulture <- CultureInfo("en-US")
                Some 1033
        else
            None

    let timeReporter = FsiTimeReporter(outWriter)

    //----------------------------------------------------------------------------
    // Console coloring
    //----------------------------------------------------------------------------

    // Testing shows "console coloring" is broken on some Mono configurations (e.g. Mono 2.4 Suse LiveCD).
    // To support fsi usage, the console coloring is switched off by default on Mono.
    do if runningOnMono then enableConsoleColoring <- false 

    do SetUninitializedErrorLoggerFallback AssertFalseErrorLogger
    

    //----------------------------------------------------------------------------
    // tcConfig - build the initial config
    //----------------------------------------------------------------------------

    let defaultFSharpBinariesDir = System.AppDomain.CurrentDomain.BaseDirectory

    let tcConfigB =
        Build.TcConfigBuilder.CreateNew(defaultFSharpBinariesDir,
                                        true, // long running: optimizeForMemory
                                        Directory.GetCurrentDirectory(),isInteractive=true,
                                        isInvalidationSupported=false)
    let tcConfigP = TcConfigProvider.BasedOnMutableBuilder(tcConfigB)
    do
        tcConfigB.resolutionEnvironment <- MSBuildResolver.RuntimeLike // See Bug 3608
        tcConfigB.useFsiAuxLib <- true

        // Preset: --optimize+ -g --tailcalls+ (see 4505)
        SetOptimizeSwitch tcConfigB On
        SetDebugSwitch    tcConfigB (Some "pdbonly") On
        SetTailcallSwitch tcConfigB On

        // set platform depending on whether the current process is a 64-bit process.
        // BUG 429882 : FsiAnyCPU.exe issues warnings (x64 v MSIL) when referencing 64-bit assemblies
        tcConfigB.platform <- if System.Environment.Is64BitProcess then Some AMD64 else Some X86

    let fsiStdinSyphon = FsiStdinSyphon(errorWriter)
    let fsiConsoleOutput = FsiConsoleOutput(tcConfigB, outWriter, errorWriter)

    //----------------------------------------------------------------------------
    // Error logging and banner text setup
    //----------------------------------------------------------------------------

    let updateBannerText() =
        tcConfigB.productNameForBannerText <- FSIstrings.SR.fsiProductName FSharpEnvironment.DotNetBuildString

    let errorLogger = ErrorLoggerThatStopsOnFirstError(tcConfigB, fsiStdinSyphon, fsiConsoleOutput)

    let fsiOptions       = FsiCommandLineOptions(argv, tcConfigB, fsiConsoleOutput)
    let fsiConsolePrompt = FsiConsolePrompt(fsiOptions, fsiConsoleOutput)

    do
        // FSI error logging on main thread.
        InstallErrorLoggingOnThisThread errorLogger
        
        // setting the correct banner so that 'fsi -?' display the right thing
        updateBannerText()
    
        // Check if we have a codepage from the console
        match fsiOptions.FsiLCID with
        | Some _ -> ()
        | None ->
            tcConfigB.lcid <- lcidFromCodePage

        // Set the ui culture
        match fsiOptions.FsiLCID with
        | None -> ()
        | Some n ->
            Thread.CurrentThread.CurrentUICulture <- CultureInfo(n)

        try
            SetServerCodePages fsiOptions
        with e -> 
            warning e

        // resetting banner text after parsing options
        updateBannerText()

        if tcConfigB.showBanner then
            fsiOptions.ShowBanner()

        fsiConsoleOutput.uprintfn ""

        // When no source files to load, print ahead prompt here
        if isNil fsiOptions.SourceFiles then 
            fsiConsolePrompt.PrintAhead()

    let fsiConsoleInput = FsiConsoleInput(fsiOptions, inReader, outWriter)

    let tcGlobals,tcImports =
      try
          TcImports.BuildTcImports(tcConfigP)
      with e ->
          stopProcessingRecovery e range0
          exit 1

    let ilGlobals  = tcGlobals.ilg

    let niceNameGen = NiceNameGenerator() 

    // Share intern'd strings across all lexing/parsing
    let lexResourceManager = new Lexhelp.LexResourceManager()

    /// The lock stops the type checker running at the same time as the server intellisense implementation.
    let tcLockObject = obj ()   // any new object will do
    
    let resolveType (aref: ILAssemblyRef) = 
        match tcImports.TryFindProviderGeneratedAssemblyByName aref.Name with
        | Some assembly -> Some (Choice2Of2 assembly)
        | None -> 
        match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef aref with
        | Some resolvedPath -> Some (Choice1Of2 resolvedPath)
        | None -> None
          
    let fsiDynamicCompiler = FsiDynamicCompiler(timeReporter, tcConfigB, tcLockObject, errorLogger, outWriter, tcImports, tcGlobals, ilGlobals, fsiOptions, fsiConsoleOutput, niceNameGen, resolveType) 
    
    let fsiInterruptController = FsiInterruptController(fsiOptions, fsiConsoleOutput)
    
    do MagicAssemblyResolution.install(tcConfigB, tcImports, fsiDynamicCompiler, fsiConsoleOutput)
    
    /// This reference cell holds the most recent interactive state 
    let initialInteractiveState = fsiDynamicCompiler.GetInitialInteractiveState ()
    let istateRef = ref initialInteractiveState
      
    let fsiStdinLexerProvider = FsiStdinLexerProvider(tcConfigB, fsiStdinSyphon, fsiConsoleInput, fsiConsoleOutput, fsiOptions, lexResourceManager, errorLogger)

    let fsiIntellisenseProvider = FsiIntellisenseProvider(tcGlobals, tcImports)

    let fsiInteractionProcessor = FsiInteractionProcessor(tcConfigB, errorLogger, fsiOptions, fsiDynamicCompiler, fsiConsolePrompt, fsiConsoleOutput, fsiInterruptController, fsiStdinLexerProvider, lexResourceManager) 

    [<MethodImpl(MethodImplOptions.NoInlining ||| MethodImplOptions.NoOptimization)>]
    static member private TrySetUnhandledExceptionMode() =  
        let i = ref 0 // stop inlining 
        try
          Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException) 
          incr i;incr i;incr i;incr i;incr i;incr i;
        with _ -> 
          decr i;decr i;decr i;decr i;()

    /// Load the dummy interaction, load the initial files, and,
    /// if interacting, start the background thread to read the standard input.
    member x.Interrupt() = fsiInterruptController.Interrupt()

    /// Performs these steps:
    ///    - Load the dummy interaction, if any
    ///    - Set up exception handling, if any
    ///    - Load the initial files, if any
    ///    - Start the background thread to read the standard input, if any
    ///    - Sit in the GUI event loop indefinitely, if needed

    [<CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2004:RemoveCallsToGCKeepAlive")>]
    member x.Run() = 
        // Update the console completion function now we've got an initial type checking state.
        // This means completion doesn't work until the initial type checking state has finished loading - fair enough!
        match fsiConsoleInput.TryGetConsole() with 
        | Some console when fsiOptions.EnableConsoleKeyProcessing -> 
            console.SetCompletionFunction(fun (s1,s2) ->
                fsiIntellisenseProvider.CompletionsForPartialLID !istateRef (match s1 with Some s -> s + "." + s2 | None -> s2)
                |> Seq.ofList)
        | _ -> ()

    
        if not runningOnMono && fsiOptions.IsInteractiveServer then 
            SpawnInteractiveServer (fsiOptions, fsiConsoleOutput, fsiInterruptController)

        use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Interactive)

        let threadException isFromThreadException exn = 
             fsi.EventLoop.Invoke (
                fun () ->
                    fsiConsoleOutput.Error.WriteLine (exn.ToString())
                    errorLogger.SetError()
                    try 
                        errorLogger.AbortOnError() 
                    with StopProcessing -> 
                        // BUG 664864: Watson Clr20r3 across buckets with: Application FSIAnyCPU.exe from Dev11 RTM; Exception AE251Y0L0P2WC0QSWDZ0E2IDRYQTDSVB; FSIANYCPU.NI.EXE!Microsoft.FSharp.Compiler.Interactive.Shell+threadException
                        // reason: some window that use System.Windows.Forms.DataVisualization types (possible FSCharts) was created in FSI.
                        // at some moment one chart has raised InvalidArgumentException from OnPaint, this exception was intercepted by the code in higher layer and 
                        // passed to Application.OnThreadException. FSI has already attached its own ThreadException handler, inside it will log the original error
                        // and then raise StopProcessing exception to unwind the stack (and possibly shut down current Application) and get to DriveFsiEventLoop.
                        // DriveFsiEventLoop handles StopProcessing by suppressing it and restarting event loop from the beginning.
                        // This schema works almost always except when FSI is started as 64 bit process (FsiAnyCpu) on Windows 7.

                        // http://msdn.microsoft.com/en-us/library/windows/desktop/ms633573(v=vs.85).aspx
                        // Remarks:
                        // If your application runs on a 32-bit version of Windows operating system, uncaught exceptions from the callback 
                        // will be passed onto higher-level exception handlers of your application when available. 
                        // The system then calls the unhandled exception filter to handle the exception prior to terminating the process. 
                        // If the PCA is enabled, it will offer to fix the problem the next time you run the application.
                        // However, if your application runs on a 64-bit version of Windows operating system or WOW64, 
                        // you should be aware that a 64-bit operating system handles uncaught exceptions differently based on its 64-bit processor architecture, 
                        // exception architecture, and calling convention. 
                        // The following table summarizes all possible ways that a 64-bit Windows operating system or WOW64 handles uncaught exceptions.
                        // 1. The system suppresses any uncaught exceptions.
                        // 2. The system first terminates the process, and then the Program Compatibility Assistant (PCA) offers to fix it the next time 
                        // you run the application. You can disable the PCA mitigation by adding a Compatibility section to the application manifest.
                        // 3. The system calls the exception filters but suppresses any uncaught exceptions when it leaves the callback scope, 
                        // without invoking the associated handlers.
                        // Behavior type 2 only applies to the 64-bit version of the Windows 7 operating system.
                        
                        // NOTE: tests on Win8 box showed that 64 bit version of the Windows 8 always apply type 2 behavior

                        // Effectively this means that when StopProcessing exception is raised from ThreadException callback - it won't be intercepted in DriveFsiEventLoop.
                        // Instead it will be interpreted as unhandled exception and crash the whole process.

                        // FIX: detect if current process in 64 bit running on Windows 7 or Windows 8 and if yes - swallow the StopProcessing and ScheduleRestart instead.
                        // Visible behavior should not be different, previosuly exception unwinds the stack and aborts currently running Application.
                        // After that it will be intercepted and suppressed in DriveFsiEventLoop.
                        // Now we explicitly shut down Application so after execution of callback will be completed the control flow 
                        // will also go out of WinFormsEventLoop.Run and again get to DriveFsiEventLoop => restart the loop. I'd like the fix to be  as conservative as possible
                        // so we use special case for problematic case instead of just always scheduling restart.

                        // http://msdn.microsoft.com/en-us/library/windows/desktop/ms724832(v=vs.85).aspx
                        let os = Environment.OSVersion
                        // Win7 6.1
                        let isWindows7 = os.Version.Major = 6 && os.Version.Minor = 1
                        // Win8 6.2
                        let isWindows8Plus = os.Version >= win8OSVersion
                        if isFromThreadException && ((isWindows7 && Environment.Is64BitProcess) || (Environment.Is64BitOperatingSystem && isWindows8Plus))
#if DEBUG
                            // for debug purposes
                            && Environment.GetEnvironmentVariable("FSI_SCHEDULE_RESTART_WITH_ERRORS") = null
#endif
                        then
                            fsi.EventLoop.ScheduleRestart()
                        else
                            reraise()
                )

        if fsiOptions.Interact then 
            // page in the type check env 
            istateRef := fsiInteractionProcessor.LoadDummyInteraction !istateRef
            if !progress then fsiConsoleOutput.Out.WriteLine "MAIN: InstallKillThread!"
            
            // Compute how long to pause before a ThreadAbort is actually executed.
            // A somewhat arbitrary choice.
            let pauseTime =
                let pauseMilliseconds = if fsiOptions.Gui then 400 else 100
                TimeSpan.FromMilliseconds (float pauseMilliseconds)

            let exitViaKillThread = fsiInterruptController.InstallKillThread(Thread.CurrentThread, pauseTime)
            if !progress then fsiConsoleOutput.Out.WriteLine "MAIN: got initial state, creating form"

            // Route background exceptions to the exception handlers
            AppDomain.CurrentDomain.UnhandledException.Add (fun args ->
                match args.ExceptionObject with
                | :? System.Exception as err -> threadException false err
                | _ -> ())

            if fsiOptions.Gui then
                try
                    Application.EnableVisualStyles()
                with _ ->
                    ()

                // Route GUI application exceptions to the exception handlers
                Application.add_ThreadException(new ThreadExceptionEventHandler(fun _ args -> threadException true args.Exception))

                if not runningOnMono then
                    try
                        FsiEvaluationSession.TrySetUnhandledExceptionMode()
                    with _ -> 
                        ();

                // This is the event loop for winforms
                fsi.EventLoop <- WinFormsEventLoop(fsiConsoleOutput, fsiOptions.FsiLCID)
                                        
            istateRef := fsiInteractionProcessor.LoadInitialFiles (exitViaKillThread, !istateRef)

            StartStdinReadAndProcessThread(fsiOptions.FsiLCID, istateRef, errorLogger, fsiConsoleInput, fsiConsoleOutput, fsiStdinLexerProvider, fsiInteractionProcessor, exitViaKillThread)            

            DriveFsiEventLoop fsiConsoleOutput 

        else // not interact
            istateRef := fsiInteractionProcessor.LoadInitialFiles (false, !istateRef)
            exit (min errorLogger.ErrorCount 1)

        // The Ctrl-C exception handler that we've passed to native code has
        // to be explicitly kept alive.
        GC.KeepAlive fsiInterruptController.EventHandlers
