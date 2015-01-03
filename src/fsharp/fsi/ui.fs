// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// Assembly resolution handling for F# interactive.
module internal Microsoft.FSharp.Compiler.Interactive.UserInterface

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


//
module internal ThreadNames =
    //
    [<Literal>]
    let serverThread = "ServerThread"
    //
    [<Literal>]
    let stdinReaderThread = "StdinReaderThread"
    //
    [<Literal>]
    let ctrlCAbortThread = "ControlCAbortThread"
    //
    [<Literal>]
    let ctrlCAbortThreadAlt = "ControlCAbortAlternativeThread"


//----------------------------------------------------------------------------
// ctrl-c handling
//----------------------------------------------------------------------------

module internal NativeMethods = 

    type ControlEventHandler = delegate of int -> bool

    [<DllImport("kernel32.dll")>]
    extern bool SetConsoleCtrlHandler(ControlEventHandler _callback,bool _add)

// One strange case: when a TAE happens a strange thing 
// occurs the next read from stdin always returns
// 0 bytes, i.e. the channel will look as if it has been closed.  So we check
// for this condition explicitly.  We also recreate the lexbuf whenever CtrlC kicks.
type internal FsiInterruptStdinState = 
    | StdinEOFPermittedBecauseCtrlCRecentlyPressed 
    | StdinNormal

type internal FsiInterruptControllerState =  
    | InterruptCanRaiseException 
    | InterruptIgnored 

type internal FsiInterruptControllerKillerThreadRequest =  
    | ThreadAbortRequest 
    | NoRequest 
    | ExitRequest 
    | PrintInterruptRequest

type internal FsiInterruptController(fsiOptions : FsiCommandLineOptions,
                                     fsiConsoleOutput: FsiConsoleOutput) =
    let mutable ctrlEventHandlers = [] : NativeMethods.ControlEventHandler list 
    let mutable ctrlEventActions  = [] : (unit -> unit) list

    let mutable posixReinstate = (fun () -> ())

    let [<Literal>] CTRL_C = 0

    member val FsiInterruptStdinState = StdinNormal with get, set

    member val InterruptRequest = NoRequest with get, set
    
    member val InterruptAllowed = InterruptIgnored with get, set
    
    member __.Interrupt() = ctrlEventActions |> List.iter (fun act -> act())
    
    member __.EventHandlers = ctrlEventHandlers

    //
    member private controller.InstallKillThreadWindows(threadToKill : Thread, pauseTime : TimeSpan) =
        let raiseCtrlC() =
            SetCurrentUICultureForThread fsiOptions.FsiLCID
            fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiInterrupt())
            controller.FsiInterruptStdinState <- StdinEOFPermittedBecauseCtrlCRecentlyPressed
            if controller.InterruptAllowed = InterruptCanRaiseException then
                controller.InterruptRequest <- ThreadAbortRequest
                let killerThread =
                    new Thread(new ThreadStart(fun () ->
                        SetCurrentUICultureForThread fsiOptions.FsiLCID
                        // sleep long enough to allow ControlEventHandler handler on main thread to return 
                        // Also sleep to give computations a bit of time to terminate
                        Thread.Sleep pauseTime
                        if controller.InterruptRequest = ThreadAbortRequest then
                            if !progress then fsiConsoleOutput.uprintnfn "%s" (FSIstrings.SR.fsiAbortingMainThread())
                            controller.InterruptRequest <- NoRequest
                            threadToKill.Abort()
                        ()),Name=ThreadNames.ctrlCAbortThread)
                killerThread.IsBackground <- true
                killerThread.Start()
        
        let ctrlEventHandler = new NativeMethods.ControlEventHandler(fun i -> if i = CTRL_C then (raiseCtrlC(); true) else false) 
        ctrlEventHandlers <- ctrlEventHandler :: ctrlEventHandlers
        ctrlEventActions  <- raiseCtrlC       :: ctrlEventActions
        let _resultOK = NativeMethods.SetConsoleCtrlHandler(ctrlEventHandler,true)
        false // don't exit via kill thread

    //
    member controller.InstallKillThreadPosix (threadToKill : Thread, pauseTime : TimeSpan) =
        // UNIX TECHNIQUE: We start up a killer thread, and it watches the mutable reference location.    
        // We can't have a dependency on Mono DLLs (indeed we don't even have them!)
        // So SOFT BIND the following code:
        // Mono.Unix.Native.Stdlib.signal(Mono.Unix.Native.Signum.SIGINT,new Mono.Unix.Native.SignalHandler(fun n -> PosixSignalProcessor.PosixInvoke(n))) |> ignore;
        match (try Choice1Of2(Assembly.Load("Mono.Posix, Version=2.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756")) with e -> Choice2Of2 e) with 
        | Choice1Of2 monoPosix ->
            try
                let stdlibTypeName = "Mono.Unix.Native.Stdlib"
                if !progress then fprintfn fsiConsoleOutput.Error "loading type %s..." stdlibTypeName
                let monoUnixStdlib = monoPosix.GetType stdlibTypeName

                let signalHandlerTypeName = "Mono.Unix.Native.SignalHandler"
                if !progress then fprintfn fsiConsoleOutput.Error "loading type %s..." signalHandlerTypeName
                let monoUnixSignalHandler = monoPosix.GetType signalHandlerTypeName

                if !progress then fprintfn fsiConsoleOutput.Error "creating delegate..."
                controller.PosixInvoke -1
                let monoHandler = System.Delegate.CreateDelegate(monoUnixSignalHandler,controller,"PosixInvoke") 
                if !progress then fprintfn fsiConsoleOutput.Error "registering signal handler...";
                let monoSignalNumber = System.Enum.Parse(monoPosix.GetType("Mono.Unix.Native.Signum"),"SIGINT")
                let register () = Utilities.callStaticMethod monoUnixStdlib "signal" [ monoSignalNumber; box monoHandler ]  |> ignore 
                posixReinstate <- register;
                register();
                let killerThread =
                    new Thread(new ThreadStart(fun () ->
                        SetCurrentUICultureForThread fsiOptions.FsiLCID
                        let pauseMilliseconds = int pauseTime.TotalMilliseconds
                        while true do 
                            //fprintf fsiConsoleOutput.Error "\n- kill thread loop...\n"; errorWriter.Flush();  
                            Thread.Sleep(pauseMilliseconds*2);
                            match controller.InterruptRequest with 
                            | PrintInterruptRequest ->
                                fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiInterrupt())
                                fsiConsoleOutput.Error.Flush()
                                controller.InterruptRequest <- NoRequest
                            | ThreadAbortRequest ->
                                fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiInterrupt())
                                fsiConsoleOutput.Error.Flush()
                                if !progress then fsiConsoleOutput.uprintnfn "%s" (FSIstrings.SR.fsiAbortingMainThread())
                                controller.InterruptRequest <- NoRequest
                                threadToKill.Abort()
                            | ExitRequest ->
                                // Mono has some wierd behaviour where it blocks on exit
                                // once CtrlC has ever been pressed.  Who knows why?  Perhaps something
                                // to do with having a signal handler installed, but it only happens _after_
                                // at least one CtrLC has been pressed.  Maybe raising a ThreadAbort causes
                                // exiting to have problems.
                                //
                                // Anyway, we make "#q" work this case by setting ExitRequest and brutally calling
                                // the process-wide 'exit'
                                fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiExit())
                                fsiConsoleOutput.Error.Flush()
                                Utilities.callStaticMethod monoUnixStdlib "exit" [ box 0 ] |> ignore
                            | _ ->  ()
                        done),Name=ThreadNames.ctrlCAbortThreadAlt) 
                killerThread.IsBackground <- true
                killerThread.Start()
                true // exit via kill thread to workaround block-on-exit bugs with Mono once a CtrlC has been pressed
            
            with e ->
                fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiCouldNotInstallCtrlCHandler e.Message)
                false

        | Choice2Of2 e ->
            fsiConsoleOutput.Error.Write (FSIstrings.SR.fsiCouldNotInstallCtrlCHandler e.Message)
            false  

    // REVIEW: streamline all this code to use the same code on Windows and Posix.
    member controller.InstallKillThread (threadToKill, pauseMilliseconds) =
        if !progress then fprintfn fsiConsoleOutput.Out "installing CtrlC handler"
        // WINDOWS TECHNIQUE: .NET has more safe points, and you can do more when a safe point.
        // Hence we actually start up the killer thread within the handler.
        try
            controller.InstallKillThreadWindows (threadToKill, pauseMilliseconds)
        with e ->
            if !progress then fprintfn fsiConsoleOutput.Error "Failed to install ctrl-c handler using Windows technique - trying to install one using Unix signal handling...";
            controller.InstallKillThreadPosix (threadToKill, pauseMilliseconds)


    member controller.PosixInvoke (n : int) =
        // we run this code once with n = -1 to make sure it is JITted before execution begins
        // since we are not allowed to JIT a signal handler. This also ensures the "PosixInvoke"
        // method is not eliminated by dead-code elimination
        if n >= 0 then
            posixReinstate()
            controller.FsiInterruptStdinState <- StdinEOFPermittedBecauseCtrlCRecentlyPressed
            controller.InterruptRequest <-
                if controller.InterruptAllowed = InterruptCanRaiseException then ThreadAbortRequest else PrintInterruptRequest


//----------------------------------------------------------------------------
// Process one parsed interaction.  This runs on the GUI thread.
// It might be simpler if it ran on the parser thread.
//----------------------------------------------------------------------------

//++GLOBAL MUTABLE STATE
let referencedAssemblies = Dictionary<string, DateTime>()

type internal FsiInteractionStepStatus = 
    | CtrlC 
    | EndOfFile 
    | Completed 
    | CompletedWithReportedError


type internal FsiInteractionProcessor
                            (tcConfigB, 
                             errorLogger : ErrorLoggerThatStopsOnFirstError, 
                             fsiOptions: FsiCommandLineOptions,
                             fsiDynamicCompiler: FsiDynamicCompiler,
                             fsiConsolePrompt : FsiConsolePrompt,
                             fsiConsoleOutput : FsiConsoleOutput,
                             fsiInterruptController : FsiInterruptController,
                             fsiStdinLexerProvider : FsiStdinLexerProvider,
                             lexResourceManager : LexResourceManager) =

    static let killThreadExitPauseTime = TimeSpan.FromSeconds 1.0

    let InteractiveCatch f istate =
        try
            // reset error count
            errorLogger.ResetErrorCount()
            f istate
        with e ->
            stopProcessingRecovery e range0
            istate,CompletedWithReportedError


    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

    let ChangeDirectory (path:string) m =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        let path = tcConfig.MakePathAbsolute path
        if Directory.Exists(path) then
            tcConfigB.implicitIncludeDir <- path
        else
            error(Error(FSIstrings.SR.fsiDirectoryDoesNotExist(path),m))

    /// Parse one interaction. Called on the parser thread.
    let ParseInteraction (tokenizer:Lexfilter.LexFilter) =
        let lastToken = ref Parser.ELSE // Any token besides SEMICOLON_SEMICOLON will do for initial value 
        try
            if !progress then fsiConsoleOutput.Out.WriteLine "In ParseInteraction..."

            let input =
                Lexhelp.reusingLexbufForParsing tokenizer.LexBuffer (fun () ->
                    let lexerWhichSavesLastToken lexbuf =
                        let tok = tokenizer.Lexer lexbuf
                        lastToken := tok
                        tok
                    Parser.interaction lexerWhichSavesLastToken tokenizer.LexBuffer)
            Some input
        with e ->
            // On error, consume tokens until to ;; or EOF.
            // Caveat: Unless the error parse ended on ;; - so check the lastToken returned by the lexer function.
            // Caveat: What if this was a look-ahead? That's fine! Since we need to skip to the ;; anyway.
            if (match !lastToken with Parser.SEMICOLON_SEMICOLON -> false | _ -> true) then
                let mutable tok = Parser.ELSE (* <-- any token <> SEMICOLON_SEMICOLON will do *)
                while (match tok with  Parser.SEMICOLON_SEMICOLON -> false | _ -> true)
                      && not tokenizer.LexBuffer.IsPastEndOfStream do
                    tok <- tokenizer.Lexer tokenizer.LexBuffer

            stopProcessingRecovery e range0
            None

    /// Execute a single parsed interaction. Called on the GUI/execute/main thread.
    let ExecInteraction (exitViaKillThread:bool, tcConfig:TcConfig, istate, action:ParsedFsiInteraction) =
        istate |> InteractiveCatch (fun istate -> 
            match action with 
            | IDefns ([  ],_) ->
                istate,Completed
            | IDefns ([  SynModuleDecl.DoExpr(_,expr,_)],_) ->
                fsiDynamicCompiler.EvalParsedExpression  (istate, expr), Completed           
            | IDefns (defs,_) -> 
                fsiDynamicCompiler.EvalParsedDefinitions (istate, true, false, defs), Completed

            | IHash (ParsedHashDirective("load",sourceFiles,m),_) -> 
                fsiDynamicCompiler.EvalSourceFiles (istate, m, sourceFiles, lexResourceManager),Completed

            | IHash (ParsedHashDirective(("reference" | "r"),[path],m),_) -> 
                let resolutions,istate = fsiDynamicCompiler.EvalRequireReference istate m path 
                resolutions |> List.iter (fun ar -> 
                    let format = 
                        if tcConfig.shadowCopyReferences then
                            let resolvedPath = ar.resolvedPath.ToUpperInvariant()
                            let fileTime = File.GetLastWriteTimeUtc(resolvedPath)
                            match referencedAssemblies.TryGetValue(resolvedPath) with
                            | false, _ -> 
                                referencedAssemblies.Add(resolvedPath, fileTime)
                                FSIstrings.SR.fsiDidAHashr(ar.resolvedPath)
                            | true, time when time <> fileTime ->
                                FSIstrings.SR.fsiDidAHashrWithStaleWarning(ar.resolvedPath)
                            | _ ->
                                FSIstrings.SR.fsiDidAHashr(ar.resolvedPath)
                        else
                            FSIstrings.SR.fsiDidAHashrWithLockWarning(ar.resolvedPath)
                    fsiConsoleOutput.uprintnfnn "%s" format)
                istate,Completed

            | IHash (ParsedHashDirective("I",[path],m),_) ->
                tcConfigB.AddIncludePath (m,path, tcConfig.implicitIncludeDir)
                fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiDidAHashI(tcConfig.MakePathAbsolute path))
                istate,Completed

            | IHash (ParsedHashDirective("cd",[path],m),_) ->
                ChangeDirectory path m
                istate,Completed

            | IHash (ParsedHashDirective("silentCd",[path],m),_) ->
                ChangeDirectory path m
                fsiConsolePrompt.SkipNext() (* "silent" directive *)
                istate,Completed

            | IHash (ParsedHashDirective("time",[],_),_) ->
                if istate.timing then
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOff())
                else
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOn())
                {istate with timing = not istate.timing},Completed

            | IHash (ParsedHashDirective("time",[("on" | "off") as v],_),_) ->
                if v <> "on" then
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOff())
                else
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOn())
                {istate with timing = (v = "on")},Completed

            | IHash (ParsedHashDirective("nowarn",numbers,m),_) ->
                List.iter (fun (d:string) -> tcConfigB.TurnWarningOff(m,d)) numbers
                istate,Completed

            | IHash (ParsedHashDirective("terms",[],_),_) ->
                tcConfigB.showTerms <- not tcConfig.showTerms
                istate,Completed

            | IHash (ParsedHashDirective("types",[],_),_) ->
                fsiOptions.ShowTypes <- not fsiOptions.ShowTypes
                istate,Completed

    #if DEBUG
            | IHash (ParsedHashDirective("ilcode",[],_m),_) ->
                fsiOptions.ShowILCode <- not fsiOptions.ShowILCode
                istate,Completed

            | IHash (ParsedHashDirective("info",[],_m),_) ->
                PrintOptionInfo tcConfigB
                istate,Completed
    #endif

            | IHash (ParsedHashDirective(("q" | "quit"),[],_),_) ->
                if exitViaKillThread then
                    fsiInterruptController.InterruptRequest <- ExitRequest
                    Thread.Sleep killThreadExitPauseTime
                exit 0

            | IHash (ParsedHashDirective("help",[],_),_) ->
                fsiOptions.ShowHelp()
                istate,Completed

            | IHash (ParsedHashDirective(c,arg,_),_) ->
                fsiConsoleOutput.uprintfn "%s" (FSIstrings.SR.fsiInvalidDirective(c, String.concat " " arg))  // REVIEW: uprintnfnn - like other directives above
                istate,Completed  (* REVIEW: cont = CompletedWithReportedError *)
        )

    /// Execute a single parsed interaction which may contain multiple items to be executed
    /// independently, because some are #directives. Called on the GUI/execute/main thread.
    /// 
    /// #directive comes through with other definitions as a SynModuleDecl.HashDirective.
    /// We split these out for individual processing.
    let rec ExecInteractions (exitViaKillThread, tcConfig, istate, action:ParsedFsiInteraction option) =
        let action,nextAction = 
            match action with
            | None                                      -> None  ,None
            | Some (IHash _)                            -> action,None
            | Some (IDefns ([],_))                      -> None  ,None
            | Some (IDefns (SynModuleDecl.HashDirective(hash,mh)::defs,m)) ->
                Some (IHash(hash,mh)),Some (IDefns(defs,m))

            | Some (IDefns (defs,m))                    ->
                let isDefHash = function SynModuleDecl.HashDirective(_,_) -> true | _ -> false
                let defsA = Seq.takeWhile (isDefHash >> not) defs |> Seq.toList
                let defsB = Seq.skipWhile (isDefHash >> not) defs |> Seq.toList

                // When the last declaration has a shape of DoExp (i.e., non-binding),
                // transform it to a shape of "let it = <exp>", so we can refer it.
                let defsA = if defsA.Length <= 1 || defsB.Length > 0 then defsA else
                            match List.headAndTail (List.rev defsA) with
                            | SynModuleDecl.DoExpr(_,exp,_), rest -> (rest |> List.rev) @ (fsiDynamicCompiler.BuildItBinding exp)
                            | _ -> defsA

                Some (IDefns(defsA,m)),Some (IDefns(defsB,m))

        match action with
          | None -> assert(nextAction.IsNone); istate,Completed
          | Some action ->
              let istate,cont = ExecInteraction (exitViaKillThread, tcConfig, istate, action)
              match cont with
                | Completed                  -> ExecInteractions (exitViaKillThread, tcConfig, istate, nextAction)
                | CompletedWithReportedError -> istate,CompletedWithReportedError  (* drop nextAction on error *)
                | EndOfFile                  -> istate,Completed                   (* drop nextAction on EOF *)
                | CtrlC                      -> istate,CtrlC                       (* drop nextAction on CtrlC *)

    /// Execute a single parsed interaction on the parser/execute thread.
    let MainThreadProcessParsedInteraction (exitViaKillThread, action, istate) =
        try
            let tcConfig = TcConfig.Create(tcConfigB,validate=false)
            if !progress then fsiConsoleOutput.Out.WriteLine "In MainThreadProcessParsedInteraction..."
            fsiInterruptController.InterruptAllowed <- InterruptCanRaiseException
            let res = ExecInteractions (exitViaKillThread, tcConfig, istate, action)
            fsiInterruptController.InterruptRequest <- NoRequest
            fsiInterruptController.InterruptAllowed <- InterruptIgnored
            res
        with
        | :? ThreadAbortException ->
           fsiInterruptController.InterruptRequest <- NoRequest
           fsiInterruptController.InterruptAllowed <- InterruptIgnored
           (try Thread.ResetAbort() with _ -> ())
           (istate,CtrlC)
        |  e ->
           fsiInterruptController.InterruptRequest <- NoRequest
           fsiInterruptController.InterruptAllowed <- InterruptIgnored
           stopProcessingRecovery e range0
           istate,CompletedWithReportedError


    /// Parse then process one parsed interaction.  
    ///
    /// During normal execution, this initially runs on the parser
    /// thread, then calls runCodeOnMainThread when it has completed 
    /// parsing and needs to typecheck and execute a definition. This blocks the parser thread
    /// until execution has competed on the GUI thread.
    ///
    /// During processing of startup scripts, this runs on the main thread.
    member __.ParseAndProcessAndEvalOneInteractionFromLexbuf (exitViaKillThread, runCodeOnMainThread, istate:FsiDynamicCompilerState, tokenizer:Lexfilter.LexFilter) =

        if tokenizer.LexBuffer.IsPastEndOfStream then
            let stepStatus =
                if fsiInterruptController.FsiInterruptStdinState = StdinEOFPermittedBecauseCtrlCRecentlyPressed then
                    fsiInterruptController.FsiInterruptStdinState <- StdinNormal
                    CtrlC
                else 
                    EndOfFile
            istate,stepStatus

        else 

            fsiConsolePrompt.Print()
            istate |> InteractiveCatch (fun istate ->
                if !progress then fsiConsoleOutput.Out.WriteLine "entering ParseInteraction..."

                // Parse the interaction. When FSI.EXE is waiting for input from the console the 
                // parser thread is blocked somewhere deep this call. 
                let action  = ParseInteraction tokenizer

                if !progress then fsiConsoleOutput.Out.WriteLine "returned from ParseInteraction...calling runCodeOnMainThread..."

                // After we've unblocked and got something to run we switch
                // over to the run-thread (e.g. the GUI thread)
                let res = istate  |> runCodeOnMainThread (fun istate -> MainThreadProcessParsedInteraction (exitViaKillThread, action, istate))

                if !progress then fprintfn fsiConsoleOutput.Out "Just called runCodeOnMainThread, res = %O..." res
                res)
        
    /// Perform an "include" on a script file (i.e. a script file specified on the command line)
    member processor.EvalIncludedScript (exitViaKillThread, istate, sourceFile, m) =
        let tcConfig = TcConfig.Create(tcConfigB, validate=false)
        // Resolve the filename to an absolute filename
        let sourceFile = tcConfig.ResolveSourceFile(m,sourceFile,tcConfig.implicitIncludeDir) 
        // During the processing of the file, further filenames are 
        // resolved relative to the home directory of the loaded file.
        WithImplicitHome (tcConfigB, directoryName sourceFile)  (fun () ->
              // An included script file may contain maybe several interaction blocks.
              // We repeatedly parse and process these, until an error occurs.
                let tokenizer = fsiStdinLexerProvider.CreateIncludedScriptLexer sourceFile
                let rec run istate =
                    let istate,cont = processor.ParseAndProcessAndEvalOneInteractionFromLexbuf (exitViaKillThread, (fun f istate -> f istate), istate, tokenizer)
                    if cont = Completed then run istate else istate,cont

                let istate,cont = run istate 

                match cont with
                | Completed -> failwith "EvalIncludedScript: Completed expected to have relooped"
                | CompletedWithReportedError -> istate,CompletedWithReportedError
                | EndOfFile -> istate,Completed // here file-EOF is normal, continue required 
                | CtrlC     -> istate,CtrlC
          )


    /// Load the source files, one by one. Called on the main thread.
    member processor.EvalIncludedScripts (istate, exitViaKillThread, sourceFiles) =
      match sourceFiles with
        | [] -> istate
        | sourceFile :: moreSourceFiles ->
            // Catch errors on a per-file basis, so results/bindings from pre-error files can be kept.
            let istate,cont = InteractiveCatch (fun istate -> processor.EvalIncludedScript (exitViaKillThread, istate, sourceFile, rangeStdin)) istate
            match cont with
              | Completed                  -> processor.EvalIncludedScripts (istate, exitViaKillThread, moreSourceFiles)
              | CompletedWithReportedError -> istate // do not process any more files              
              | CtrlC                      -> istate // do not process any more files 
              | EndOfFile                  -> assert false; istate // This is unexpected. EndOfFile is replaced by Completed in the called function 


    member processor.LoadInitialFiles (exitViaKillThread, istate) =
        /// Consume initial source files in chunks of scripts or non-scripts
        let rec consume istate sourceFiles =
            match sourceFiles with
            | [] -> istate
            | (_,isScript1) :: _ -> 
                let sourceFiles,rest = List.takeUntil (fun (_,isScript2) -> isScript1 <> isScript2) sourceFiles 
                let sourceFiles = List.map fst sourceFiles 
                let istate = 
                    if isScript1 then 
                        processor.EvalIncludedScripts (istate, exitViaKillThread, sourceFiles)
                    else 
                        istate |> InteractiveCatch (fun istate -> fsiDynamicCompiler.EvalSourceFiles(istate, rangeStdin, sourceFiles, lexResourceManager), Completed) |> fst 
                consume istate rest 

        let istate = consume istate fsiOptions.SourceFiles

        if nonNil fsiOptions.SourceFiles then 
            fsiConsolePrompt.PrintAhead(); // Seems required. I expected this could be deleted. Why not?
        istate 

    /// Send a dummy interaction through F# Interactive, to ensure all the most common code generation paths are 
    /// JIT'ed and ready for use.
    member processor.LoadDummyInteraction istate =
        istate |> InteractiveCatch (fun istate ->  fsiDynamicCompiler.EvalParsedDefinitions (istate, true, false, []), Completed) |> fst
        
    member processor.FsiOptions = fsiOptions

//----------------------------------------------------------------------------
// GUI runCodeOnMainThread
//----------------------------------------------------------------------------

//type InteractionStateConverter = delegate of FsiDynamicCompilerState -> FsiDynamicCompilerState * stepStatus

///Use a dummy to access protected member
type internal DummyForm() =
    inherit Form()
    member x.DoCreateHandle() = x.CreateHandle()

/// This is the event loop implementation for winforms
type internal WinFormsEventLoop(fsiConsoleOutput: FsiConsoleOutput, lcid : int option) =
    let mainForm = new DummyForm()
    do mainForm.DoCreateHandle()

    // Set the default thread exception handler
    let mutable restart = false

    interface Microsoft.FSharp.Compiler.Interactive.IEventLoop with
         member __.Run() =
             restart <- false
             if !progress then fsiConsoleOutput.Out.WriteLine "MAIN: Calling Application.Run..."
             Application.Run()
             if !progress then fsiConsoleOutput.Out.WriteLine "MAIN: Returned from Application.Run..."
             restart

         member __.Invoke (f: unit -> 'T) : 'T =
            if !progress then fsiConsoleOutput.Out.WriteLine "RunCodeOnWinFormsMainThread: entry..."
            if not mainForm.InvokeRequired then
                f()
            else
                // Workaround: Mono's Control.Invoke returns a null result.  Hence avoid the problem by 
                // transferring the resulting state using a mutable location.
                let mainFormInvokeResultHolder = ref None

                // Actually, Mono's Control.Invoke isn't even blocking (or wasn't on 1.1.15)!  So use a signal to indicate completion.
                // Indeed, we should probably do this anyway with a timeout so we can report progress from 
                // the GUI thread.
                use doneSignal = new AutoResetEvent(false)

                if !progress then fsiConsoleOutput.Out.WriteLine "RunCodeOnWinFormsMainThread: invoking..."

                // BLOCKING: This blocks the stdin-reader thread until the
                // form invocation has completed.
                // NOTE: does not block on Mono, or did not on 1.1.15
                mainForm.Invoke(new MethodInvoker(fun () ->
                    try 
                        // When we get called back, someone may jack our culture
                        // So we must reset our UI culture every time
                        SetCurrentUICultureForThread lcid
                        mainFormInvokeResultHolder := Some(f ())
                    finally
                        doneSignal.Set() |> ignore
                    )) |> ignore

                if !progress then fsiConsoleOutput.Out.WriteLine "RunCodeOnWinFormsMainThread: Waiting for completion signal...."
                
                let oneSec = TimeSpan.FromSeconds 1.0
                while not <| doneSignal.WaitOne (oneSec, true) do
                    if !progress then fsiConsoleOutput.Out.Write "."
                    fsiConsoleOutput.Out.Flush()

                if !progress then fprintfn fsiConsoleOutput.Out "RunCodeOnWinFormsMainThread: Got completion signal, res = %b" (Option.isSome !mainFormInvokeResultHolder)
                !mainFormInvokeResultHolder |> Option.get

         member __.ScheduleRestart() =
            restart <- true
            Application.Exit()
