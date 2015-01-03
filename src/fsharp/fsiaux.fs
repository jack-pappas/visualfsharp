// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.Interactive

open System
open System.Diagnostics
open System.Threading

[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: System.CLSCompliant(true)>]  
do()

type IEventLoop =
    abstract Run : unit -> bool
    abstract Invoke : (unit -> 'T) -> 'T 
    abstract ScheduleRestart : unit -> unit
    
// An implementation of IEventLoop suitable for the command-line console
[<Sealed; AutoSerializable(false)>]
type internal SimpleEventLoop () =
    let runSignal = new AutoResetEvent(false)
    let exitSignal = new AutoResetEvent(false)
    let doneSignal = new AutoResetEvent(false)

    let mutable queue : (unit -> obj) list = []
    let mutable result : obj option = None
    let mutable running = false
    let mutable restart = false

    static let setSignal (signal : AutoResetEvent) =
        while not <| signal.Set () do
            Thread.Sleep 1

    static let waitSignal (signal : #WaitHandle) =
        signal.WaitOne () |> ignore

    static let waitSignal2 (signal1 : #WaitHandle) (signal2 : #WaitHandle) =
        WaitHandle.WaitAny [| signal1; signal2 |]

    interface IEventLoop with
        member __.Run () =
            running <- true
            let rec run () =
                match waitSignal2 runSignal exitSignal with
                | 0 ->
                    for f in queue do
                       result <-
                           try Some <| f ()
                           with _ -> None
                     
                    setSignal doneSignal
                    run ()
                | 1 ->
                    running <- false
                    restart
                | _ ->
                    run ()

            run ()

        member __.Invoke (f : unit -> 'T) : 'T  =
            queue <- [f >> box]
            setSignal runSignal
            waitSignal doneSignal
            result |> Option.get |> unbox

        member __.ScheduleRestart() =
            // nb. very minor race condition here on 'running' here, but totally
            // unproblematic as ScheduleRestart and Exit are almost never called.
            if running then
                restart <- true
                setSignal exitSignal

    interface System.IDisposable with
        member __.Dispose () =
            runSignal.Close ()
            exitSignal.Close ()
            doneSignal.Close ()


[<Sealed>]
type InteractiveSession () =
    let mutable evLoop =
        new SimpleEventLoop() :> IEventLoop

    member val AddedPrinters = [] with get, set

    [<CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")>]
    member val CommandLineArgs =
        System.Environment.GetCommandLineArgs ()
        with get, set

    member __.EventLoop
        with get () = evLoop
        and set (newLoop : IEventLoop) =
            evLoop.ScheduleRestart ()
            evLoop <- newLoop

    member val FormatProvider =
        (System.Globalization.CultureInfo.InvariantCulture :> System.IFormatProvider)
        with get, set

    member val FloatingPointFormat = "g10" with get, set
    member val PrintWidth = 78 with get, set
    member val PrintDepth = 100 with get, set
    member val PrintLength = 100 with get, set
    member val PrintSize = 10000 with get, set

    member val ShowDeclarationValues = true with get, set
    member val ShowProperties = true with get, set
    member val ShowIEnumerable = true with get, set
    member val ShowIDictionary = true with get, set

    member self.AddPrinter (printer : 'T -> string) =
        self.AddedPrinters <-
            Choice1Of2 (typeof<'T>, unbox >> printer) :: self.AddedPrinters

    member self.AddPrintTransformer (printer : 'T -> obj) =
        self.AddedPrinters <-
            Choice2Of2 (typeof<'T>, unbox >> printer) :: self.AddedPrinters
    
[<assembly: CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1009:DeclareEventHandlersCorrectly", Scope="member", Target="Microsoft.FSharp.Compiler.Interactive.InteractiveSession.#ThreadException")>]
do()
  
  
module Settings = 
    let fsi = new InteractiveSession()
   
    [<assembly: AutoOpen("Microsoft.FSharp.Compiler.Interactive.Settings")>]
    do()

module RuntimeHelpers = 
    open System
    open System.Reflection

    let internal savedIt = ref (typeof<int>,box 0)
    let SaveIt (x:'T) = (savedIt := (typeof<'T>, box x))
    let internal GetSavedIt () = snd !savedIt
    let internal GetSavedItType () = fst !savedIt