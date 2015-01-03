// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.Interactive.Utilities

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

open Internal.Utilities.Collections
open Internal.Utilities.StructuredFormat
open Internal.Utilities.FileSystem

type IAnyToLayoutCall =
    abstract AnyToLayout : FormatOptions * obj -> Internal.Utilities.StructuredFormat.Layout
    abstract FsiAnyToLayout : FormatOptions * obj -> Internal.Utilities.StructuredFormat.Layout

type private AnyToLayoutSpecialization<'T>() =
    interface IAnyToLayoutCall with
        member this.AnyToLayout(options, o : obj) =
            Internal.Utilities.StructuredFormat.Display.any_to_layout options (Unchecked.unbox o : 'T)
        member this.FsiAnyToLayout(options, o : obj) =
            Internal.Utilities.StructuredFormat.Display.fsi_any_to_layout options (Unchecked.unbox o : 'T)
    
let getAnyToLayoutCall ty =
    let specialized = typedefof<AnyToLayoutSpecialization<_>>.MakeGenericType [| ty |]
    Activator.CreateInstance(specialized) :?> IAnyToLayoutCall

let callStaticMethod (ty:Type) name args =
    let allStaticMethodsBindingFlags =
        BindingFlags.InvokeMethod
        ||| BindingFlags.Static
        ||| BindingFlags.Public
        ||| BindingFlags.NonPublic
    ty.InvokeMember(name, allStaticMethodsBindingFlags, null, null, Array.ofList args, CultureInfo.InvariantCulture)

let ignoreAllErrors f = try f() with _ -> ()

/// Get the directory name from a string, with some defaults if it doesn't have one
let internal directoryName (s:string) =
    if s = "" then "."
    else
        match Path.GetDirectoryName s with
        | null -> if Path.IsPathRooted s then s else "."
        | res -> if res = "" then "." else res


//----------------------------------------------------------------------------
// Timing support
//----------------------------------------------------------------------------

[<Sealed; AutoSerializable(false)>]
type internal FsiTimeReporter(outWriter: TextWriter) =
    static let currentProcess = System.Diagnostics.Process.GetCurrentProcess()
    static let numGC = System.GC.MaxGeneration
    let stopwatch = new System.Diagnostics.Stopwatch()

    static member private FormatTimeSpan (ts : TimeSpan) =
        sprintf "%02d:%02d:%02d.%03d" (int ts.TotalHours) ts.Minutes ts.Seconds ts.Milliseconds

    static member private GetCollectionCounts () =
        Array.init numGC System.GC.CollectionCount

    member __.TimeOp f =
        let startTotal = currentProcess.TotalProcessorTime
        let startGC = FsiTimeReporter.GetCollectionCounts ()
        stopwatch.Reset()
        stopwatch.Start()
        let res = f ()
        stopwatch.Stop()
        let total = currentProcess.TotalProcessorTime - startTotal
        let gcInfoStr =
            let spanGC =
                let endGC = FsiTimeReporter.GetCollectionCounts ()
                Array.map2 (-) endGC startGC
            
            spanGC
            |> Array.mapi (sprintf "%s%d: %d" (FSIstrings.SR.fsiTimeInfoGCGenerationLabelSomeShorthandForTheWordGeneration()))
            |> String.concat ", "
        outWriter.Write (FSIstrings.SR.fsiTimeInfoMainString(FsiTimeReporter.FormatTimeSpan stopwatch.Elapsed, FsiTimeReporter.FormatTimeSpan total, gcInfoStr))
        res

    member tr.TimeOpIf flag f = if flag then tr.TimeOp f else f ()
