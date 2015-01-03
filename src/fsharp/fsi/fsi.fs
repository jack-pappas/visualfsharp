// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.Interactive.Shell

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

open Internal.Utilities.Collections
open Internal.Utilities.StructuredFormat
open Internal.Utilities.FileSystem

open Microsoft.FSharp.Compiler.Interactive.UserInterface
open Microsoft.FSharp.Compiler.Interactive.Server


// Mark the main thread as STAThread since it is a GUI thread.
[<EntryPoint>]
[<STAThread>]
[<LoaderOptimization(LoaderOptimization.MultiDomainHost)>]
let MainMain argv =
    ignore argv
    let argv = System.Environment.GetCommandLineArgs()

    let evaluateSession () =
        // When VFSI is running, set the input/output encoding to UTF8.
        // Otherwise, unicode gets lost during redirection.
        // It is required only under Net4.5 or above (with unicode console feature).
        if FSharpEnvironment.IsRunningOnNetFx45OrAbove &&
            argv |> Array.exists (fun x -> x.Contains "fsi-server") then
            Console.InputEncoding <- System.Text.Encoding.UTF8
            Console.OutputEncoding <- System.Text.Encoding.UTF8

#if DEBUG
        if argv |> Array.exists  (fun x -> x = "/pause" || x = "--pause") then
            Console.WriteLine("Press any key to continue...")
            Console.ReadKey() |> ignore
    
        try
            let fsi = FsiEvaluationSession (argv, Console.In, Console.Out, Console.Error)
            fsi.Run()
        with e ->
            printf "Exception by fsi.exe:\n%+A\n" e
#else
        let fsi = FsiEvaluationSession (argv, Console.In, Console.Out, Console.Error)
        fsi.Run()
#endif

    let isShadowCopy x = (x = "/shadowcopyreferences" || x = "--shadowcopyreferences" || x = "/shadowcopyreferences+" || x = "--shadowcopyreferences+")
    if AppDomain.CurrentDomain.IsDefaultAppDomain() && argv |> Array.exists isShadowCopy then
        let setupInformation = AppDomain.CurrentDomain.SetupInformation
        setupInformation.ShadowCopyFiles <- "true"
        let helper = AppDomain.CreateDomain("FSI_Domain", null, setupInformation)
        helper.ExecuteAssemblyByName(Assembly.GetExecutingAssembly().GetName()) |> ignore
    else
        evaluateSession()
    0
