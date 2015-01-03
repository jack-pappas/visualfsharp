// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// Assembly resolution handling for F# interactive.
module internal Microsoft.FSharp.Compiler.Interactive.MagicAssemblyResolution

#nowarn "40"

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

(*
 Source: http://msdn.microsoft.com/en-us/library/ff527268.aspx

 What the Event Handler Does

 The handler for the AssemblyResolve event receives the display name of the assembly to 
 be loaded, in the ResolveEventArgs.Name property. If the handler does not recognize the 
 assembly name, it returns null (Nothing in Visual Basic, nullptr in Visual C++). 

 - If the handler recognizes the assembly name, it can load and return an assembly that 
   satisfies the request. The following list describes some sample scenarios. 

 - If the handler knows the location of a version of the assembly, it can load the assembly by 
   using the Assembly.LoadFrom or Assembly.LoadFile method, and can return the loaded assembly if successful. 

 - If the handler has access to a database of assemblies stored as byte arrays, it can load a byte array by 
   using one of the Assembly.Load method overloads that take a byte array. 

 - The handler can generate a dynamic assembly and return it.
 
 It is the responsibility of the event handler to return a suitable assembly. The handler can parse the display 
 name of the requested assembly by passing the ResolveEventArgs.Name property value to the AssemblyName(String) 
 constructor. Beginning with the .NET Framework version 4, the handler can use the ResolveEventArgs.RequestingAssembly 
 property to determine whether the current request is a dependency of another assembly. This information can help 
 identify an assembly that will satisfy the dependency.
 
 The event handler can return a different version of the assembly than the version that was requested. 
 
 In most cases, the assembly that is returned by the handler appears in the load context, regardless of the context 
 the handler loads it into. For example, if the handler uses the Assembly.LoadFrom method to load an assembly into 
 the load-from context, the assembly appears in the load context when the handler returns it. However, in the following 
 case the assembly appears without context when the handler returns it:
 
 - The handler loads an assembly without context.
 - The ResolveEventArgs.RequestingAssembly property is not null.
 - The requesting assembly (that is, the assembly that is returned by the ResolveEventArgs.RequestingAssembly property) 
   was loaded without context. 
 
 For information about contexts, see the Assembly.LoadFrom(String) method overload.
*)


// FxCop identifies Assembly.LoadFrom.
[<CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods", MessageId="System.Reflection.Assembly.UnsafeLoadFrom")>]
let private assemblyLoadFrom (path:string) =
// See bug 5501 for details on decision to use UnsafeLoadFrom here.
// Summary:
//  It is an explicit user trust decision to load an assembly with #r. Scripts are not run automatically (for example, by double-clicking in explorer).
//  We considered setting loadFromRemoteSources in fsi.exe.config but this would transitively confer unsafe loading to the code in the referenced 
//  assemblies. Better to let those assemblies decide for themselves which is safer.
    Assembly.UnsafeLoadFrom(path)

/// <summary>This method implements the resolution logic.</summary>
/// <remarks>
/// For testing purposes, this code is not protected by a try/with block to protect against
/// and suppress exceptions, which is the reason it is called "unsafe".
/// Aside from unit tests, only the <see cref="Resolve(TcConfigBuilder,TcImports,FsiDynamicCompiler,FsiConsoleOutput,ResolveEventArgs)"/>
/// should call this method.
/// </remarks>
[<CompiledName("ResolveUnsafe")>]
let resolveUnsafe tcConfigB (tcImports : TcImports) (fsiDynamicCompiler : FsiDynamicCompiler) (fsiConsoleOutput : FsiConsoleOutput) (rangeStdin : range) (args : ResolveEventArgs) =    
    // Grab the name of the assembly
    let tcConfig = TcConfig.Create(tcConfigB,validate=false)
    let fullAssemName = args.Name
    let simpleAssemName = fullAssemName.Split([| ',' |]).[0]          
    if !progress then
        // "Attempting to load a dynamically required assembly in response to an AssemblyResolve event by using known static assembly references..." 
        fsiConsoleOutput.uprintfn "ATTEMPT MAGIC LOAD ON ASSEMBLY, simpleAssemName = %s" simpleAssemName
               
    // Special case: Mono Windows Forms attempts to load an assembly called something like "Windows.Forms.resources"
    // We can't resolve this, so don't try.
    // REVIEW: Suggest 4481, delete this special case.
    if simpleAssemName.EndsWith(".resources",StringComparison.OrdinalIgnoreCase) || 
        // See F# 1.0 Product Studio bug 1171
        simpleAssemName.EndsWith(".XmlSerializers",StringComparison.OrdinalIgnoreCase) || 
        (runningOnMono && simpleAssemName = "UIAutomationWinforms") then null else

    // Special case: Is this the global unique dynamic assembly for FSI code? In this case just
    // return the dynamic assembly itself.
    if fsiDynamicCompiler.DynamicAssemblyName = simpleAssemName then fsiDynamicCompiler.DynamicAssembly else

    // Otherwise continue
    let assemblyReferenceTextDll = simpleAssemName + ".dll"
    let assemblyReferenceTextExe = simpleAssemName + ".exe"
    let overallSearchResult =           
        // OK, try to resolve as a .dll
        let searchResult = tcImports.TryResolveAssemblyReference (AssemblyReference(rangeStdin,assemblyReferenceTextDll),ResolveAssemblyReferenceMode.Speculative)

        match searchResult with
        | OkResult (warns,[r]) -> OkResult (warns, Choice1Of2 r.resolvedPath)
        | _ -> 

        // OK, try to resolve as a .exe
        let searchResult = tcImports.TryResolveAssemblyReference (AssemblyReference(rangeStdin,assemblyReferenceTextExe),ResolveAssemblyReferenceMode.Speculative)

        match searchResult with
        | OkResult (warns, [r]) -> OkResult (warns, Choice1Of2 r.resolvedPath)
        | _ -> 

        if !progress then fsiConsoleOutput.uprintfn "ATTEMPT LOAD, assemblyReferenceTextDll = %s" assemblyReferenceTextDll;
        /// Take a look through the files quoted, perhaps with explicit paths
        let searchResult = 
            tcConfig.referencedDLLs
                |> List.tryPick (fun assemblyReference -> 
                    if !progress then fsiConsoleOutput.uprintfn "ATTEMPT MAGIC LOAD ON FILE, referencedDLL = %s" assemblyReference.Text;
                    if System.String.Compare(Filename.fileNameOfPath assemblyReference.Text, assemblyReferenceTextDll,StringComparison.OrdinalIgnoreCase) = 0 ||
                        System.String.Compare(Filename.fileNameOfPath assemblyReference.Text, assemblyReferenceTextExe,StringComparison.OrdinalIgnoreCase) = 0 then
                            Some(tcImports.TryResolveAssemblyReference(assemblyReference,ResolveAssemblyReferenceMode.Speculative))
                    else None)

        match searchResult with
        | Some (OkResult (warns,[r])) -> OkResult (warns, Choice1Of2 r.resolvedPath)
        | _ -> 

        match tcImports.TryFindProviderGeneratedAssemblyByName(simpleAssemName) with
        | Some(assembly) -> OkResult([],Choice2Of2 assembly)
        | None -> 
                   
        // As a last resort, try to find the reference without an extension
        match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef(ILAssemblyRef.Create(simpleAssemName,None,None,false,None,None)) with
        | Some(resolvedPath) -> 
            OkResult([],Choice1Of2 resolvedPath)
        | None -> 
                   
        ErrorResult([],Failure (FSIstrings.SR.fsiFailedToResolveAssembly(simpleAssemName)))
                           
    match overallSearchResult with 
    | ErrorResult _ -> null
    | OkResult _ -> 
        let res = CommitOperationResult overallSearchResult
        match res with 
        | Choice1Of2 assemblyName -> 
            if simpleAssemName <> "Mono.Posix" then fsiConsoleOutput.uprintfn "%s" (FSIstrings.SR.fsiBindingSessionTo(assemblyName));
            assemblyLoadFrom assemblyName
        | Choice2Of2 assembly -> 
            assembly

/// Assembly resolution function.
[<CompiledName("Resolve")>]
let resolve tcConfigB (tcImports : TcImports) (fsiDynamicCompiler : FsiDynamicCompiler) (fsiConsoleOutput : FsiConsoleOutput) (rangeStdin : range) (args : ResolveEventArgs) =
    try
        // Call the unsafe resolution logic.
        // In the event of an exception, stop processing and return null
        // (indicating to the CLR that this handler could not resolve the requested assembly).
        resolveUnsafe tcConfigB tcImports fsiDynamicCompiler fsiConsoleOutput rangeStdin args
    with e ->
        stopProcessingRecovery e range0
        null

/// <summary>
/// Installs the magic resolver into the specified <see cref="AppDomain"/>.
/// </summary>
[<CompiledName("InstallInto")>]
let installInto (appDomain : AppDomain, tcConfigB, tcImports: TcImports, fsiDynamicCompiler: FsiDynamicCompiler, fsiConsoleOutput: FsiConsoleOutput) =
    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

    appDomain.add_AssemblyResolve(new ResolveEventHandler(fun _ args ->
        resolve tcConfigB tcImports fsiDynamicCompiler fsiConsoleOutput rangeStdin args))

/// <summary>
/// Installs the magic resolver into the current <see cref="AppDomain"/>.
/// </summary>
[<CompiledName("Install")>]
let install (tcConfigB, tcImports: TcImports, fsiDynamicCompiler: FsiDynamicCompiler, fsiConsoleOutput: FsiConsoleOutput) =
    installInto (AppDomain.CurrentDomain, tcConfigB, tcImports, fsiDynamicCompiler, fsiConsoleOutput)
