// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp

open System
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


[<assembly: AssemblyDescription("fsi.exe")>]
[<assembly: AssemblyCompany("Microsoft Corporation")>]
[<assembly: AssemblyTitle("fsi.exe")>]
[<assembly: AssemblyCopyright("\169 Microsoft Corporation.  Apache 2.0 License.")>]
[<assembly: AssemblyProduct("Microsoft\174 F#")>]
[<assembly: ComVisible(false)>]
[<assembly: CLSCompliant(true)>]

//----------------------------------------------------------------------------
// Hardbinding dependencies should we NGEN fsi.exe
//----------------------------------------------------------------------------
[<assembly: Dependency("FSharp.Compiler", LoadHint.Always)>]
[<assembly: Dependency("FSharp.Core", LoadHint.Always)>]

do()
