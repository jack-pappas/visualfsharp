// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.NativeInterop

    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Collections

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    /// <summary>Contains operations on native pointers. Use of these operators may
    /// result in the generation of unverifiable code.</summary>
    module NativePtr =
        /// <summary>The null typed native pointer.</summary>
        [<GeneralizableValue>]
        [<NoDynamicInvocation>]
        [<CompiledName("Zero")>]
        val inline zero<'T when 'T : unmanaged> : nativeptr<'T>

        /// <summary>Determines if a typed native pointer is null.</summary>
        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("IsNull")>]
        val inline isNull : address:nativeptr<'T> -> bool

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("OfNativeIntInlined")>]
        /// <summary>Returns a typed native pointer for a given machine address.</summary>
        /// <param name="address">The pointer address.</param>
        /// <returns>A typed pointer.</returns>
        val inline ofNativeInt : address:nativeint -> nativeptr<'T>

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("ToNativeIntInlined")>]
        /// <summary>Returns a machine address for a given typed native pointer.</summary>
        /// <param name="address">The input pointer.</param>
        /// <returns>The machine address.</returns>
        val inline toNativeInt : address:nativeptr<'T> -> nativeint

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("AddPointerInlined")>]
        /// <summary>Returns a typed native pointer by adding index * sizeof&lt;'T&gt; to the 
        /// given input pointer.</summary>
        /// <param name="address">The input pointer.</param>
        /// <param name="index">The index by which to offset the pointer.</param>
        /// <returns>A typed pointer.</returns>
        val inline add : address:nativeptr<'T> -> index:int -> nativeptr<'T>

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("GetPointerInlined")>]
        /// <summary>Dereferences the typed native pointer computed by adding index * sizeof&lt;'T&gt; to the 
        /// given input pointer.</summary>
        /// <param name="address">The input pointer.</param>
        /// <param name="index">The index by which to offset the pointer.</param>
        /// <returns>The value at the pointer address.</returns>
        val inline get : address:nativeptr<'T> -> index:int -> 'T

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("ReadPointerInlined")>]
        /// <summary>Dereferences the given typed native pointer.</summary>
        /// <param name="address">The input pointer.</param>
        /// <returns>The value at the pointer address.</returns>
        val inline read : address:nativeptr<'T> -> 'T

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("WritePointerInlined")>]
        /// <summary>Assigns the <c>value</c> into the memory location referenced by the given typed native pointer.</summary>
        /// <param name="address">The input pointer.</param>
        /// <param name="value">The value to assign.</param>
        val inline write : address:nativeptr<'T> -> value:'T -> unit

        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("SetPointerInlined")>]
        /// <summary>Assigns the <c>value</c> into the memory location referenced by the typed native 
        /// pointer computed by adding index * sizeof&lt;'T&gt; to the given input pointer.</summary>
        /// <param name="address">The input pointer.</param>
        /// <param name="index">The index by which to offset the pointer.</param>
        /// <param name="value">The value to assign.</param>
        val inline set : address:nativeptr<'T> -> index:int -> value:'T -> unit

        /// <summary>Allocates a region of memory on the stack.</summary>
        /// <param name="count">The number of objects of type T to allocate.</param>
        /// <returns>A typed pointer to the allocated memory.</returns>
        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("StackAllocate")>]
        val inline stackalloc : count:int -> nativeptr<'T>

        /// <summary>Initializes the memory location referenced by the typed native pointer
        /// <paramref name="address"/> with the default value of type 'T.</summary>
        /// <param name="address">The input pointer.</param>
        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("InitializePointerInlined")>]
        val inline init : address:nativeptr<'T> -> unit
        
        /// <summary>Copies a value from the memory location <paramref name="srcAddress"/>
        /// to the memory location specified by <paramref name="dstAddress"/>.</summary>
        /// <param name="dstAddress">The destination pointer.</param>
        /// <param name="srcAddress">The source pointer.</param>
        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("CopyPointerInlined")>]
        val inline copy : dstAddress:nativeptr<'T> -> srcAddress:nativeptr<'T> -> unit

        /// <summary>Copies a block of values starting at the memory location <paramref name="srcAddress"/>
        /// to the memory location <paramref name="dstAddress"/>.</summary>
        /// <param name="dstAddress">The destination pointer.</param>
        /// <param name="srcAddress">The source pointer.</param>
        /// <param name="count">The number of values of type T to copy.</param>
        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("CopyBlockInlined")>]
        val inline copyBlock : dstAddress:nativeptr<'T> -> srcAddress:nativeptr<'T> -> count:uint32 -> unit

        /// <summary>Fills a contiguous block of bytes with a specified value, beginning at the
        /// memory location referenced by the untyped native pointer <paramref name="address"/>.</summary>
        /// <param name="address">The input pointer.</param>
        /// <param name="value">The value to assign.</param>
        /// <param name="size">The number of bytes to fill with <paramref name="value"/>.</param>
        [<Unverifiable>]
        [<NoDynamicInvocation>]
        [<CompiledName("FillPointerInlined")>]
        val inline fill : address:nativeint -> value:byte -> size:uint32 -> unit