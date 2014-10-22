// Copyright (c) Microsoft Open Technologies, Inc.
// All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

// Various tests for:
// Microsoft.FSharp.Core.NativeInterop

namespace FSharp.Core.Unittests.FSharp_Core.Microsoft_FSharp_Core

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open NUnit.Framework

#nowarn "9"

[<TestFixture>]
module NativePtr =
    [<Test>]
    let ``zero returns null pointer`` () : unit =
        Assert.AreEqual (0n, NativePtr.toNativeInt NativePtr.zero<char>,
            "NativePtr.zero returned a non-zero pointer value.")
            
    [<Test>]
    let ``isNull is true for zero pointer`` () : unit =
        let ptr = NativePtr.zero<char>
        Assert.IsTrue (NativePtr.isNull ptr)
        
    [<Test>]
    let ``isNull is false for nonzero pointer`` () : unit =
        let ptr = NativePtr.add NativePtr.zero<char> 1
        Assert.IsFalse (NativePtr.isNull ptr)
        
    [<Test>]
    let ``init clears existing value`` () : unit =
        let ptr = NativePtr.stackalloc<DateTime> 1
        
        // Write a DateTime value into the stack-allocated memory location.
        DateTime.Now
        |> NativePtr.write ptr
        
        // Call 'init' on the same memory location.
        NativePtr.init ptr
        
        // Read the DateTime value from memory -- it should be equal to Unchecked.defaultof<DateTime> now.
        Assert.AreEqual (Unchecked.defaultof<DateTime>, NativePtr.read ptr)
        
    [<Test>]
    let ``copy simple`` () : unit =
        let mutable origTimeSpan = TimeSpan.FromDays Math.PI
        let copyTimeSpanPtr = NativePtr.stackalloc<TimeSpan> 1
        
        // Copy the TimeSpan value.
        NativePtr.copy copyTimeSpanPtr &&origTimeSpan
        
        // Check that the copied value matches the original.
        Assert.AreEqual (origTimeSpan, NativePtr.get copyTimeSpanPtr)
        
    [<Test>]
    let ``copyBlock simple`` () : unit =
        let data = [| 123L; 456L; 7L; 999L; -1L; 0L; 6472L |]
        
        // Allocate some memory to hold copies of the data values.
        let copyDataPtr =
            data
            |> Array.length
            |> NativePtr.stackalloc<int64>
            
        let handle = GCHandle.Alloc (data, GCHandleType.Pinned)
        try
            let origDataPtr =
                handle.AddrOfPinnedObject ()
                |> NativePtr.ofNativeInt<int64>
                
            // Copy the data values.
            NativePtr.copyBlock copyDataPtr origDataPtr (uint32 <| Array.length data)
        finally
            if handle.IsAllocated then
                handle.Free ()
                
        // Check that the copied data values match the originals.
        for i = 0 to data.Length - 1 do
            Assert.AreEqual (data.[i], NativePtr.get copyDataPtr i)
            
    [<Test>]
    let ``fill simple`` () : unit =
        let byteCount = 100
        
        // Allocate some bytes on the stack.
        let bytesPtr = NativePtr.stackalloc<byte> byteCount
        
        // The bytes should all initially be cleared.
        // Double-check though -- after all, this is a test!
        for i = 0 to byteCount - 1 do
            Assert.AreEqual (0uy, NativePtr.get bytesPtr i)
            
        // Fill some span of bytes within the allocated space.
        // The span shouldn't touch the beginning or end of the space.
        let fillByte = 10uy
        let spanStart = 14
        let spanLength = 55u
        let spanPtr =
            spanStart
            |> NativePtr.add bytesPtr
            |> NativePtr.toNativeInt
        NativePtr.fill spanPtr fillByte spanLength
        
        // Make sure the bytes in the span were filled and the others were left untouched.
        for i = 0 to spanStart - 1 do
            Assert.AreEqual (0uy, NativePtr.get bytesPtr i)
        
        for i = spanStart to (spanStart + int spanLength - 1) do
            Assert.AreEqual (fillByte, NativePtr.get bytesPtr i)
            
        for i = spanStart + int spanLength to byteCount - 1 do
            Assert.AreEqual (0uy, NativePtr.get bytesPtr i)
