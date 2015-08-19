// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Internal.Utilities.Filename

open System.IO
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 

exception IllegalFileNameChar of string * char

/// The set of characters which may not be used in a path.
let illegalPathChars =
    let chars = Path.GetInvalidPathChars ()
    Array.sortInPlace chars
    chars

let checkPathForIllegalChars (path:string) =
    let len = path.Length
    for i = 0 to len - 1 do
        // The current character in the string.
        let c = path.[i]

        // Determine if this character is disallowed within a path by
        // attempting to find it in the array of illegal path characters.
        for badChar in illegalPathChars do
            if c = badChar then
                // The character is not allowed to be used within a path, raise an exception.
                raise(IllegalFileNameChar(path, c))

// Case sensitive (original behaviour preserved).
let checkSuffix (x:string) (y:string) = x.EndsWith(y,System.StringComparison.Ordinal) 

let hasExtension (s:string) = 
    let sLen = s.Length
    if sLen >= 1 && s.[sLen - 1] = '.' && s <> ".." && s <> "." then true
    else
        try Path.HasExtension(s)
        with
        | :? System.ArgumentException ->
            // Suppress the original exception and let 'checkPathForIllegalChars' raise a new one.
            checkPathForIllegalChars s
            // In case the exception was raised for some other reason.
            reraise ()

let chopExtension (s:string) =
    if s = "." then "" else // for OCaml compatibility
    if not (hasExtension s) then 
        raise (System.ArgumentException("chopExtension")) // message has to be precisely this, for OCaml compatibility, and no argument name can be set

    try Path.Combine (Path.GetDirectoryName s,Path.GetFileNameWithoutExtension(s))
    with
    | :? System.ArgumentException ->
        // Path.GetDirectoryName(string) can raise ArgumentException when the string is empty or whitespace.
        // Check for that condition and propagate the original exception if it's the case.
        if System.String.IsNullOrWhiteSpace s then reraise ()
        else
            // Suppress the original exception and let 'checkPathForIllegalChars' raise a new one.
            checkPathForIllegalChars s
            // In case the exception was raised for some other reason.
            reraise ()

let directoryName (s:string) = 
    if s.Length = 0 then "."
    else
        let dirName =
            try Path.GetDirectoryName(s)
            with
            | :? System.ArgumentException ->
                // Path.GetDirectoryName(string) can raise ArgumentException when the string is empty or whitespace.
                // Check for that condition and propagate the original exception if it's the case.
                if System.String.IsNullOrWhiteSpace s then reraise ()
                else
                    // Suppress the original exception and let 'checkPathForIllegalChars' raise a new one.
                    checkPathForIllegalChars s
                    // In case the exception was raised for some other reason.
                    reraise ()

        match dirName with
        | null -> if FileSystem.IsPathRootedShim(s) then s else "."
        | res -> if res.Length = 0 then "." else res

let fileNameOfPath s = 
    // This is implemented in such a way that the original behavior is preserved
    // (where 'checkPathForIllegalChars' used to be called first to validate the input).
    // This implementation avoids that call unless necessary, so is more efficient.
    try Path.GetFileName(s)
    with
    | :? System.ArgumentException ->
        // Suppress the original exception and let 'checkPathForIllegalChars' raise a new one.
        checkPathForIllegalChars s
        // In case the exception was raised for some other reason.
        reraise ()

let fileNameWithoutExtension s = 
    try Path.GetFileNameWithoutExtension(s)
    with
    | :? System.ArgumentException ->
        // Suppress the original exception and let 'checkPathForIllegalChars' raise a new one.
        checkPathForIllegalChars s
        // In case the exception was raised for some other reason.
        reraise ()
