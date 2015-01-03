// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.Interactive

open System
open System.Collections.Generic
open System.Text
open Internal.Utilities

/// System.Console.ReadKey appears to return an ANSI character (not the expected the unicode character).
/// When this fix flag is true, this byte is converted to a char using the System.Console.InputEncoding.
/// This is a code-around for bug://1345.
/// Fixes to System.Console.ReadKey may break this code around, hence the option here.
module internal ConsoleOptions =

  // Bug 4254 was fixed in Dev11 (Net4.5), so this flag tracks making this fix up version specific.
  let fixupRequired = not FSharpEnvironment.IsRunningOnNetFx45OrAbove
   
  let fixNonUnicodeSystemConsoleReadKey = ref fixupRequired
  let readKeyFixup (c:char) =
    if !fixNonUnicodeSystemConsoleReadKey then
      // Assumes the c:char is actually a byte in the System.Console.InputEncoding.
      // Convert it to a Unicode char through the encoding.
      if 0 <= int c && int c <= 255 then
        let chars = System.Console.InputEncoding.GetChars [| byte c |]
        if chars.Length = 1 then
          chars.[0] // fixed up char
        else
          assert("readKeyFixHook: InputEncoding.GetChars(single-byte) returned multiple chars" = "")
          c // no fix up
      else
        assert("readKeyFixHook: given char is outside the 0..255 byte range" = "")
        c // no fix up
    else
      c


module internal Utils = 

    open System
    open System.Reflection
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Collections

    let guard(f) = 
        try f() 
        with e -> 
             Microsoft.FSharp.Compiler.ErrorLogger.warning(Failure(sprintf "Note: an unexpected exception in fsi.exe readline console support. Consider starting fsi.exe with the --no-readline option and report the stack trace below to the .NET or Mono implementors\n%s\n%s\n" e.Message e.StackTrace));

    // Quick and dirty dirty method lookup for inlined IL
    // In some situations, we can't use ldtoken to obtain a RuntimeMethodHandle, since the method
    // in question's token may contain typars from an external type environment.  Such a token would
    // cause the PE file to be flagged as invalid.
    // In such a situation, we'll want to search out the MethodRef in a similar fashion to bindMethodBySearch
    // but since we can't use ldtoken to obtain System.Type objects, we'll need to do everything with strings.
    // This is the least fool-proof method for resolving the binding, but since the scenarios it's used in are
    // so constrained, (fsi 2.0, methods with generic multi-dimensional arrays in their signatures), it's 
    // acceptable
    let findMethod (parentT:Type,nm,marity,argtys : string [],rty : string) =
        let staticOrInstanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
        let methInfos = parentT.GetMethods(staticOrInstanceBindingFlags) |> Array.toList 

        let methInfos = methInfos |> List.filter (fun methInfo -> methInfo.Name = nm)
        match methInfos with 
        | [methInfo] -> 
            methInfo
        | _ -> 
            let select (methInfo:MethodInfo) =
                let mtyargTIs = if methInfo.IsGenericMethod then methInfo.GetGenericArguments() else [| |] 
                if mtyargTIs.Length  <> marity then false else

                let haveArgTs = 
                    let parameters = Array.toList (methInfo.GetParameters()) 
                    parameters |> List.map (fun param -> param.ParameterType) 
                let haveResT  = methInfo.ReturnType 

                if argtys.Length <> haveArgTs.Length then false else
                let res = rty :: (Array.toList argtys) = (List.map (fun (t : System.Type) -> t.Name) (haveResT::haveArgTs))
                res

            match List.tryFind select methInfos with
            | None          -> failwith "Internal Error: cannot bind to method"
            | Some methInfo -> methInfo        


type internal Style = Prompt | Out | Error

/// Class managing the command History.
type internal History() =
    let list = new List<string>()
    let mutable current  = 0 

    member __.Count = list.Count
    member __.Current = 
        if current >= 0 && current < list.Count then list.[current] else String.Empty

    member __.Clear () =
        list.Clear ()
        current <- -1

    member x.Add line =
        match line with
        | null | "" -> ()
        | _ -> list.Add(line)

    member x.AddLast line = 
        match line with
        | null | "" -> ()
        | _ ->
            list.Add(line)
            current <- list.Count

    member x.Previous() = 
        if (list.Count > 0) then
            current <- ((current - 1) + list.Count) % list.Count
        x.Current

    member x.Next() = 
        if (list.Count > 0) then
            current <- (current + 1) % list.Count
        x.Current

/// List of available options.
[<Sealed>]
type internal Options() =
    inherit History()
    member val Root = "" with get, set

/// Console cursor position management.
[<Sealed>]
type internal Cursor =
    static member ResetTo(top,left) =
        Utils.guard <| fun () -> 
           Console.CursorTop <- min top (Console.BufferHeight - 1)
           Console.CursorLeft <- left

    static member Move(inset, delta) =
        let position = Console.CursorTop * (Console.BufferWidth - inset) + (Console.CursorLeft - inset) + delta
        let top  = position / (Console.BufferWidth - inset)
        let left = inset + position % (Console.BufferWidth - inset)
        Cursor.ResetTo(top,left)
    
type internal Anchor =
    { top : int; left : int; }

    static member Current (inset) =
        { top = Console.CursorTop;
          left = max inset Console.CursorLeft; }

    member p.PlaceAt(inset, index) =
        //printf "p.top = %d, p.left = %d, inset = %d, index = %d\n" p.top p.left inset index;
        let left = inset + (( (p.left - inset) + index) % (Console.BufferWidth - inset))
        let top = p.top + ( (p.left - inset) + index) / (Console.BufferWidth - inset)
        Cursor.ResetTo(top,left)

/// <summary>
/// Encapsulates state information for the <see cref="ReadLineConsole.ReadLine()"/> method
/// and helper methods it depends on.
/// </summary>
type internal ReadLineState = {
    /// Cursor anchor - position of anchor when the routine was called.
    Anchor : Anchor ref;
    /// Length of the output currently rendered on screen.
    Rendered : int ref;
    /// Input has changed, therefore options cache is invalidated.
    Changed : bool ref;
    /// Cache of options.
    OptionsCache : Options ref;
    /// The console input buffer.
    Input : StringBuilder;
    /// Current position - index into the input buffer.
    Current : int ref;
} with
    /// Creates a new ReadLineState record.
    static member Create inset =
      { Anchor = ref (Anchor.Current inset);
        Rendered = ref 0;
        Changed = ref false;
        OptionsCache = ref (new Options());
        Input = new StringBuilder ();
        Current = ref 0; }
    
[<Sealed>]
type internal ReadLineConsole() =
    let history = new History()
    let mutable complete : (string option * string -> seq<string>) = fun (_s1,_s2) -> Seq.empty
    member __.SetCompletionFunction f = complete <- f

    member __.Prompt = "> "
    member __.Prompt2 = "- "
    /// Inset all inputs by this amount
    member private x.Inset = x.Prompt.Length
    
    member x.GetOptions(input:string) =
        /// Tab options available in current context
        let optionsCache = new Options()

        let rec look parenCount i =
            if i <= 0 then i else
            match input.Chars(i - 1) with
            | c when Char.IsLetterOrDigit(c) (* or Char.IsWhiteSpace(c) *) -> look parenCount (i-1)
            | '.' | '_' -> look parenCount (i-1)
            | '}' | ')' | ']' -> look (parenCount+1) (i-1)
            | '(' | '{' | '[' -> look (parenCount-1) (i-1)
            | _ when parenCount > 0 -> look parenCount (i-1)
            | _ -> i
        let start = look 0 input.Length

        let name = input.Substring(start, input.Length - start)
        // REVIEW : Can we use String.IsNullOrWhiteSpace here instead of calling Trim() on 'name'?
        if name.Trim().Length = 0 then
            optionsCache, false
        else
            let lastDot = name.LastIndexOf '.'
            let attr, pref, root =
                if lastDot < 0 then
                    None, name, input.Substring(0, start)
                else
                    Some(name.Substring(0, lastDot)),
                    name.Substring(lastDot + 1),
                    input.Substring(0, start + lastDot + 1)
            //printf "attr, pref, root = %s\n" (any_to_string (attr, pref, root)) 
            try
                complete(attr,pref)
                |> Seq.filter(fun option -> option.StartsWith(pref,StringComparison.Ordinal))
                |> Seq.iter (fun option -> optionsCache.Add option)
                 // engine.Evaluate(String.Format("dir({0})", attr)) as IEnumerable;
                optionsCache.Root <- root
            with e ->
                optionsCache.Clear ()
            optionsCache, true

    /// Maps control characters to a printable string for display.
    static member private MapCharacter c : string =
        assert (Char.IsControl c)
        match c with
        | '\x1A'-> "^Z"
        | _ -> "^?"

    static member private GetCharacterSize c =
        if Char.IsControl c then
            ReadLineConsole.MapCharacter(c).Length
        else 1

    static member TabSize = 4

    member private x.CheckLeftEdge prompt =
        let currLeft = Console.CursorLeft
        if currLeft < x.Inset then
            if currLeft = 0 then
                Console.Write (if prompt then x.Prompt2 else String(' ', x.Inset))
            Utils.guard <| fun () ->
                Console.CursorTop <- min Console.CursorTop (Console.BufferHeight - 1)
                Console.CursorLeft <- x.Inset

    member private x.WriteBlank () =
        Console.Write ' '
        x.CheckLeftEdge false

    member private x.WriteChar c state =
        if Console.CursorTop = Console.BufferHeight - 1 && Console.CursorLeft = Console.BufferWidth - 1 then
            //printf "bottom right!\n";
            state.Anchor := { !state.Anchor with top = (!state.Anchor).top - 1 }
        x.CheckLeftEdge true
        if Char.IsControl c then
            let s = ReadLineConsole.MapCharacter c
            Console.Write s
            state.Rendered := !state.Rendered + s.Length
        else
            Console.Write c
            incr state.Rendered
        x.CheckLeftEdge true

    member private x.Render state =
        //printf "render\n";
        let curr = !state.Current
        (!state.Anchor).PlaceAt(x.Inset, 0)
        
        let input = state.Input
        // Initialize the output StringBuilder's capacity to the input length,
        // plus a small additional amount. 'input' usually contains few (or zero)
        // control characters, and initializing the output capacity avoids resizes.
        let output = StringBuilder (input.Length + 10)
        let mutable position = -1
        
        for i = 0 to input.Length - 1 do
            if i = curr then
                position <- output.Length
            let c = input.[i]
            if Char.IsControl c then
                ReadLineConsole.MapCharacter c
                |> output.Append
                |> ignore
            else 
                output.Append c |> ignore

        if curr = input.Length then
            position <- output.Length

        // render the current text, computing a new value for "rendered"
        let old_rendered = !state.Rendered
        state.Rendered := 0
        for i = 0 to input.Length - 1 do
            x.WriteChar input.[i] state

        // blank out any dangling old text
        for i = !state.Rendered to old_rendered - 1 do
            x.WriteBlank()

        (!state.Anchor).PlaceAt(x.Inset, position)

    member private x.InsertChar (c : char) state =
        let current = state.Current
        let input = state.Input

        if !current = input.Length then
            incr current
            input.Append c |> ignore
            x.WriteChar c state
        else
            input.Insert (!current, c) |> ignore
            incr current
            x.Render state

    member private x.InsertTab state =
        for i = ReadLineConsole.TabSize - (!state.Current % ReadLineConsole.TabSize) downto 1 do
            x.InsertChar ' ' state

    member private x.MoveLeft state =
        let current = state.Current
        let input = state.Input
        if !current > 0 && !current - 1 < input.Length then
            decr current
            let c = input.[!current]
            Cursor.Move(x.Inset, - ReadLineConsole.GetCharacterSize c)

    member private x.MoveRight state =
        let current = state.Current
        let input = state.Input
        if !current < input.Length then
            let c = input.[!current]
            incr current
            Cursor.Move(x.Inset, ReadLineConsole.GetCharacterSize c)

    member private x.SetInput (line : string) state =
        let current = state.Current
        let input = state.Input
        input.Length <- 0
        input.Append line |> ignore
        current := input.Length
        x.Render state

    member private x.TabPress shift state =
        let input = state.Input
        let opts, prefix =
            if !state.Changed then
                state.Changed := false
                x.GetOptions(input.ToString())
            else
                !state.OptionsCache, false
        state.OptionsCache := opts
            
        if opts.Count > 0 then
            let part =
                if shift
                then opts.Previous() 
                else opts.Next()
            x.SetInput (opts.Root + part) state
        else
            if prefix then
                Console.Beep ()
            else
                x.InsertTab state

    member private x.Delete state =
        let current = state.Current
        let input = state.Input
        if input.Length > 0 && !current < input.Length then
            input.Remove(!current, 1) |> ignore
            x.Render state

    member private x.Insert (key : ConsoleKeyInfo) state =
        // REVIEW: is this F6 rewrite required? 0x1A looks like Ctrl-Z.
        // REVIEW: the Ctrl-Z code is not recognised as EOF by the lexer.
        // REVIEW: looks like a relic of the port of readline, which is currently removable.
        let c =
            let c = if key.Key = ConsoleKey.F6 then '\x1A' else key.KeyChar
            ConsoleOptions.readKeyFixup c
        x.InsertChar c state

    member private x.Backspace state =
        let current = state.Current
        let input = state.Input
        if input.Length > 0 && !current > 0 then
            input.Remove(!current - 1, 1) |> ignore
            decr current
            x.Render state

    member private x.Enter state =
        let input = state.Input
        // REVIEW: Why not use Console.WriteLine() here instead of Console.Write()?
        Console.Write '\n'
        let line = input.ToString()
        if line = "\x1A" then null
        else 
            if line.Length > 0 then 
                history.AddLast line
            line

    member private x.Read state =
        let key = Console.ReadKey true
        match key.Key with
        | ConsoleKey.Backspace ->
            x.Backspace state
            x.Change state
        | ConsoleKey.Delete ->
            x.Delete state
            x.Change state
        | ConsoleKey.Enter ->
            x.Enter state
        | ConsoleKey.Tab ->
            x.TabPress (key.Modifiers &&& ConsoleModifiers.Shift <> enum 0) state
            x.Read state
        | ConsoleKey.UpArrow ->
            x.SetInput (history.Previous()) state
            x.Change state
        | ConsoleKey.DownArrow ->
            x.SetInput (history.Next()) state
            x.Change state
        | ConsoleKey.RightArrow ->
            x.MoveRight state
            x.Change state
        | ConsoleKey.LeftArrow ->
            x.MoveLeft state
            x.Change state
        | ConsoleKey.Escape ->
            x.SetInput String.Empty state
            x.Change state
        | ConsoleKey.Home ->
            state.Current := 0
            (!state.Anchor).PlaceAt(x.Inset, 0)
            x.Change state
        | ConsoleKey.End ->
            state.Current := state.Input.Length
            (!state.Anchor).PlaceAt(x.Inset, !state.Rendered)
            x.Change state
        | _ ->
            // Note: If KeyChar=0, the not a proper char, e.g. it could be part of a multi key-press character,
            //       e.g. e-acute is ' and e with the French (Belgium) IME and US Intl KB.
            // Here: skip KeyChar=0 (except for F6 which maps to 0x1A (ctrl-Z?)).
            if key.KeyChar <> '\000' || key.Key = ConsoleKey.F6 then
                x.Insert key state
                x.Change state
            else
                // Skip and read again.
                x.Read state

    member private x.Change state =
        state.Changed := true
        x.Read state

    member x.ReadLine() =
        // The caller writes the primary prompt.  If we are reading the 2nd and subsequent lines of the
        // input we're responsible for writing the secondary prompt.
        x.CheckLeftEdge true

        /// State information for this method and helper methods.
        let state = ReadLineState.Create x.Inset

        x.Read state
