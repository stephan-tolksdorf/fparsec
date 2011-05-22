// Copyright (c) Stephan Tolksdorf 2009-2011
// License: Simplified BSD License. See accompanying documentation.

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FParsec.Internals

open System.Diagnostics

// The following functions are defined using inline IL to help fsc generate code
// the JIT knows better how to optimize.
// Should F# stop supporting inline IL outside the standard library, you can switch
// to the commented out alternatives (which by then will probably be just as efficient).
let inline referenceEquals<'a when 'a : not struct> (x: 'a) (y: 'a) =
    (# "ceq" x y : bool #) // LanguagePrimitives.PhysicalEquality x y
let inline isNull<'a when 'a : not struct> (x: 'a) =
    (# "ldnull ceq" x : bool #) // referenceEquals (box x) null
let inline isNotNull<'a when 'a : not struct> (x: 'a) =
    (# "ldnull cgt.un" x : bool #) // not (isNull x)

let inline isNullOrEmpty (s: string) = System.String.IsNullOrEmpty(s)

// the F# compiler doesn't yet "fuse" multiple '+' string concatenations into one, as the C# compiler does
let inline concat3 (a: string) (b: string) (c: string) = System.String.Concat(a, b, c)
let inline concat4 (a: string) (b: string) (c: string) (d: string) = System.String.Concat(a, b, c, d)
let inline concat5 (a: string) (b: string) (c: string) (d: string) (e: string) = System.String.Concat([|a;b;c;d;e|])
let inline concat6 (a: string) (b: string) (c: string) (d: string) (e: string) (f: string) = System.String.Concat([|a;b;c;d;e;f|])
let inline concat7 (a: string) (b: string) (c: string) (d: string) (e: string) (f: string) (g: string) = System.String.Concat([|a;b;c;d;e;f;g|])

let findNewlineOrEOSChar = Text.FindNewlineOrEOSChar

let getSortedUniqueValues (s: seq<_>) =
     let a = Array.ofSeq s
     if a.Length = 0 then a
     else
        Array.sortInPlace a
        let mutable previous = a.[0]
        let mutable n = 1
        for i = 1 to a.Length - 1 do
            let c = a.[i]
            if c <> previous then n <- n + 1
            previous <- c
        if n = a.Length then a
        else
            let b = Array.zeroCreate n
            let mutable i = 0
            for j = 0 to b.Length - 1 do
                let c = a.[i]
                b.[j] <- c
                i <- i + 1
                while i < a.Length && a.[i] = c do i <- i + 1
            b

/// A primitive pretty printer.
type LineWrapper(tw: System.IO.TextWriter, columnWidth: int, writerIsMultiCharGraphemeSafe: bool) =
    do if columnWidth < 1 then invalidArg "columnWidth" "columnWidth must be positive."

    let mutable indentation = ""
    let mutable maxSpace = columnWidth
    let mutable space = columnWidth
    let mutable afterNewline = true
    let mutable afterSpace = false

    new (tw: System.IO.TextWriter, columnWidth: int) =
        new LineWrapper(tw, columnWidth,
                                     #if SILVERLIGHT
                                         true)
                                     #else
                                         not tw.Encoding.IsSingleByte)
                                     #endif

    member t.TextWriter = tw
    member t.ColumnWidth = columnWidth
    member t.WriterIsMultiCharGraphemeSafe = writerIsMultiCharGraphemeSafe

    member t.Indentation
      with get() = indentation
       and set (s: string) =
               let s = if s.Length <= columnWidth - 1 then s
                       else s.Substring(0, columnWidth - 1) // guarantee maxSpace >= 1
               indentation <- s
               maxSpace <- columnWidth - s.Length
               if afterNewline then space <- maxSpace

    member t.Newline() =
        tw.WriteLine()
        afterNewline <- true
        afterSpace <- false
        space <- maxSpace

    member t.Space() =
        afterSpace <- true

    member t.Print(s: string) =
        if isNotNull s then
            let mutable start = 0
            for i = 0 to s.Length - 1 do
                let c = s.[i]
                if (if   c <= ' ' then c = ' ' || (c >= '\t' && c <= '\r')
                    else c >= '\u0085' && (c = '\u0085' || c = '\u2028' || c = '\u2029'))
                then // any ' ', tab or newlines
                    if start < i then
                        t.Write(s.Substring(start, i - start))
                    t.Space()
                    start <- i + 1
            if start < s.Length then
                if start = 0 then t.Write(s)
                else t.Write(s.Substring(start, s.Length - start))

    member t.Print(s1, s2) = t.Print(s1); t.Print(s2)
    member t.Print(s1, s2, s3) = t.Print(s1); t.Print(s2); t.Print(s3)
    member t.PrintLine(s: string) = t.Print(s); t.Newline()
    member t.PrintLine(s1: string, s2: string) = t.Print(s1); t.Print(s2); t.Newline()
    member t.PrintLine(s1: string, s2: string, s3: string) = t.Print(s1); t.Print(s2); t.Print(s3); t.Newline()

    member private t.Write(s: string) =
        Debug.Assert(s.Length > 0)
        if afterNewline then
            tw.Write(indentation)
            afterNewline <- false
        let n = if writerIsMultiCharGraphemeSafe then Text.CountTextElements(s) else s.Length
        match afterSpace with
        | true when n + 1 <= space ->
            tw.Write(' ')
            tw.Write(s)
            space <- space - 1 - n
            afterSpace <- false
        | false when n <= space ->
            tw.Write(s)
            space <- space - n
        | _ when s.Length <= maxSpace ->
            tw.WriteLine()
            tw.Write(indentation)
            tw.Write(s)
            space <- maxSpace - n
            afterSpace <- false
        | _ ->
            t.Break(s)

    /// breaks a string into multiple lines along text element boundaries.
    member private t.Break(s: string) =
        Debug.Assert(s.Length > 0 && not afterNewline)
        if afterSpace then
            afterSpace <- false
            if space > 1 then
                tw.Write(' ')
                space <- space - 1
            else
                tw.WriteLine()
                tw.Write(indentation)
                space <- maxSpace
        elif space = 0 then
            tw.WriteLine()
            tw.Write(indentation)
            space <- maxSpace
        let te = System.Globalization.StringInfo.GetTextElementEnumerator(s)
        te.MoveNext() |> ignore
        Debug.Assert(te.ElementIndex = 0)
        if writerIsMultiCharGraphemeSafe then
            let mutable startIndex = 0
            while te.MoveNext() do
                space <- space - 1
                if space = 0 then
                    let index = te.ElementIndex
                    tw.WriteLine(s.Substring(startIndex, index - startIndex))
                    tw.Write(indentation)
                    space <- maxSpace
                    startIndex <- index
            space <- space - 1
            tw.Write(s.Substring(startIndex, s.Length - startIndex))
        else
            // We don't break up text elements, but when we fit string pieces into lines we
            // use UTF-16 lengths instead of text element counts (in order to support displays
            // that have problems with combining character sequences).
            let mutable startIndex = 0
            let mutable lastIndex = 0
            while te.MoveNext() do
                let index = te.ElementIndex
                let count = index - startIndex
                if count < space then
                    lastIndex <- index
                elif count = space || lastIndex <= startIndex then
                    tw.WriteLine(s.Substring(startIndex, count))
                    tw.Write(indentation)
                    space <- maxSpace
                    startIndex <- index
                else
                    tw.WriteLine(s.Substring(startIndex, lastIndex - startIndex))
                    tw.Write(indentation)
                    space <- maxSpace
                    startIndex <- lastIndex
            let index = s.Length
            let count = index - startIndex
            if count <= space then
                tw.Write(s.Substring(startIndex, count))
                space <- space - count
            elif lastIndex <= startIndex then
                tw.WriteLine(s.Substring(startIndex, index - startIndex))
                space <- maxSpace
                afterNewline <- true
            else
                tw.WriteLine(s.Substring(startIndex, lastIndex - startIndex))
                tw.Write(indentation)
                tw.Write(s.Substring(lastIndex, index - lastIndex))
                space <- maxSpace - (index - lastIndex)
                if space < 0 then
                    tw.WriteLine()
                    space <- maxSpace
                    afterNewline <- true


type LineSnippet = {
    String: string
    TextElementIndex: int
    Index: int
    IndexOfTextElement: int
    LengthOfTextElement: int
    UnaccountedNewlines: int
    Column: int64
    Utf16Column: int64 // the UTF16 tabs are only counted as 1 char
    LineContainsTabsBeforeIndex: bool
    IsBetweenCRAndLF: bool
}

let getLineSnippet (stream: CharStream<'u>) (p: Position) (space: int) (tabSize: int) multiCharGraphemeSafe =
    Debug.Assert(space > 0 && tabSize > 0)
    Debug.Assert(p.Index >= stream.IndexOfFirstChar && p.Index <= stream.IndexOfLastCharPlus1)

    let isCombiningChar (s: string) =
        match System.Char.GetUnicodeCategory(s, 0) with
        | System.Globalization.UnicodeCategory.NonSpacingMark
        | System.Globalization.UnicodeCategory.SpacingCombiningMark
        | System.Globalization.UnicodeCategory.EnclosingMark
        | System.Globalization.UnicodeCategory.Surrogate
            -> true
        | _ -> false

    let isUnicodeNewlineOrEos c =
        match c with
        | '\n' | '\r'| '\u0085'| '\u2028'| '\u2029'
        | '\uffff' -> true
        | _  -> false

    // we restrict the maximum column count, so that we don't accidentally
    // completely reread a multi-gigabyte file when it has no newlines
    let maxColForColCount = 1000
    let maxExtraChars = 32
    let colTooLarge = p.Column > int64 maxColForColCount

    let oldState = stream.State

    let mutable index = p.Index
    stream.Seek(index) // throws if index is too small
    if index <> stream.Index then
        raise (System.ArgumentException("The error position lies beyond the end of the stream."))
    let isBetweenCRAndLF = stream.Peek() = '\n' && stream.Peek(-1) = '\r'
    if isBetweenCRAndLF then
        stream.Skip(-1)
        index <- index - 1L
    else
        let mutable c = stream.Peek()
        let mutable n = 2*space + maxExtraChars
        // skip to end of line, but not over more than n chars
        while not (isUnicodeNewlineOrEos c) && n <> 0 do
            c <- stream.SkipAndPeek()
            n <- n - 1
        if not (isUnicodeNewlineOrEos c) then
            n <- maxExtraChars
            while isCombiningChar (stream.PeekString(2)) && n <> 0 do
                stream.Skip() |> ignore
                n <- n - 1
    let endIndexToken = stream.IndexToken

    stream.Seek(index)
    let lineBegin = index - p.Column + 1L
    // use SkipAndPeek instead of Skip, so that we can't move past the beginning of the stream
    stream.SkipAndPeek(if not colTooLarge then -(int32 p.Column - 1) else -(maxColForColCount - 1)) |> ignore
    if colTooLarge then
        let mutable n = if p.Column > int64 System.Int32.MaxValue then maxExtraChars
                        else min maxExtraChars (int32 p.Column - maxColForColCount)
        while isCombiningChar (stream.PeekString(2)) && n <> 0 do
            stream.SkipAndPeek(-1) |> ignore
            n <- n - 1
    let mutable beginIndex = stream.Index
    let mutable columnOffset = beginIndex - lineBegin
    let mutable idx = int (index - beginIndex)

    let beginIndexToken = stream.IndexToken
    stream.Seek(endIndexToken)
    let mutable str = stream.ReadFrom(beginIndexToken)

    // we're done with the stream now
    stream.BacktrackTo(oldState)

    let mutable lastLineBeginIdx = 0
    let mutable unaccountedNLs = 0
    let mutable mayContainMultiCharGraphemes = false
    let mutable nTabs = 0

    for i = 0 to str.Length - 1 do
        let c = str.[i]
        if c >= ' ' then
            if c >= '\u0300' then
                mayContainMultiCharGraphemes <- true
        elif c = '\t' then
            nTabs <- nTabs + 1
        elif c = '\n' || (c = '\r' && (i + 1 >= str.Length || str.[i + 1] <> '\n')) then
            // there can be no newline after idx
            lastLineBeginIdx <- i + 1
            unaccountedNLs <- unaccountedNLs + 1
            mayContainMultiCharGraphemes <- false
            nTabs <- 0

    if unaccountedNLs <> 0 then
        str <- str.Substring(lastLineBeginIdx)
        idx <- idx - lastLineBeginIdx
        columnOffset <- 0L

    let utf16Column = columnOffset + int64 (idx + 1)
    let mutable lineContainsTabsBeforeIndex = false
    if nTabs > 0 then // replace tabs with spaces
        let mutable off = if columnOffset = 0L then 0
                          else int32 (columnOffset%(int64 tabSize))
        let sb = new System.Text.StringBuilder(str.Length + nTabs*tabSize)
        let mutable i0 = 0
        let mutable idxIncr = 0
        for i = 0 to str.Length - 1 do
            if str.[i] = '\t' then
                if i > i0 then sb.Append(str, i0, i - i0) |> ignore
                let n = tabSize - (off + i)%tabSize
                sb.Append(' ', n) |> ignore
                off <- off + (n - 1)
                if i < idx then
                    lineContainsTabsBeforeIndex <- true
                    idxIncr <- idxIncr + (n - 1)
                i0 <- i + 1
        if i0 < str.Length then sb.Append(str, i0, str.Length - i0) |> ignore
        str <- sb.ToString()
        idx <- idx + idxIncr

    let clip nBefore nAfter =
        let mutable nBefore, nAfter = nBefore, nAfter
        let mutable diff = nBefore + nAfter + 1 - space
        if diff > 0 then
            let d = nBefore - nAfter
            if d > 0 then
                let dd = min diff d
                nBefore <- nBefore - dd
                diff    <- diff - dd
            elif d < 0 then
                let dd = min diff -d
                nAfter <- nAfter - dd
                diff   <- diff - dd
            if diff <> 0 then
                if diff%2 = 0 then
                    nBefore <- nBefore - diff/2
                    nAfter  <- nAfter  - diff/2
                else
                    nBefore <- nBefore - diff/2
                    nAfter  <- nAfter  - diff/2 - 1
        nBefore, nAfter

    if not mayContainMultiCharGraphemes then
        let nBefore, nAfter = clip idx (if idx < str.Length then str.Length - idx - 1 else 0)
        {String = str.Substring(idx - nBefore, nBefore + nAfter + (if idx < str.Length then 1 else 0))
         Index = nBefore
         TextElementIndex = nBefore
         IndexOfTextElement = nBefore
         LengthOfTextElement = 1
         UnaccountedNewlines = unaccountedNLs
         Column = columnOffset + int64 (idx + 1)
         Utf16Column = utf16Column
         LineContainsTabsBeforeIndex = lineContainsTabsBeforeIndex
         IsBetweenCRAndLF = isBetweenCRAndLF}
    else
        let indices = System.Globalization.StringInfo.ParseCombiningCharacters(str)
        let mutable idxIdx = 0 // the indices index of the text element containing the str char at idx
        while idxIdx < indices.Length && indices.[idxIdx] < idx do idxIdx <- idxIdx + 1
        if (if idxIdx < indices.Length then indices.[idxIdx] > idx else idx < str.Length) then idxIdx <- idxIdx - 1
        let col = columnOffset + int64 (idxIdx + 1)
        let teIdx    =  if idxIdx     < indices.Length then indices.[idxIdx]     else str.Length
        let teLength = (if idxIdx + 1 < indices.Length then indices.[idxIdx + 1] else str.Length) - teIdx
        let mutable nBefore, nAfter = clip idxIdx (if idxIdx = indices.Length then 0 else indices.Length - idxIdx - 1)
        let mutable strBegin = let ii = idxIdx - nBefore    in if ii < indices.Length then indices.[ii] else str.Length
        let mutable strEnd   = let ii = idxIdx + nAfter + 1 in if ii < indices.Length then indices.[ii] else str.Length
        if not multiCharGraphemeSafe then
            while strEnd - strBegin > space && (nBefore > 0 || nAfter > 0) do
                if nBefore > nAfter then
                    nBefore  <- nBefore - 1
                    strBegin <- indices.[idxIdx - nBefore]
                else
                    nAfter <- nAfter - 1
                    strEnd <- indices.[idxIdx + nAfter + 1]
        {String = str.Substring(strBegin, strEnd - strBegin)
         Index = idx - strBegin
         TextElementIndex = nBefore
         IndexOfTextElement = teIdx - strBegin
         LengthOfTextElement = teLength
         UnaccountedNewlines = unaccountedNLs
         Column = col
         Utf16Column = utf16Column
         LineContainsTabsBeforeIndex = lineContainsTabsBeforeIndex
         IsBetweenCRAndLF = isBetweenCRAndLF}


