// Copyright (c) Stephan Tolksdorf 2009
// License: Simplified BSD License. See accompanying documentation.

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

let inline isNullOrEmpty (s: string) = isNull s || s.Length = 0

// These operators are faster than = and <>. They are not public because
// their names conflict with the operators in the OCaml compatibility module
let inline (==) (s1: State<'u>) (s2: State<'u>) = s1.Equals(s2)
let inline (!=) (s1: State<'u>) (s2: State<'u>) = not (s1 == s2)

// the F# compiler doesn't yet "fuse" multiple '+' string concatenations into one, as the C# compiler does
let inline concat3 (a: string) (b: string) (c: string) = System.String.Concat(a, b, c)
let inline concat4 (a: string) (b: string) (c: string) (d: string) = System.String.Concat(a, b, c, d)
let inline concat5 (a: string) (b: string) (c: string) (d: string) (e: string) = System.String.Concat([|a;b;c;d;e|])
let inline concat6 (a: string) (b: string) (c: string) (d: string) (e: string) (f: string) = System.String.Concat([|a;b;c;d;e;f|])
let inline concat7 (a: string) (b: string) (c: string) (d: string) (e: string) (f: string) (g: string) = System.String.Concat([|a;b;c;d;e;f;g|])

let containsNewlineChar = Helper.ContainsNewlineChar

let ordinalEnding (i: int) =
    match i%10 with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th"

let hexEscapeChar c =
    let n = int c
    let cs = Array.zeroCreate 6
    cs.[0] <- '\\'; cs.[1] <- 'u'
    for j = 0 to 3 do
        cs.[5 - j] <- "0123456789abcdef".[((n >>> 4*j) &&& 0xf)]
    new string(cs)

[<NoDynamicInvocation>]
let inline private escapeCharHelper escapeSingleQuote escapeDoubleQuote escapeNonAscii (c: char) (f: char -> string) =
    if c > '\'' && c < '\u007f' then
        if c <> '\\' then f c else "\\\\"
    else
        match c with
        | '\b' -> "\\b"
        | '\t' -> "\\t"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\"' when escapeDoubleQuote -> "\\\""
        | '\'' when escapeSingleQuote -> "\\'"
        | _ -> if (escapeNonAscii && c >= '\u007f') || System.Char.IsControl(c) then hexEscapeChar c else f c

[<NoDynamicInvocation>]
let inline escapeStringHelper escapeSingleQuote escapeDoubleQuote escapeNonAscii (s: string) =
    let rec escape sb i start =
        if i < s.Length then
            let esc = escapeCharHelper escapeSingleQuote escapeDoubleQuote escapeNonAscii s.[i] (fun _ -> null)
            if isNull esc then escape sb (i + 1) start
            else
                let sb = if isNull sb then (new System.Text.StringBuilder(s.Length + 6))
                         else sb
                sb.Append(s, start, i - start).Append(esc) |> ignore
                escape sb (i + 1) (i + 1)
        elif isNull sb then s
        else sb.Append(s, start, s.Length - start).ToString()
    escape null 0 0

[<NoDynamicInvocation>]
let inline quoteStringHelper (quote: string) escapeSingleQuote escapeDoubleQuote escapeNonAscii (s: string) =
    let rec escape sb i start =
        if i < s.Length then
            let esc = escapeCharHelper escapeSingleQuote escapeDoubleQuote escapeNonAscii s.[i] (fun _ -> null)
            if isNull esc then escape sb (i + 1) start
            else
                let sb = if isNull sb then (new System.Text.StringBuilder(s.Length + 8)).Append(quote)
                         else sb
                sb.Append(s, start, i - start).Append(esc) |> ignore
                escape sb (i + 1) (i + 1)
        elif isNull sb then concat3 quote s quote
        else sb.Append(s, start, s.Length - start).Append(quote).ToString()
    escape null 0 0

let escapeStringInDoubleQuotes s = escapeStringHelper false true false s

let quoteChar c =
    if c <> '\'' then concat3 "'" (escapeCharHelper false false false c string) "'"
    else "\"'\""

let quoteString s      = quoteStringHelper "'" true false false s
let asciiQuoteString s = quoteStringHelper "'" true false true  s


/// A primitive pretty printer.
type LineWrapper(tw: System.IO.TextWriter, columnWidth: int, writerIsMultiCharGraphemeSafe: bool) =
    do if columnWidth < 1 then invalidArg "columnWidth" "columnWidth must be positive."

    let mutable indentation = ""
    let mutable maxSpace = columnWidth
    let mutable space = columnWidth
    let mutable afterNewline = true
    let mutable afterSpace = false

    new (tw: System.IO.TextWriter, columnWidth: int) =
        new LineWrapper(tw, columnWidth, not tw.Encoding.IsSingleByte)

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
                if (if   c <= ' '  then c = ' ' || (c >= '\t' && c <= '\r')
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
        let n = if writerIsMultiCharGraphemeSafe then Helper.CountTextElements(s) else s.Length
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

let getLineSnippet (stream: CharStream) (p: Position) (space: int) (tabSize: int) multiCharGraphemeSafe =
    Debug.Assert(space > 0 && tabSize > 0)
    Debug.Assert(p.Index >= stream.BeginIndex && p.Index <= stream.EndIndex)

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
        | '\n' | '\u000C' | '\r'| '\u0085'| '\u2028'| '\u2029'
        | '\uffff' -> true
        | _  -> false

    // we restrict the maximum column count, so that we don't accidentally
    // completely reread a multi-gigabyte file when it has no newlines
    let maxColForColCount = 1000
    let maxExtraChars = 32
    let colTooLarge = p.Column > int64 maxColForColCount

    let mutable index = p.Index
    let mutable iterBegin = stream.Seek(index) // throws if index is too small
    let mutable iterEnd = iterBegin
    if index <> iterEnd.Index then
        raise (System.ArgumentException("The error position lies beyond the end of the stream."))
    let isBetweenCRAndLF = iterEnd.Read() = '\n' && iterEnd.Peek(-1) = '\r'
    if not isBetweenCRAndLF then
        let mutable c = iterEnd.Read()
        let mutable n = 2*space + maxExtraChars
        // skip to end of line, but not over more than n chars
        while not (isUnicodeNewlineOrEos c) && n <> 0 do
            c <- iterEnd._Increment()
            n <- n - 1
        if not (isUnicodeNewlineOrEos c) then
            n <- maxExtraChars
            while isCombiningChar (iterEnd.Read(2)) && n <> 0 do
                iterEnd._Increment() |> ignore
                n <- n - 1
    else
        iterEnd._Decrement() |> ignore
        iterBegin <- iterEnd
        index <- index - 1L

    let lineBegin = index - p.Column + 1L
    // use _Decrement instead of Advance, so that we don't move past the beginning of the stream
    iterBegin._Decrement(if not colTooLarge then uint32 p.Column - 1u else uint32 maxColForColCount - 1u) |> ignore
    if colTooLarge then
        let mutable n = if p.Column < int64 System.Int32.MaxValue then
                            min maxExtraChars (int32 p.Column - maxColForColCount)
                        else maxExtraChars
        while isCombiningChar (iterBegin.Read(2)) && n <> 0 do
            iterBegin._Decrement() |> ignore
            n <- n - 1
    let iterBeginIndex = iterBegin.Index
    let mutable columnOffset = iterBeginIndex - lineBegin
    let mutable idx = int (index - iterBeginIndex)
    let mutable str = iterBegin.ReadUntil(iterEnd)

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
        if (if idxIdx < indices.Length then indices.[idxIdx] > idx else idxIdx <> 0) then idxIdx <- idxIdx - 1
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




