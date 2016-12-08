// Copyright (c) Stephan Tolksdorf 2007-2011
// License: BSD-style. See accompanying documentation.

[<AutoOpen>]
module FParsec.Error

//open FParsec

open System.Diagnostics
open System.Globalization
open System.IO
open FParsec.Internals

// Unfortunately, F# currently doesn't support active patterns with more than 7
// cases, so we have to use partial patterns.

type Expected = ErrorMessage.Expected
type ExpectedString = ErrorMessage.ExpectedString
type ExpectedStringCI = ErrorMessage.ExpectedCaseInsensitiveString
type Unexpected = ErrorMessage.Unexpected
type UnexpectedString = ErrorMessage.UnexpectedString
type UnexpectedStringCI = ErrorMessage.UnexpectedCaseInsensitiveString
type Message = ErrorMessage.Message
type NestedError = ErrorMessage.NestedError
type CompoundError = ErrorMessage.CompoundError
type OtherErrorMessage = ErrorMessage.Other

let (|Expected|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.Expected then Some msg.String else None

let (|ExpectedString|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.ExpectedString then Some msg.String else None

let (|ExpectedStringCI|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.ExpectedCaseInsensitiveString then Some msg.String else None

let (|Unexpected|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.Unexpected then Some msg.String else None

let (|UnexpectedString|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.UnexpectedString then Some msg.String else None

let (|UnexpectedStringCI|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.UnexpectedCaseInsensitiveString then Some msg.String else None

let (|Message|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.Message then Some msg.String else None

let (|NestedError|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.NestedError then
        let ne = msg :?> ErrorMessage.NestedError
        Some((ne.Position, ne.UserState, ne.Messages))
    else
        None

let (|CompoundError|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.CompoundError then
        let ce = msg :?> ErrorMessage.CompoundError
        Some((ce.LabelOfCompound, ce.NestedErrorPosition, ce.NestedErrorUserState, ce.NestedErrorMessages))
    else
        None

let (|OtherErrorMessage|_|) (msg: ErrorMessage) =
    if msg.Type = ErrorMessageType.Other then
        let om = msg :?> ErrorMessage.Other
        Some om.Data
    else
        None

[<Literal>]
let NoErrorMessages = null : ErrorMessageList

let (|ErrorMessageList|NoErrorMessages|) (error: ErrorMessageList) =
    if isNotNull error then ErrorMessageList(error.Head, error.Tail)
    else NoErrorMessages

let inline isSingleErrorMessageOfType (ty: ErrorMessageType) (error: ErrorMessageList) =
    isNotNull error && error.Head.Type = ty && isNull error.Tail

let expected           label = ErrorMessageList(ErrorMessage.Expected(label))
let expectedString     str   = ErrorMessageList(ErrorMessage.ExpectedString(str))
let expectedStringCI   str   = ErrorMessageList(ErrorMessage.ExpectedCaseInsensitiveString(str))
let unexpected         label = ErrorMessageList(ErrorMessage.Unexpected(label))
let unexpectedString   str   = ErrorMessageList(ErrorMessage.UnexpectedString(str))
let unexpectedStringCI str   = ErrorMessageList(ErrorMessage.UnexpectedCaseInsensitiveString(str))
let messageError       msg   = ErrorMessageList(ErrorMessage.Message(msg))
let otherError         obj   = ErrorMessageList(ErrorMessage.Other(obj : obj))

let nestedError (stream: CharStream<'u>) (error: ErrorMessageList) =
    (*
    // manually inlined:
    match error with
    | ErrorMessageList(NestedError _, NoErrorMessages) -> error
    | _ -> ErrorMessageList(NestedError(stream.Position, stream.UserState, error), NoErrorMessages)
    *)
    if error |> isSingleErrorMessageOfType ErrorMessageType.NestedError
    then error
    else ErrorMessageList(ErrorMessage.NestedError(stream.Position, stream.UserState, error))

let compoundError label (stream: CharStream<'u>) (error: ErrorMessageList) =
    // manually inlined:
    (*
    match error with
    | ErrorMessageList(NestedError(pos, ustate, msgs), NoErrorMessages) ->
           ErrorMessageList(CompoundError(label, pos, ustate, msgs), NoErrorMessages)
    | _ -> ErrorMessageList(CompoundError(label, stream.Position, stream.UserState, error), NoErrorMessages)
    *)
    if error |> isSingleErrorMessageOfType ErrorMessageType.NestedError
    then
        let ne = error.Head :?> ErrorMessage.NestedError
        ErrorMessageList(ErrorMessage.CompoundError(label, ne.Position, ne.UserState, ne.Messages))
    else
        ErrorMessageList(ErrorMessage.CompoundError(label, stream.Position, stream.UserState, error))

let
#if NOINLINE
#else
    inline
#endif
           mergeErrors errorMessages1 errorMessages2 = ErrorMessageList.Merge(errorMessages1, errorMessages2)

/// the default position printer
let internal printPosition (tw: System.IO.TextWriter) (p: Position) (indent: string) (columnWidth: int) =
    tw.Write(indent)
    tw.WriteLine(Strings.ErrorPosition(p))

let internal printErrorPosition (tabSize: int) (lw: LineWrapper) (stream: CharStream<'u>) (p: Position) =
    /// writes the string with all whitespace chars replaced with ' '
    let writeStringWithSimplifiedWhitespace (tw: TextWriter) (s: string) =
        let mutable i0 = 0
        for i = 0 to s.Length - 1 do
            let c = s.[i]
            if Text.IsWhitespace(c) then
                if i0 < i then
                    tw.Write(s.Substring(i0, i - i0))
                tw.Write(' ')
                i0 <- i + 1
        if i0 < s.Length then
            if i0 = 0 then tw.Write(s)
            else tw.Write(s.Substring(i0, s.Length - i0))

    let sn = getLineSnippet stream p (lw.ColumnWidth - lw.Indentation.Length) tabSize lw.WriterIsMultiCharGraphemeSafe
    let str = sn.String

    lw.PrintLine(Strings.ErrorPosition(p, sn.UnaccountedNewlines, sn.Column, sn.Utf16Column))

    let msgs = ResizeArray<_>()
    if sn.LineContainsTabsBeforeIndex then
        let mutable msg = Strings.ColumnCountAssumesTabStopDistanceOfNChars(tabSize)
        if sn.Column = sn.Utf16Column then
            msg <- msg + Strings.Utf16ColumnCountOnlyCountsEachTabAs1Char
        msgs.Add(msg)

    if str.Length > 0 then
        let tw = lw.TextWriter
        tw.Write(lw.Indentation)
        writeStringWithSimplifiedWhitespace tw str
        tw.WriteLine()
        tw.Write(lw.Indentation)
        if sn.TextElementIndex > 0 then
            tw.Write(new string(' ', sn.TextElementIndex))
        tw.Write('^')
        let d = sn.Index - sn.TextElementIndex
        if d <> 0 && not lw.WriterIsMultiCharGraphemeSafe then
            if d > 1 then
                tw.Write(new string('-', d - 1))
            tw.Write('^')
            msgs.Add(Strings.ExactPositionBetweenCaretsDependsOnDisplayUnicodeCapabilities)
        tw.WriteLine()

    if sn.Index < str.Length then
        let i = sn.Index
        let c = str.[i]
        if System.Char.IsSurrogate(c) then
            if Text.IsHighSurrogate(c) then
                if i + 1 < str.Length && Text.IsLowSurrogate(str.[i + 1]) then
                    msgs.Add(Strings.ErrorOccurredAtBeginningOfSurrogatePair(str.Substring(i, 2)))
                else
                    msgs.Add(Strings.CharAtErrorPositionIsIsolatedHighSurrogate(c))
            else // low surrogate
                if i > 0 && Text.IsHighSurrogate(str.[i - 1]) then
                    msgs.Add(Strings.ErrorOccurredAtSecondCharInSurrogatePair(str.Substring(i - 1, 2)))
                else
                    msgs.Add(Strings.CharAtErrorPositionIsIsolatedLowSurrogate(c))
        elif i > 0 then
            let c1 = str.[i - 1]
            if Text.IsHighSurrogate(c1) then
                msgs.Add(Strings.CharBeforeErrorPositionIsIsolatedHighSurrogate(c1))
            elif Text.IsLowSurrogate(c1) then
                msgs.Add(Strings.CharBeforeErrorPositionIsIsolatedLowSurrogate(c1))
    else
        if p.Index = stream.IndexOfLastCharPlus1 then msgs.Add(Strings.ErrorOccurredAtEndOfInputStream)
        elif str.Length = 0 then msgs.Add(Strings.ErrorOccurredOnAnEmptyLine)
        else msgs.Add(Strings.ErrorOccurredAtEndOfLine)

    if sn.LengthOfTextElement > 1 && (sn.LengthOfTextElement > 2 || not (System.Char.IsSurrogate(str.[sn.Index]))) then
        let n = sn.Index - sn.IndexOfTextElement + 1
        let te = str.Substring(sn.IndexOfTextElement, sn.LengthOfTextElement)
        msgs.Add(Strings.ErrorOccurredAtNthCharInCombiningCharacterSequence(n, te))
    elif sn.IsBetweenCRAndLF then
        msgs.Add(Strings.ErrorOccurredAtSecondCharInNewline)

    if sn.UnaccountedNewlines > 0 then
        let n = sn.UnaccountedNewlines
        msgs.Add(Strings.InputContainsAtLeastNUnaccountedNewlines(n))

    if msgs.Count = 1 then lw.PrintLine(Strings.Note, msgs.[0])
    elif msgs.Count > 1 then
        let ind  = lw.Indentation
        let ind2 = ind + "  "
        lw.PrintLine(Strings.Note)
        for msg in msgs do
            lw.Print("* ")
            lw.Indentation <- ind2
            lw.PrintLine(msg)
            lw.Indentation <- ind

[<Sealed>]
type ParserError(position: Position, userState: obj, messages: ErrorMessageList) =
    do if isNull position then nullArg "pos"

    let defaultColumnWidth = 79
    let defaultIndentation = ""
    let defaultIndentationIncrement = "  "
    let defaultTabSize = 8

    member t.Position = position
    member t.UserState = userState
    member T.Messages = messages

    override t.ToString() =
        use sw = new System.IO.StringWriter()
        t.WriteTo(sw)
        sw.ToString()

    member t.ToString(streamWhereErrorOccurred: CharStream<'u>) =
        use sw = new System.IO.StringWriter()
        t.WriteTo(sw, streamWhereErrorOccurred)
        sw.ToString()

    member t.WriteTo(textWriter: System.IO.TextWriter,
                     ?positionPrinter: (System.IO.TextWriter -> Position -> string -> int -> unit),
                     ?columnWidth: int, ?initialIndentation: string, ?indentationIncrement: string) =

        let positionPrinter = defaultArg positionPrinter printPosition
        let columnWidth     = defaultArg columnWidth defaultColumnWidth
        let ind             = defaultArg initialIndentation defaultIndentation
        let indIncrement    = defaultArg indentationIncrement defaultIndentationIncrement
        let lw = new LineWrapper(textWriter, columnWidth, Indentation = ind)
        t.WriteTo(lw, positionPrinter, indIncrement)

    member t.WriteTo(textWriter: System.IO.TextWriter,
                     streamWhereErrorOccurred: CharStream<'u>,
                     ?tabSize: int,
                     ?columnWidth: int, ?initialIndentation: string, ?indentationIncrement: string) =

        let originalStreamName = t.Position.StreamName
        let getStream = fun (pos: Position) -> if pos.StreamName = originalStreamName then streamWhereErrorOccurred else null
        t.WriteTo(textWriter, getStream, ?tabSize = tabSize, ?columnWidth = columnWidth, ?initialIndentation = initialIndentation, ?indentationIncrement = indentationIncrement)

    member t.WriteTo(textWriter: System.IO.TextWriter,
                     getStream: (Position -> CharStream<'u>),
                     ?tabSize: int,
                     ?columnWidth: int, ?initialIndentation: string, ?indentationIncrement: string) =

        let columnWidth  = defaultArg columnWidth defaultColumnWidth
        let ind          = defaultArg initialIndentation defaultIndentation
        let indIncrement = defaultArg indentationIncrement defaultIndentationIncrement
        let tabSize      = defaultArg tabSize defaultTabSize
        let lw = new LineWrapper(textWriter, columnWidth, Indentation = ind)
        let positionPrinter =
            fun tw position indent columnWidth ->
                let stream = getStream position
                if isNotNull stream then
                   printErrorPosition tabSize lw stream position
                else
                   printPosition lw.TextWriter position indent columnWidth
        t.WriteTo(lw, positionPrinter, indIncrement)

    member private t.WriteTo(lw: LineWrapper,
                             positionPrinter: System.IO.TextWriter -> Position -> string -> int -> unit,
                             indentationIncrement: string) =

        let rec printMessages (position: Position) (msgs: ErrorMessageList) =
            positionPrinter lw.TextWriter position lw.Indentation lw.ColumnWidth
            let nra() = new ResizeArray<_>()
            let expectedA, unexpectedA, messageA, nestedA, compoundA = nra(), nra(), nra(), nra(), nra()
            let mutable otherCount = 0
            for msg in ErrorMessageList.ToSortedArray(msgs) do
                match msg.Type with
                | ErrorMessageType.Expected                        -> expectedA.Add(msg.String)
                | ErrorMessageType.ExpectedString                  -> expectedA.Add(Strings.Quote(msg.String))
                | ErrorMessageType.ExpectedCaseInsensitiveString   -> expectedA.Add(Strings.QuoteCaseInsensitive(msg.String))
                | ErrorMessageType.Unexpected                      -> unexpectedA.Add(msg.String)
                | ErrorMessageType.UnexpectedString                -> unexpectedA.Add(Strings.Quote(msg.String))
                | ErrorMessageType.UnexpectedCaseInsensitiveString -> unexpectedA.Add(Strings.QuoteCaseInsensitive(msg.String))
                | ErrorMessageType.Message                         -> messageA.Add(msg.String)
                | ErrorMessageType.NestedError ->
                    let ne = msg :?> ErrorMessage.NestedError
                    nestedA.Add((ne.Position, ne.Messages))
                | ErrorMessageType.CompoundError ->
                    if not (isNullOrEmpty msg.String) then expectedA.Add(msg.String)
                    let ce = msg :?> ErrorMessage.CompoundError
                    compoundA.Add((ce.String, ce.NestedErrorPosition, ce.NestedErrorMessages))
                | ErrorMessageType.Other ->
                    otherCount <- otherCount + 1
                | _ ->
                    failwith "printMessages"

            let printArray title (a: ResizeArray<string>) (sep: string) =
                lw.Print(title, " ")
                let n = a.Count
                for i = 0 to n - 3 do
                    lw.Print(a.[i], ", ")
                if n > 1 then lw.Print(a.[n - 2], sep)
                if n > 0 then lw.Print(a.[n - 1])
                lw.Newline()
            if expectedA.Count > 0 then
                printArray Strings.Expecting expectedA Strings.Or
            if unexpectedA.Count > 0 then
                printArray Strings.Unexpected unexpectedA Strings.And
            let ind = lw.Indentation
            let indInd = ind + indentationIncrement
            if messageA.Count > 0 then
                if expectedA.Count > 0 || unexpectedA.Count > 0 then
                    lw.PrintLine(Strings.OtherErrors)
                    lw.Indentation <- indInd
                for m in messageA do
                    lw.PrintLine(m)
                if expectedA.Count > 0 || unexpectedA.Count > 0 then
                    lw.Indentation <- ind
            for label, pos2, msgs2 in compoundA do
                lw.Newline()
                lw.PrintLine(Strings.CompoundCouldNotBeParsedBecause(label))
                lw.Indentation <- indInd
                printMessages pos2 msgs2
                lw.Indentation <- ind
            for pos2, msgs2 in nestedA do
                lw.Newline()
                lw.PrintLine(Strings.ParserBacktrackedAfter)
                lw.Indentation <- indInd
                printMessages pos2 msgs2
                lw.Indentation <- ind
            if    expectedA.Count = 0 && unexpectedA.Count = 0 && messageA.Count = 0
               && compoundA.Count = 0 && nestedA.Count = 0
            then
                lw.PrintLine(Strings.UnknownErrors)
        printMessages position messages

    override t.Equals(value: obj) =
        referenceEquals (t :> obj) value
        ||  match value with
            | null -> false
            | :? ParserError as other ->
                   t.Position = other.Position
                && t.Messages = other.Messages
                && t.UserState = other.UserState
            | _ -> false

    override t.GetHashCode() = t.Position.GetHashCode() ^^^ hash t.Messages

let inline internal raiseInfiniteLoopException name stream =
    raise (FParsec.Internal.ParserCombinatorInInfiniteLoopHelper.CreateException(name, stream))
