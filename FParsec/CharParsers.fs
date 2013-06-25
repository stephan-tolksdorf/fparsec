// Copyright (c) Stephan Tolksdorf 2007-2012
// License: Simplified BSD License. See accompanying documentation.

[<AutoOpen>]
module FParsec.CharParsers

open System.Diagnostics
open System.Text
open System.Text.RegularExpressions
open System.Runtime.CompilerServices // for MethodImplAttribute

#if LOW_TRUST
#else
open Microsoft.FSharp.NativeInterop
#endif

open FParsec
open FParsec.Internals
open FParsec.Error
open FParsec.Primitives

#nowarn "9" // "Uses of this construct may result in the generation of unverifiable .NET IL code."
#nowarn "51" // "The address-of operator may result in non-verifiable code."

// ================
// Helper functions
// ================

[<Literal>]
let EOS = '\uffff'

let foldCase = Text.FoldCase : string -> string
let normalizeNewlines = Text.NormalizeNewlines

let floatToHexString = HexFloat.DoubleToHexString
let floatOfHexString = HexFloat.DoubleFromHexString

let float32ToHexString = HexFloat.SingleToHexString
let float32OfHexString = HexFloat.SingleFromHexString

// ========================
// Running parsers on input
// ========================

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type ParserResult<'Result,'UserState> =
     | Success of 'Result * 'UserState * Position
     | Failure of string * ParserError * 'UserState
     with
        member private t.StructuredFormatDisplay =
            match t with
            | Success(r,_,_) ->
                if typeof<'Result> = typeof<unit> then "Success: ()"
                else sprintf "Success: %A" r
            | Failure(msg,_,_) ->
                sprintf "Failure:\n%s" msg

let internal applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let runParserOnString (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (streamName: string) (chars: string) =
    CharStream.ParseString(chars, 0, chars.Length, applyParser parser, ustate, streamName)

let runParserOnSubstring (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (streamName: string) (chars: string) (index: int) length =
    CharStream.ParseString(chars, index, length, applyParser parser, ustate, streamName)

let runParserOnStream (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (streamName: string) (byteStream: System.IO.Stream) (encoding: System.Text.Encoding) =
#if LOW_TRUST
    let
#else
    use
#endif
        stream = new CharStream<'UserState>(byteStream, encoding)
    stream.UserState <- ustate
    stream.Name <- streamName
    applyParser parser stream

let runParserOnFile (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (path: string) (encoding: System.Text.Encoding) =
#if LOW_TRUST
    let
#else
    use
#endif
        stream = new CharStream<'UserState>(path, encoding)
    stream.UserState <- ustate
    applyParser parser stream

let run parser (string: string) =
    runParserOnString parser () "" string

// =======
// Parsers
// =======

// -------------------------------------------------------------
// Reading the input stream position and handling the user state
// -------------------------------------------------------------

let getPosition : Parser<Position,'u> =
    fun stream -> Reply(stream.Position)

let getUserState : Parser<'u,'u> =
    fun stream -> Reply(stream.UserState)

let setUserState (newUserState: 'u) : Parser<unit,'u> =
    fun stream ->
        stream.UserState <- newUserState
        Reply(())

let updateUserState (f: 'u -> 'u) : Parser<unit,'u> =
    fun stream ->
        stream.UserState <- f stream.UserState
        Reply(())

let userStateSatisfies f : Parser<unit,'u> =
    fun stream ->
        let status = if f stream.UserState then Ok else Error
        Reply(status, (), NoErrorMessages)

// --------------------
// Parsing single chars
// --------------------

let newlineReturn result : Parser<_,'u> =
    fun stream ->
        if stream.SkipNewline() then Reply(result)
        else Reply(Error, Errors.ExpectedNewline)

let newline<'u>     = newlineReturn '\n' : Parser<_,'u>
let skipNewline<'u> = newlineReturn  ()  : Parser<_,'u>

let unicodeNewlineReturn result : Parser<_,'u> =
    fun stream ->
        if stream.SkipUnicodeNewline() then Reply(result)
        else Reply(Error, Errors.ExpectedNewline)

let unicodeNewline<'u>     = unicodeNewlineReturn '\n' : Parser<_,'u>
let skipUnicodeNewline<'u> = unicodeNewlineReturn  ()  : Parser<_,'u>

let internal charReturnE (c: char) result error : Parser<'a,'u> =
    fun stream ->
        if stream.Skip(c) then Reply(result)
        else Reply(Error, error)

let charReturn c result : Parser<'a,'u> =
    match c with
    | '\r' | '\n' -> newlineReturn result
    | EOS -> invalidArg "c" "The char '\uffff' (EOS) is not a valid argument for the pchar/skipChar/charReturn parser. If you want to check for the end of the stream, consider using the `eof` parser."
    | _   -> charReturnE c result (expectedString (string c))

let pchar    c = charReturn c c
let skipChar c = charReturn c ()


/// returns true for chars '\u000E' - '\ufffe'
let inline internal isCertainlyNoNLOrEOS (c: char) =
    // '\n' = '\u000A', '\r' = '\u000D'
    unativeint c - 0xEun < unativeint EOS - 0xEun

let anyChar : Parser<char,'u> =
    fun stream ->
        let c = stream.ReadCharOrNewline()
        if c <> EOS then Reply(c)
        else Reply(Error, Errors.ExpectedAnyChar)

let skipAnyChar : Parser<unit,'u> =
    fun stream ->
        if stream.ReadCharOrNewline() <> EOS then Reply(())
        else Reply(Error, Errors.ExpectedAnyChar)


// doesn't check for newlines or EOS
let
#if NOINLINE
#else
    inline
#endif
           internal fastInlineSatisfyE f error : Parser<char,'u> =
    fun stream ->
        let c = stream.Peek()
        if f c then
            stream.Skip()
            Reply(c)
        else
            Reply(Error, error)

let internal satisfyE f error : Parser<char,'u> =
    fun stream ->
        let mutable reply = Reply()
        match stream.Peek() with
        | c when isCertainlyNoNLOrEOS c ->
            if f c then
                stream.Skip()
                reply.Status <- Ok
                reply.Result <- c
            else
                reply.Error <- error
        | '\r' | '\n' ->
            if f '\n' then
                stream.SkipNewline() |> ignore
                reply.Status <- Ok
                reply.Result <- '\n'
            else
                reply.Error <- error
        | c ->
             if c <> EOS && f c then
                stream.Skip()
                reply.Status <- Ok
                reply.Result <- c
             else
                reply.Error <- error
        reply

let internal skipSatisfyE f error : Parser<unit,'u> =
    fun stream ->
        let mutable reply = Reply()
        match stream.Peek() with
        | c when isCertainlyNoNLOrEOS c ->
            if f c then
                stream.Skip()
                reply.Status <- Ok
            else
                reply.Error <- error
        | '\r' | '\n' ->
            if f '\n' then
                stream.SkipNewline() |> ignore
                reply.Status <- Ok
            else
                reply.Error <- error
        | c ->
             if c <> EOS && f c then
                stream.Skip()
                reply.Status <- Ok
             else
                reply.Error <- error
        reply

let satisfy f        = satisfyE f NoErrorMessages
let satisfyL f label = satisfyE f (expected label)

let skipSatisfy f        = skipSatisfyE f NoErrorMessages
let skipSatisfyL f label = skipSatisfyE f (expected label)


let private charsToString (chars: seq<char>) =
    match chars with
    | :? string as str -> str
    | _ -> new string(Array.ofSeq chars)

let isAnyOf (chars: seq<char>) =
#if LOW_TRUST
    let cs = new CharSet(charsToString chars)
    fun c -> cs.Contains(c)
#else
    #if USE_STATIC_MAPPING_FOR_IS_ANY_OF
         StaticMapping.createStaticCharIndicatorFunction false chars
    #else
        let cs = new CharSet(charsToString chars)
        fun c -> cs.Contains(c)
    #endif
#endif

let isNoneOf (chars: seq<char>) =
#if LOW_TRUST
    let cs = new CharSet(charsToString chars)
    fun c -> not (cs.Contains(c))
#else
    #if USE_STATIC_MAPPING_FOR_IS_ANY_OF
        StaticMapping.createStaticCharIndicatorFunction true chars
    #else
        let cs = new CharSet(charsToString chars)
        fun c -> not (cs.Contains(c))
    #endif
#endif

let anyOf (chars: seq<char>) =
    let str = charsToString chars
    satisfyE (isAnyOf str) (Errors.ExpectedAnyCharIn(str))

let skipAnyOf (chars: seq<char>) =
    let str = charsToString chars
    skipSatisfyE (isAnyOf str) (Errors.ExpectedAnyCharIn(str))

let noneOf (chars: seq<char>) =
    let str = charsToString chars
    satisfyE (isNoneOf str) (Errors.ExpectedAnyCharNotIn(str))

let skipNoneOf (chars: seq<char>) =
    let str = charsToString chars
    skipSatisfyE (isNoneOf str) (Errors.ExpectedAnyCharNotIn(str))

let inline isAsciiUpper (c: char) =
    uint32 c - uint32 'A' <= uint32 'Z' - uint32 'A'

let inline isAsciiLower (c: char) = 
    uint32 c - uint32 'a' <= uint32 'z' - uint32 'a'

let inline isAsciiLetter (c: char) = 
    let cc = uint32 c ||| uint32 ' '
    cc - uint32 'a' <= uint32 'z' - uint32 'a'

let inline isUpper (c: char) =
    isAsciiUpper c || (c > '\u007F' && System.Char.IsUpper(c))

let inline isLower (c: char) =
    isAsciiLower c || (c > '\u007F' && System.Char.IsLower(c))

let inline isLetter (c: char) =
    isAsciiLetter c || (c > '\u007F' && System.Char.IsLetter(c))

let inline isDigit (c: char) =
    uint32 c - uint32 '0' <= uint32 '9' - uint32 '0'

let inline isHex (c: char) =
    let cc = uint32 c ||| uint32 ' '
    isDigit c || cc - uint32 'a' <= uint32 'f' - uint32 'a'

let inline isOctal (c: char) =
    uint32 c - uint32 '0' <= uint32 '7' - uint32 '0'

let asciiUpper  stream = fastInlineSatisfyE isAsciiUpper  Errors.ExpectedAsciiUppercaseLetter stream
let asciiLower  stream = fastInlineSatisfyE isAsciiLower  Errors.ExpectedAsciiLowercaseLetter stream
let asciiLetter stream = fastInlineSatisfyE isAsciiLetter Errors.ExpectedAsciiLetter stream

// unicode is the default for letters and ascii the default for numbers
let upper  stream = fastInlineSatisfyE isUpper  Errors.ExpectedUppercaseLetter stream
let lower  stream = fastInlineSatisfyE isLower  Errors.ExpectedLowercaseLetter stream
let letter stream = fastInlineSatisfyE isLetter Errors.ExpectedLetter          stream

let digit  stream = fastInlineSatisfyE isDigit  Errors.ExpectedDecimalDigit     stream
let hex    stream = fastInlineSatisfyE isHex    Errors.ExpectedHexadecimalDigit stream
let octal  stream = fastInlineSatisfyE isOctal  Errors.ExpectedOctalDigit       stream

let tab stream = fastInlineSatisfyE ((=) '\t') Errors.ExpectedTab stream

let spaces : Parser<unit,'u> =
    fun stream ->
        stream.SkipWhitespace() |> ignore
        Reply(())

let spaces1 : Parser<unit,'u> =
    fun stream ->
        if stream.SkipWhitespace() then Reply(())
        else Reply(Error, Errors.ExpectedWhitespace)

let unicodeSpaces : Parser<unit,'u> =
    fun stream ->
        stream.SkipUnicodeWhitespace() |> ignore
        Reply(())

let unicodeSpaces1 : Parser<unit,'u> =
    fun stream ->
        if stream.SkipUnicodeWhitespace() then Reply(())
        else Reply(Error, Errors.ExpectedWhitespace)

let eof : Parser<unit,'u>=
    fun stream ->
        if stream.IsEndOfStream then Reply(())
        else Reply(Error, Errors.ExpectedEndOfInput)


// ------------------------
// Parsing strings directly
// ------------------------

let internal newlineOrEOSCharInStringArg name (arg: string) i =
    let msg2 = match arg.[i] with
               |'\r'|'\n' -> " may not contain newline chars ('\r' or '\n')."
               | EOS      -> " may not contain the char '\uffff' (EOS)"
               | _        -> failwith "newlineOrEOSCharInStringArg"
    raise (System.ArgumentException(concat3 "The string argument to " name msg2))

let internal checkStringContainsNoNewlineOrEOSChar s name =
    let i = findNewlineOrEOSChar s
    if i >= 0 then newlineOrEOSCharInStringArg name s i

let stringReturn s result : Parser<'a,'u> =
    let inline checkNoNewlineOrEOSChar c i =
        if not (isCertainlyNoNLOrEOS c) then
            match c with
            |'\r'|'\n'|EOS -> newlineOrEOSCharInStringArg "pstring/skipString/stringReturn" s i
            | _ -> ()

    let error = expectedString s
    match s.Length with
    | 0 -> preturn result
    | 1 ->
        let c = s.[0]
        checkNoNewlineOrEOSChar c 0
        charReturnE c result error
    | 2 ->
        let c0, c1 = s.[0], s.[1]
        checkNoNewlineOrEOSChar c0 0
        checkNoNewlineOrEOSChar c1 1
        let cs = TwoChars(c0, c1)
        fun stream ->
            if stream.Skip(cs) then Reply(result)
            else Reply(Error, error)
    | _ ->
        checkStringContainsNoNewlineOrEOSChar s "pstring/skipString/stringReturn"
        fun stream ->
            if stream.Skip(s) then Reply(result)
            else Reply(Error, error)

let pstring s    = stringReturn s s
let skipString s = stringReturn s ()

let pstringCI s : Parser<string,'u> =
    checkStringContainsNoNewlineOrEOSChar s "pstringCI"
    let error = expectedStringCI s
    let cfs = foldCase s
    fun stream ->
        let index0 = stream.IndexToken
        if stream.SkipCaseFolded(cfs) then
             Reply(stream.ReadFrom(index0))
        else Reply(Error, error)

let stringCIReturn (s: string) result : Parser<'a,'u> =
    let error = expectedStringCI s
    if s.Length = 1 then
        let c = s.[0]
        if not (isCertainlyNoNLOrEOS c) then
            match c with '\r'|'\n'|EOS -> newlineOrEOSCharInStringArg "skipStringCI/stringCIReturn"  s 0 | _ -> ()
        let cfc = Text.FoldCase(c)
        fun stream ->
            if stream.SkipCaseFolded(cfc) then Reply(result)
            else Reply(Error, error)
    else
        checkStringContainsNoNewlineOrEOSChar s "skipStringCI/stringCIReturn"
        let cfs = foldCase s
        fun stream ->
            if stream.SkipCaseFolded(cfs) then Reply(result)
            else Reply(Error, error)

let skipStringCI s = stringCIReturn s ()


let anyString n : Parser<string,'u> =
    let error = Errors.ExpectedAnySequenceOfNChars(n)
    fun stream ->
        let state = stream.State
        let str = stream.ReadCharsOrNewlines(n, true)
        if str.Length = n then Reply(str)
        else
            stream.BacktrackTo(state)
            Reply(Error, error)

let skipAnyString n : Parser<unit,'u> =
    let error = Errors.ExpectedAnySequenceOfNChars(n)
    fun stream ->
        let state = stream.State
        if stream.SkipCharsOrNewlines(n) = n then Reply(())
        else
            stream.BacktrackTo(state)
            Reply(Error, error)

let restOfLine skipNewline : Parser<_,_> =
    fun stream ->
        Reply(stream.ReadRestOfLine(skipNewline))

let skipRestOfLine skipNewline : Parser<_,_> =
    fun stream ->
         stream.SkipRestOfLine(skipNewline)
         Reply(())

let charsTillString (s: string) skipString maxCount : Parser<string,'u> =
    checkStringContainsNoNewlineOrEOSChar s "charsTillString"
    if maxCount < 0 then raise (System.ArgumentOutOfRangeException("maxCount", "maxCount is negative."))
    let error = Errors.CouldNotFindString(s)
    fun stream ->
        let mutable charsBeforeString = null
        stream.SkipCharsOrNewlinesUntilString(s, maxCount, true, &charsBeforeString) |> ignore
        if isNotNull charsBeforeString then
            if skipString then stream.Skip(s.Length)
            Reply(charsBeforeString)
        else
            Reply(Error, error)

let charsTillStringCI (s: string) skipString maxCount : Parser<string,'u> =
    checkStringContainsNoNewlineOrEOSChar s "charsTillStringCI"
    if maxCount < 0 then raise (System.ArgumentOutOfRangeException("maxCount", "maxCount is negative."))
    let cfs = foldCase s
    let error = Errors.CouldNotFindCaseInsensitiveString(s)
    fun stream ->
        let mutable charsBeforeString = null
        stream.SkipCharsOrNewlinesUntilCaseFoldedString(cfs, maxCount, true, &charsBeforeString) |> ignore
        if isNotNull charsBeforeString then
            if skipString then stream.Skip(s.Length)
            Reply(charsBeforeString)
        else
            Reply(Error, error)


let skipCharsTillString (s: string) skipString maxCount : Parser<unit,'u> =
    checkStringContainsNoNewlineOrEOSChar s "skipCharsTillString"
    if maxCount < 0 then raise (System.ArgumentOutOfRangeException("maxCount", "maxCount is negative."))
    let error = Errors.CouldNotFindString(s)
    fun stream ->
        let mutable foundString = false
        stream.SkipCharsOrNewlinesUntilString(s, maxCount, &foundString) |> ignore
        if foundString then
            if skipString then stream.Skip(s.Length)
            Reply(())
        else
            Reply(Error, error)

let skipCharsTillStringCI (s: string) skipString maxCount : Parser<unit,'u> =
    checkStringContainsNoNewlineOrEOSChar s "skipCharsTillStringCI"
    if maxCount < 0 then raise (System.ArgumentOutOfRangeException("maxCount", "maxCount is negative."))
    let cfs = foldCase s
    let error = Errors.CouldNotFindCaseInsensitiveString(s)
    fun stream ->
        let mutable foundString = false
        stream.SkipCharsOrNewlinesUntilCaseFoldedString(cfs, maxCount, &foundString) |> ignore
        if foundString then
            if skipString then stream.Skip(s.Length)
            Reply(())
        else
            Reply(Error, error)

let
#if NOINLINE
#else
    inline
#endif
           internal manySatisfyImpl require1 (f1: char -> bool) (f: char -> bool) error : Parser<string,'u> =
    fun stream ->
        let str = stream.ReadCharsOrNewlinesWhile(f1, f, true)
        if not require1 || str.Length <> 0 then Reply(str)
        else Reply(Error, error)

let
#if NOINLINE
#else
    inline
#endif
           internal skipManySatisfyImpl require1 (f1: char -> bool) (f: char -> bool) error : Parser<unit,'u> =
    fun stream ->
        let n = stream.SkipCharsOrNewlinesWhile(f1, f)
        if not require1 || n <> 0 then Reply(())
        else Reply(Error, error)

let manySatisfy2   f1 f       = manySatisfyImpl false f1 f NoErrorMessages
let many1Satisfy2  f1 f       = manySatisfyImpl true  f1 f NoErrorMessages
let many1Satisfy2L f1 f label = manySatisfyImpl true  f1 f (expected label)

let skipManySatisfy2   f1 f       = skipManySatisfyImpl false f1 f NoErrorMessages
let skipMany1Satisfy2  f1 f       = skipManySatisfyImpl true  f1 f NoErrorMessages
let skipMany1Satisfy2L f1 f label = skipManySatisfyImpl true  f1 f (expected label)

let manySatisfy   f       = manySatisfy2   f f
let many1Satisfy  f       = many1Satisfy2  f f
let many1SatisfyL f label = many1Satisfy2L f f label

let skipManySatisfy   f       = skipManySatisfy2   f f
let skipMany1Satisfy  f       = skipMany1Satisfy2  f f
let skipMany1SatisfyL f label = skipMany1Satisfy2L f f label


let internal manyMinMaxSatisfy2E minCount maxCount f1 f error : Parser<string,'u> =
    if maxCount < 0 then raise (System.ArgumentOutOfRangeException("maxCount", "maxCount is negative."))
    if minCount > 0 then
        fun stream ->
            let str = stream.ReadCharsOrNewlinesWhile(f1, f, minCount, maxCount, true)
            if str.Length <> 0 then Reply(str)
            else Reply(Error, error)
    else
        fun stream ->
            Reply(stream.ReadCharsOrNewlinesWhile(f1, f, 0, maxCount, true))

let internal skipManyMinMaxSatisfy2E minCount maxCount f1 f error : Parser<unit,'u> =
    if maxCount < 0 then raise (System.ArgumentOutOfRangeException("maxCount", "maxCount is negative."))
    if minCount > 0 then
        fun stream ->
            let n = stream.SkipCharsOrNewlinesWhile(f1, f, minCount, maxCount)
            if n <> 0 then Reply(())
            else Reply(Error, error)
    else
        fun stream ->
            stream.SkipCharsOrNewlinesWhile(f1, f, 0, maxCount) |> ignore
            Reply(())

let manyMinMaxSatisfy   minCount maxCount    f       = manyMinMaxSatisfy2E minCount maxCount f  f NoErrorMessages
let manyMinMaxSatisfyL  minCount maxCount    f label = manyMinMaxSatisfy2E minCount maxCount f  f (expected label)
let manyMinMaxSatisfy2  minCount maxCount f1 f       = manyMinMaxSatisfy2E minCount maxCount f1 f NoErrorMessages
let manyMinMaxSatisfy2L minCount maxCount f1 f label = manyMinMaxSatisfy2E minCount maxCount f1 f (expected label)

let skipManyMinMaxSatisfy   minCount maxCount    f       = skipManyMinMaxSatisfy2E minCount maxCount f  f NoErrorMessages
let skipManyMinMaxSatisfyL  minCount maxCount    f label = skipManyMinMaxSatisfy2E minCount maxCount f  f (expected label)
let skipManyMinMaxSatisfy2  minCount maxCount f1 f       = skipManyMinMaxSatisfy2E minCount maxCount f1 f NoErrorMessages
let skipManyMinMaxSatisfy2L minCount maxCount f1 f label = skipManyMinMaxSatisfy2E minCount maxCount f1 f (expected label)


let internal regexE pattern error : Parser<string,'u> =
    let regex = new Regex("\\A" + pattern, RegexOptions.Multiline |||
                                           RegexOptions.ExplicitCapture)
    fun stream ->
        let m = stream.Match(regex)
        if m.Success then
            let str = m.Value
            if findNewlineOrEOSChar str < 0 then
                if str.Length <> 0 then stream.Skip(str.Length)
                Reply(str)
            else
                let nStr = normalizeNewlines str
                let mutable nSkippedChars = 0
                let n = stream.SkipCharsOrNewlines(nStr.Length)
                if n = nStr.Length then Reply(nStr)
                else Reply(FatalError, messageError "Internal error in the regex parser. Please report this error to fparsec@quanttec.com.")
        else Reply(Error, error)

let regex  pattern       = regexE pattern (Errors.ExpectedStringMatchingRegex(pattern))
let regexL pattern label = regexE pattern (expected label)

type private IdFlags = IdentifierValidator.IdentifierCharFlags

type IdentifierOptions(?isAsciiIdStart, ?isAsciiIdContinue,
                   #if SILVERLIGHT
                   #else
                       ?normalization,
                       ?normalizeBeforeValidation,
                   #endif
                       ?allowJoinControlChars, ?preCheckStart, ?preCheckContinue, ?allowAllNonAsciiCharsInPreCheck, ?label, ?invalidCharMessage) =
    // we use match instead of defaultArg here, so that the function wrapper objects only get constructed when needed
    let isAsciiIdStart    = match isAsciiIdStart    with Some v -> v | _ -> IdentifierValidator.IsXIdStartOrSurrogate
    let isAsciiIdContinue = match isAsciiIdContinue with Some v -> v | _ -> IdentifierValidator.IsXIdContinueOrSurrogate
#if SILVERLIGHT
#else
    let normalizationForm = defaultArg normalization (enum<NormalizationForm> 0)
    let normalizeBeforeValidation = defaultArg normalizeBeforeValidation false
#endif
    let allowJoinControlChars = defaultArg allowJoinControlChars false
    let expectedIdentifierError = expected (defaultArg label Strings.Identifier)
    let invalidCharError = messageError (defaultArg invalidCharMessage Strings.IdentifierContainsInvalidCharacterAtIndicatedPosition)
    let allowAllNonAsciiCharsInPreCheck = defaultArg allowAllNonAsciiCharsInPreCheck false

    let preCheckStart = if preCheckStart.IsSome then preCheckStart.Value
                        elif allowAllNonAsciiCharsInPreCheck then isAsciiIdStart
                        else Unchecked.defaultof<_>
    let preCheckContinue = if preCheckContinue.IsSome then preCheckContinue.Value
                           elif allowAllNonAsciiCharsInPreCheck then isAsciiIdContinue
                           else Unchecked.defaultof<_>

    let asciiOptions = Array.zeroCreate 128
    do for i = 1 to 127 do
          let c = char i
          let mutable v = IdFlags.None
          if isAsciiIdStart c then v <- v ||| IdFlags.NonContinue
          if isAsciiIdContinue c then v <- v ||| IdFlags.Continue
          if allowAllNonAsciiCharsInPreCheck then
             if preCheckStart c then v <- v ||| IdFlags.PreCheckNonContinue
             if preCheckContinue c then v <- v ||| IdFlags.PreCheckContinue
          asciiOptions.[i] <- v

    let iv = new IdentifierValidator(asciiOptions)
    do
    #if SILVERLIGHT
    #else
       iv.NormalizationForm <- normalizationForm
       iv.NormalizeBeforeValidation <- normalizeBeforeValidation
    #endif
       iv.AllowJoinControlCharsAsIdContinueChars <- allowJoinControlChars

    let preCheck1 =
        if allowAllNonAsciiCharsInPreCheck then
            fun c -> let i = int c
                     if i <= 0x7f then
                         // not (x = y) currently yields better code here than (x <> y)
                         not (asciiOptions.[int c] &&& IdFlags.PreCheckNonContinue = IdFlags.None)
                     else true
        elif isNotNull preCheckStart then preCheckStart
        else iv.IsIdStartOrSurrogateFunc

    let preCheck =
        if allowAllNonAsciiCharsInPreCheck then
            fun c -> let i = int c
                     if i <= 0x7f then
                         not (asciiOptions.[i] &&& IdFlags.PreCheckContinue = IdFlags.None)
                     else true
        elif isNotNull preCheckContinue then preCheckContinue
        else iv.IsIdContinueOrJoinControlOrSurrogateFunc

    member internal t.IdentifierValidator = iv
    member internal t.PreCheck1 = preCheck1
    member internal t.PreCheck  = preCheck
    member internal t.ExpectedIdentifierError = expectedIdentifierError
    member internal t.InvalidCharError = invalidCharError

let identifier (identifierOptions: IdentifierOptions) : Parser<string, _> =
    let validator = identifierOptions.IdentifierValidator
    let preCheck1 = identifierOptions.PreCheck1
    let preCheck  = identifierOptions.PreCheck
    let expectedIdentifierError = identifierOptions.ExpectedIdentifierError
    let invalidCharError = identifierOptions.InvalidCharError
    fun stream ->
        let str = stream.ReadCharsOrNewlinesWhile(preCheck1, preCheck, true)
        if str.Length <> 0 then
            let mutable errorPos = 0
            let nstr = validator.ValidateAndNormalize(str, &errorPos)
            if isNotNull nstr then Reply(nstr)
            else
                stream.Skip(errorPos - str.Length)
                Reply(FatalError, invalidCharError)
        else
            Reply(Error, expectedIdentifierError)

// ----------------------------------------------
// Parsing strings with the help of other parsers
// ----------------------------------------------


let manyChars2 p1 p = ManyChars(p1, p).AsFSharpFunc
let manyChars     p = manyChars2 p p

let many1Chars2 p1 p = Many1Chars(p1, p).AsFSharpFunc
let many1Chars     p = many1Chars2 p p

let manyCharsTillApply2 p1 p endp f = ManyCharsTill(p1, p, endp, f).AsFSharpFunc
let manyCharsTillApply     p endp f = manyCharsTillApply2 p p endp f
let manyCharsTill2      p1 p endp   = manyCharsTillApply2 p1 p endp (fun str _ -> str)
let manyCharsTill          p endp   = manyCharsTill2 p p endp

let many1CharsTillApply2 p1 p endp f = Many1CharsTill(p1, p, endp, f).AsFSharpFunc
let many1CharsTillApply     p endp f = many1CharsTillApply2 p p endp f
let many1CharsTill2      p1 p endp   = many1CharsTillApply2 p1 p endp (fun str _ -> str)
let many1CharsTill          p endp   = many1CharsTill2 p p endp



let
#if NOINLINE
#else
    inline
#endif
           internal manyStringsImpl require1 (p1: Parser<string,'u>) (p: Parser<string,'u>) : Parser<string,'u> =
    fun stream ->
        let mutable stateTag = stream.StateTag
        let mutable reply = p1 stream
        if reply.Status = Ok then
            let result1 = reply.Result
            let mutable error  = reply.Error
            stateTag <- stream.StateTag
            reply <- p stream
            if reply.Status <> Ok then reply.Result <- result1
            else
                let result2 = reply.Result
                error <- reply.Error
                stateTag <- stream.StateTag
                reply <- p stream
                if reply.Status <> Ok then reply.Result <- result1 + result2
                else
                    let result3 = reply.Result
                    error <- reply.Error
                    stateTag <- stream.StateTag
                    reply <- p stream
                    if reply.Status <> Ok then reply.Result <- concat3 result1 result2 result3
                    else
                        let result4 = reply.Result
                        error <- reply.Error
                        stateTag <- stream.StateTag
                        reply <- p stream
                        if reply.Status <> Ok then reply.Result <- concat4 result1 result2 result3 result4
                        else
                            let n = 2*(result1.Length + result2.Length + result3.Length + result4.Length) + reply.Result.Length
                            let sb = new StringBuilder(n)
                            sb.Append(result1).Append(result2).Append(result3).Append(result4).Append(reply.Result) |> ignore
                            error <- reply.Error
                            stateTag <- stream.StateTag
                            reply <- p stream
                            while reply.Status = Ok do
                                if stateTag = stream.StateTag then
                                    raiseInfiniteLoopException "manyStrings" stream
                                error <- reply.Error
                                sb.Append(reply.Result) |> ignore
                                stateTag <- stream.StateTag
                                reply <- p stream
                            reply.Result <- sb.ToString()
            // We assume that the string parser changes the state when it succeeds,
            // so we don't need to merge more than 2 error message lists.
            if stateTag = stream.StateTag then
                if reply.Status = Error then
                    reply.Status <- Ok
                if isNotNull error then
                    reply.Error <- mergeErrors error reply.Error
        elif not require1 && reply.Status = Error && stateTag = stream.StateTag then
            reply.Status <- Ok
            reply.Result <- ""
        reply

let manyStrings2 p1 p = manyStringsImpl false p1 p
let manyStrings p = manyStrings2 p p
let many1Strings2 p1 p = manyStringsImpl true p1 p
let many1Strings p = many1Strings2 p p

let stringsSepBy (p: Parser<string,'u>) (sep: Parser<string,'u>) : Parser<string,'u> =
    fun stream ->
        let mutable stateTag = stream.StateTag
        let mutable reply = p stream
        if reply.Status = Ok then
            let result1 = reply.Result
            let mutable error = reply.Error
            stateTag <- stream.StateTag
            reply <- sep stream
            if reply.Status <> Ok then
                if stateTag = stream.StateTag then
                    if reply.Status = Error then
                        reply.Status <- Ok
                        reply.Result <- result1
                    if isNotNull error then
                        reply.Error <- mergeErrors error reply.Error
            else
                // We assume that at least one of the parsers sep and p consume
                // input when both are called consecutively and succeed. This
                // way we only have to merge a maximum of 3 error message lists.
                let mutable result = null
                let mutable error0 = error
                let mutable stateTag0 = stateTag
                let result2 = reply.Result
                error <- reply.Error
                stateTag <- stream.StateTag
                reply <- p stream
                if reply.Status = Ok then
                    let result3 = reply.Result
                    error0 <- error
                    stateTag0 <- stateTag
                    error <- reply.Error
                    stateTag <- stream.StateTag
                    reply <- sep stream
                    if reply.Status <> Ok then result <- concat3 result1 result2 result3
                    else
                        let result4 = reply.Result
                        error0 <- error
                        stateTag0 <- stateTag
                        error <- reply.Error
                        stateTag <- stream.StateTag
                        reply <- p stream
                        if reply.Status = Ok then
                            let n = 2*(result1.Length + result2.Length + result3.Length + result4.Length) + reply.Result.Length
                            let sb = new StringBuilder(n)
                            sb.Append(result1).Append(result2).Append(result3).Append(result4) |> ignore
                            while reply.Status = Ok do
                                sb.Append(reply.Result) |> ignore
                                error0 <- error
                                stateTag0 <- stateTag
                                error <- reply.Error
                                stateTag <- stream.StateTag
                                reply <- sep stream
                                if reply.Status <> Ok then result <- sb.ToString()
                                else
                                    sb.Append(reply.Result) |> ignore
                                    if stateTag0 = stream.StateTag then
                                        raiseInfiniteLoopException "stringsSepBy" stream
                                    error0 <- error
                                    stateTag0 <- stateTag
                                    error <- reply.Error
                                    stateTag <- stream.StateTag
                                    reply <- p stream
                if stateTag = stream.StateTag then
                    if isNotNull result && reply.Status = Error then
                        reply.Status <- Ok
                        reply.Result <- result
                    error <- mergeErrors error reply.Error
                    if stateTag0 = stateTag then
                        error <- mergeErrors error0 error
                    reply.Error <- error
        elif reply.Status = Error && stateTag = stream.StateTag then
            reply.Status <- Ok
            reply.Result <- ""
        reply

let skipped (p: Parser<unit,'u>) : Parser<string,'u> =
    fun stream ->
        let index0 = stream.IndexToken
        let line0 = stream.Line
        let reply = p stream
        if reply.Status = Ok then
            let str = stream.ReadFrom(index0)
            let nstr = if line0 = stream.Line then str
                       else Text.NormalizeNewlines(str)
            Reply(Ok, nstr, reply.Error)
        else
            Reply(reply.Status, reply.Error)

let withSkippedString (f: string -> 'a -> 'b) (p: Parser<'a,'u>) : Parser<'b,'u> =
    let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
    fun stream ->
        let index0 = stream.IndexToken
        let line0 = stream.Line
        let reply = p stream
        if reply.Status = Ok then
            let str = stream.ReadFrom(index0)
            let nstr = if line0 = stream.Line then str
                       else Text.NormalizeNewlines(str)
            let result = optF.Invoke(nstr, reply.Result)
            Reply(Ok, result, reply.Error)
        else
            Reply(reply.Status, reply.Error)

// ---------------
// Parsing numbers
// ---------------

[<System.Flags>]
type NumberLiteralOptions =
     | None                             = 0
     | AllowSuffix                      = 0b000000000001
     | AllowMinusSign                   = 0b000000000010
     | AllowPlusSign                    = 0b000000000100
     | AllowFraction                    = 0b000000001000
     | AllowFractionWOIntegerPart       = 0b000000010000
     | AllowExponent                    = 0b000000100000
     | AllowHexadecimal                 = 0b000001000000
     | AllowBinary                      = 0b000010000000
     | AllowOctal                       = 0b000100000000
     | AllowInfinity                    = 0b001000000000
     | AllowNaN                         = 0b010000000000

     | IncludeSuffixCharsInString       = 0b100000000000

     | DefaultInteger                   = 0b000111000110
     | DefaultUnsignedInteger           = 0b000111000000
     | DefaultFloat                     = 0b011001101110

type internal NLO = NumberLiteralOptions

[<System.Flags>]
type NumberLiteralResultFlags =
     | None             = 0
     | SuffixLengthMask = 0b0000000000001111
     | HasMinusSign     = 0b0000000000010000
     | HasPlusSign      = 0b0000000000100000
     | HasIntegerPart   = 0b0000000001000000
     | HasFraction      = 0b0000000010000000
     | HasExponent      = 0b0000000100000000
     | IsDecimal        = 0b0000001000000000
     | IsHexadecimal    = 0b0000010000000000
     | IsBinary         = 0b0000100000000000
     | IsOctal          = 0b0001000000000000
     | BaseMask         = 0b0001111000000000
     | IsInfinity       = 0b0010000000000000
     | IsNaN            = 0b0100000000000000

type internal NLF = NumberLiteralResultFlags

type NumberLiteral(string, info, suffixChar1, suffixChar2, suffixChar3, suffixChar4) =
    member t.String = string

    member t.SuffixLength = int (info &&& NLF.SuffixLengthMask)
    member t.SuffixChar1  = suffixChar1
    member t.SuffixChar2  = suffixChar2
    member t.SuffixChar3  = suffixChar3
    member t.SuffixChar4  = suffixChar4

    member t.Info = info

    member t.HasMinusSign   = int (info &&& NLF.HasMinusSign) <> 0
    member t.HasPlusSign    = int (info &&& NLF.HasPlusSign) <> 0
    member t.HasIntegerPart = int (info &&& NLF.HasIntegerPart) <> 0
    member t.HasFraction    = int (info &&& NLF.HasFraction) <> 0
    member t.HasExponent    = int (info &&& NLF.HasExponent) <> 0
    member t.IsInteger      = int (info &&& (NLF.HasFraction ||| NLF.HasExponent)) = 0 // HasIntegerPart must be set if HasFraction and HasExponent both aren't
    member t.IsDecimal      = int (info &&& NLF.IsDecimal) <> 0
    member t.IsHexadecimal  = int (info &&& NLF.IsHexadecimal) <> 0
    member t.IsBinary       = int (info &&& NLF.IsBinary) <> 0
    member t.IsOctal        = int (info &&& NLF.IsOctal) <> 0
    member t.IsNaN          = int (info &&& NLF.IsNaN) <> 0
    member t.IsInfinity     = int (info &&& NLF.IsInfinity) <> 0

    override t.Equals(other: obj) =
        match other with
        | :? NumberLiteral as other ->
               t.String = other.String
            && t.Info = other.Info
            && t.SuffixChar1 = other.SuffixChar1
            && t.SuffixChar2 = other.SuffixChar2
            && t.SuffixChar3 = other.SuffixChar3
            && t.SuffixChar4 = other.SuffixChar4
        | _ -> false

    override t.GetHashCode() =
        if isNotNull string then string.GetHashCode() else 0

let numberLiteralE (opt: NumberLiteralOptions) (errorInCaseNoLiteralFound: ErrorMessageList) (stream: CharStream<'u>) =
    let index0 = stream.IndexToken
    let stateTag = stream.StateTag
    let mutable c = stream.Peek()
    let mutable error = NoErrorMessages
    let mutable flags = NLF.None

    if c = '-' && (opt &&& NLO.AllowMinusSign) <> NLO.None then
        flags <- NLF.HasMinusSign
        c <- stream.SkipAndPeek()
    elif c = '+' && (opt &&& NLO.AllowPlusSign) <> NLO.None then
        flags <- NLF.HasPlusSign
        c <- stream.SkipAndPeek()

    let allowStartingPoint = NLO.AllowFraction ||| NLO.AllowFractionWOIntegerPart // for starting point both flags are required

    if isDigit c || (c = '.' && (opt &&& allowStartingPoint) = allowStartingPoint) then
        let mutable c1  = '\u0000'
        if    c <> '0'
           || (c1 <- stream.SkipAndPeek();
                  c1 <= '9'
               || (opt &&& (NLO.AllowBinary ||| NLO.AllowOctal ||| NLO.AllowHexadecimal)) = NLO.None
               || ((int c1 ||| int ' ') = int 'e'))
        then
            flags <- flags ||| NLF.IsDecimal
            if c <> '.' then
                flags <- flags ||| NLF.HasIntegerPart
                if c <> '0' then
                    c <- stream.SkipAndPeek()
                else
                    c <- c1
                while isDigit c do
                    c <- stream.SkipAndPeek()
            if c = '.' && (opt &&& NLO.AllowFraction) <> NLO.None then
                flags <- flags ||| NLF.HasFraction
                c <- stream.SkipAndPeek()
                if isDigit c then
                    c <- stream.SkipAndPeek()
                elif (flags &&& NLF.HasIntegerPart) = NLF.None then
                    // at least one digit before or after the . is required
                    error <- Errors.ExpectedDecimalDigit
                while isDigit c do
                    c <- stream.SkipAndPeek()
            if (int c ||| int ' ') = int 'e' && isNull error && (opt &&& NLO.AllowExponent) <> NLO.None then
                flags <- flags ||| NLF.HasExponent
                c <- stream.SkipAndPeek()
                if c = '-' || c = '+' then
                    c <- stream.SkipAndPeek()
                if not (isDigit c) then
                    error <- Errors.ExpectedDecimalDigit
                while isDigit c do
                    c <- stream.SkipAndPeek()
        else
            match int c1 ||| int ' ' with
            | 0x78 (* 'x' *) when (opt &&& NLO.AllowHexadecimal) <> NLO.None ->
                flags <- flags ||| NLF.IsHexadecimal
                c <- stream.SkipAndPeek()
                if isHex c then
                    flags <- flags ||| NLF.HasIntegerPart
                    c <- stream.SkipAndPeek()
                elif (opt &&& NLO.AllowFractionWOIntegerPart) = NLO.None then
                    // integer part required
                    error <- Errors.ExpectedHexadecimalDigit
                while isHex c do
                    c <- stream.SkipAndPeek()
                if c = '.' && isNull error && (opt &&& NLO.AllowFraction) <> NLO.None then
                    flags <- flags ||| NLF.HasFraction
                    c <- stream.SkipAndPeek()
                    if isHex c then
                        c <- stream.SkipAndPeek()
                    elif (flags &&& NLF.HasIntegerPart) = NLF.None then
                        // at least one digit before or after the . is required
                        error <- Errors.ExpectedHexadecimalDigit
                    while isHex c do
                        c <- stream.SkipAndPeek()
                elif (flags &&& NLF.HasIntegerPart) = NLF.None then
                    // we neither have an integer part nor a fraction
                    error <- Errors.ExpectedHexadecimalDigit
                if (int c ||| int ' ') = int 'p' && isNull error && (opt &&& NLO.AllowExponent) <> NLO.None then
                    flags <- flags ||| NLF.HasExponent
                    c <- stream.SkipAndPeek()
                    if c = '-' || c = '+' then
                        c <- stream.SkipAndPeek()
                    if not (isDigit c) then
                        error <- Errors.ExpectedDecimalDigit
                    while isDigit c do
                        c <- stream.SkipAndPeek()
            | 0x6f (* 'o' *) when (opt &&& NLO.AllowOctal) <> NLO.None ->
                flags <- flags ||| NLF.IsOctal
                c <- stream.SkipAndPeek()
                if isOctal c then
                    flags <- flags ||| NLF.HasIntegerPart
                    c <- stream.SkipAndPeek()
                else
                    error <- Errors.ExpectedOctalDigit
                while isOctal c do
                    c <- stream.SkipAndPeek()
            | 0x62 (* 'b' *) when (opt &&& NLO.AllowBinary) <> NLO.None ->
                flags <- flags ||| NLF.IsBinary
                c <- stream.SkipAndPeek()
                if c = '0' || c = '1' then
                    flags <- flags ||| NLF.HasIntegerPart
                    c <- stream.SkipAndPeek()
                else
                    error <- Errors.ExpectedBinaryDigit
                while c = '0' || c = '1' do
                    c <- stream.SkipAndPeek()
            | _ ->
                flags <- flags ||| (NLF.IsDecimal ||| NLF.HasIntegerPart)
                c <- c1

        if isNull error then
            if (opt &&& NLO.AllowSuffix) = NLO.None  || not (isAsciiLetter c) then
                let str = stream.ReadFrom(index0)
                Reply(NumberLiteral(str, flags, EOS, EOS, EOS, EOS))
            else
                let mutable str = if (opt &&& NLO.IncludeSuffixCharsInString) <> NLO.None then null
                                  else stream.ReadFrom(index0)
                let mutable nSuffix = 1
                let mutable s1 = c
                let mutable s2 = EOS
                let mutable s3 = EOS
                let mutable s4 = EOS
                c <- stream.SkipAndPeek()
                if isAsciiLetter c then
                    nSuffix <- 2
                    s2 <- c
                    c <- stream.SkipAndPeek()
                    if isAsciiLetter c then
                        nSuffix <- 3
                        s3 <- c
                        c <- stream.SkipAndPeek()
                        if isAsciiLetter c then
                            nSuffix <- 4
                            s4 <- c
                            c <- stream.SkipAndPeek()
                flags <- flags ||| (enum) nSuffix
                if (opt &&& NLO.IncludeSuffixCharsInString) <> NLO.None then
                    str <- stream.ReadFrom(index0)
                Reply(NumberLiteral(str, flags, s1, s2, s3, s4))
        else
            Reply(Error, error)
    else
       let cc = int c ||| int ' '
       if
           if cc = int 'i' then
                 (opt &&& NLO.AllowInfinity) <> NLO.None
              && stream.SkipCaseFolded("inf") && (flags <- flags ||| NLF.IsInfinity
                                                  stream.SkipCaseFolded("inity") |> ignore
                                                  true)
           elif cc = int 'n' then
                   (opt &&& NLO.AllowNaN) <> NLO.None
                && stream.SkipCaseFolded("nan") && (flags <- flags ||| NLF.IsNaN
                                                    true)
           else false
       then
           let str = stream.ReadFrom(index0)
           Reply(NumberLiteral(str, flags, EOS, EOS, EOS, EOS))
       else
           if flags &&& (NLF.HasMinusSign |||  NLF.HasPlusSign) <> NLF.None then
              stream.Seek(index0)
              stream.StateTag <- stateTag
           Reply(Error, errorInCaseNoLiteralFound)

let numberLiteral opt label = numberLiteralE opt (expected label)

let pfloat : Parser<float,'u> =
    fun stream ->
        let reply = numberLiteralE NLO.DefaultFloat Errors.ExpectedFloatingPointNumber stream
        if reply.Status = Ok then
            let nl = reply.Result
            try
                let d = if nl.IsDecimal then
                            System.Double.Parse(nl.String, System.Globalization.CultureInfo.InvariantCulture)
                        elif nl.IsHexadecimal then
                            floatOfHexString nl.String
                        elif nl.IsInfinity then
                            if nl.HasMinusSign then System.Double.NegativeInfinity else System.Double.PositiveInfinity
                        else
                            System.Double.NaN
                Reply(d)
            with e ->
                let error = if   (e :? System.OverflowException) then Errors.NumberOutsideOfDoubleRange
                            elif (e :? System.FormatException) then messageError "The floating-point number has an invalid format (this error is unexpected, please report this error message to fparsec@quanttec.com)."
                            else reraise()
                stream.Skip(-nl.String.Length)
                Reply(FatalError, error)
        else
            Reply(reply.Status, reply.Error)

let internal parseUInt64 (c0: char) (stream: CharStream<'u>) (status: ReplyStatus byref) (error: ErrorMessageList byref) =
    Debug.Assert(isDigit c0 && (status = Ok))

    // we rely on the compiler eliminating inactive branches
    let opt = NumberLiteralOptions.DefaultUnsignedInteger
    let limit10  = 1844674407370955160UL //(System.UInt64.MaxValue - 9UL)/10UL
    let maxDiv10 = 1844674407370955161UL //System.UInt64.MaxValue/10UL
    let maxMod10 = 5u //System.UInt64.MaxValue%10UL

    let limit16  = 1152921504606846975UL //(System.UInt64.MaxValue - 15UL)/16UL
    let maxDiv16 = 1152921504606846975UL //System.UInt64.MaxValue/16UL
    let maxMod16 = 15u //System.UInt64.MaxValue%16UL

    let limit8  = 2305843009213693951UL  //(System.UInt64.MaxValue - 7UL)/8UL
    let maxDiv8 = 2305843009213693951UL //System.UInt64.MaxValue/8UL
    let maxMod8 = 7u //System.UInt64.MaxValue%8UL

    let limit2  = 9223372036854775807UL //(System.UInt64.MaxValue - 1UL)/2UL
    let maxDiv2 = 9223372036854775807UL //System.UInt64.MaxValue/2UL
    let maxMod2 = 1u //System.UInt64.MaxValue%2UL

    let mutable n = 0UL
    let mutable c = c0
    let c1 = stream.SkipAndPeek()

    if    (opt &&& (NLO.AllowBinary ||| NLO.AllowOctal ||| NLO.AllowHexadecimal)) = NLO.None
       || c <> '0' || c1 <= '9'
    then
        n <- uint64 (uint32 c - uint32 '0')
        c <- c1
        while c >= '0' && c <= '9' do
            let nc = uint32 c - uint32 '0'
            if n <= limit10 || (maxMod10 < 9u && n = maxDiv10 && nc <= maxMod10) then
                n <- 10UL*n + uint64 nc
                c <- stream.SkipAndPeek()
            else
                status <- FatalError
                c <- '!' // break

    else
        let cc1 = uint32 c1 ||| uint32 ' '
        if (opt &&& NLO.AllowHexadecimal) <> NLO.None && cc1 = uint32 'x' then
            c <- stream.SkipAndPeek()
            let mutable nc = uint32 0
            if  (let cc = uint32 c ||| uint32 ' '
                 if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                 else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a')) // 0x57u = uint32 'a' - 10u
            then
                n <- uint64 nc
                c <- stream.SkipAndPeek()
                while
                    (let cc = uint32 c ||| uint32 ' '
                     if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                     else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a'))
                  do
                    if n <= limit16 || (maxMod16 < 15u && n = maxDiv16 && nc <= maxMod16) then
                        n <- 16UL*n + uint64 nc
                        c <- stream.SkipAndPeek()
                    else
                        status <- FatalError
                        c <- '!' // break
            else
                status <- Error
                error <- Errors.ExpectedHexadecimalDigit

        elif (opt &&& NLO.AllowOctal) <> NLO.None && cc1 = uint32 'o' then
            c <- stream.SkipAndPeek()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 7u) then
                n <- uint64 nc
                c <- stream.SkipAndPeek()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 7u) do
                    if n <= limit8 || (maxMod8 < 7u && n = maxDiv8 && nc <= maxMod8) then
                        n <- 8UL*n + uint64 nc
                        c <- stream.SkipAndPeek()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- Errors.ExpectedOctalDigit

        elif (opt &&& NLO.AllowBinary) <> NLO.None && cc1 = uint32 'b' then
            c <- stream.SkipAndPeek()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 1u) then
                n <- uint64 nc
                c <- stream.SkipAndPeek()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 1u) do
                    if n <= limit2 || (maxMod2 = 0u && n = maxDiv2 && nc = 0u) then
                        n <- 2UL*n + uint64 nc
                        c <- stream.SkipAndPeek()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- Errors.ExpectedBinaryDigit
        // else c = 0 && not (isDigit c1)
    n

let internal parseUInt32 (c0: char) (stream: CharStream<'u>) (status: ReplyStatus byref) (error: ErrorMessageList byref) =
    Debug.Assert(isDigit c0 && (status = Ok))

    // we rely on the compiler eliminating inactive branches
    let opt = NumberLiteralOptions.DefaultUnsignedInteger
    let limit10  = 429496728u  //(System.UInt32.MaxValue - 9u)/10u
    let maxDiv10 = 429496729u //System.UInt32.MaxValue/10u
    let maxMod10 = 5u //System.UInt32.MaxValue%10u

    let limit16  = 268435455u  //(System.UInt32.MaxValue - 15u)/16u
    let maxDiv16 = 268435455u //System.UInt32.MaxValue/16u
    let maxMod16 = 15u //System.UInt32.MaxValue%16u

    let limit8  = 536870911u  //(System.UInt32.MaxValue - 7u)/8u
    let maxDiv8 = 536870911u //System.UInt32.MaxValue/8u
    let maxMod8 = 7u //System.UInt32.MaxValue%8u

    let limit2  = 2147483647u //(System.UInt32.MaxValue - 1u)/2u
    let maxDiv2 = 2147483647u //System.UInt32.MaxValue/2u
    let maxMod2 = 1u //System.UInt32.MaxValue%2u

    let mutable n = 0u
    let mutable c = c0
    let c1 = stream.SkipAndPeek()

    if    (opt &&& (NLO.AllowBinary ||| NLO.AllowOctal ||| NLO.AllowHexadecimal)) = NLO.None
       || c <> '0' || c1 <= '9'
    then
        n <- uint32 c - uint32 '0'
        c <- c1
        while c >= '0' && c <= '9' do
            let nc = uint32 c - uint32 '0'
            if n <= limit10 || (maxMod10 < 9u && n = maxDiv10 && nc <= maxMod10) then
                n <- 10u*n + nc
                c <- stream.SkipAndPeek()
            else
                status <- FatalError
                c <- '!' // break

    else
        let cc1 = uint32 c1 ||| uint32 ' '
        if (opt &&& NLO.AllowHexadecimal) <> NLO.None && cc1 = uint32 'x' then
            c <- stream.SkipAndPeek()
            let mutable nc = uint32 0
            if  (let cc = uint32 c ||| uint32 ' '
                 if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                 else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a')) // 0x57u = uint32 'a' - 10u
            then
                n <- uint32 nc
                c <- stream.SkipAndPeek()
                while
                    (let cc = uint32 c ||| uint32 ' '
                     if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                     else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a'))
                  do
                    if n <= limit16 || (maxMod16 < 15u && n = maxDiv16 && nc <= maxMod16) then
                        n <- 16u*n + nc
                        c <- stream.SkipAndPeek()
                    else
                        status <- FatalError
                        c <- '!' // break
            else
                status <- Error
                error <- Errors.ExpectedHexadecimalDigit

        elif (opt &&& NLO.AllowOctal) <> NLO.None && cc1 = uint32 'o' then
            c <- stream.SkipAndPeek()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 7u) then
                n <- uint32 nc
                c <- stream.SkipAndPeek()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 7u) do
                    if n <= limit8 || (maxMod8 < 7u && n = maxDiv8 && nc <= maxMod8) then
                        n <- 8u*n + nc
                        c <- stream.SkipAndPeek()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- Errors.ExpectedOctalDigit

        elif (opt &&& NLO.AllowBinary) <> NLO.None && cc1 = uint32 'b' then
            c <- stream.SkipAndPeek()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 1u) then
                n <- uint32 nc
                c <- stream.SkipAndPeek()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 1u) do
                    if n <= limit2 || (maxMod2 = 0u && n = maxDiv2 && nc = 0u) then
                        n <- 2u*n + nc
                        c <- stream.SkipAndPeek()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- Errors.ExpectedBinaryDigit
        // else c = 0 && not (isDigit c1)
    n

[<MethodImplAttribute(MethodImplOptions.NoInlining)>]
let internal overflowError message =
    if isNotNull message then messageError message // isNotNull prevents fsc from inlining the function
    else NoErrorMessages

let inline internal pint (opt: NumberLiteralOptions) (max: 'uint) (uint64_: 'uint -> uint64) (uint: int -> 'uint) (uint_: uint32 -> 'uint) (uint__: uint64 -> 'uint) (int: 'uint -> 'int) (int_: int -> 'int) (errorInCaseNoLiteralFound: ErrorMessageList) (outOfRangeError: ErrorMessageList) (stream: CharStream<'u>) =
    // we rely on the compiler eliminating inactive branches after inlining

    let minusIsAllowed = (opt &&& NLO.AllowMinusSign) <> NLO.None

    let index = stream.IndexToken
    let stateTag = stream.StateTag
    let mutable c = stream.Peek()

    let mutable plusMinus1  = 1
    let mutable signPresent = false
    if minusIsAllowed && c = '-' then
        plusMinus1 <- -1
        signPresent <- true
        c <- stream.SkipAndPeek()
    elif (opt &&& NLO.AllowPlusSign) <> NLO.None && c = '+' then
        signPresent <- true
        c <- stream.SkipAndPeek()

    let mutable status = Ok
    let mutable error = NoErrorMessages
    let mutable result = Unchecked.defaultof<_>
    if c >= '0' && c <= '9' then
        let n = if uint64_ max <= uint64 System.UInt32.MaxValue then
                    uint_  (parseUInt32 c stream (&status) (&error))
                else
                    uint__ (parseUInt64 c stream (&status) (&error))
        let isUInt32Or64 = uint64_ max = uint64 System.UInt32.MaxValue || uint64_ max = System.UInt64.MaxValue
        if status = Ok && (isUInt32Or64 || (n <= max || (minusIsAllowed && plusMinus1 = -1 && n = max + uint 1))) then
            result <- if minusIsAllowed then int_ plusMinus1 * int n else int n
        elif status <> Error then
            status <- FatalError
            stream.Seek(index)
            stream.StateTag <- stateTag
            error <- outOfRangeError
    else
        status <- Error
        error <- errorInCaseNoLiteralFound
        if signPresent then
            stream.Seek(index)
            stream.StateTag <- stateTag
    Reply(status, result, error)

let pint64 stream = pint NumberLiteralOptions.DefaultInteger (uint64 System.Int64.MaxValue)            uint64 uint64 uint64 uint64 int64 int64 Errors.ExpectedInt64 Errors.NumberOutsideOfInt64Range stream
let pint32 stream = pint NumberLiteralOptions.DefaultInteger (uint32 System.Int32.MaxValue)            uint64 uint32 uint32 uint32 int32 int32 Errors.ExpectedInt32 Errors.NumberOutsideOfInt32Range stream
                                                           // fsc's optimizer seems to have problems with literals of small int types
let pint16 stream = pint NumberLiteralOptions.DefaultInteger ((*uint32 System.Int16.MaxValue*)0x7fffu) uint64 uint32 uint32 uint32 int16 int16 Errors.ExpectedInt16 Errors.NumberOutsideOfInt16Range stream
let pint8  stream = pint NumberLiteralOptions.DefaultInteger ((*uint32 System.SByte.MaxValue*)0x7fu)   uint64 uint32 uint32 uint32 sbyte sbyte Errors.ExpectedInt8  Errors.NumberOutsideOfInt8Range stream

let puint64 stream = pint NumberLiteralOptions.DefaultUnsignedInteger System.UInt64.MaxValue uint64 uint64 uint64 uint64 uint64 uint64 Errors.ExpectedUInt64 Errors.NumberOutsideOfUInt64Range stream
let puint32 stream = pint NumberLiteralOptions.DefaultUnsignedInteger System.UInt32.MaxValue uint64 uint32 uint32 uint32 uint32 uint32 Errors.ExpectedUInt32 Errors.NumberOutsideOfUInt32Range stream
let puint16 stream = pint NumberLiteralOptions.DefaultUnsignedInteger 0xffffu                uint64 uint32 uint32 uint32 uint16 uint16 Errors.ExpectedUInt16 Errors.NumberOutsideOfUInt16Range stream
let puint8  stream = pint NumberLiteralOptions.DefaultUnsignedInteger 0xffu                  uint64 uint32 uint32 uint32 byte   byte   Errors.ExpectedUInt8  Errors.NumberOutsideOfUInt8Range stream



// -------------------
// Conditional parsing
// -------------------

let notFollowedByEof : Parser<unit,'u> =
    fun stream ->
        if not (stream.IsEndOfStream) then Reply(())
        else Reply(Error, Errors.UnexpectedEndOfInput)

let followedByNewline : Parser<unit,'u> =
    fun stream ->
        match stream.Peek() with
        |'\r' | '\n' -> Reply(())
        | _ -> Reply(Error, Errors.ExpectedNewline)

let notFollowedByNewline : Parser<unit,'u> =
    fun stream ->
        match stream.Peek() with
        |'\r' | '\n' -> Reply(Error, Errors.UnexpectedNewline)
        | _ -> Reply(())

let followedByString (str: string) : Parser<unit,'u> =
    checkStringContainsNoNewlineOrEOSChar str "followedByString"
    let error = expectedString str
    if str.Length = 1 then
        let chr = str.[0]
        fun stream ->
            if stream.Match(chr) then Reply(())
            else Reply(Error, error)
    else
        fun stream ->
            if stream.Match(str) then Reply(())
            else Reply(Error, error)

let followedByStringCI str : Parser<unit,'u> =
    checkStringContainsNoNewlineOrEOSChar str "followedByStringCI"
    let error = expectedStringCI str
    if str.Length = 1 then
        let cfChr = Text.FoldCase(str.[0])
        fun stream ->
            if stream.MatchCaseFolded(cfChr) then Reply(())
            else Reply(Error, error)
    else
        let cfStr = foldCase str
        fun stream ->
            if stream.MatchCaseFolded(cfStr) then Reply(())
            else Reply(Error, error)

let notFollowedByString str : Parser<unit,'u> =
    checkStringContainsNoNewlineOrEOSChar str "notFollowedByString"
    let error = unexpectedString str
    if str.Length = 1 then
        let chr = str.[0]
        fun stream ->
            if not (stream.Match(chr)) then Reply(())
            else Reply(Error, error)
    else
        fun stream ->
            if not (stream.Match(str)) then Reply(())
            else Reply(Error, error)

let notFollowedByStringCI str : Parser<unit,'u> =
    checkStringContainsNoNewlineOrEOSChar str "notFollowedByStringCI"
    let error = unexpectedStringCI str
    if str.Length = 1 then
        let cfChr = Text.FoldCase(str.[0])
        fun stream ->
            if not (stream.MatchCaseFolded(cfChr)) then Reply(())
            else Reply(Error, error)
    else
        let cfStr = foldCase str
        fun stream ->
            if not (stream.MatchCaseFolded(cfStr)) then Reply(())
            else Reply(Error, error)


let inline private charDoesSatisfy f c =
    match c with
    | EOS -> Error
    | _ -> if f (if c <> '\r' then c else '\n') then Ok else Error

let inline private charDoesSatisfyNot f c =
    match c with
    | EOS -> Ok
    | _ -> if not (f (if c <> '\r' then c else '\n')) then Ok else Error

let previousCharSatisfies f : Parser<unit,'u> =
    fun stream ->
        let status = charDoesSatisfy f (stream.Peek(-1))
        Reply(status, (), NoErrorMessages)

let previousCharSatisfiesNot f : Parser<unit,'u> =
    fun stream ->
        let status = charDoesSatisfyNot f (stream.Peek(-1))
        Reply(status, (), NoErrorMessages)

let nextCharSatisfies f : Parser<unit,'u> =
    fun stream ->
        let status = charDoesSatisfy f (stream.Peek())
        Reply(status, (), NoErrorMessages)

let nextCharSatisfiesNot f : Parser<unit,'u> =
    fun stream ->
        let status = charDoesSatisfyNot f (stream.Peek())
        Reply(status, (), NoErrorMessages)

let next2CharsSatisfy f : Parser<unit,'u> =
    let optF = OptimizedClosures.FSharpFunc<char, char, bool>.Adapt(f)
    fun stream ->
        let cs = stream.Peek2()
        let status = match cs.Char0, cs.Char1 with
                     | _, EOS
                     | EOS, _ -> Error
                     | '\r', '\n' ->
                         match stream.Peek(2u) with
                         | EOS -> Error
                         | c1 -> if optF.Invoke('\n', if c1 <> '\r' then c1 else '\n')
                                 then Ok else Error
                     | c0, c1 ->
                         if optF.Invoke((if c0 <> '\r' then c0 else '\n'),
                                        (if c1 <> '\r' then c1 else '\n'))
                         then Ok else Error
        Reply(status, (), NoErrorMessages)

let next2CharsSatisfyNot f : Parser<unit,'u> =
    let optF = OptimizedClosures.FSharpFunc<char, char, bool>.Adapt(f)
    fun stream ->
        let cs = stream.Peek2()
        let status = match cs.Char0, cs.Char1 with
                     | _, EOS
                     | EOS, _ -> Ok
                     | '\r', '\n' ->
                         match stream.Peek(2u) with
                         | EOS -> Ok
                         | c1 -> if not (optF.Invoke('\n', if c1 <> '\r' then c1 else '\n'))
                                 then Ok else Error
                     | c0, c1 ->
                         if not (optF.Invoke((if c0 <> '\r' then c0 else '\n'),
                                             (if c1 <> '\r' then c1 else '\n')))
                         then Ok else Error
        Reply(status, (), NoErrorMessages)
