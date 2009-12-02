// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.CharParsers

open System.Diagnostics
open System.Text
open System.Text.RegularExpressions
open System.Runtime.CompilerServices

#if LOW_TRUST
#else
open Microsoft.FSharp.NativeInterop
#endif

open FParsec.Internals
open FParsec.Error
open FParsec.Primitives

#nowarn "9" // "Uses of this construct may result in the generation of unverifiable .NET IL code."
#nowarn "51" // "The address-of operator may result in non-verifiable code."

// ================
// Helper functions
// ================

[<Literal>]
let EOS = CharStream.Iterator.EndOfStreamChar

let foldCase = CharStream.FoldCase
let normalizeNewlines = CharStream.NormalizeNewlines

let floatToHexString d = HexFloat.DoubleToHexString(d)
let floatOfHexString s = HexFloat.DoubleFromHexString(s)

let float32ToHexString d = HexFloat.SingleToHexString(d)
let float32OfHexString s = HexFloat.SingleFromHexString(s)

// ========================
// Running parsers on input
// ========================

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type ParserResult<'Result,'UserState> =
     | Success of 'Result * 'UserState * Pos
     | Failure of string * ParserError * 'UserState
     with
        member private t.StructuredFormatDisplay =
            match t with
            | Success(r,_,_) ->
                if typeof<'Result> = typeof<unit> then "Success: ()"
                else sprintf "Success: %A" r
            | Failure(msg,_,_) ->
                sprintf "Failure:\n%s" msg

let internal applyParser (parser: Parser<'Result,'UserState>) (state: State<'UserState>) =
    let reply = parser state
    if reply.Status = Ok then
        Success(reply.Result, reply.State.UserState, reply.State.Pos)
    else
        let error = ParserError(reply.State.Pos, reply.Error)
        Failure(error.ToString(reply.State.Stream), error, reply.State.UserState)

let runParser (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (name: string) (stream: CharStream) =
    let state0 = new State<'UserState>(stream, ustate, name)
    applyParser parser state0

let runParserOnString (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (streamName: string) (chars: string) =
    if isNull chars then nullArg "chars"
#if LOW_TRUST
    let stream = new CharStream(chars)
    let state0 = new State<'UserState>(stream, ustate, streamName)
    applyParser parser state0
#else
    // use stream = new CharStream(chars)
    // let state0 = new State<'UserState>(stream, ustate, streamName)
    // applyParser parser state0

    // Helper.RunParserOnString is an optimized internal helper function
    Helper.RunParserOnString(chars, 0, chars.Length, applyParser, parser, ustate, streamName)
#endif

let runParserOnSubstring (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (streamName: string) (chars: string) (index: int) length =
#if LOW_TRUST
    let stream = new CharStream(chars, index, length)
    let state0 = new State<'UserState>(stream, ustate, streamName)
    applyParser parser state0
#else
    // Helper.RunParserOnString does no argument checking
    if index < 0 then
        raise (System.ArgumentOutOfRangeException("index", "The index is negative."))
    if length < 0 || length > chars.Length - index then
        raise (System.ArgumentOutOfRangeException("length", "The length is out of range."))
    Helper.RunParserOnString(chars, index, length, applyParser, parser, ustate, streamName)
#endif

let runParserOnStream (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (streamName: string) (byteStream: System.IO.Stream) (encoding: System.Text.Encoding) =
#if LOW_TRUST
    let
#else
    use
#endif
        stream = new CharStream(byteStream, encoding)
    let state0 = new State<'UserState>(stream, ustate, streamName)
    applyParser parser state0

let runParserOnFile (parser: Parser<'Result,'UserState>) (ustate: 'UserState) (path: string) (encoding: System.Text.Encoding) =
#if LOW_TRUST
    let
#else
    use
#endif
        stream = new CharStream(path, encoding)
    let state0 = new State<'UserState>(stream, ustate, path)
    applyParser parser state0

let runParserOnSubstream (parser: Parser<'Result,'SubstreamUserState>) ustate (stateBeforeSubstream: State<'UserState>) stateAfterSubStream =
    Helper.RunParserOnSubstream(applyParser parser, ustate, stateBeforeSubstream, stateAfterSubStream)

let run parser (string: string) =
    runParserOnString parser () "" string

// some predefined error messages

let internal expectedEndOfFile            = expectedError "end of file"
let internal expectedAnyChar              = expectedError "any char"
let internal expectedWhitespace           = expectedError "whitespace"
let internal expectedAsciiUppercaseLetter = expectedError "Ascii uppercase letter"
let internal expectedAsciiLowercaseLetter = expectedError "Ascii lowercase letter"
let internal expectedAsciiLetter          = expectedError "Ascii letter"
let internal expectedUppercaseLetter      = expectedError "uppercase letter"
let internal expectedLowercaseLetter      = expectedError "lowercase letter"
let internal expectedLetter               = expectedError "letter"
let internal expectedBinaryDigit          = expectedError "binary digit"
let internal expectedOctalDigit           = expectedError "octal digit"
let internal expectedDecimalDigit         = expectedError "digit"
let internal expectedHexadecimalDigit     = expectedError "hexadecimal digit"
let internal expectedNewline              = expectedError "newline"
let internal expectedTab                  = expectedError "tab"
let internal expectedFloatingPointNumber  = expectedError "floating-point number"
let internal expectedInt64                = expectedError "integer number (64-bit, signed)"
let internal expectedInt32                = expectedError "integer number (32-bit, signed)"
let internal expectedInt16                = expectedError "integer number (16-bit, signed)"
let internal expectedInt8                 = expectedError "integer number (8-bit, signed)"
let internal expectedUInt64               = expectedError "integer number (64-bit, unsigned)"
let internal expectedUInt32               = expectedError "integer number (32-bit, unsigned)"
let internal expectedUInt16               = expectedError "integer number (16-bit, unsigned)"
let internal expectedUInt8                = expectedError "integer number (8-bit, unsigned)"

let internal unexpectedNewline            = unexpectedError "newline"
let internal unexpectedEndOfFile          = unexpectedError "end of file"

// =======
// Parsers
// =======

// -------------------------------------------------------------
// Reading the input stream position and handling the user state
// -------------------------------------------------------------

let getPos : Parser<Pos,'u> =
    fun state -> Reply(state.Pos, state)

let getUserState : Parser<'u,'u> =
    fun state -> Reply(state.UserState, state)

let setUserState (newUserState: 'u) : Parser<unit,'u> =
    fun state -> Reply((), state.WithUserState(newUserState))

let updateUserState (f: 'u -> 'u) : Parser<unit,'u> =
    fun state -> Reply((), state.WithUserState(f state.UserState))

let userStateSatisfies f : Parser<unit,'u> =
    fun state ->
        Reply<unit,_>((if f state.UserState then Ok else Error), NoErrorMessages, state)

// --------------------
// Parsing single chars
// --------------------

let newlineReturn result : Parser<_,'u> =
    fun state ->
        let newState = state.SkipNewline()
        if not (referenceEquals state newState) then
            Reply(result, newState)
        else
            Reply(Error, expectedNewline, newState)

let newline<'u>     = newlineReturn '\n' : Parser<_,'u>
let skipNewline<'u> = newlineReturn  ()  : Parser<_,'u>

let charReturn c result : Parser<'a,'u> =
    if c <> '\r' && c <> '\n' then
        if c = EOS then invalidArg "c" "The char '\uffff' (EOS) is not a valid argument for the pchar/skipChar/charReturn parser. If you want to check for the end of the stream, consider using the `eof` parser."
        let error = expectedError (quoteChar c)
        fun state ->
            if state.Iter.Read() = c then
                 Reply(result, state.Next)
            else Reply(Error, error, state)
    else newlineReturn result

let pchar    c = charReturn c c
let skipChar c = charReturn c ()


/// returns true for chars '\u000E' - '\ufffe'
let inline internal isCertainlyNoNLOrEOS (c: char) =
    // '\n' = '\u000A', '\r' = '\u000D'
    unativeint c - 0xEun < unativeint EOS - 0xEun

let anyChar : Parser<char,'u> =
    fun state ->
        let c  = state.Iter.Read()
        if isCertainlyNoNLOrEOS c then
            Reply(c, state.Next)
        elif c = '\r' || c = '\n' then
            Reply('\n', state.SkipNewline())
        elif c <> EOS then
            Reply(c, state.Next)
        else
            Reply(Error, expectedAnyChar, state)

let skipAnyChar : Parser<unit,'u> =
    fun state ->
        let newState = state.SkipCharOrNewline()
        if not (referenceEquals state newState) then
            Reply((), newState)
        else
            Reply(Error, expectedAnyChar, newState)


// doesn't check for newlines or EOS
let
#if NOINLINE
#else
    inline
#endif
           internal fastInlineSatisfyE f error : Parser<char,'u> =
    fun state ->
        let c = state.Iter.Read()
        if f c then Reply(c, state.Next)
        else Reply(Error, error, state)

let
#if NOINLINE
#else
    inline
#endif
           internal fastInlineSkipSatisfyE f error : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Read()
        if f c then Reply((), state.Next)
        else Reply(Error, error, state)

let
#if NOINLINE
#else
    inline
#endif
           internal inlineSatisfyE f error : Parser<char,'u> =
    fun state ->
        let c = state.Iter.Read()
        if isCertainlyNoNLOrEOS c then
            if f c then Reply(c, state.Next)
            else Reply(Error, error, state)
        elif c = '\r' || c = '\n' then
            if f '\n' then Reply('\n', state.SkipNewline())
            else Reply(Error, error, state)
        elif c <> EOS && f c then Reply(c, state.Next)
        else Reply(Error, error, state)

let
#if NOINLINE
#else
    inline
#endif
           internal inlineSkipSatisfyE f error : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Read()
        if isCertainlyNoNLOrEOS c then
            if f c then Reply((), state.Next)
            else Reply(Error, error, state)
        elif c = '\r' || c = '\n' then
            if f '\n' then Reply((), state.SkipNewline())
            else Reply(Error, error, state)
        elif c <> EOS && f c then Reply((), state.Next)
        else Reply(Error, error, state)

let internal satisfyE f error     = inlineSatisfyE f error
let internal skipSatisfyE f error = inlineSkipSatisfyE f error

let satisfy f        = satisfyE f NoErrorMessages
let satisfyL f label = satisfyE f (expectedError label)

let skipSatisfy f        = skipSatisfyE f NoErrorMessages
let skipSatisfyL f label = skipSatisfyE f (expectedError label)


let isAnyOf (chars: string) =
    let cs = new FParsec.Helper.CharSet(chars)
    fun c -> cs.Contains(c)

let isNoneOf (chars: string) =
    let cs = new FParsec.Helper.CharSet(chars)
    fun c -> not (cs.Contains(c))

let anyOf (chars: string) =
    let error = expectedError ("any char in " + quoteString chars)
    let cs = new FParsec.Helper.CharSet(chars)
    inlineSatisfyE (fun c -> cs.Contains(c)) error

let skipAnyOf (chars: string) =
    let error = expectedError ("any char in " + quoteString chars)
    let cs = new FParsec.Helper.CharSet(chars)
    inlineSkipSatisfyE (fun c -> cs.Contains(c)) error

let noneOf (chars: string) =
    let error = expectedError ("any char not in " + quoteString chars)
    let cs = new FParsec.Helper.CharSet(chars)
    inlineSatisfyE (fun c -> not (cs.Contains(c))) error

let skipNoneOf (chars: string) =
    let error = expectedError ("any char not in " + quoteString chars)
    let cs = new FParsec.Helper.CharSet(chars)
    inlineSkipSatisfyE (fun c -> not (cs.Contains(c))) error


let inline isAsciiUpper c  = c >= 'A' && c <= 'Z'
let inline isAsciiLower c  = c >= 'a' && c <= 'z'
let inline isAsciiLetter (c: char) = let cc = int c ||| int ' '
                                     cc >= int 'a' && cc <= int 'z'

let inline isUpper c =
    c >= 'A' && (c <= 'Z' || (c > '\u007F' && System.Char.IsUpper(c)))

let inline isLower c =
    c >= 'a' && (c <= 'z' || (c > '\u007F' && System.Char.IsLower(c)))

let inline isLetter c =
    if c <= '\u007F' then
         let cc = int c ||| int ' '
         cc >= int 'a' && cc <= int 'z'
    else System.Char.IsLetter(c)

let inline isDigit c = c <= '9' && c >= '0'

let inline isHex c    =
    if   c <= '9' then c >= '0'
    else
        let cc = int c ||| int ' '
        cc <= int 'f' && cc >= int 'a'

let inline isOctal c  = c <= '7' && c >= '0'

let asciiUpper  state = fastInlineSatisfyE isAsciiUpper  expectedAsciiUppercaseLetter  state
let asciiLower  state = fastInlineSatisfyE isAsciiLower  expectedAsciiLowercaseLetter  state
let asciiLetter state = fastInlineSatisfyE isAsciiLetter expectedAsciiLetter           state

// unicode is the default for letters and ascii the default for numbers
let upper  state = fastInlineSatisfyE isUpper  expectedUppercaseLetter  state
let lower  state = fastInlineSatisfyE isLower  expectedLowercaseLetter  state
let letter state = fastInlineSatisfyE isLetter expectedLetter           state

let digit  state = fastInlineSatisfyE isDigit  expectedDecimalDigit     state
let hex    state = fastInlineSatisfyE isHex    expectedHexadecimalDigit state
let octal  state = fastInlineSatisfyE isOctal  expectedOctalDigit       state

let tab state = fastInlineSatisfyE ((=) '\t') expectedTab state

let unicodeNewline : Parser<_,'u> =
    fun state ->
        let c  = state.Iter.Read()
        if c < '\u0085' then
            if c = '\r' || c = '\n' then
                Reply('\n', state.SkipNewline())
            elif c <> '\u000C' then
                Reply(Error, expectedNewline, state)
            else // c = '\u000C'
                Reply('\n', state.Advance(1, 1, 0))
        elif c <= '\u2029' && (c >= '\u2028' || c = '\u0085') then
            Reply('\n', state.Advance(1, 1, 0))
        else
            Reply(Error, expectedNewline, state)

let whitespace : Parser<char,'u> =
    fun state ->
        let c = state.Iter.Read()
        if c <= ' ' then
            match c with
            | ' '  | '\t' -> Reply(c, state.Next)
            | '\r' | '\n' -> Reply('\n', state.SkipNewline())
            | _           -> Reply(Error, expectedWhitespace, state)
        else Reply(Error, expectedWhitespace, state)

let unicodeWhitespace : Parser<char,'u> =
    fun state ->
        let c  = state.Iter.Read()
        if c = ' ' then Reply(c, state.Next)
        elif System.Char.IsWhiteSpace(c) then
            match c with
            | '\r' | '\n' ->
               Reply('\n', state.SkipNewline())
            | '\u000C' | '\u0085' | '\u2028' | '\u2029' ->
               Reply('\n', state.Advance(1, 1, 0))
            | _ ->
               Reply(c, state.Next)
        else Reply(Error, expectedWhitespace, state)


let spaces : Parser<unit,'u> =
    fun state ->
        Reply((), state.SkipWhitespace())

let spaces1 : Parser<unit,'u> =
    fun state ->
        let newState = state.SkipWhitespace()
        if not (referenceEquals newState state) then Reply((), newState)
        else Reply(Error, expectedWhitespace, newState)

let eof : Parser<unit,'u>=
    fun state ->
        if state.Iter.IsEndOfStream then Reply((), state)
        else Reply(Error, expectedEndOfFile, state)


// ------------------------
// Parsing strings directly
// ------------------------

let internal checkStringContainsNoNewlineChar s name =
    if containsNewlineChar s then
        raise (System.ArgumentException(concat3 "The string argument to " name " may not contain newline chars ('\r' or '\n')."))

let stringReturn s result : Parser<'a,'u> =
    checkStringContainsNoNewlineChar s "pstring/skipString/stringReturn"
    let error = expectedError (quoteString s)
    fun state ->
        if state.Iter.Match(s) then Reply(result, state.Advance(s.Length))
        else Reply(Error, error, state)
let pstring s    = stringReturn s s
let skipString s = stringReturn s ()


let pstringCI s : Parser<string,'u> =
    checkStringContainsNoNewlineChar s "pstringCI"
    let error = expectedError (quoteString s + " (case-insensitive)")
    let cfs = foldCase s
    fun state ->
        if state.Iter.MatchCaseFolded(cfs) then
             Reply(state.Iter.Read(s.Length), state.Advance(s.Length))
        else Reply(Error, error, state)

let stringCIReturn s result : Parser<'a,'u> =
    checkStringContainsNoNewlineChar s "skipStringCI/stringCIReturn"
    let error = expectedError (quoteString s + " (case-insensitive)")
    let cfs = foldCase s
    fun state ->
        if state.Iter.MatchCaseFolded(cfs) then
             Reply(result, state.Advance(s.Length))
        else Reply(Error, error, state)

let skipStringCI s = stringCIReturn s ()


let anyString n : Parser<string,'u> =
    let error = expectedError (concat3 "any sequence of " (string n) " chars")
    fun state ->
        let mutable str = null
        let newState = state.SkipCharsOrNewlines(n, &str)
        if str.Length = n then Reply(str, newState)
        else Reply(Error, error, state)

let skipAnyString n : Parser<unit,'u> =
    let error = expectedError (concat3 "any sequence of " (string n) " chars")
    fun state ->
        let mutable nSkipped = 0
        let newState = state.SkipCharsOrNewlines(n, &nSkipped)
        if n = nSkipped then Reply((), newState)
        else Reply(Error, error, state)

let restOfLine : Parser<_,_> =
    fun state ->
        let mutable str = null
        let newState = state.SkipRestOfLine(true, &str)
        Reply(str, newState)

let skipRestOfLine : Parser<_,_> =
    fun state ->
         Reply((), state.SkipRestOfLine(true))

let skipToEndOfLine : Parser<_,_> =
    fun state ->
        Reply((), state.SkipRestOfLine(false))


let skipToString (s: string) maxChars : Parser<unit,'u> =
    checkStringContainsNoNewlineChar s "skipToString"
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    let error = messageError (concat3 "Could not find the string " (quoteString s) ".")
    fun state ->
        let mutable foundString = false
        let state2 = state.SkipToString(s, maxChars, &foundString)
        if foundString then Reply((), state2)
        else Reply(Error, error, state2)

let skipToStringCI (s: string) maxChars : Parser<unit,'u> =
    checkStringContainsNoNewlineChar s "skipToStringCI"
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    let cfs = foldCase s
    let error = messageError (concat3 "Could not find the case-insensitive string " (quoteString s) ".")
    fun state ->
        let mutable foundString = false
        let state2 = state.SkipToStringCI(cfs, maxChars, &foundString)
        if foundString then Reply((), state2)
        else Reply(Error, error, state2)

let charsTillString (s: string) maxChars : Parser<string,'u> =
    checkStringContainsNoNewlineChar s "charsTillString"
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    let error = messageError (concat3 "Could not find the string " (quoteString s) ".")
    fun state ->
        let mutable charsBeforeString = null
        let state2 = state.SkipToString(s, maxChars, &charsBeforeString)
        if isNotNull charsBeforeString then Reply(charsBeforeString, state2.Advance(s.Length))
        else Reply(Error, error, state2)

let charsTillStringCI (s: string) maxChars : Parser<string,'u> =
    checkStringContainsNoNewlineChar s "charsTillStringCI"
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    let cfs = foldCase s
    let error = messageError (concat3 "Could not find the case-insensitive string " (quoteString s) ".")
    fun state ->
        let mutable charsBeforeString = null
        let state2 = state.SkipToStringCI(cfs, maxChars, &charsBeforeString)
        if isNotNull charsBeforeString then Reply(charsBeforeString, state2.Advance(s.Length))
        else Reply(Error, error, state2)

let skipCharsTillString (s: string) maxChars : Parser<unit,'u> =
    checkStringContainsNoNewlineChar s "skipCharsTillString"
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    let error = messageError (concat3 "Could not find the string " (quoteString s) ".")
    fun state ->
        let mutable foundString = false
        let state2 = state.SkipToString(s, maxChars, &foundString)
        if foundString then Reply((), state2.Advance(s.Length))
        else Reply(Error, error, state2)

let skipCharsTillStringCI (s: string) maxChars : Parser<unit,'u> =
    checkStringContainsNoNewlineChar s "skipCharsTillStringCI"
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    let cfs = foldCase s
    let error = messageError (concat3 "Could not find the case-insensitive string " (quoteString s) ".")
    fun state ->
        let mutable foundString = false
        let state2 = state.SkipToStringCI(cfs, maxChars, &foundString)
        if foundString then Reply((), state2.Advance(s.Length))
        else Reply(Error, error, state2)

let
#if NOINLINE
#else
    inline
#endif
           internal manySatisfyImpl require1 (f1: char -> bool) (f: char -> bool) error : Parser<string,'u> =
    fun state ->
        let mutable str = null
        let newState = state.SkipCharsOrNewlinesWhile(f1, f, &str)
        if not require1 || not (referenceEquals newState state) then Reply(str, newState)
        else Reply(Error, error, newState)

let
#if NOINLINE
#else
    inline
#endif
           internal skipManySatisfyImpl require1 (f1: char -> bool) (f: char -> bool) error : Parser<unit,'u> =
    fun state ->
        let newState = state.SkipCharsOrNewlinesWhile(f1, f)
        if not require1 || not (referenceEquals newState state) then Reply((), newState)
        else Reply(Error, error, newState)

let manySatisfy2   f1 f       = manySatisfyImpl false f1 f NoErrorMessages
let many1Satisfy2  f1 f       = manySatisfyImpl true  f1 f NoErrorMessages
let many1Satisfy2L f1 f label = manySatisfyImpl true  f1 f (expectedError label)

let skipManySatisfy2   f1 f       = skipManySatisfyImpl false f1 f NoErrorMessages
let skipMany1Satisfy2  f1 f       = skipManySatisfyImpl true  f1 f NoErrorMessages
let skipMany1Satisfy2L f1 f label = skipManySatisfyImpl true  f1 f (expectedError label)

let manySatisfy   f       = manySatisfy2   f f
let many1Satisfy  f       = many1Satisfy2  f f
let many1SatisfyL f label = many1Satisfy2L f f label

let skipManySatisfy   f       = skipManySatisfy2   f f
let skipMany1Satisfy  f       = skipMany1Satisfy2  f f
let skipMany1SatisfyL f label = skipMany1Satisfy2L f f label


let internal manyMinMaxSatisfy2E minChars maxChars f1 f error : Parser<string,'u> =
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    if minChars > 0 then
        fun state ->
            let mutable str = null
            let newState = state.SkipCharsOrNewlinesWhile(f1, f, minChars, maxChars, &str)
            if not (referenceEquals newState state) then Reply(str, newState)
            else Reply(Error, error, newState)
    else
        fun state ->
            let mutable str = null
            let newState = state.SkipCharsOrNewlinesWhile(f1, f, 0, maxChars, &str)
            Reply(str, newState)

let internal skipManyMinMaxSatisfy2E minChars maxChars f1 f error : Parser<unit,'u> =
    if maxChars < 0 then raise (System.ArgumentOutOfRangeException("maxChars", "maxChars is negative."))
    if minChars > 0 then
        fun state ->
            let newState = state.SkipCharsOrNewlinesWhile(f1, f, minChars, maxChars)
            if not (referenceEquals newState state) then Reply((), newState)
            else Reply(Error, error, newState)
    else
        fun state ->
            let mutable str = null
            let newState = state.SkipCharsOrNewlinesWhile(f1, f, 0, maxChars)
            Reply((), newState)

let manyMinMaxSatisfy   minChars maxChars    f       = manyMinMaxSatisfy2E minChars maxChars f  f NoErrorMessages
let manyMinMaxSatisfyL  minChars maxChars    f label = manyMinMaxSatisfy2E minChars maxChars f  f (expectedError label)
let manyMinMaxSatisfy2  minChars maxChars f1 f       = manyMinMaxSatisfy2E minChars maxChars f1 f NoErrorMessages
let manyMinMaxSatisfy2L minChars maxChars f1 f label = manyMinMaxSatisfy2E minChars maxChars f1 f (expectedError label)

let skipManyMinMaxSatisfy   minChars maxChars    f       = skipManyMinMaxSatisfy2E minChars maxChars f  f NoErrorMessages
let skipManyMinMaxSatisfyL  minChars maxChars    f label = skipManyMinMaxSatisfy2E minChars maxChars f  f (expectedError label)
let skipManyMinMaxSatisfy2  minChars maxChars f1 f       = skipManyMinMaxSatisfy2E minChars maxChars f1 f NoErrorMessages
let skipManyMinMaxSatisfy2L minChars maxChars f1 f label = skipManyMinMaxSatisfy2E minChars maxChars f1 f (expectedError label)


let internal regexE pattern error : Parser<string,'u> =
    let regex = new Regex("\A" + pattern, RegexOptions.Multiline |||
                                          RegexOptions.ExplicitCapture)
    fun state ->
        let m = state.Iter.Match(regex)
        if m.Success then
            let s = m.Value
            if not (containsNewlineChar s) then Reply(s, if s.Length > 0 then state.Advance(s.Length) else state)
            else
                let s2 = normalizeNewlines s
                let mutable nSkippedChars = 0
                let newState = state.SkipCharsOrNewlines(s2.Length, &nSkippedChars)
                if nSkippedChars = s2.Length then Reply(s2, newState)
                else Reply(FatalError, messageError "Internal error in the regex parser. Please report this error to fparsec@quanttec.com.", newState)
        else Reply(Error, error, state)

let regex  pattern       = regexE pattern (expectedError ("string matching the regex " + quoteString pattern))
let regexL pattern label = regexE pattern (expectedError label)

// ----------------------------------------------
// Parsing strings with the help of other parsers
// ----------------------------------------------

#if LOW_TRUST
/// StructCharList is only meant for internal use within FParsec.
type internal StructCharList = struct
    val mutable chars: char[]
    val mutable count: int

    member inline t.AppendFirst(c) =
        t.chars <- Array.zeroCreate 16
        t.chars.[0] <- c
        t.count <- 1

    member inline t.Append(c) =
        let i = t.count
        let chars = t.chars
        if i < chars.Length then
            chars.[i] <- c
            t.count <- i + 1
        else
            t._AppendContinue(c)

    member t._AppendContinue(c) =
        let count = t.count
        let newChars = Array.zeroCreate (2*count)
        System.Buffer.BlockCopy(t.chars, 0, newChars, 0, count*sizeof<char>)
        newChars.[count] <- c
        t.chars <- newChars
        t.count <- count + 1

    member t.GetString() =
        new string(t.chars, 0, t.count)
end
#else
/// StructCharList is only meant for internal use within FParsec.
/// CAUTION: Its implementation depends on instances only being allocated on the stack
/// (i.e. on the GC not moving around the instances).
type internal StructCharList = struct
    val mutable buffer_ui64_0: uint64
    val mutable buffer_ui64_1: uint64
    val mutable buffer_ui64_2: uint64
    val mutable buffer_ui64_3: uint64

    val mutable chars: char[]
    val mutable count: int

    member inline t.BufferPtr =
        NativePtr.ofNativeInt<char> (NativePtr.toNativeInt (&&t.buffer_ui64_0))

    /// an optimized version of Append(c) for the first char
    member inline t.AppendFirst(c) =
        Debug.Assert(t.count = 0)
        let p = t.BufferPtr
        NativePtr.set p 0 c
        t.count <- 1

    member inline t.Append(c) =
        let i = t.count &&& 0xf
        t.count <- t.count + 1
        if i <> 0 then
            let p = t.BufferPtr
            NativePtr.set p i c
        else
            t._AppendContinue(c)

    /// append char with index%16 = 0
    member t._AppendContinue(c) =
        let p = t.BufferPtr
        let count = t.count - 1
        Debug.Assert(count%16 = 0 || count = -1)
        let mutable chars = t.chars
        if isNotNull chars then
            if count = chars.Length then
                let newChars = Array.zeroCreate (count*2)
                System.Buffer.BlockCopy(chars, 0, newChars, 0, (count - 16)*sizeof<char>)
                t.chars <- newChars
                chars   <- newChars
            for i = 0 to 15 do
                chars.[count - 16 + i] <- NativePtr.get p i
        elif count <> 0 then
            chars   <- Array.zeroCreate 48
            t.chars <- chars
            for i = 0 to 15 do
                chars.[i] <- NativePtr.get p i
        NativePtr.set p 0 c

    // can't use "override t.ToString() =" here, since the F# compiler
    // (v. 1.9.6.16) boxes structs before calling an object override
    member t.GetString() =
        let p = t.BufferPtr
        let count = t.count
        if count <= 16 then new string(p, 0, count)
        else
            let chars = t.chars
            for i = (count - 1) &&& 0x7ffffff0 to count - 1 do
                chars.[i] <- NativePtr.get p (i &&& 0xf)
            new string(chars, 0, count)
end
#endif // LOW_TRUST

let
#if NOINLINE
#else
    inline
#endif
           internal manyCharsImpl require1 (p1: Parser<char,'u>) (p: Parser<char,'u>) : Parser<string,'u> =
    fun state ->
        let mutable reply = p1 state
        if reply.Status = Ok then
            let mutable cl = new StructCharList()
            cl.AppendFirst(reply.Result)
            let mutable state = reply.State
            reply <- p state
            while reply.Status = Ok do
                if referenceEquals reply.State state then
                    _raiseInfiniteLoopException "manyChars" state
                cl.Append(reply.Result)
                state <- reply.State
                reply <- p state
            let error = if reply.State == state then reply.Error
                        else backtrackError reply.State reply.Error
            Reply(Ok, cl.GetString(), error, state)
        else
            let error = if reply.State == state then reply.Error
                        else backtrackError reply.State reply.Error
            if require1 then Reply(Error, error, state)
            else Reply(Ok, "", error, state)

let
#if NOINLINE
#else
    inline
#endif
           internal skipManyCharsImpl require1 (p1: Parser<'a,'u>) (p: Parser<'a,'u>) : Parser<unit,'u> =
    fun state ->
        let mutable reply = p1 state
        if reply.Status = Ok then
            let mutable state = reply.State
            reply <- p state
            while reply.Status = Ok do
                if referenceEquals reply.State state then
                    _raiseInfiniteLoopException "skipManyChars" state
                state <- reply.State
                reply <- p state
            let error = if reply.State == state then reply.Error
                        else backtrackError reply.State reply.Error
            Reply(Ok, (), error, state)
         else
            let error = if reply.State == state then reply.Error
                        else backtrackError reply.State reply.Error
            if require1 then Reply(Error, error, state)
            else Reply(Ok, (), error, state)


let manyChars2 p1 p = manyCharsImpl false p1 p
let manyChars p = manyChars2 p p

let many1Chars2 p1 p = manyCharsImpl true p1 p
let many1Chars p = many1Chars2 p p

let skipManyChars2 (p1: Parser<'a,'u>) (p: Parser<'a,'u>) = skipManyCharsImpl false p1 p
let skipManyChars p = skipManyChars2 p p

let skipMany1Chars2 (p1: Parser<'a,'u>) (p: Parser<'a,'u>) = skipManyCharsImpl true p1 p
let skipMany1Chars p = skipMany1Chars2 p p


let
#if NOINLINE
#else
    inline
#endif
           inlineManyCharsTillApply (p: Parser<char,'u>) (endp: Parser<'b,'u>) (f: string -> 'b -> 'c) =
    fun state ->
        let mutable state  = state
        let mutable reply2 = endp state
        if reply2.Status <> Ok then
            let mutable reply1 = p state
            let mutable cl = new StructCharList()
            if reply1.Status = Ok then
                cl.AppendFirst(reply1.Result)
                state <- reply1.State
                reply2 <- endp state
                while reply2.Status <> Ok && (reply1 <- p state; reply1.Status = Ok) do
                    if referenceEquals reply1.State state then
                        _raiseInfiniteLoopException "manyCharsTill" state
                    cl.Append(reply1.Result)
                    state  <- reply1.State
                    reply2 <- endp state
            if reply2.Status = Ok then
                let error = if not (referenceEquals reply2.State state) then reply2.Error
                            else mergeErrors reply1.Error reply2.Error
                Reply(Ok, f (cl.GetString()) reply2.Result, error, reply2.State)
            elif reply1.Status = Error && reply1.State == state then
                let error = if reply2.State != state then reply2.Error
                            else mergeErrors reply1.Error reply2.Error
                Reply(reply2.Status, error, reply2.State)
            else
                Reply(reply1.Status, reply1.Error, reply1.State)
        else
            Reply(Ok, f "" reply2.Result, reply2.Error, reply2.State)

let
#if NOINLINE
#else
    inline
#endif
           inlineMany1CharsTill2Apply (p1: Parser<char,'u>) (p: Parser<char,'u>) (endp: Parser<'b,'u>) (f: string -> 'b -> 'c) =
    fun state ->
        let mutable reply1 = p1 state
        if reply1.Status = Ok then
            let mutable cl = new StructCharList()
            cl.AppendFirst(reply1.Result)
            let mutable state = reply1.State
            let mutable reply2 = endp state
            while reply2.Status <> Ok && (reply1 <- p state; reply1.Status = Ok) do
                if referenceEquals reply1.State state then
                    _raiseInfiniteLoopException "manyCharsTill" state
                cl.Append(reply1.Result)
                state  <- reply1.State
                reply2 <- endp state
            if reply2.Status = Ok then
                let error = if not (referenceEquals reply2.State state) then reply2.Error
                            else mergeErrors reply1.Error reply2.Error
                Reply(Ok, f (cl.GetString()) reply2.Result, error, reply2.State)
            elif reply1.Status = Error && reply1.State == state then
                let error = if reply2.State != state then reply2.Error
                            else mergeErrors reply1.Error reply2.Error
                Reply(reply2.Status, error, reply2.State)
            else
                Reply(reply1.Status, reply1.Error, reply1.State)
        else
            Reply(reply1.Status, reply1.Error, reply1.State)


let manyCharsTill      p endp   = inlineManyCharsTillApply p endp (fun str _ -> str)
let manyCharsTillApply p endp f = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                  inlineManyCharsTillApply p endp (fun str x -> optF.Invoke(str, x))
let skipManyCharsTill  p endp   = skipManyTill p endp

let many1CharsTill2      p1 p endp   = inlineMany1CharsTill2Apply p1 p endp (fun str _ -> str)
let many1CharsTillApply2 p1 p endp f = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                       inlineMany1CharsTill2Apply p1 p endp (fun str x -> optF.Invoke(str, x))
let many1CharsTill          p endp   = many1CharsTill2 p p endp
let many1CharsTillApply     p endp f = many1CharsTillApply2 p p endp f

let skipMany1CharsTill2  (p1: Parser<'a,'u>) (p: Parser<'a,'u>) endp   = p1 >>. skipManyTill p endp
let skipMany1CharsTill   p    endp   = skipMany1CharsTill2 p p endp


let
#if NOINLINE
#else
    inline
#endif
           manyStringsImpl require1 (p1: Parser<string,'u>) (p: Parser<string,'u>) : Parser<string,'u> =
    fun state ->
        let mutable reply = p1 state
        if reply.Status = Ok then
            let result1 = reply.Result
            let mutable error  = reply.Error
            let mutable state  = reply.State
            reply <- p state
            if reply.Status <> Ok then reply.Result <- result1
            else
                let result2 = reply.Result
                error <- reply.Error
                state <- reply.State
                reply <- p state
                if reply.Status <> Ok then reply.Result <- result1 + result2
                else
                    let result3 = reply.Result
                    error <- reply.Error
                    state <- reply.State
                    reply <- p state
                    if reply.Status <> Ok then reply.Result <- concat3 result1 result2 result3
                    else
                        let result4 = reply.Result
                        error <- reply.Error
                        state <- reply.State
                        reply <- p state
                        if reply.Status <> Ok then reply.Result <- concat4 result1 result2 result3 result4
                        else
                            let n = 2*(result1.Length + result2.Length + result3.Length + result4.Length) + reply.Result.Length
                            let sb = new StringBuilder(n)
                            sb.Append(result1).Append(result2).Append(result3).Append(result4).Append(reply.Result) |> ignore
                            error <- reply.Error
                            state <- reply.State
                            reply <- p state
                            while reply.Status = Ok do
                                if reply.State == state then
                                    _raiseInfiniteLoopException "manyStrings" state
                                error <- reply.Error
                                sb.Append(reply.Result) |> ignore
                                state <- reply.State
                                reply <- p state
                            reply.Result <- sb.ToString()
            // we assume that the string parser changes the state when it succeeds, so we don't need to merge more than one error
            if reply.Status = Error then
                if reply.State == state then
                    reply.Status <- Ok
                    if isNotNull error then
                        reply.Error <- concatErrorMessages error reply.Error
            else
                reply.Error <- mergeErrorsIfNeeded state error reply.State reply.Error
        elif not require1 && reply.Status = Error && reply.State == state then
            reply.Status <- Ok
            reply.Result <- ""
        reply

let manyStrings2 p1 p = manyStringsImpl false p1 p
let manyStrings p = manyStrings2 p p
let many1Strings2 p1 p = manyStringsImpl true p1 p
let many1Strings p = many1Strings2 p p


let skipped (p: Parser<unit,'u>) : Parser<string,'u> =
    fun state ->
        let reply = p state
        let result = if reply.Status = Ok then state.ReadUntil(reply.State)  else ""
        Reply(reply.Status, result, reply.Error, reply.State)

let withSkippedString (f: string -> 'a -> 'b) (p: Parser<'a,'u>) : Parser<'b,'u> =
    let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
    fun state ->
        let reply = p state
        let result = if reply.Status = Ok then
                         optF.Invoke(state.ReadUntil(reply.State), reply.Result)
                     else Unchecked.defaultof<_>
        Reply(reply.Status, result, reply.Error, reply.State)


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
    member t.IsInteger      = info &&& (NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent) = NLF.HasIntegerPart
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

let numberLiteralE (opt: NumberLiteralOptions) (errorInCaseNoLiteralFound: ErrorMessageList) (state: State<'u>) =
    let mutable iter = state.Iter
    let mutable c = iter.Read()
    let mutable error = NoErrorMessages
    let mutable flags = NLF.None

    if c = '-' && (opt &&& NLO.AllowMinusSign) <> NLO.None then
        flags <- NLF.HasMinusSign
        c <- iter._Increment()
    elif c = '+' && (opt &&& NLO.AllowPlusSign) <> NLO.None then
        flags <- NLF.HasPlusSign
        c <- iter._Increment()

    let allowStartingPoint = NLO.AllowFraction ||| NLO.AllowFractionWOIntegerPart // for starting point both flags are required

    if isDigit c || (c = '.' && (opt &&& allowStartingPoint) = allowStartingPoint) then
        let mutable c1  = '\u0000'
        if    c <> '0'
           || (c1 <- iter._Increment();
                  c1 <= '9'
               || (opt &&& (NLO.AllowBinary ||| NLO.AllowOctal ||| NLO.AllowHexadecimal)) = NLO.None
               || ((int c1 ||| int ' ') = int 'e'))
        then
            flags <- flags ||| NLF.IsDecimal
            if c <> '.' then
                flags <- flags ||| NLF.HasIntegerPart
                if c <> '0' then
                    c <- iter._Increment()
                else
                    c <- c1
                while isDigit c do
                    c <- iter._Increment()
            if c = '.' && (opt &&& NLO.AllowFraction) <> NLO.None then
                flags <- flags ||| NLF.HasFraction
                c <- iter._Increment()
                if isDigit c then
                    c <- iter._Increment()
                elif (flags &&& NLF.HasIntegerPart) = NLF.None then
                    // at least one digit before or after the . is required
                    error <- expectedDecimalDigit
                while isDigit c do
                    c <- iter._Increment()
            if (int c ||| int ' ') = int 'e' && isNull error && (opt &&& NLO.AllowExponent) <> NLO.None then
                flags <- flags ||| NLF.HasExponent
                c <- iter._Increment()
                if c = '-' || c = '+' then
                    c <- iter._Increment()
                if not (isDigit c) then
                    error <- expectedDecimalDigit
                while isDigit c do
                    c <- iter._Increment()
        else
            match int c1 ||| int ' ' with
            | 0x78 (* 'x' *) when (opt &&& NLO.AllowHexadecimal) <> NLO.None ->
                flags <- flags ||| NLF.IsHexadecimal
                c <- iter._Increment()
                if isHex c then
                    flags <- flags ||| NLF.HasIntegerPart
                    c <- iter._Increment()
                elif (opt &&& NLO.AllowFractionWOIntegerPart) = NLO.None then
                    // integer part required
                    error <- expectedHexadecimalDigit
                while isHex c do
                    c <- iter._Increment()
                if c = '.' && isNull error && (opt &&& NLO.AllowFraction) <> NLO.None then
                    flags <- flags ||| NLF.HasFraction
                    c <- iter._Increment()
                    if isHex c then
                        c <- iter._Increment()
                    elif (flags &&& NLF.HasIntegerPart) = NLF.None then
                        // at least one digit before or after the . is required
                        error <- expectedHexadecimalDigit
                    while isHex c do
                        c <- iter._Increment()
                elif (flags &&& NLF.HasIntegerPart) = NLF.None then
                    // we neither have an integer part nor a fraction
                    error <- expectedHexadecimalDigit
                if (int c ||| int ' ') = int 'p' && isNull error && (opt &&& NLO.AllowExponent) <> NLO.None then
                    flags <- flags ||| NLF.HasExponent
                    c <- iter._Increment()
                    if c = '-' || c = '+' then
                        c <- iter._Increment()
                    if not (isDigit c) then
                        error <- expectedDecimalDigit
                    while isDigit c do
                        c <- iter._Increment()
            | 0x6f (* 'o' *) when (opt &&& NLO.AllowOctal) <> NLO.None ->
                flags <- flags ||| NLF.IsOctal
                c <- iter._Increment()
                if isOctal c then
                    flags <- flags ||| NLF.HasIntegerPart
                    c <- iter._Increment()
                else
                    error <- expectedOctalDigit
                while isOctal c do
                    c <- iter._Increment()
            | 0x62 (* 'b' *) when (opt &&& NLO.AllowBinary) <> NLO.None ->
                flags <- flags ||| NLF.IsBinary
                c <- iter._Increment()
                if c = '0' || c = '1' then
                    flags <- flags ||| NLF.HasIntegerPart
                    c <- iter._Increment()
                else
                    error <- expectedBinaryDigit
                while c = '0' || c = '1' do
                    c <- iter._Increment()
            | _ ->
                flags <- flags ||| (NLF.IsDecimal ||| NLF.HasIntegerPart)
                c <- c1

        if isNull error then
            if (opt &&& NLO.AllowSuffix) = NLO.None  || not (isAsciiLetter c) then
                let str = state.Iter.ReadUntil(iter)
                let newState = state.AdvanceTo(iter)
                Reply(NumberLiteral(str, flags, EOS, EOS, EOS, EOS), newState)
            else
                let mutable str = if (opt &&& NLO.IncludeSuffixCharsInString) <> NLO.None then null
                                  else state.Iter.ReadUntil(iter)
                let mutable nSuffix = 1
                let mutable s1 = c
                let mutable s2 = EOS
                let mutable s3 = EOS
                let mutable s4 = EOS
                c <- iter._Increment()
                if isAsciiLetter c then
                    nSuffix <- 2
                    s2 <- c
                    c <- iter._Increment()
                    if isAsciiLetter c then
                        nSuffix <- 3
                        s3 <- c
                        c <- iter._Increment()
                        if isAsciiLetter c then
                            nSuffix <- 4
                            s4 <- c
                            c <- iter._Increment()
                flags <- flags ||| (enum) nSuffix
                if (opt &&& NLO.IncludeSuffixCharsInString) <> NLO.None then
                    str <- state.Iter.ReadUntil(iter)
                let newState = state.AdvanceTo(iter)
                Reply(NumberLiteral(str, flags, s1, s2, s3, s4), newState)
        else
            Reply(Error, error, state.AdvanceTo(iter))
    else
       let cc = int c ||| int ' '
       if
           if cc = int 'i' then
                 (opt &&& NLO.AllowInfinity) <> NLO.None
              && iter.MatchCaseFolded("inf") && (flags <- flags ||| NLF.IsInfinity
                                                 iter._Increment(3u) |> ignore
                                                 if iter.MatchCaseFolded("inity") then iter._Increment(5u) |> ignore
                                                 true)
           elif  cc = int 'n' then
                 (opt &&& NLO.AllowNaN) <> NLO.None
              && iter.MatchCaseFolded("nan") && (flags <- flags ||| NLF.IsNaN
                                                 iter._Increment(3u) |> ignore
                                                 true)
           else false
       then
           let str = state.Iter.ReadUntil(iter)
           let newState = state.AdvanceTo(iter)
           Reply(NumberLiteral(str, flags, EOS, EOS, EOS, EOS), newState)
       else
           Reply(Error, errorInCaseNoLiteralFound, state)


let pfloat : Parser<float,'u> =
    fun state ->
        // reply is mutable to prevent fsc from splitting up the function
        let mutable reply = numberLiteralE NLO.DefaultFloat expectedFloatingPointNumber state
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
                Reply(d, reply.State)
            with e ->
                let msg = if   (e :? System.OverflowException) then "This number is outside the allowable range for double precision floating-pointer numbers."
                          elif (e :? System.FormatException) then "The floating-point number has an invalid format (this error is unexpected, please report this error message to fparsec@quanttec.com)."
                          else reraise()
                Reply(FatalError, messageError msg, state)
        else Reply(reply.Status, reply.Error, reply.State)

let numberLiteral opt label = numberLiteralE opt (expectedError label)

let internal parseUInt64 (c0: char) (iter: CharStream.Iterator byref) (status: ReplyStatus byref) (error: ErrorMessageList byref) =
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
    let c1 = iter._Increment()

    if    (opt &&& (NLO.AllowBinary ||| NLO.AllowOctal ||| NLO.AllowHexadecimal)) = NLO.None
       || c <> '0' || c1 <= '9'
    then
        n <- uint64 (uint32 c - uint32 '0')
        c <- c1
        while c >= '0' && c <= '9' do
            let nc = uint32 c - uint32 '0'
            if n <= limit10 || (maxMod10 < 9u && n = maxDiv10 && nc <= maxMod10) then
                n <- 10UL*n + uint64 nc
                c <- iter._Increment()
            else
                status <- FatalError
                c <- '!' // break

    else
        let cc1 = uint32 c1 ||| uint32 ' '
        if (opt &&& NLO.AllowHexadecimal) <> NLO.None && cc1 = uint32 'x' then
            c <- iter._Increment()
            let mutable nc = uint32 0
            if  (let cc = uint32 c ||| uint32 ' '
                 if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                 else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a')) // 0x57u = uint32 'a' - 10u
            then
                n <- uint64 nc
                c <- iter._Increment()
                while
                    (let cc = uint32 c ||| uint32 ' '
                     if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                     else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a'))
                  do
                    if n <= limit16 || (maxMod16 < 15u && n = maxDiv16 && nc <= maxMod16) then
                        n <- 16UL*n + uint64 nc
                        c <- iter._Increment()
                    else
                        status <- FatalError
                        c <- '!' // break
            else
                status <- Error
                error <- expectedHexadecimalDigit

        elif (opt &&& NLO.AllowOctal) <> NLO.None && cc1 = uint32 'o' then
            c <- iter._Increment()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 7u) then
                n <- uint64 nc
                c <- iter._Increment()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 7u) do
                    if n <= limit8 || (maxMod8 < 7u && n = maxDiv8 && nc <= maxMod8) then
                        n <- 8UL*n + uint64 nc
                        c <- iter._Increment()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- expectedOctalDigit

        elif (opt &&& NLO.AllowBinary) <> NLO.None && cc1 = uint32 'b' then
            c <- iter._Increment()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 1u) then
                n <- uint64 nc
                c <- iter._Increment()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 1u) do
                    if n <= limit2 || (maxMod2 = 0u && n = maxDiv2 && nc = 0u) then
                        n <- 2UL*n + uint64 nc
                        c <- iter._Increment()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- expectedBinaryDigit
        // else c = 0 && not (isDigit c1)
    n

let internal parseUInt32 (c0: char) (iter: CharStream.Iterator byref) (status: ReplyStatus byref) (error: ErrorMessageList byref) =
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
    let c1 = iter._Increment()

    if    (opt &&& (NLO.AllowBinary ||| NLO.AllowOctal ||| NLO.AllowHexadecimal)) = NLO.None
       || c <> '0' || c1 <= '9'
    then
        n <- uint32 c - uint32 '0'
        c <- c1
        while c >= '0' && c <= '9' do
            let nc = uint32 c - uint32 '0'
            if n <= limit10 || (maxMod10 < 9u && n = maxDiv10 && nc <= maxMod10) then
                n <- 10u*n + nc
                c <- iter._Increment()
            else
                status <- FatalError
                c <- '!' // break

    else
        let cc1 = uint32 c1 ||| uint32 ' '
        if (opt &&& NLO.AllowHexadecimal) <> NLO.None && cc1 = uint32 'x' then
            c <- iter._Increment()
            let mutable nc = uint32 0
            if  (let cc = uint32 c ||| uint32 ' '
                 if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                 else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a')) // 0x57u = uint32 'a' - 10u
            then
                n <- uint32 nc
                c <- iter._Increment()
                while
                    (let cc = uint32 c ||| uint32 ' '
                     if c <= '9' then nc <- uint32 c - uint32 '0'; c >= '0'
                     else cc <= uint32 'f' && (nc <- cc - 0x57u; cc >= uint32 'a'))
                  do
                    if n <= limit16 || (maxMod16 < 15u && n = maxDiv16 && nc <= maxMod16) then
                        n <- 16u*n + nc
                        c <- iter._Increment()
                    else
                        status <- FatalError
                        c <- '!' // break
            else
                status <- Error
                error <- expectedHexadecimalDigit

        elif (opt &&& NLO.AllowOctal) <> NLO.None && cc1 = uint32 'o' then
            c <- iter._Increment()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 7u) then
                n <- uint32 nc
                c <- iter._Increment()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 7u) do
                    if n <= limit8 || (maxMod8 < 7u && n = maxDiv8 && nc <= maxMod8) then
                        n <- 8u*n + nc
                        c <- iter._Increment()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- expectedOctalDigit

        elif (opt &&& NLO.AllowBinary) <> NLO.None && cc1 = uint32 'b' then
            c <- iter._Increment()
            let mutable nc = uint32 c - uint32 '0'
            if nc = (nc &&& 1u) then
                n <- uint32 nc
                c <- iter._Increment()
                nc <- uint32 c - uint32 '0'
                while nc = (nc &&& 1u) do
                    if n <= limit2 || (maxMod2 = 0u && n = maxDiv2 && nc = 0u) then
                        n <- 2u*n + nc
                        c <- iter._Increment()
                        nc <- uint32 c - uint32 '0'
                    else
                        status <- FatalError
                        nc <- 11u // break
            else
                status <- Error
                error <- expectedBinaryDigit
        // else c = 0 && not (isDigit c1)
    n

[<MethodImplAttribute(MethodImplOptions.NoInlining)>]
let internal overflowError message =
    if isNotNull message then messageError message // isNotNull prevents fsc from inlining the function
    else NoErrorMessages

let inline internal pint (opt: NumberLiteralOptions) (max: 'uint) (uint64_: 'uint -> uint64) (uint: int -> 'uint) (uint_: uint32 -> 'uint) (uint__: uint64 -> 'uint) (int: 'uint -> 'int) (minus1: 'int) (errorInCaseNoLiteralFound: ErrorMessageList) (overflowMessage: string) (state: State<'u>) =
    // we rely on the compiler eliminating inactive branches after inlining

    let minusIsAllowed = (opt &&& NLO.AllowMinusSign) <> NLO.None

    let mutable iter = state.Iter
    let mutable c = iter.Read()

    let mutable plusMinus1 = int (uint 1)
    if minusIsAllowed && c = '-' then
        plusMinus1 <- minus1
        c <- iter._Increment()
    elif (opt &&& NLO.AllowPlusSign) <> NLO.None && c = '+' then
        c <- iter._Increment()

    let mutable status = Ok
    let mutable error = NoErrorMessages
    let mutable result = Unchecked.defaultof<_>
    let mutable newState = state
    if c >= '0' && c <= '9' then
        let n = if uint64_ max <= uint64 System.UInt32.MaxValue then
                    uint_  (parseUInt32 c (&iter) (&status) (&error))
                else
                    uint__ (parseUInt64 c (&iter) (&status) (&error))
        let isUInt32Or64 = uint64_ max = uint64 System.UInt32.MaxValue || uint64_ max = System.UInt64.MaxValue
        if status = Ok && (isUInt32Or64 || (n <= max || (minusIsAllowed && plusMinus1 = minus1 && n = max + uint 1))) then
            result <- if minusIsAllowed then plusMinus1 * int n else int n
            newState <- state.AdvanceTo(iter)
        elif status = Error then
            newState <- state.AdvanceTo(iter)
        else
            status <- FatalError
            error  <- overflowError overflowMessage
    else
        status <- Error
        error  <- errorInCaseNoLiteralFound
    Reply(status, result, error, newState)

let pint64 state = pint NumberLiteralOptions.DefaultInteger (uint64 System.Int64.MaxValue)            uint64 uint64 uint64 uint64 int64 -1L expectedInt64 "This number is outside the allowable range for 64-bit signed integers." state
let pint32 state = pint NumberLiteralOptions.DefaultInteger (uint32 System.Int32.MaxValue)            uint64 uint32 uint32 uint32 int32 -1  expectedInt32 "This number is outside the allowable range for 32-bit signed integers." state
                                                           // fsc's optimizer seems to have problems with literals of small int types
let pint16 state = pint NumberLiteralOptions.DefaultInteger ((*uint32 System.Int16.MaxValue*)0x7fffu) uint64 uint32 uint32 uint32 int16 -1s expectedInt16 "This number is outside the allowable range for 16-bit signed integers." state
let pint8  state = pint NumberLiteralOptions.DefaultInteger ((*uint32 System.SByte.MaxValue*)0x7fu)   uint64 uint32 uint32 uint32 sbyte -1y expectedInt8  "This number is outside the allowable range for 8-bit signed integers." state

let puint64 state = pint NumberLiteralOptions.DefaultUnsignedInteger System.UInt64.MaxValue uint64 uint64 uint64 uint64 uint64 1UL expectedUInt64 "This number is outside the allowable range for 64-bit unsigned integers." state
let puint32 state = pint NumberLiteralOptions.DefaultUnsignedInteger System.UInt32.MaxValue uint64 uint32 uint32 uint32 uint32 1u  expectedUInt32 "This number is outside the allowable range for 32-bit unsigned integers." state
let puint16 state = pint NumberLiteralOptions.DefaultUnsignedInteger 0xffffu                uint64 uint32 uint32 uint32 uint16 1us expectedUInt16 "This number is outside the allowable range for 16-bit unsigned integers." state
let puint8  state = pint NumberLiteralOptions.DefaultUnsignedInteger 0xffu                  uint64 uint32 uint32 uint32 byte   1uy expectedUInt8  "This number is outside the allowable range for 8-bit unsigned integers." state



// -------------------
// Conditional parsing
// -------------------

let followedByChar c : Parser<unit,'u> =
    if c <> '\r' && c <> '\n' then
        let error = expectedError (quoteChar c)
        fun state ->
            if state.Iter.Match(c) then Reply((), state)
            else Reply(Error, error, state)
    else
        let error = expectedNewline
        fun state ->
            let c = state.Iter.Read()
            if c = '\r' || c = '\n' then Reply((), state)
            else Reply(Error, error, state)

let notFollowedByChar c : Parser<unit,'u> =
    if c <> '\r' && c <> '\n' then
        let error = unexpectedError (quoteChar c)
        fun state ->
            if not (state.Iter.Match(c)) then Reply((), state)
            else Reply(Error, error, state)
    else
        let error = unexpectedNewline
        fun state ->
            let c = state.Iter.Read()
            if c <> '\r' && c <> '\n' then  Reply((), state)
            else Reply(Error, error, state)

let followedByString s : Parser<unit,'u> =
    checkStringContainsNoNewlineChar s "followedByString"
    let error = expectedError (quoteString s)
    fun state ->
        if state.Iter.Match(s) then Reply((), state)
        else Reply(Error, error, state)

let notFollowedByString s : Parser<unit,'u> =
    checkStringContainsNoNewlineChar s "notFollowedByString"
    let error = unexpectedError (quoteString s)
    fun state ->
        if not (state.Iter.Match(s)) then Reply((), state)
        else Reply(Error, error, state)

let inline charSatisfies c f =
    if isCertainlyNoNLOrEOS c then
        if f c then Ok else Error
     elif c = '\r' || c = '\n' then
        if f '\n' then Ok else Error
     else
        if c <> EOS && f c then Ok else Error

let inline charSatisfiesNot c f =
    if isCertainlyNoNLOrEOS c then
        if not (f c) then Ok else Error
     elif c = '\r' || c = '\n' then
        if not (f '\n') then Ok else Error
     else
        if c = EOS || not (f c) then Ok else Error

let nextCharSatisfies f : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Peek()
        Reply<unit,_>(charSatisfies c f, NoErrorMessages, state)

let nextCharSatisfiesNot f : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Peek()
        Reply<unit,_>(charSatisfiesNot c f, NoErrorMessages, state)

let previousCharSatisfies f : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Peek(-1)
        Reply<unit,_>(charSatisfies c f, NoErrorMessages, state)

let previousCharSatisfiesNot f : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Peek(-1)
        Reply<unit,_>(charSatisfiesNot c f, NoErrorMessages, state)

let currentCharSatisfies f : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Read()
        Reply<unit,_>(charSatisfies c f,  NoErrorMessages, state)

let currentCharSatisfiesNot f : Parser<unit,'u> =
    fun state ->
        let c = state.Iter.Read()
        Reply<unit,_>(charSatisfiesNot c f,  NoErrorMessages, state)
