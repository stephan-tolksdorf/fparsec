// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.CharParsersTests

open System.Text.RegularExpressions

open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

open FParsec.Test.Test

type NLO = NumberLiteralOptions
type NLF = NumberLiteralResultFlags

let testCharParsers() =
    pchar ' '  |> ROk " " 1 ' '
    pchar '\t' |> ROk "\t\t" 1 '\t'
    pchar ' '  |> RError "" 0 (expectedString " ")
    pchar ' '  |> RError "x" 0 (expectedString " ")

    pchar '\r' |> RError "_\r" 0 Errors.ExpectedNewline
    newline    |> RError "_\n" 0 Errors.ExpectedNewline
    newline    |> RError "" 0 Errors.ExpectedNewline

    pchar '\n'   |> ROkNL "\r"   1 '\n'
    newline      |> ROkNL "\r"   1 '\n'
    pchar '\r'   |> ROkNL "\r"   1 '\r'
    pchar '\n'   |> ROkNL "\r\n" 2 '\n'
    pchar '\r'   |> ROkNL "\r\n" 2 '\r'
    pchar '\n'   |> ROkNL "\n"   1 '\n'
    pchar '\r'   |> ROkNL "\n"   1 '\r'

    skipChar '\t'     |> ROk   "\t"   1  ()
    charReturn '\t' 0 |> ROk   "\t"   1  0
    skipNewline       |> ROkNL "\n"   1  ()
    newlineReturn 0   |> ROkNL "\r\n" 2  0

    try pchar EOS |> ignore; Fail()
    with :? System.ArgumentException -> ()

    anyChar     |> RError "" 0 Errors.ExpectedAnyChar
    skipAnyChar |> RError "" 0 Errors.ExpectedAnyChar

    anyChar     |> ROk " "      1 ' '
    anyChar     |> ROk "\ufffe" 1 '\ufffe'
    skipAnyChar |> ROk " "      1 ()
    anyChar     |> ROk "\t\t"   1 '\t'
    skipAnyChar |> ROk "\t\t"   1 ()

    anyChar     |> ROkNL "\r\n" 2 '\n'
    skipAnyChar |> ROkNL "\r\n" 2 ()
    anyChar     |> ROkNL "\n\n" 1 '\n'
    skipAnyChar |> ROkNL "\n\n" 1 ()

    satisfy      (fun c -> true) |> RError "" 0 NoErrorMessages
    skipSatisfy  (fun c -> true) |> RError "" 0 NoErrorMessages
    satisfyL     (fun c -> true) "test" |> RError "" 0 (expected "test")
    skipSatisfyL (fun c -> true) "test" |> RError "" 0 (expected "test")

    satisfy ((=) '1')  |> ROk "1"  1 '1'
    satisfy ((=) '\t') |> ROk "\t" 1 '\t'
    satisfy ((=) '1')  |> ROk "11" 1 '1'
    satisfy ((=) '1')  |> RError "0" 0 NoErrorMessages
    satisfyL ((=) '1') "test"  |> RError "2" 0 (expected "test")
    satisfyL ((=) '\r') "test" |> RError "\r" 0 (expected "test")
    satisfy ((=) '\n') |> ROkNL "\r"   1 '\n'
    satisfy ((=) '\n') |> ROkNL "\r\n" 2 '\n'
    satisfy ((=) '\n') |> ROkNL "\n"   1 '\n'

    skipSatisfy ((=) '1')  |> ROk "1"  1 ()
    skipSatisfy ((=) '\t') |> ROk "\t" 1 ()
    skipSatisfy ((=) '1')  |> ROk "11" 1 ()
    skipSatisfy ((=) '1')  |> RError "0" 0 NoErrorMessages
    skipSatisfyL ((=) '1') "test"  |> RError "2"  0 (expected "test")
    skipSatisfyL ((=) '\r') "test" |> RError "\r" 0 (expected "test")
    skipSatisfy ((=) '\n') |> ROkNL "\r"   1 ()
    skipSatisfy ((=) '\n') |> ROkNL "\r\n" 2 ()
    skipSatisfy ((=) '\n') |> ROkNL "\n"   1 ()

let testAnyNoneOf() =
    anyOf "1"  |> ROk "1" 1 '1'
    anyOf "1"  |> RError "2" 0 (Errors.ExpectedAnyCharIn("1"))
    noneOf "1" |> RError "1" 0 (Errors.ExpectedAnyCharNotIn("1"))
    noneOf "1" |> ROk "2" 1 '2'
    skipAnyOf "1"  |> ROk "1" 1 ()
    skipAnyOf "1"  |> RError "2" 0 (Errors.ExpectedAnyCharIn("1"))
    skipNoneOf "1" |> RError "1" 0 (Errors.ExpectedAnyCharNotIn("1"))
    skipNoneOf "1" |> ROk "2" 1 ()

//#nowarn "44" // "This construct is deprecated."

let testSpecialCharParsers() =
    for i = 0 to 1023 do
        let c = char i
        isUpper  c |> Equal (System.Char.IsUpper(c))
        isLower  c |> Equal (System.Char.IsLower(c))
        isLetter c |> Equal (System.Char.IsLetter(c))
        isAsciiUpper  c |> Equal (c <= '\u007f' && System.Char.IsUpper(c))
        isAsciiLower  c |> Equal (c <= '\u007f' && System.Char.IsLower(c))
        isAsciiLetter c |> Equal (c <= '\u007f' && System.Char.IsLetter(c))
        isDigit c |> Equal (c >= '0' && c <= '9')
        isHex c |> Equal ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c  <= 'F'))
        isOctal c |> Equal (c >= '0' && c <= '7')

    asciiUpper  |> ROk "A" 1 'A'
    asciiUpper  |> RError "a" 0 Errors.ExpectedAsciiUppercaseLetter
    asciiLower  |> ROk "a" 1 'a'
    asciiLower  |> RError "A" 0 Errors.ExpectedAsciiLowercaseLetter
    asciiLetter |> ROk "A" 1 'A'
    asciiLetter |> RError "1" 0 Errors.ExpectedAsciiLetter

    upper  |> ROk "Ä" 1 'Ä'
    upper  |> RError "ä" 0 Errors.ExpectedUppercaseLetter
    lower  |> ROk "ä" 1 'ä'
    lower  |> RError "Ä" 0 Errors.ExpectedLowercaseLetter
    letter |> ROk "Ä" 1 'Ä'
    letter |> RError "1" 0 Errors.ExpectedLetter

    digit |> ROk "1" 1 '1'
    digit |> RError "a" 0 Errors.ExpectedDecimalDigit
    hex   |> ROk "a" 1 'a'
    hex   |> RError "g" 0 Errors.ExpectedHexadecimalDigit
    octal |> ROk "7" 1 '7'
    octal |> RError "8" 0 Errors.ExpectedOctalDigit

    tab |> ROk "\t" 1 '\t'
    tab |> RError "\r" 0 Errors.ExpectedTab

    unicodeNewline |> ROkNL "\r"     1 '\n'
    unicodeNewline |> ROkNL "\r\n"   2 '\n'
    unicodeNewline |> ROkNL "\n"     1 '\n'
    unicodeNewline |> ROkNL "\u0085" 1 '\n'
    unicodeNewline |> ROkNL "\u2028" 1 '\n'
    unicodeNewline |> ROkNL "\u2029" 1 '\n'
    unicodeNewline |> RError "\f"    0 Errors.ExpectedNewline
    unicodeNewline |> RError "\t"    0 Errors.ExpectedNewline
    unicodeNewline |> RError ""      0 Errors.ExpectedNewline

    skipUnicodeNewline |> ROkNL "\u2028" 1 ()

    let count p = many p |>> List.fold (fun c x -> c + 1) 0

    match run (count unicodeNewline) "\n\r\r\n\u0085\u2028\u2029\r\n" with
    | Success(c,_,pos) -> c |> Equal 7; pos.Index |> Equal 9L; pos.Line |> Equal 8L; pos.Column |> Equal 1L
    | Failure _        -> Fail()

    spaces  |> ROk ""   0 ()
    spaces  |> ROk " "  1 ()
    spaces  |> ROk "  " 2 ()
    spaces1 |> RError "" 0 Errors.ExpectedWhitespace
    spaces1 |> ROk " "  1 ()
    spaces1 |> ROk "  " 2 ()

    unicodeSpaces  |> ROk ""   0 ()
    unicodeSpaces  |> ROk " "  1 ()
    unicodeSpaces  |> ROk " \u200A" 2 () // '\u200A' is a "hair space" (interestingly, the '\u200B' "zero width space" character is not recognized as white space)
    unicodeSpaces1 |> RError "" 0 Errors.ExpectedWhitespace
    unicodeSpaces1 |> ROk " "  1 ()
    unicodeSpaces1 |> ROk " \u200A" 2 ()

    match run spaces "\n \r\t\t\r\n\n " with
    | Success(_, _, pos) -> pos.Index |> Equal 9L; pos.Line |> Equal 5L; pos.Column |> Equal 2L
    | _ -> Fail()
    match run spaces1 "\n \r\t\t\r\n\n " with
    | Success(_, _, pos) -> pos.Index |> Equal 9L; pos.Line |> Equal 5L; pos.Column |> Equal 2L
    | _ -> Fail()

    match run unicodeSpaces "\n \r\t\t\r\n\n \u0085\u000C\u2028\u2029 \r\n\t\u200A" with
    | Success(_, _, pos) -> pos.Index |> Equal 18L; pos.Line |> Equal 9L; pos.Column |> Equal 3L
    | _ -> Fail()
    match run unicodeSpaces1 "\n \r\t\t\r\n\n \u0085\u000C\u2028\u2029 \r\n\t\u200A" with
    | Success(_, _, pos) -> pos.Index |> Equal 18L; pos.Line |> Equal 9L; pos.Column |> Equal 3L
    | _ -> Fail()

    eof |> ROk "" 0 ()
    (pchar '1' >>. eof) |> ROk "1" 1 ()
    eof |> RError "1" 0 Errors.ExpectedEndOfInput


let testStringParsers() =
    pstring "" |> ROk "1" 0 ""

    pstring "1" |> RError "" 0 (expectedString "1")
    pstring "1" |> RError "2" 0 (expectedString "1")
    pstring "1" |> ROk "1" 1 "1"

    pstring "12" |> RError ""   0 (expectedString "12")
    pstring "12" |> RError "1"  0 (expectedString "12")
    pstring "12" |> RError "22" 0 (expectedString "12")
    pstring "12" |> RError "13" 0 (expectedString "12")
    pstring "12" |> ROk "12" 2 "12"

    pstring      "test" |> RError "pest" 0 (expectedString "test")
    pstring      "test" |> ROk "test" 4 "test"
    skipString   "test" |> ROk "test" 4 ()
    stringReturn "test" -1 |> ROk "test" 4 -1

    try pstring "\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try pstring "\n" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try pstring "\uffff" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try pstring "\r1" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try pstring "1\n" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try pstring "12\n" |> ignore; Fail()
    with :? System.ArgumentException -> ()

    pstringCI    "t" |> RError "p" 0 (expectedStringCI "t")
    pstringCI    "t" |> ROk "t" 1 "t"
    pstringCI    "t" |> ROk "T" 1 "T"
    pstringCI    "T" |> ROk "t" 1 "t"
    pstringCI    "T" |> ROk "T" 1 "T"
    skipStringCI "t" |> RError "p" 0 (expectedStringCI "t")
    skipStringCI "t" |> ROk "t" 1 ()
    skipStringCI "t" |> ROk "T" 1 ()
    skipStringCI "T" |> ROk "t" 1 ()
    skipStringCI "T" |> ROk "T" 1 ()

    pstringCI      "tEsT"    |> RError "pest" 0 (expectedStringCI "tEsT")
    pstringCI      "tEsT"    |> ROk "TeSt" 4 "TeSt"
    skipStringCI   "tEsT"    |> RError "pest" 0 (expectedStringCI "tEsT")
    skipStringCI   "tEsT"    |> ROk "TeSt" 4 ()
    stringCIReturn "tEsT" -1 |> ROk "TeSt" 4 -1

    try skipStringCI "\n" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try skipStringCI "12\n" |> ignore; Fail()
    with :? System.ArgumentException -> ()

    anyString 3      |> RError "12" 0 (Errors.ExpectedAnySequenceOfNChars(3))
    skipAnyString 3  |> RError "12" 0 (Errors.ExpectedAnySequenceOfNChars(3))
    anyString 3      |> ROkNL "12\r\n4" 4 "12\n"
    skipAnyString 3  |> ROkNL "12\r\n4" 4 ()

    skipped (skipAnyString 3) |> RError "12" 0 (Errors.ExpectedAnySequenceOfNChars(3))
    skipAnyString 3 |> withSkippedString (fun str () -> str) |> RError "12" 0 (Errors.ExpectedAnySequenceOfNChars(3))
    skipped (skipAnyString 3) |> ROk "123" 3 "123"
    skipAnyString 3 |> withSkippedString (fun str () -> str) |> ROk "123" 3 "123"
    skipped (skipAnyString 3) |> ROkNL "12\r\n4" 4 "12\n"
    skipAnyString 3 |> withSkippedString (fun str () -> str) |> ROkNL "12\r\n4" 4 "12\n"

    restOfLine     true  |> ROk "" 0 ""
    skipRestOfLine true  |> ROk "" 0 ()
    restOfLine     true  |> ROkNL "\r\n1"   2  ""
    skipRestOfLine true  |> ROkNL "\r\n1"   2  ()
    restOfLine     true  |> ROkNL "  \r\n1" 4  "  "
    skipRestOfLine true  |> ROkNL "  \r\n1" 4  ()

    restOfLine     false |> ROk "" 0 ""
    skipRestOfLine false |> ROk "" 0 ()
    restOfLine     false |> ROk "\r\n1"   0  ""
    skipRestOfLine false |> ROk "\r\n1"   0  ()
    restOfLine     false |> ROk "  \r\n1" 2  "  "
    skipRestOfLine false |> ROk "  \r\n1" 2  ()

    regex "abc"               |> ROk    "abc" 3 "abc"
    (anyChar >>. regex "abc") |> ROk    "_abc" 4 "abc"
    regex ".*\r\r\n.*"        |> ROkNL  "abc\r\r\nabc" 9 "abc\n\nabc"
    regex "abc"               |> RError "ab" 0 (Errors.ExpectedStringMatchingRegex("abc"))
    regexL "abc" "test"       |> RError "ab" 0 (expected "test")

let testIdentifier() =
    // We do most of the testing in IdentifierValidatorTests.fs.
    // Here we only test the identifier parser wrapper.

    let U =  System.Char.ConvertFromUtf32
    let ud800 = string (char 0xd800)
    let a_ud800 = "a" + ud800
    let mc2 = "MC" + (string '²')
    let s1 = U 0x00010280

    let expectedIdentifierError = expected Strings.Identifier
    let invalidCharacterError = messageError Strings.IdentifierContainsInvalidCharacterAtIndicatedPosition

    let defaultOpts = IdentifierOptions()
    identifier defaultOpts |> RError ""  0 expectedIdentifierError
    identifier defaultOpts |> RError "1" 0 expectedIdentifierError
    identifier defaultOpts |> RFatalError ud800 0 invalidCharacterError
    identifier defaultOpts |> RFatalError a_ud800 1 invalidCharacterError

    identifier defaultOpts |> ROk "a" 1 "a"
    identifier defaultOpts |> ROk "abc1" 4 "abc1"

    identifier defaultOpts |> ROk s1 2 s1

    identifier defaultOpts |> RFatalError "क्‍" 2 invalidCharacterError
    identifier (IdentifierOptions(allowJoinControlChars=true)) |> ROk "क्‍" 3 "क्‍"

    identifier (IdentifierOptions(label="test")) |> RError "1" 0 (expected "test")
    identifier (IdentifierOptions(invalidCharMessage="test")) |> RFatalError "क्‍" 2 (messageError "test")

    let normOpts = IdentifierOptions(normalization=System.Text.NormalizationForm.FormKC)
    let preNormOpts = IdentifierOptions(normalization=System.Text.NormalizationForm.FormKC,
                                        normalizeBeforeValidation=true,
                                        preCheckContinue= fun c -> FParsec.IdentifierValidator.IsXIdContinueOrSurrogate(c) || c > '\u007f')

    identifier defaultOpts |> ROk "ϒ\u0308" 2 "ϒ\u0308"
    identifier normOpts    |> ROk "ϒ\u0308" 2 "\u03AB"

    identifier defaultOpts |> ROk mc2 2 "MC"
    identifier normOpts    |> ROk mc2 2 "MC"
    identifier preNormOpts |> ROk mc2 3 "MC2"

    let abOpts = IdentifierOptions(isAsciiIdStart=((=) 'a'), isAsciiIdContinue=((=) 'b'))

    identifier abOpts |> RError "b"  0 (expected Strings.Identifier)
    identifier abOpts |> ROk "aa" 1 "a"
    identifier abOpts |> ROk "abc" 2 "ab"

    let abNonAsciiOpts = IdentifierOptions(isAsciiIdStart=((=) 'a'), isAsciiIdContinue=((=) 'b'),
                                           allowAllNonAsciiCharsInPreCheck = true)

    identifier abNonAsciiOpts |> RError "b"  0 (expected Strings.Identifier)
    identifier abNonAsciiOpts |> ROk "aa" 1 "a"
    identifier abNonAsciiOpts |> ROk "abc" 2 "ab"
    identifier abNonAsciiOpts |> ROk "abä" 3 "abä"
    identifier abNonAsciiOpts |> RFatalError "ab\uFB1C" 2 invalidCharacterError

    let abPreOpts = IdentifierOptions(isAsciiIdStart=((=) 'a'), isAsciiIdContinue=((=) 'b'),
                                      preCheckStart    = (fun c -> c >= 'a' && c <= 'b'),
                                      preCheckContinue = (fun c -> c >= 'b' && c <= 'c'))
    identifier abPreOpts |> RFatalError "b" 0   invalidCharacterError
    identifier abPreOpts |> RFatalError "abc" 2 invalidCharacterError

    let abPreNonAsciiOpts = IdentifierOptions(isAsciiIdStart=((=) 'a'), isAsciiIdContinue=((=) 'b'),
                                              preCheckStart    = (fun c -> c >= 'a' && c <= 'b'),
                                              preCheckContinue = (fun c -> c >= 'b' && c <= 'c'),
                                              allowAllNonAsciiCharsInPreCheck = true)
    identifier abPreNonAsciiOpts |> RFatalError "b" 0   invalidCharacterError
    identifier abPreNonAsciiOpts |> RFatalError "abc" 2 invalidCharacterError
    identifier abPreNonAsciiOpts |> ROk "abä" 3 "abä"


let testManySatisfy() =
    manySatisfy  isDigit       |> ROk ""     0 ""
    manySatisfy2 isHex isDigit |> ROk ""     0 ""
    manySatisfy  isDigit       |> ROk "123"  3 "123"
    manySatisfy2 isHex isDigit |> ROk "a23a" 3 "a23"

    skipManySatisfy  isDigit       |> ROk ""     0 ()
    skipManySatisfy2 isHex isDigit |> ROk ""     0 ()
    skipManySatisfy  isDigit       |> ROk "123"  3 ()
    skipManySatisfy2 isHex isDigit |> ROk "a23a" 3 ()

    many1Satisfy   isDigit              |> RError "a" 0 NoErrorMessages
    many1Satisfy2  isHex isDigit        |> RError "g" 0 NoErrorMessages
    many1SatisfyL  isDigit "test"       |> RError "a" 0 (expected "test")
    many1Satisfy2L isHex isDigit "test" |> RError "g" 0 (expected "test")
    many1Satisfy   isDigit              |> ROk "123"  3 "123"
    many1Satisfy2  isHex isDigit        |> ROk "a23a" 3 "a23"

    skipMany1SatisfyL  isDigit "test"       |> RError "a" 0 (expected "test")
    skipMany1Satisfy2L isHex isDigit "test" |> RError "g" 0 (expected "test")
    skipMany1Satisfy   isDigit              |> ROk "123"  3 ()
    skipMany1Satisfy2  isHex isDigit        |> ROk "a23a" 3 ()

    manyMinMaxSatisfy   0 3 isDigit              |> ROk    "1234" 3 "123"
    manyMinMaxSatisfy   3 3 isDigit              |> ROk    "1234" 3 "123"
    manyMinMaxSatisfyL  4 4 isDigit "test"       |> RError "123a" 0  (expected "test")
    manyMinMaxSatisfy2  0 3 isHex isDigit        |> ROk    "a234" 3 "a23"
    manyMinMaxSatisfy2  3 3 isHex isDigit        |> ROk    "a234" 3 "a23"
    manyMinMaxSatisfy2L 4 4 isHex isDigit "test" |> RError "a23a" 0 (expected "test")

    skipManyMinMaxSatisfy   0 3 isDigit              |> ROk "1234" 3 ()
    skipManyMinMaxSatisfy   3 3 isDigit              |> ROk "1234" 3 ()
    skipManyMinMaxSatisfyL  4 4 isDigit "test"       |> RError "123a" 0 (expected "test")
    skipManyMinMaxSatisfy2  0 3 isHex isDigit        |> ROk "a234" 3 ()
    skipManyMinMaxSatisfy2  3 3 isHex isDigit        |> ROk "a234" 3 ()
    skipManyMinMaxSatisfy2L 4 4 isHex isDigit "test" |> RError "a23a" 0 (expected "test")

    try manyMinMaxSatisfy 0 -1 isDigit |> ROk "1234" 3 "123"; Fail()
    with :? System.ArgumentException -> ()

    try skipManyMinMaxSatisfy 0 -1 isDigit |> ROk "1234" 3 (); Fail()
    with :? System.ArgumentException -> ()

let testMany() =
    let ps1  = (constantTestParsers '1' (expected "1")).[1..] // no parser that returns OK without changing the state
    let ps2  = (constantTestParsers '2' (expected "2")).[1..]
    let ps3  = (constantTestParsers '3' (expected "3")).[1..]

    let content = "the content doesn't matter"
    use stream = new FParsec.CharStream<int>(content, 0, content.Length)

    let many1Chars2Ref p1 p = Inline.Many((fun c -> (new System.Text.StringBuilder()).Append(c: char)),
                                          (fun sb c -> sb.Append(c)),
                                          (fun sb -> sb.ToString()),
                                          p, p1)
    let manyChars2Ref p1 p = many1Chars2Ref p1 p <|>% ""

    let manySeq2 = seq {for p2 in ps2 do
                        for p3 in ps3 do
                            yield [p2; p3]}

    for p1 in ps1 do
        for ps in manySeq2 do
            let p_1, p_2, pr = seqParserAndReset2 ps

            checkParser (manyChars2 p1 p_1)      (manyChars2Ref p1 p_2)  stream; pr()
            checkParser (many1Chars2 p1 p_1)     (many1Chars2Ref p1 p_2) stream; pr()

    manyChars digit |> ROkE "123" 3 "123" Errors.ExpectedDecimalDigit
    many1Chars digit |> ROkE "123" 3 "123" Errors.ExpectedDecimalDigit

    try manyChars (preturn ' ') stream |> ignore; Fail()
    with :? System.InvalidOperationException -> ()

    let anyCharWithIndexMessage : Parser<char,_> =
        fun stream ->
            let c = stream.ReadCharOrNewline()
            if c <> EOS then
                Reply(Ok, c, messageError (string stream.Index))
            else
                Reply(Error, Errors.ExpectedAnyChar)

    let sb = new System.Text.StringBuilder()
    for i = 1 to 200 do
        let s = sb.Append(char (i%10)).ToString()
        manyChars (anyCharWithIndexMessage)
        |> ROkE s s.Length s (mergeErrors (messageError (string i)) Errors.ExpectedAnyChar)

    sb.Length <- 0 // Clear() is only supported in >= .NET 4
    for i = 1 to 200 do
        let s = sb.Append(char (i%10)).ToString()
        manyCharsTill (anyCharWithIndexMessage) eof
        |> ROkE s s.Length s (messageError (string i))

    let eps1 = constantTestParsers 1 (expected "11")
    let eps2 = constantTestParsers 2 (expected "22")
    let eps3 = constantTestParsers 3 (expected "33")

    let manyCharsTillRef p endp = Inline.ManyTill((fun c -> (new System.Text.StringBuilder()).Append(c: char)),
                                                  (fun sb c -> sb.Append(c)),
                                                  (fun sb _ -> sb.ToString()),
                                                  p, endp,
                                                  resultForEmptySequence = (fun _ -> ""))

    let many1CharsTillRef p endp = pipe2 p (manyCharsTillRef p endp) (fun c0 s -> string c0 + s)

    let manyTillSeq =
        seq {for endp1 in eps1 do
             for p1    in ps1 do
             for endp2 in eps2 do
             for p2    in ps2 do
             for endp3 in eps3 do
             for p3    in ps3.[1..] do
             yield [p1; p2; p3;], [endp1; endp2; endp3; eps3.[1]]}

    for ps, es in manyTillSeq do
        let p_1, p_2, pr = seqParserAndReset2 ps
        let e_1, e_2, er = seqParserAndReset2 es
        checkParser (manyCharsTill     p_1 e_1) (manyCharsTillRef     p_2 e_2) stream; pr(); er()
        checkParser (many1CharsTill    p_1 e_1) (many1CharsTillRef    p_2 e_2) stream; pr(); er()

    manyCharsTill2 letter digit (pchar '.') |> ROk "a23." 4 "a23"
    many1CharsTill2 letter digit (pchar '.') |> ROk "a23." 4 "a23"

    manyCharsTillApply digit (pchar '.') (fun str c -> str + string c) |> ROk "23." 3 "23."
    many1CharsTillApply  digit (pchar '.') (fun str c -> str + string c) |> ROk "23." 3 "23."

    try manyCharsTill (preturn ' ') (fail "t") stream |> ignore; Fail()
    with :? System.InvalidOperationException  -> ()

    try many1CharsTill (preturn ' ') (fail "t") stream |> ignore; Fail()
    with :? System.InvalidOperationException  -> ()

    let sps1  = constantTestParsers "1" (expected "1")
    let sps2  = constantTestParsers "2" (expected "2")
    let sps3  = constantTestParsers "3" (expected "3")
    let sps4  = constantTestParsers "4" (expected "4")
    let sps5  = constantTestParsers "5" (expected "5")
    let sps6  = constantTestParsers "6" (expected "6")
    let sps7  = constantTestParsers "7" (expected "7")

    let manyStringsRef p  = many p |>> List.fold (fun acc s -> acc + s) ""
    let many1StringsRef p = many1 p |>> List.reduce (+)

    let manySeq7 = seq {for p1 in sps1.[1..] do
                        for p2 in sps2.[1..] do
                        for p3 in sps3.[1..] do
                        for p4 in sps4.[1..] do
                        for p5 in sps5.[1..] do
                        for p6 in sps6.[1..] do
                        for p7 in sps7.[1..] do
                            yield [p1;p2;p3;p4;p5;p6;p7]}

    let sw = new System.Diagnostics.Stopwatch()
    for ps in manySeq7 do
        let p_1, p_2, pr = seqParserAndReset2 ps

        checkParser (manyStrings  p_1) (manyStringsRef  p_2) stream; pr()
        checkParser (many1Strings p_1) (many1StringsRef p_2) stream

    manyStrings2 (pstring "1") (pstring "2") |> ROkE "12223" 4 "1222" (expectedString "2")

    try manyStrings (preturn "1") stream |> ignore; Fail()
    with :? System.InvalidOperationException  -> ()

    let sepByTestParsers r1 e1 r2 e2 =
        let p1s = constantTestParsers r1 e1
        let p2s = constantTestParsers r2 e2
        seq {for p1 in p1s.[1..] do
                for p2 in p2s do
                    yield p1, p2}

    let sepBySeq3 =
        seq {for p1       in (constantTestParsers "1" (expected "p1")).[1..] do
              for sep1, p2 in sepByTestParsers "a" (expected "sep1") "2" (expected "p2") do
               for sep2, p3 in sepByTestParsers "b" (expected "sep2") "3" (expected "p3") do
                for sep3, p4 in sepByTestParsers "c" (expected "sep3") "4" (expected "p4") do
                    yield [p1; p2; p3; p4], [sep1; sep2; sep3]

            // We exclude the following parameter combinations from regular test runs
            // because executing all of them just takes too much time.
            (*
                   for sep4, p5 in sepByTestParsers "d" (expected "sep4") "5" (expected "p5") do
                      yield [p1; p2; p3; p4; p5], [sep1; sep2; sep3; sep4]

             for p1, sep1 in sepByTestParsers "1" (expected "p1") "a" (expected "sep1") do
              for p2, sep2 in sepByTestParsers "2" (expected "p2") "b" (expected "sep2") do
               for p3, sep3 in sepByTestParsers "3" (expected "p3") "c" (expected "sep3") do
                for p4, sep4 in sepByTestParsers "4" (expected "p4") "d" (expected "sep4") do
                 for p5 in (constantTestParsers "5" (expected "p5")).[1..] do
                    yield [p1; p2; p3; p4; p5], [sep1; sep2; sep3; sep4]  *)
              }


    let mutable i = 0
    let userState0 = stream.UserState
    let tag0 = stream.StateTag
    for ps, ss in sepBySeq3 do
        i <- i + 1
        let p, pr = seqParserAndReset ps
        let s, sr = seqParserAndReset ss
        checkParser (stringsSepBy p s)
                    (fun stream ->
                        pr(); sr()
                        let r = sepBy p s stream
                        let result =
                            if r.Status <> Ok then null
                            else match r.Result with
                                    | []          -> ""
                                    | [_]         -> "1"
                                    | [_;_]       -> "1a2"
                                    | [_;_;_]     -> "1a2b3"
                                    | [_;_;_;_]   -> "1a2b3c4"
                                    | [_;_;_;_;_] -> "1a2b3c4d5"
                                    | _ -> failwith "manyStringsSepByTest"
                        Reply(r.Status, result, r.Error)) stream

    try stringsSepBy (preturn "1") (preturn ";") stream |> ignore; Fail()
    with :? System.InvalidOperationException  -> ()


let testSkipToString() =
    charsTillString "abc" false System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindString("abc"))
    charsTillString "abc" false System.Int32.MaxValue |> ROk "abc"    0 ""
    charsTillString "abc" false 0                     |> ROk "abc"    0 ""
    charsTillString "abc" false System.Int32.MaxValue |> ROk "abdabc" 3 "abd"
    charsTillString "abc" false 3                     |> ROk "abdabc" 3 "abd"
    charsTillString "abc" false 2 |> RError "abdabc" 2 (Errors.CouldNotFindString("abc"))

    charsTillStringCI "AbC" false System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindCaseInsensitiveString("AbC"))
    charsTillStringCI "AbC" false System.Int32.MaxValue |> ROk "aBc"    0 ""
    charsTillStringCI "AbC" false 0                     |> ROk "abc"    0 ""
    charsTillStringCI "AbC" false System.Int32.MaxValue |> ROk "aBdaBc" 3 "aBd"
    charsTillStringCI "AbC" false 3                     |> ROk "aBdaBc" 3 "aBd"
    charsTillStringCI "AbC" false 2 |> RError "aBdaBc" 2 (Errors.CouldNotFindCaseInsensitiveString("AbC"))

    skipCharsTillString "abc" false System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindString("abc"))
    skipCharsTillString "abc" false System.Int32.MaxValue |> ROk "abc"    0 ()
    skipCharsTillString "abc" false 0                     |> ROk "abc"    0 ()
    skipCharsTillString "abc" false System.Int32.MaxValue |> ROk "abdabc" 3 ()
    skipCharsTillString "abc" false 3                     |> ROk "abdabc" 3 ()
    skipCharsTillString "abc" false 2 |> RError "abdabc" 2 (Errors.CouldNotFindString("abc"))

    skipCharsTillStringCI "AbC" false System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindCaseInsensitiveString("AbC"))
    skipCharsTillStringCI "AbC" false System.Int32.MaxValue |> ROk "aBc"    0 ()
    skipCharsTillStringCI "AbC" false 0                     |> ROk "abc"    0 ()
    skipCharsTillStringCI "AbC" false System.Int32.MaxValue |> ROk "aBdaBc" 3 ()
    skipCharsTillStringCI "AbC" false 3                     |> ROk "aBdaBc" 3 ()
    skipCharsTillStringCI "AbC" false 2 |> RError "aBdaBc" 2 (Errors.CouldNotFindCaseInsensitiveString("AbC"))

    charsTillString "abc" true System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindString("abc"))
    charsTillString "abc" true System.Int32.MaxValue |> ROk "abc"    3 ""
    charsTillString "abc" true 0                     |> ROk "abc"    3 ""
    charsTillString "abc" true System.Int32.MaxValue |> ROk "abdabc" 6 "abd"
    charsTillString "abc" true 3                     |> ROk "abdabc" 6 "abd"
    charsTillString "abc" true 2                     |> RError "abdabc" 2 (Errors.CouldNotFindString("abc"))

    charsTillStringCI "AbC" true System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindCaseInsensitiveString("AbC"))
    charsTillStringCI "AbC" true System.Int32.MaxValue |> ROk "aBc"    3 ""
    charsTillStringCI "AbC" true 0                     |> ROk "abc"    3 ""
    charsTillStringCI "AbC" true System.Int32.MaxValue |> ROk "aBdaBc" 6 "aBd"
    charsTillStringCI "AbC" true 3                     |> ROk "aBdaBc" 6  "aBd"
    charsTillStringCI "AbC" true 2                     |> RError "aBdaBc" 2 (Errors.CouldNotFindCaseInsensitiveString("AbC"))

    skipCharsTillString "abc" true System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindString("abc"))
    skipCharsTillString "abc" true System.Int32.MaxValue |> ROk "abc"    3 ()
    skipCharsTillString "abc" true 0                     |> ROk "abc"    3 ()
    skipCharsTillString "abc" true System.Int32.MaxValue |> ROk "abdabc" 6 ()
    skipCharsTillString "abc" true 3                     |> ROk "abdabc" 6 ()
    skipCharsTillString "abc" true 2                     |> RError "abdabc" 2 (Errors.CouldNotFindString("abc"))

    skipCharsTillStringCI "AbC" true System.Int32.MaxValue |> RError "abbab" 5 (Errors.CouldNotFindCaseInsensitiveString("AbC"))
    skipCharsTillStringCI "AbC" true System.Int32.MaxValue |> ROk "aBc"    3 ()
    skipCharsTillStringCI "AbC" true 0                     |> ROk "abc"    3 ()
    skipCharsTillStringCI "AbC" true System.Int32.MaxValue |> ROk "aBdaBc" 6 ()
    skipCharsTillStringCI "AbC" true 3                     |> ROk "aBdaBc" 6 ()
    skipCharsTillStringCI "AbC" true 2                     |> RError "aBdaBc" 2 (Errors.CouldNotFindCaseInsensitiveString("AbC"))

    try charsTillString "1\r" false 1 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try charsTillStringCI "1\r" false 1 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try skipCharsTillString "1\r" false 1 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try skipCharsTillStringCI "1\r" false 1 |> ignore; Fail()
    with :? System.ArgumentException -> ()

    try charsTillString "1" false -1 |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try charsTillStringCI "1" false -1 |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try skipCharsTillString "1" false -1 |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try skipCharsTillStringCI "1" false -1 |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()


let testNumberParsers() =
    let ROkI   content i result parser = ROk content i result parser
    let ROk    content   result parser = ROk content (content.Length - 1) result parser

    let testNumberLiteral() =
        let all =    NLO.AllowSuffix
                 ||| NLO.AllowMinusSign
                 ||| NLO.AllowPlusSign
                 ||| NLO.AllowFraction
                 ||| NLO.AllowFractionWOIntegerPart
                 ||| NLO.AllowExponent
                 ||| NLO.AllowHexadecimal
                 ||| NLO.AllowBinary
                 ||| NLO.AllowOctal
                 ||| NLO.AllowInfinity
                 ||| NLO.AllowNaN


        numberLiteral all "nl" |> RError "|"  0 (expected "nl")
        numberLiteral all "nl" |> RError "+|" 0 (expected "nl")
        numberLiteral all "nl" |> RError "-|" 0 (expected "nl")
        numberLiteral all "nl" |> RError "+n" 0 (expected "nl")
        numberLiteral all "nl" |> RError "-n" 0 (expected "nl")
        numberLiteral all "nl" |> ROk "0|"     (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+0|"    (NumberLiteral("+0", NLF.HasPlusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "-0|"    (NumberLiteral("-0", NLF.HasMinusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))

        numberLiteral all "nl" |> ROk "0u|"    (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 1), 'u', EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0az|"   (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 2), 'a', 'z', EOS, EOS))
        numberLiteral all "nl" |> ROk "0uAZ|"  (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 3), 'u', 'A', 'Z', EOS))
        numberLiteral all "nl" |> ROk "0ulLF|" (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 4), 'u', 'l', 'L', 'F'))

        let all2 = all ||| NLO.IncludeSuffixCharsInString

        numberLiteral all2 "nl" |> ROk "0u|"    (NumberLiteral("0u", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 1), 'u', EOS, EOS, EOS))
        numberLiteral all2 "nl" |> ROk "0az|"   (NumberLiteral("0az", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 2), 'a', 'z', EOS, EOS))
        numberLiteral all2 "nl" |> ROk "0uAZ|"  (NumberLiteral("0uAZ", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 3), 'u', 'A', 'Z', EOS))
        numberLiteral all2 "nl" |> ROk "0ulLF|" (NumberLiteral("0ulLF", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 4), 'u', 'l', 'L', 'F'))

        numberLiteral all "nl" |> ROk ".0|"       (NumberLiteral(".0", NLF.IsDecimal ||| NLF.HasFraction, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "1.|"       (NumberLiteral("1.", NLF.IsDecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk ".0E0|"     (NumberLiteral(".0E0", NLF.IsDecimal ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+0e-123f|" (NumberLiteral("+0e-123", NLF.IsDecimal ||| NLF.HasPlusSign ||| NLF.HasIntegerPart ||| NLF.HasExponent ||| ((enum) 1), 'f', EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0.1E+123|" (NumberLiteral("0.1E+123", NLF.IsDecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))

        numberLiteral all "nl" |> ROk  "0.0E0|"     (NumberLiteral("0.0E0", NLF.IsDecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "9.9E9|"     (NumberLiteral("9.9E9", NLF.IsDecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "00.00E00|"  (NumberLiteral("00.00E00", NLF.IsDecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "99.99E99|"  (NumberLiteral("99.99E99", NLF.IsDecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "-909.090e-09909z|" (NumberLiteral("-909.090e-09909", NLF.HasMinusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent ||| (enum) 1, 'z', EOS, EOS, EOS))

        numberLiteral all "nl" |> ROk  "0x.0|"     (NumberLiteral("0x.0", NLF.IsHexadecimal ||| NLF.HasFraction, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "0x0.|"     (NumberLiteral("0x0.", NLF.IsHexadecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "0X.fP0|"   (NumberLiteral("0X.fP0", NLF.IsHexadecimal ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "+0xFp-0f|" (NumberLiteral("+0xFp-0", NLF.HasPlusSign ||| NLF.IsHexadecimal ||| NLF.HasIntegerPart ||| NLF.HasExponent ||| ((enum) 1), 'f', EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk  "0xf.0AP+123|" (NumberLiteral("0xf.0AP+123", NLF.IsHexadecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))

        numberLiteral all "nl" |> ROk "0x0.0P0|"    (NumberLiteral("0x0.0P0", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xff.fp9|"   (NumberLiteral("0xff.fp9", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xa.aP9|"    (NumberLiteral("0xa.aP9", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xF.Fp0|"    (NumberLiteral("0xF.Fp0", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xA.AP9|"    (NumberLiteral("0xA.AP9", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0x00.00P00|" (NumberLiteral("0x00.00P00", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xff.ffp99|" (NumberLiteral("0xff.ffp99", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xaa.aaP99|" (NumberLiteral("0xaa.aaP99", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xFF.FFp00|" (NumberLiteral("0xFF.FFp00", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0xAA.AAP99|" (NumberLiteral("0xAA.AAP99", NLF.IsHexadecimal |||  NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+0x0afFA0.afFA0P+9099A|" (NumberLiteral("+0x0afFA0.afFA0P+9099", NLF.HasPlusSign ||| NLF.IsHexadecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction ||| NLF.HasExponent ||| (enum) 1, 'A', EOS, EOS, EOS))

        numberLiteral all "nl" |> ROk "0b02"      (NumberLiteral("0b0", NLF.IsBinary ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "-0B0102"   (NumberLiteral("-0B010", NLF.HasMinusSign ||| NLF.IsBinary ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+0B010ul|" (NumberLiteral("+0B010", NLF.HasPlusSign ||| NLF.IsBinary ||| NLF.HasIntegerPart ||| (enum) 2, 'u', 'l', EOS, EOS))

        numberLiteral all "nl" |> ROk "-0o08"      (NumberLiteral("-0o0", NLF.HasMinusSign ||| NLF.IsOctal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0O0778"     (NumberLiteral("0O077", NLF.IsOctal  ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+0o1770ul|" (NumberLiteral("+0o1770", NLF.HasPlusSign ||| NLF.IsOctal ||| NLF.HasIntegerPart ||| (enum) 2, 'u', 'l', EOS, EOS))

        numberLiteral all "nl" |> ROk "Infinityy"  (NumberLiteral("Infinity", NLF.IsInfinity, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "-InFINitYy" (NumberLiteral("-InFINitY", NLF.HasMinusSign ||| NLF.IsInfinity, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+iNfi"      (NumberLiteral("+iNf", NLF.HasPlusSign ||| NLF.IsInfinity, EOS, EOS, EOS, EOS))

        numberLiteral all "nl" |> ROk "NaNn"  (NumberLiteral("NaN", NLF.IsNaN, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "-nAna" (NumberLiteral("-nAn", NLF.HasMinusSign ||| NLF.IsNaN, EOS, EOS, EOS, EOS))

        numberLiteral all "nl" |> RError ".a"    1 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError ".ea"   1 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError ".E-1"  1 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError ".1ea"  3 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError "-1ea"  3 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError "1.e-a" 4 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError "1e+a"  3 Errors.ExpectedDecimalDigit

        numberLiteral all "nl" |> RError "0x.g"    3 Errors.ExpectedHexadecimalDigit
        numberLiteral all "nl" |> RError "0x.pa"   3 Errors.ExpectedHexadecimalDigit
        numberLiteral all "nl" |> RError "0x.p-1"   3 Errors.ExpectedHexadecimalDigit
        numberLiteral all "nl" |> RError "+0x.1pa" 6 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError "0x1pa"   4 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError "0x1.p-a" 6 Errors.ExpectedDecimalDigit
        numberLiteral all "nl" |> RError "0x1p+a"  5 Errors.ExpectedDecimalDigit

        numberLiteral all "nl" |> RError "0b3"   2 Errors.ExpectedBinaryDigit
        numberLiteral all "nl" |> RError "-0b.0" 3 Errors.ExpectedBinaryDigit
        numberLiteral all "nl" |> RError "+0ou"  3 Errors.ExpectedOctalDigit
        numberLiteral all "nl" |> RError "0o.0"  2 Errors.ExpectedOctalDigit

        numberLiteral (all ^^^ NLO.AllowPlusSign)  "nl" |> RError "+1|" 0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowPlusSign)  "nl" |> ROk    "-1|" (NumberLiteral("-1", NLF.HasMinusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowMinusSign) "nl" |> RError "-1|" 0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowMinusSign) "nl" |> ROk    "+1|" (NumberLiteral("+1", NLF.HasPlusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ (NLO.AllowPlusSign ||| NLO.AllowMinusSign)) "nl" |> ROk "1|" (NumberLiteral("1", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))

        numberLiteral (all ^^^ NLO.AllowFractionWOIntegerPart) "nl" |> RError ".0|"   0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowFractionWOIntegerPart) "nl" |> RError "0x.0|" 2 Errors.ExpectedHexadecimalDigit

        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> ROk    "1."         (NumberLiteral("1", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> ROkI    "10.10E2" 2 (NumberLiteral("10", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> RError ".1"       0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> RError ".1"       0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> ROkI    "0x0.1p2" 3 (NumberLiteral("0x0", NLF.IsHexadecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> ROkI    "10.10E2" 2 (NumberLiteral("10", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))

        numberLiteral (all ^^^ NLO.AllowExponent) "nl" |> ROkI "1e1"     2 (NumberLiteral("1", NLF.IsDecimal ||| NLF.HasIntegerPart ||| (enum) 1, 'e', EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowExponent) "nl" |> ROkI "1.0e1"   4 (NumberLiteral("1.0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction ||| (enum) 1, 'e', EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowExponent) "nl" |> ROkI "0x1p1"   4 (NumberLiteral("0x1", NLF.IsHexadecimal ||| NLF.HasIntegerPart ||| (enum) 1, 'p', EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowExponent) "nl" |> ROkI "0x1.0p1" 6 (NumberLiteral("0x1.0", NLF.IsHexadecimal ||| NLF.HasIntegerPart ||| NLF.HasFraction ||| (enum) 1, 'p', EOS, EOS, EOS))

        numberLiteral (all ^^^ NLO.AllowSuffix) "nl" |> ROk  "0u"   (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowSuffix) "nl" |> ROk  "0x1u" (NumberLiteral("0x1", NLF.IsHexadecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))

        numberLiteral (all ^^^ NLO.AllowBinary) "nl"      |> ROkI "0b1|" 2 (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| (enum) 1, 'b', EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowOctal) "nl"       |> ROkI "0o1|" 2 (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| (enum) 1, 'o', EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowHexadecimal) "nl" |> ROkI "0x1|" 2 (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| (enum) 1, 'x', EOS, EOS, EOS))

        numberLiteral (all ^^^ NLO.AllowInfinity) "nl" |> RError  "Infinity|" 0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowInfinity) "nl" |> ROk     "NaN|" (NumberLiteral("NaN", NLF.IsNaN, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowNaN) "nl"      |> RError  "NaN|" 0 (expected "nl")
        numberLiteral (all ^^^ NLO.AllowNaN) "nl"      |> ROk     "Infinity|" (NumberLiteral("Infinity", NLF.IsInfinity, EOS, EOS, EOS, EOS))

    testNumberLiteral()

    let testPfloat() =
        pfloat |> RError "" 0 Errors.ExpectedFloatingPointNumber
        pfloat |> RError "-0x" 3 Errors.ExpectedHexadecimalDigit
        pfloat |> ROk "0|" 0.
        pfloat |> ROk "+0|" 0.
        pfloat |> ROk "-0|" -0.
        pfloat |> ROk "0x0|" 0.
        pfloat |> ROk "+0x0|" 0.
        pfloat |> ROk "-0X0|" -0.
        pfloat |> ROk "+123|" 123.
        pfloat |> ROk "+0x123|" (floatOfHexString "0x123")
        pfloat |> ROk "+123e2|" 123e2
        pfloat |> ROk "+0x123p2|" (floatOfHexString "0x123p2")
        pfloat |> ROk "-123.456e123|" -123.456e123
        pfloat |> RFatalError "1e99999|" 0 Errors.NumberOutsideOfDoubleRange
        pfloat |> RFatalError "0x1p99999|" 0 Errors.NumberOutsideOfDoubleRange
        pfloat |> ROk "-0x123cde.123afAcEp123|" (floatOfHexString "-0x123cde.123afAcEp123")
        pfloat |> ROk "-0x1.fffffffffffffp1023|"  -System.Double.MaxValue
        pfloat |> RFatalError "0x1.fffffffffffffp1024|" 0 Errors.NumberOutsideOfDoubleRange
        pfloat |> ROk "Inf|" System.Double.PositiveInfinity
        pfloat |> ROk "-Infinity|" System.Double.NegativeInfinity
        pfloat >>% 1 |> ROk  "NaN|" 1

    testPfloat()

    let testPuint64() =
        let expectedE = Errors.ExpectedUInt64
        let overflowE = Errors.NumberOutsideOfUInt64Range

        puint64 |> RError "" 0 expectedE
        puint64 |> RError "+1" 0 expectedE
        puint64 |> RError "-1" 0 expectedE
        puint64 |> RFatalError "18446744073709551620" 0 overflowE
        puint64 |> RFatalError "18446744073709551619" 0 overflowE
        puint64 |> RFatalError "18446744073709551618" 0 overflowE
        puint64 |> RFatalError "18446744073709551617" 0 overflowE
        puint64 |> RFatalError "18446744073709551616" 0 overflowE
        puint64 |> RFatalError "0000018446744073709551616" 0 overflowE
        puint64 |> RFatalError "111111111111111111111" 0 overflowE

        puint64 |> ROk "0|" 0UL
        puint64 |> ROk "000|" 0UL
        puint64 |> ROk "12345678901234567890|" 12345678901234567890UL
        puint64 |> ROk "18446744073709551615|" System.UInt64.MaxValue
        puint64 |> ROk "018446744073709551614|" (System.UInt64.MaxValue - 1UL)
        puint64 |> ROk "018446744073709551613|" (System.UInt64.MaxValue - 2UL)
        puint64 |> ROk "018446744073709551612|" (System.UInt64.MaxValue - 3UL)
        puint64 |> ROk "018446744073709551611|" (System.UInt64.MaxValue - 4UL)
        puint64 |> ROk "018446744073709551610|" (System.UInt64.MaxValue - 5UL)
        puint64 |> ROk "018446744073709551609|" (System.UInt64.MaxValue - 6UL)
        puint64 |> ROk "0000018446744073709551615|" System.UInt64.MaxValue

        puint64 |> RError "0x"  2 Errors.ExpectedHexadecimalDigit
        puint64 |> RError "+0x1" 0 expectedE
        puint64 |> RFatalError "0x10000000000000000" 0 overflowE
        puint64 |> RFatalError "0x11111111111111111" 0 overflowE
        puint64 |> RFatalError "0Xfffffffffffffffff" 0 overflowE

        puint64 |> ROk "0x0|" 0UL
        puint64 |> ROk "0x000|" 0UL
        puint64 |> ROk "0x1234567890abcdef|" 0x1234567890abcdefUL
        puint64 |> ROk "0X1234567890ABCDEF|" 0x1234567890abcdefUL
        puint64 |> ROk "0xffffffffffffffff|" System.UInt64.MaxValue
        puint64 |> ROk "0xfffffffffffffffe|" (System.UInt64.MaxValue - 1UL)
        puint64 |> ROk "0xfffffffffffffff1|" (System.UInt64.MaxValue - 14UL)
        puint64 |> ROk "0xfffffffffffffff0|" (System.UInt64.MaxValue - 15UL)
        puint64 |> ROk "0xffffffffffffffef|" (System.UInt64.MaxValue - 16UL)
        puint64 |> ROk "0x00000ffffffffffffffff|" System.UInt64.MaxValue

        puint64 |> RError "0o"  2 Errors.ExpectedOctalDigit
        puint64 |> RError "+0o1" 0 expectedE
        puint64 |> RFatalError "0o2000000000000000000001" 0 overflowE
        puint64 |> RFatalError "0o2000000000000000000000" 0 overflowE
        puint64 |> RFatalError "0o7777777777777777777777" 0 overflowE
        puint64 |> RFatalError "0O77777777777777777777777" 0 overflowE

        puint64 |> ROk "0o0|" 0UL
        puint64 |> ROk "0o000|" 0UL
        puint64 |> ROk "0o1234567123456701234567|" 0o1234567123456701234567UL
        puint64 |> ROk "0o1777777777777777777777|" System.UInt64.MaxValue
        puint64 |> ROk "0o1777777777777777777776|" (System.UInt64.MaxValue - 1UL)
        puint64 |> ROk "0o1777777777777777777771|" (System.UInt64.MaxValue - 6UL)
        puint64 |> ROk "0o1777777777777777777770|" (System.UInt64.MaxValue - 7UL)
        puint64 |> ROk "0o1777777777777777777767|" (System.UInt64.MaxValue - 8UL)
        puint64 |> ROk "0O000001777777777777777777777|" System.UInt64.MaxValue

        puint64 |> RError "0b"  2 Errors.ExpectedBinaryDigit
        puint64 |> RError "+0b1" 0 expectedE
        puint64 |> RFatalError "0b10000000000000000000000000000000000000000000000000000000000000001" 0 overflowE
        puint64 |> RFatalError "0b10000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        puint64 |> RFatalError "0b11111111111111111111111111111111111111111111111111111111111111111" 0 overflowE

        puint64 |> ROk "0b0|" 0UL
        puint64 |> ROk "0b000|" 0UL
        puint64 |> ROk "0b1111111111111111111111111111111111111111111111111111111111111111|" System.UInt64.MaxValue
        puint64 |> ROk "0b1111111111111111111111111111111111111111111111111111111111111110|" (System.UInt64.MaxValue - 1UL)
        puint64 |> ROk "0b1111111111111111111111111111111111111111111111111111111111111101|" (System.UInt64.MaxValue - 2UL)
        puint64 |> ROk "0b1111111111111111111111111111111111111111111111111111111111111100|" (System.UInt64.MaxValue - 3UL)
        puint64 |> ROk "0B000001111111111111111111111111111111111111111111111111111111111111111|" System.UInt64.MaxValue

    testPuint64()

    let testPint64() =
        let expectedE = Errors.ExpectedInt64
        let overflowE = Errors.NumberOutsideOfInt64Range

        pint64 |> RFatalError "18446744073709551615" 0 overflowE
        pint64 |> RFatalError "+00018446744073709551615" 0 overflowE
        pint64 |> RFatalError "-18446744073709551615" 0 overflowE
        pint64 |> RFatalError "-0018446744073709551615" 0 overflowE

        pint64 |> RFatalError "9223372036854775808" 0 overflowE
        pint64 |> RFatalError "+0009223372036854775808" 0 overflowE
        pint64 |> RFatalError "-9223372036854775809" 0 overflowE
        pint64 |> RFatalError "-09223372036854775809" 0 overflowE

        pint64 |> ROk "9223372036854775807|" System.Int64.MaxValue
        pint64 |> ROk "+000009223372036854775807|" System.Int64.MaxValue
        pint64 |> ROk "-9223372036854775808|" System.Int64.MinValue
        pint64 |> ROk "-009223372036854775808|" System.Int64.MinValue

        pint64 |> RFatalError "0xffffffffffffffff" 0 overflowE
        pint64 |> RFatalError "+0x000ffffffffffffffff" 0 overflowE
        pint64 |> RFatalError "-0xffffffffffffffff" 0 overflowE
        pint64 |> RFatalError "-0X0ffffffffffffffff" 0 overflowE

        pint64 |> RFatalError "0x8000000000000000" 0 overflowE
        pint64 |> RFatalError "+0x0008000000000000000" 0 overflowE
        pint64 |> RFatalError "-0x8000000000000001" 0 overflowE
        pint64 |> RFatalError "-0x0008000000000000001" 0 overflowE

        pint64 |> ROk "0x7fffffffffffffff|" System.Int64.MaxValue
        pint64 |> ROk "+0x000007fffffffffffffff|" System.Int64.MaxValue
        pint64 |> ROk "-0x8000000000000000|" System.Int64.MinValue
        pint64 |> ROk "-0x008000000000000000|" System.Int64.MinValue

        pint64 |> RFatalError "0o2000000000000000000000" 0 overflowE
        pint64 |> RFatalError "+0o002000000000000000000000" 0 overflowE
        pint64 |> RFatalError "-0o0002000000000000000000000" 0 overflowE

        pint64 |> RFatalError "0o1000000000000000000000" 0 overflowE
        pint64 |> RFatalError "0o1000000000000000000000" 0 overflowE
        pint64 |> RFatalError "+0o001000000000000000000000" 0 overflowE
        pint64 |> RFatalError "-0o1000000000000000000001" 0 overflowE
        pint64 |> RFatalError "-0O001000000000000000000001" 0 overflowE

        pint64 |> ROk "0o777777777777777777777|" System.Int64.MaxValue
        pint64 |> ROk "+0o00777777777777777777777|" System.Int64.MaxValue
        pint64 |> ROk "-0o1000000000000000000000|" System.Int64.MinValue
        pint64 |> ROk "-0O001000000000000000000000|" System.Int64.MinValue

        pint64 |> RFatalError "+0b00011111111111111111111111111111111111111111111111111111111111111111" 0 overflowE
        pint64 |> RFatalError "-0b011111111111111111111111111111111111111111111111111111111111111111" 0 overflowE

        pint64 |> RFatalError "+0B1000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        pint64 |> RFatalError "0b0001000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        pint64 |> RFatalError "-0b0001000000000000000000000000000000000000000000000000000000000000001" 0 overflowE
        pint64 |> RFatalError "-0b1000000000000000000000000000000000000000000000000000000000000001" 0 overflowE

        pint64 |> ROk "0b111111111111111111111111111111111111111111111111111111111111111|" System.Int64.MaxValue
        pint64 |> ROk "+0b00111111111111111111111111111111111111111111111111111111111111111|" System.Int64.MaxValue
        pint64 |> ROk "-0b1000000000000000000000000000000000000000000000000000000000000000|"System.Int64.MinValue
        pint64 |> ROk "-0B0001000000000000000000000000000000000000000000000000000000000000000|" System.Int64.MinValue

    testPint64()

    let testPuint32() =
        let expectedE = Errors.ExpectedUInt32
        let overflowE = Errors.NumberOutsideOfUInt32Range

        puint32 |> RError "" 0 expectedE
        puint32 |> RError "+1" 0 expectedE
        puint32 |> RError "-1" 0 expectedE

        puint32 |> RFatalError "4294967300" 0 overflowE
        puint32 |> RFatalError "4294967299" 0 overflowE
        puint32 |> RFatalError "4294967298" 0 overflowE
        puint32 |> RFatalError "4294967297" 0 overflowE
        puint32 |> RFatalError "4294967296" 0 overflowE
        puint32 |> RFatalError "000004294967296" 0 overflowE
        puint32 |> RFatalError "11111111111" 0 overflowE

        puint32 |> ROk "0|" 0u
        puint32 |> ROk "000|" 0u
        puint32 |> ROk "1234567890|" 1234567890u
        puint32 |> ROk "4294967295|" System.UInt32.MaxValue
        puint32 |> ROk "4294967294|" (System.UInt32.MaxValue - 1u)
        puint32 |> ROk "4294967293|" (System.UInt32.MaxValue - 2u)
        puint32 |> ROk "4294967292|" (System.UInt32.MaxValue - 3u)
        puint32 |> ROk "4294967291|" (System.UInt32.MaxValue - 4u)
        puint32 |> ROk "4294967290|" (System.UInt32.MaxValue - 5u)
        puint32 |> ROk "4294967289|" (System.UInt32.MaxValue - 6u)
        puint32 |> ROk "000004294967295|" System.UInt32.MaxValue

        puint32 |> RError "0x"  2 Errors.ExpectedHexadecimalDigit
        puint32 |> RError "+0x1" 0 expectedE
        puint32 |> RFatalError "0x100000001" 0 overflowE
        puint32 |> RFatalError "0x100000000" 0 overflowE
        puint32 |> RFatalError "0x111111111" 0 overflowE
        puint32 |> RFatalError "0Xfffffffff" 0 overflowE

        puint32 |> ROk "0x0|" 0u
        puint32 |> ROk "0x000|" 0u
        puint32 |> ROk "0x1234abcd|" 0x1234abcdu
        puint32 |> ROk "0X1234ABCD|" 0x1234abcdu
        puint32 |> ROk "0xffffffff|" System.UInt32.MaxValue
        puint32 |> ROk "0xfffffffe|" (System.UInt32.MaxValue - 1u)
        puint32 |> ROk "0xfffffff1|" (System.UInt32.MaxValue - 14u)
        puint32 |> ROk "0xfffffff0|" (System.UInt32.MaxValue - 15u)
        puint32 |> ROk "0xffffffef|" (System.UInt32.MaxValue - 16u)
        puint32 |> ROk "0x00000ffffffff|" System.UInt32.MaxValue

        puint32 |> RError "0o"  2 Errors.ExpectedOctalDigit
        puint32 |> RError "+0o1" 0 expectedE
        puint32 |> RFatalError "0o40000000001" 0 overflowE
        puint32 |> RFatalError "0o40000000000" 0 overflowE
        puint32 |> RFatalError "0o777777777777" 0 overflowE
        puint32 |> RFatalError "0O7777777777777" 0 overflowE

        puint32 |> ROk "0o0|" 0u
        puint32 |> ROk "0o000|" 0u
        puint32 |> ROk "0o12345670123|" 0o12345670123u
        puint32 |> ROk "0o37777777777|" System.UInt32.MaxValue
        puint32 |> ROk "0o37777777776|" (System.UInt32.MaxValue - 1u)
        puint32 |> ROk "0o37777777771|" (System.UInt32.MaxValue - 6u)
        puint32 |> ROk "0o37777777770|" (System.UInt32.MaxValue - 7u)
        puint32 |> ROk "0o37777777767|" (System.UInt32.MaxValue - 8u)
        puint32 |> ROk "0O0000037777777777|" System.UInt32.MaxValue

        puint32 |> RError "0b"  2 Errors.ExpectedBinaryDigit
        puint32 |> RError "+0b1" 0 expectedE
        puint32 |> RFatalError "0b100000000000000000000000000000001" 0 overflowE
        puint32 |> RFatalError "0b100000000000000000000000000000000" 0 overflowE
        puint32 |> RFatalError "0B111111111111111111111111111111111" 0 overflowE

        puint32 |> ROk "0b0|" 0u
        puint32 |> ROk "0b000|" 0u
        puint32 |> ROk "0b11111111111111111111111111111111|" System.UInt32.MaxValue
        puint32 |> ROk "0b11111111111111111111111111111110|" (System.UInt32.MaxValue - 1u)
        puint32 |> ROk "0b11111111111111111111111111111101|" (System.UInt32.MaxValue - 2u)
        puint32 |> ROk "0b11111111111111111111111111111100|" (System.UInt32.MaxValue - 3u)
        puint32 |> ROk "0B0000011111111111111111111111111111111|" System.UInt32.MaxValue

    testPuint32()

    let testPint32() =
        let expectedE = Errors.ExpectedInt32
        let overflowE = Errors.NumberOutsideOfInt32Range

        pint32 |> RFatalError "4294967295" 0 overflowE
        pint32 |> RFatalError "+4294967295" 0 overflowE
        pint32 |> RFatalError "-4294967295" 0 overflowE
        pint32 |> RFatalError "-004294967295" 0 overflowE

        pint32 |> RFatalError "2147483648" 0 overflowE
        pint32 |> RFatalError "+0002147483648" 0 overflowE
        pint32 |> RFatalError "-2147483649" 0 overflowE
        pint32 |> RFatalError "-02147483649" 0 overflowE

        pint32 |> ROk "2147483647|" System.Int32.MaxValue
        pint32 |> ROk "+000002147483647|" System.Int32.MaxValue
        pint32 |> ROk "-2147483648|" System.Int32.MinValue
        pint32 |> ROk "-002147483648|" System.Int32.MinValue

        pint32 |> RFatalError "0xffffffffffffffff" 0 overflowE
        pint32 |> RFatalError "+0x000ffffffffffffffff" 0 overflowE
        pint32 |> RFatalError "-0xffffffffffffffff" 0 overflowE
        pint32 |> RFatalError "-0X0ffffffffffffffff" 0 overflowE

        pint32 |> RFatalError "0x80000000" 0 overflowE
        pint32 |> RFatalError "+0x00080000000" 0 overflowE
        pint32 |> RFatalError "-0x80000001" 0 overflowE
        pint32 |> RFatalError "-0x00080000001" 0 overflowE

        pint32 |> ROk "0x7fffffff|" System.Int32.MaxValue
        pint32 |> ROk "+0x000007fffffff|" System.Int32.MaxValue
        pint32 |> ROk "-0x80000000|" System.Int32.MinValue
        pint32 |> ROk "-0x0080000000|" System.Int32.MinValue

        pint32 |> RFatalError "0o40000000000" 0 overflowE
        pint32 |> RFatalError "+0o0040000000000" 0 overflowE
        pint32 |> RFatalError "-0o00040000000000" 0 overflowE

        pint32 |> RFatalError "0o20000000000" 0 overflowE
        pint32 |> RFatalError "+0o0020000000000" 0 overflowE
        pint32 |> RFatalError "-0o20000000001" 0 overflowE
        pint32 |> RFatalError "-0O0020000000001" 0 overflowE

        pint32 |> ROk "0o17777777777|" System.Int32.MaxValue
        pint32 |> ROk "+0o0017777777777|" System.Int32.MaxValue
        pint32 |> ROk "-0o20000000000|" System.Int32.MinValue
        pint32 |> ROk "-0O0020000000000|" System.Int32.MinValue

        pint32 |> RFatalError "+0b000111111111111111111111111111111111" 0 overflowE
        pint32 |> RFatalError  "-0b0111111111111111111111111111111111" 0 overflowE

        pint32 |> RFatalError "+0B1000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        pint32 |> RFatalError "0b0001000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        pint32 |> RFatalError "-0b0001000000000000000000000000000000000000000000000000000000000000001" 0 overflowE
        pint32 |> RFatalError "-0b1000000000000000000000000000000000000000000000000000000000000001" 0 overflowE

        pint32 |> ROk "0b1111111111111111111111111111111|" System.Int32.MaxValue
        pint32 |> ROk "+0b001111111111111111111111111111111|" System.Int32.MaxValue
        pint32 |> ROk "-0b10000000000000000000000000000000|"System.Int32.MinValue
        pint32 |> ROk "-0B00010000000000000000000000000000000|" System.Int32.MinValue

    testPint32()

    let testPintOther() =
       let overflowInt32  = Errors.NumberOutsideOfInt32Range
       let overflowInt16  = Errors.NumberOutsideOfInt16Range
       let overflowInt8   = Errors.NumberOutsideOfInt8Range
       let overflowUInt32 = Errors.NumberOutsideOfUInt32Range
       let overflowUInt16 = Errors.NumberOutsideOfUInt16Range
       let overflowUInt8  = Errors.NumberOutsideOfUInt8Range

       puint32 |> RError "+0|" 0 Errors.ExpectedUInt32
       puint32 |> RFatalError "4294967296|" 0 overflowUInt32
       puint32 |> RFatalError "00004294967296|" 0 overflowUInt32
       puint32 |> RFatalError "11111111111|" 0 overflowUInt32

       puint32 |> ROk "0|" 0u
       puint32 |> ROk "000|" 0u
       puint32 |> ROk "1234567890|" 1234567890u
       puint32 |> ROk "0001234567890|" 1234567890u
       puint32 |> ROk "4294967295|" System.UInt32.MaxValue
       puint32 |> ROk "0004294967295|" System.UInt32.MaxValue

       pint32 |> RError "+|" 0 Errors.ExpectedInt32
       pint32 |> RFatalError "2147483648|" 0 overflowInt32
       pint32 |> RFatalError "-00002147483649|" 0 overflowInt32
       pint32 |> RFatalError "11111111111|" 0 overflowInt32

       pint32 |> ROk "0|" 0
       pint32 |> ROk "+000|"  0
       pint32 |> ROk "1234567890|" 1234567890
       pint32 |> ROk "0001234567890|" 1234567890
       pint32 |> ROk "2147483647|" System.Int32.MaxValue
       pint32 |> ROk "+0002147483647|" System.Int32.MaxValue
       pint32 |> ROk "-2147483648|" System.Int32.MinValue
       pint32 |> ROk "-0002147483648|" System.Int32.MinValue

       pint32 |> RFatalError "0x80000000|" 0 overflowInt32
       pint32 |> ROk "0x7fffffff|" 0x7fffffff
       pint32 |> ROk "-0x80000000|" -0x80000000
       pint32 |> RFatalError "-0x80000001|" 0 overflowInt32

       puint32 |> RFatalError "0x100000000|" 0 overflowUInt32
       puint32 |> ROk "0xffffffff|" 0xffffffffu

       pint16 |> RFatalError  "0x8000|" 0 overflowInt16
       pint16 |> ROk "0x7fff|" 0x7fffs
       pint16 |> ROk "-0x8000|" -0x8000s
       pint16 |> RFatalError "-0x8001|" 0 overflowInt16

       puint16 |> RFatalError "0x10000|" 0 overflowUInt16
       puint16 |> ROk "0xffff|"  0xffffus

       pint8 |> RFatalError "0x80|" 0 overflowInt8
       pint8 |> ROk  "0x7f|" 0x7fy
       pint8 |> ROk "-0x80|" -0x80y
       pint8 |> RFatalError "-0x81|" 0 overflowInt8

       puint8 |> RFatalError "0x100|" 0 overflowUInt8
       puint8 |> ROk "0xff|" 0xffuy

    testPintOther()

let testFollowedBy() =
    notFollowedByEof |> ROk " " 0 ()
    notFollowedByEof |> RError "" 0 Errors.UnexpectedEndOfInput

    followedByNewline |> RError "1" 0 Errors.ExpectedNewline
    followedByNewline |> RError " " 0 Errors.ExpectedNewline
    followedByNewline |> ROk "\r" 0 ()
    followedByNewline |> ROk "\n" 0 ()

    notFollowedByNewline |> ROk "1" 0 ()
    notFollowedByNewline |> ROk " " 0 ()
    notFollowedByNewline |> RError "\r" 0 Errors.UnexpectedNewline
    notFollowedByNewline |> RError "\n" 0 Errors.UnexpectedNewline

    followedByString "a" |> ROk "a" 0 ()
    followedByString "a" |> RError "A" 0 (expectedString "a")
    followedByString "123" |> ROk "123" 0 ()
    followedByString "123" |> RError "124" 0 (expectedString "123")
    notFollowedByString "a" |> ROk "A" 0 ()
    notFollowedByString "a" |> RError "a" 0 (unexpectedString "a")
    notFollowedByString "123" |> ROk "124" 0 ()
    notFollowedByString "123" |> RError "123" 0 (unexpectedString "123")

    try followedByString "13\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try notFollowedByString "13\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()

    followedByStringCI "A" |> ROk "a" 0 ()
    followedByStringCI "A" |> ROk "A" 0 ()
    followedByStringCI "A" |> RError "B" 0 (expectedStringCI "A")
    followedByStringCI "aBc" |> ROk "AbC" 0 ()
    followedByStringCI "aBc" |> RError "Abd" 0 (expectedStringCI "aBc")
    notFollowedByStringCI "A" |> ROk "B" 0 ()
    notFollowedByStringCI "A" |> RError "a" 0 (unexpectedStringCI "A")
    notFollowedByStringCI "A" |> RError "A" 0 (unexpectedStringCI "A")
    notFollowedByStringCI "aBc" |> ROk "Abd" 0 ()
    notFollowedByStringCI "aBc" |> RError "AbC" 0 (unexpectedStringCI "aBc")

    try followedByStringCI "13\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try notFollowedByStringCI "13\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()

    let one chr = fun c -> if c = chr then true
                           else Fail()

    let oneN chr = fun c -> if c = chr then false
                            else Fail()

    let eos1 = fun c -> Fail()

    nextCharSatisfies (one '2')   |> ROk    "2"    0 ()
    nextCharSatisfies (one '\n')  |> ROk    "\n"   0 ()
    nextCharSatisfies (one '\n')  |> ROk    "\r\n" 0 ()
    nextCharSatisfies (one '\n')  |> ROk    "\r"   0 ()
    nextCharSatisfies eos1        |> RError ""     0 NoErrorMessages
    nextCharSatisfies (oneN '1')  |> RError "1"    0 NoErrorMessages
    nextCharSatisfies (oneN '\n') |> RError "\r"   0 NoErrorMessages

    nextCharSatisfiesNot (oneN '2')  |> ROk    "2"    0 ()
    nextCharSatisfiesNot (oneN '\n') |> ROk    "\n"   0 ()
    nextCharSatisfiesNot (oneN '\n') |> ROk    "\r\n" 0 ()
    nextCharSatisfiesNot (oneN '\n') |> ROk    "\r"   0 ()
    nextCharSatisfiesNot eos1        |> ROk    ""     0 ()
    nextCharSatisfiesNot (one '1')  |> RError "1"    0 NoErrorMessages
    nextCharSatisfiesNot (one '\n') |> RError "\r"   0 NoErrorMessages

    let two (str: string) =
        fun c0 c1 -> if c0 = str.[0] && c1 = str.[1] then true
                     else Fail()
    let twoN (str: string) =
        fun c0 c1 -> if c0 = str.[0] && c1 = str.[1] then false
                     else Fail()

    let eos2 = fun c0 c1 -> Fail()

    next2CharsSatisfy (two "12")    |> ROk    "12"     0 ()
    next2CharsSatisfy (two "\n2")   |> ROk    "\r2"    0 ()
    next2CharsSatisfy (two "\n2")   |> ROk    "\r\n2"  0 ()
    next2CharsSatisfy (two "\n2")   |> ROk    "\n2"    0 ()
    next2CharsSatisfy (two "\n\n")  |> ROk    "\n\r"   0 ()
    next2CharsSatisfy (two "\n\n")  |> ROk    "\r\r"   0 ()
    next2CharsSatisfy (two "\n\n")  |> ROk    "\r\n\r" 0 ()
    next2CharsSatisfy eos2          |> RError ""       0 NoErrorMessages
    next2CharsSatisfy eos2          |> RError "1"      0 NoErrorMessages
    next2CharsSatisfy eos2          |> RError "\r"     0 NoErrorMessages
    next2CharsSatisfy eos2          |> RError "\r\n"   0 NoErrorMessages
    next2CharsSatisfy eos2          |> RError "\n"     0 NoErrorMessages
    next2CharsSatisfy (twoN "13")   |> RError "13"     0 NoErrorMessages
    next2CharsSatisfy (twoN "\n\t") |> RError "\n\t"   0 NoErrorMessages
    next2CharsSatisfy (twoN "\n\t") |> RError "\r\n\t" 0 NoErrorMessages
    next2CharsSatisfy (twoN "\n\t") |> RError "\r\t"   0 NoErrorMessages

    next2CharsSatisfyNot (twoN "12")   |> ROk    "12"     0 ()
    next2CharsSatisfyNot (twoN "\n2")  |> ROk    "\r2"    0 ()
    next2CharsSatisfyNot (twoN "\n2")  |> ROk    "\r\n2"  0 ()
    next2CharsSatisfyNot (twoN "\n2")  |> ROk    "\n2"    0 ()
    next2CharsSatisfyNot (twoN "\n\n") |> ROk    "\n\r"   0 ()
    next2CharsSatisfyNot (twoN "\n\n") |> ROk    "\r\r"   0 ()
    next2CharsSatisfyNot (twoN "\n\n") |> ROk    "\r\n\r" 0 ()
    next2CharsSatisfyNot eos2          |> ROk ""          0 ()
    next2CharsSatisfyNot eos2          |> ROk "1"         0 ()
    next2CharsSatisfyNot eos2          |> ROk "\r"        0 ()
    next2CharsSatisfyNot eos2          |> ROk "\r\n"      0 ()
    next2CharsSatisfyNot eos2          |> ROk "\n"        0 ()
    next2CharsSatisfyNot (two "13")    |> RError "13"     0 NoErrorMessages
    next2CharsSatisfyNot (two "\n\t")  |> RError "\n\t"   0 NoErrorMessages
    next2CharsSatisfyNot (two "\n\t")  |> RError "\r\n\t" 0 NoErrorMessages
    next2CharsSatisfyNot (two "\n\t")  |> RError "\r\t"   0 NoErrorMessages

    anyChar >>. previousCharSatisfies (one '1')  |> ROk    "12"    1 ()
    anyChar >>. previousCharSatisfies (one '\n') |> ROkNL  "\n1"   1 ()
    anyChar >>. previousCharSatisfies (one '\n') |> ROkNL  "\r\n1" 2 ()
    anyChar >>. previousCharSatisfies (one '\n') |> ROkNL  "\r1"   1 ()
    anyChar >>. previousCharSatisfies (oneN '0') |> RError "01"    1 NoErrorMessages
    previousCharSatisfies eos1  |> RError "1" 0 NoErrorMessages
    previousCharSatisfies eos1  |> RError ""  0 NoErrorMessages

    anyChar >>. previousCharSatisfiesNot (oneN '1')  |> ROk    "12"    1 ()
    anyChar >>. previousCharSatisfiesNot (oneN '\n') |> ROkNL  "\n1"   1 ()
    anyChar >>. previousCharSatisfiesNot (oneN '\n') |> ROkNL  "\r\n1" 2 ()
    anyChar >>. previousCharSatisfiesNot (oneN '\n') |> ROkNL  "\r1"   1 ()
    anyChar >>. previousCharSatisfiesNot (one  '0')  |> RError "01"    1 NoErrorMessages
    previousCharSatisfiesNot eos1 |> ROk "1" 0 ()
    previousCharSatisfiesNot eos1 |> ROk "" 0 ()

let testUserStateParsers() =
    use stream = new CharStream<_>("test")
    stream.UserState <- 1

    let reply = getUserState stream
    reply.Status  |> Equal Ok
    reply.Error   |> Equal NoErrorMessages
    reply.Result  |> Equal 1

    let reply = setUserState 2 stream
    reply.Status |> Equal Ok
    reply.Error  |> Equal NoErrorMessages
    stream.UserState |> Equal 2

    let reply = updateUserState (fun i -> i + 1) stream
    reply.Status |> Equal Ok
    reply.Error  |> Equal NoErrorMessages
    stream.UserState |> Equal 3

    let reply = userStateSatisfies ((=) 3) stream
    reply.Status |> Equal Ok
    reply.Error  |> Equal NoErrorMessages

    let reply = userStateSatisfies ((<>) 3) stream
    reply.Status |> Equal Error
    reply.Error  |> Equal NoErrorMessages

let run() =
    testCharParsers()
    testAnyNoneOf()
    testSpecialCharParsers()
    testStringParsers()
    testIdentifier()
    testManySatisfy()
    testMany()
    testSkipToString()
    testNumberParsers()
    testFollowedBy()
    testUserStateParsers()

