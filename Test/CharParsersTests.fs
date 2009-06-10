// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

namespace FParsec.Test

module CharParsersTests
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
    pchar ' '  |> RError "" 0 (expectedError "' '")
    pchar ' '  |> RError "x" 0 (expectedError "' '")

    pchar '\r' |> RError "_\r" 0 (expectedError "newline")
    newline    |> RError "_\n" 0 (expectedError "newline")
    newline    |> RError "" 0 (expectedError "newline")

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

    anyChar     |> RError "" 0 (expectedError "any char")
    skipAnyChar |> RError "" 0 (expectedError "any char")

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
    satisfyL     (fun c -> true) "test" |> RError "" 0 (expectedError "test")
    skipSatisfyL (fun c -> true) "test" |> RError "" 0 (expectedError "test")

    satisfy ((=) '1')  |> ROk "1"  1 '1'
    satisfy ((=) '\t') |> ROk "\t" 1 '\t'
    satisfy ((=) '1')  |> ROk "11" 1 '1'
    satisfy ((=) '1')  |> RError "0" 0 NoErrorMessages
    satisfyL ((=) '1') "test"  |> RError "2" 0 (expectedError "test")
    satisfyL ((=) '\r') "test" |> RError "\r" 0 (expectedError "test")
    satisfy ((=) '\n') |> ROkNL "\r"   1 '\n'
    satisfy ((=) '\n') |> ROkNL "\r\n" 2 '\n'
    satisfy ((=) '\n') |> ROkNL "\n"   1 '\n'

    skipSatisfy ((=) '1')  |> ROk "1"  1 ()
    skipSatisfy ((=) '\t') |> ROk "\t" 1 ()
    skipSatisfy ((=) '1')  |> ROk "11" 1 ()
    skipSatisfy ((=) '1')  |> RError "0" 0 NoErrorMessages
    skipSatisfyL ((=) '1') "test"  |> RError "2"  0 (expectedError "test")
    skipSatisfyL ((=) '\r') "test" |> RError "\r" 0 (expectedError "test")
    skipSatisfy ((=) '\n') |> ROkNL "\r"   1 ()
    skipSatisfy ((=) '\n') |> ROkNL "\r\n" 2 ()
    skipSatisfy ((=) '\n') |> ROkNL "\n"   1 ()

let testAnyNoneOf() =
    // anyOf/noneOf share the implementation with satisfy/skipSatisfy
    // so we only need to do some basic testing...

    anyOf "1"  |> ROk "1" 1 '1'
    anyOf "1"  |> RError "2" 0 (expectedError "any char in '1'")
    noneOf "1" |> RError "1" 0 (expectedError "any char not in '1'")
    noneOf "1" |> ROk "2" 1 '2'
    skipAnyOf "1"  |> ROk "1" 1 ()
    skipAnyOf "1"  |> RError "2" 0 (expectedError "any char in '1'")
    skipNoneOf "1" |> RError "1" 0 (expectedError "any char not in '1'")
    skipNoneOf "1" |> ROk "2" 1 ()

    // ... and then test Helper.CharSet

    let testCharSet s (sin: string) (sout: string) =
        let cs = FParsec.Helper.CharSet(s)
        for c in sin do
            cs.Contains(c) |> True
        for c in sout do
            cs.Contains(c) |> False

    testCharSet "" "" "a\u0000\uffff"
    testCharSet "a" "a" "\u0000\uffff"
    testCharSet "\u0000\uffffa" "a\u0000\uffffa" "b\u0001\ufffe"
    testCharSet "\u0002\u0001\u0399\u0400\u0401\u0399\u0400\u0401\uffffabc123" "\u0002\u0001\u0399\u0400\u0401\uffffabc123" "\u0000\u0398\u0402\ufffed0"

    let rand = new System.Random(12345)

    for j = 0 to 20000 do
        let n = rand.Next(1, 100)
        let cs : char[] = Array.zeroCreate n
        for i = 1 to n/2 do
            let r = rand.Next()
            cs.[i*2 - 2] <- char r
            cs.[i*2 - 1] <- char (r >>> 16)
        if n%2 = 1 then cs.[cs.Length - 1] <- char (rand.Next())

        let set = FParsec.Helper.CharSet(new string(cs))

        Array.sortInPlace cs

        let mutable c_1 = EOS
        let mutable c = cs.[0]

        for i = 0 to n - 1 do
            set.Contains(c) |> True
            if c <> c_1 && int c - 1 <> int c_1 then
                set.Contains(char (int c - 1)) |> False
            if i + 1 < n then
                let c1 = cs.[i + 1]
                if c < EOS && c <> c1 && int c + 1 <> int c1 then
                    set.Contains(char (int c + 1)) |> False
                c_1 <- c
                c <- c1


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
    asciiUpper  |> RError "a" 0 (expectedError "Ascii uppercase letter")
    asciiLower  |> ROk "a" 1 'a'
    asciiLower  |> RError "A" 0  (expectedError "Ascii lowercase letter")
    asciiLetter |> ROk "A" 1 'A'
    asciiLetter |> RError "1" 0 (expectedError "Ascii letter")

    upper  |> ROk "Ä" 1 'Ä'
    upper  |> RError "ä" 0 (expectedError "uppercase letter")
    lower  |> ROk "ä" 1 'ä'
    lower  |> RError "Ä" 0 (expectedError "lowercase letter")
    letter |> ROk "Ä" 1 'Ä'
    letter |> RError "1" 0 (expectedError "letter")

    digit |> ROk "1" 1 '1'
    digit |> RError "a" 0 (expectedError "digit")
    hex   |> ROk "a" 1 'a'
    hex   |> RError "g" 0 (expectedError "hexadecimal digit")
    octal |> ROk "7" 1 '7'
    octal |> RError "8" 0 (expectedError "octal digit")

    tab |> ROk "\t" 1 '\t'
    tab |> RError "\r" 0 (expectedError "tab")

    unicodeNewline |> ROkNL "\r"     1 '\n'
    unicodeNewline |> ROkNL "\r\n"   2 '\n'
    unicodeNewline |> ROkNL "\n"     1 '\n'
    unicodeNewline |> ROkNL "\u0085" 1 '\n'
    unicodeNewline |> ROkNL "\u000C" 1 '\n'
    unicodeNewline |> ROkNL "\u2028" 1 '\n'
    unicodeNewline |> ROkNL "\u2029" 1 '\n'
    unicodeNewline |> RError "\t"    0 (expectedError "newline")
    unicodeNewline |> RError ""      0 (expectedError "newline")

    let count p = manyFold 0 (fun c x -> c + 1) p

    match run (count unicodeNewline) "\n\r\r\n\u0085\u000C\u2028\u2029\r\n" with
    | Success(c,_,pos) -> c |> Equal 8; pos.Index |> Equal 10L; pos.Line |> Equal 9L; pos.Column |> Equal 1L
    | Failure _        -> Fail()

    whitespace |> ROkNL "\r "     1 '\n'
    whitespace |> ROkNL "\r\n"    2 '\n'
    whitespace |> ROkNL "\n\n"    1 '\n'
    whitespace |> ROk    "  "     1 ' '
    whitespace |> ROk    "\t"     1 '\t'
    whitespace |> RError "\u000C" 0 (expectedError "whitespace")
    whitespace |> RError ""       0 (expectedError "whitespace")

    unicodeWhitespace |> ROkNL  "\r"     1 '\n'
    unicodeWhitespace |> ROkNL  "\r\n"   2 '\n'
    unicodeWhitespace |> ROkNL  "\n"     1 '\n'
    unicodeWhitespace |> ROkNL  "\u0085" 1 '\n'
    unicodeWhitespace |> ROkNL  "\u000C" 1 '\n'
    unicodeWhitespace |> ROkNL  "\u2028" 1 '\n'
    unicodeWhitespace |> ROkNL  "\u2029" 1 '\n'
    unicodeWhitespace |> ROk    "  "     1 ' '
    unicodeWhitespace |> ROk    "\t"     1 '\t'
    unicodeWhitespace |> RError "" 0 (expectedError "whitespace")

    match run (count unicodeWhitespace) "\n \r\t\t\r\n\n \u0085\u000C\u2028\u2029 \r\n\t\u200A" with // '\u200A' is "hair space" (interestingly, the '\u200B' "zero width space" character is not recognized as white space)
    | Success(c,_,pos) -> c |> Equal 16; pos.Index |> Equal 18L; pos.Line |> Equal 10L; pos.Column |> Equal 3L
    | _ -> Fail()

    spaces  |> ROk ""   0 ()
    spaces  |> ROk " "  1 ()
    spaces  |> ROk "  " 2 ()
    spaces1 |> RError "" 0  (expectedError "whitespace")
    spaces1 |> ROk " "  1 ()
    spaces1 |> ROk "  " 2 ()

    match run spaces "\n \r\t\t\r\n\n " with
    | Success(c,_,pos) -> pos.Index |> Equal 9L; pos.Line |> Equal 5L; pos.Column |> Equal 2L
    | _ -> Fail()
    match run spaces1 "\n \r\t\t\r\n\n " with
    | Success(c,_,pos) -> pos.Index |> Equal 9L; pos.Line |> Equal 5L; pos.Column |> Equal 2L
    | _ -> Fail()


let testStringParsers() =
    pstring "test"    |> RError "pest" 0 (expectedError "'test'")
    pstring "test"    |> ROk "test" 4 "test"
    skipString "test" |> ROk "test" 4 ()
    stringReturn "test" -1 |> ROk "test" 4 -1

    try pstring "\n" |> ROkNL "\n" 1 "\n"; Fail()
    with :? System.ArgumentException -> ()

    pstringCI      "tEsT"    |> RError "pest" 0 (expectedError "'tEsT' (case-insensitive)")
    pstringCI      "tEsT"    |> ROk "TeSt" 4 "TeSt"
    skipStringCI   "tEsT"    |> ROk "TeSt" 4 ()
    stringCIReturn "tEsT" -1 |> ROk "TeSt" 4 -1

    try pstringCI "\n" |> ROkNL "\n" 1 "\n"; Fail()
    with :? System.ArgumentException -> ()

    anyString 3      |> RError "12" 0 (expectedError "any sequence of 3 chars")
    skipAnyString 3  |> RError "12" 0 (expectedError "any sequence of 3 chars")
    anyString 3      |> ROkNL "12\r\n4" 4 "12\n"
    skipAnyString 3  |> ROkNL "12\r\n4" 4 ()

    restOfLine      |> ROk "" 0 ""
    skipRestOfLine  |> ROk "" 0 ()
    restOfLine      |> ROkNL "\r\n1"   2  ""
    skipRestOfLine  |> ROkNL "\r\n1"   2  ()
    restOfLine      |> ROkNL "  \r\n1" 4  "  "
    skipRestOfLine  |> ROkNL "  \r\n1" 4  ()

    skipToEndOfLine |> ROk ""     0 ()
    skipToEndOfLine |> ROk "  "   2 ()
    skipToEndOfLine |> ROk "  \r" 2 ()

    regex "abc"         |> ROk    "abc" 3  "abc"
    regex ".*\r\r\n.*"  |> ROkNL  "abc\r\r\nabc" 9 "abc\n\nabc"


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
    many1SatisfyL  isDigit "test"       |> RError "a" 0 (expectedError "test")
    many1Satisfy2L isHex isDigit "test" |> RError "g" 0 (expectedError "test")
    many1Satisfy   isDigit              |> ROk "123"  3 "123"
    many1Satisfy2  isHex isDigit        |> ROk "a23a" 3 "a23"

    skipMany1SatisfyL  isDigit "test"       |> RError "a" 0 (expectedError "test")
    skipMany1Satisfy2L isHex isDigit "test" |> RError "g" 0 (expectedError "test")
    skipMany1Satisfy   isDigit              |> ROk "123"  3 ()
    skipMany1Satisfy2  isHex isDigit        |> ROk "a23a" 3 ()

    manyMinMaxSatisfy   0 3 isDigit              |> ROk    "1234" 3 "123"
    manyMinMaxSatisfy   3 3 isDigit              |> ROk    "1234" 3 "123"
    manyMinMaxSatisfyL  4 4 isDigit "test"       |> RError "123a" 0  (expectedError "test")
    manyMinMaxSatisfy2  0 3 isHex isDigit        |> ROk    "a234" 3 "a23"
    manyMinMaxSatisfy2  3 3 isHex isDigit        |> ROk    "a234" 3 "a23"
    manyMinMaxSatisfy2L 4 4 isHex isDigit "test" |> RError "a23a" 0 (expectedError "test")

    skipManyMinMaxSatisfy   0 3 isDigit              |> ROk "1234" 3 ()
    skipManyMinMaxSatisfy   3 3 isDigit              |> ROk "1234" 3 ()
    skipManyMinMaxSatisfyL  4 4 isDigit "test"       |> RError "123a" 0 (expectedError "test")
    skipManyMinMaxSatisfy2  0 3 isHex isDigit        |> ROk "a234" 3 ()
    skipManyMinMaxSatisfy2  3 3 isHex isDigit        |> ROk "a234" 3 ()
    skipManyMinMaxSatisfy2L 4 4 isHex isDigit "test" |> RError "a23a" 0 (expectedError "test")

    try manyMinMaxSatisfy 0 -1 isDigit |> ROk "1234" 3 "123"; Fail()
    with :? System.ArgumentException -> ()

    try skipManyMinMaxSatisfy 0 -1 isDigit |> ROk "1234" 3 (); Fail()
    with :? System.ArgumentException -> ()

let testMany() =

    // no Ok parser that doesn't consume input or that returns an error message
    let charTestParsers r e : Parser<'a, int>[] = [| // we rely on the order of these parsers
        fun s -> Reply<_,_>(Ok, r, NoErrorMessages, s.WithUserState(s.UserState + 1))
        fun s -> Reply<_,_>(Error, e, s);
        fun s -> Reply<_,_>(Error, e, s.WithUserState(s.UserState + 1));
        fun s -> Reply<_,_>(FatalError, e, s);
        fun s -> Reply<_,_>(FatalError, e, s.WithUserState(s.UserState + 1));
    |]

    let ps1  = charTestParsers '1' (expectedError "1")
    let ps2  = charTestParsers '2' (expectedError "2")
    let ps3  = charTestParsers '3' (expectedError "3")

    let content = "the content doesn't matter"
    use cs = new FParsec.CharStream(content, 0, content.Length)
    let s0 = new FParsec.State<_>(cs, 0, "")

    let manyChars2Ref p1 p = manyFoldApply2 (fun (c: char) -> (new System.Text.StringBuilder()).Append(c)) (fun sb (c: char) -> sb.Append(c)) (fun sb -> sb.ToString()) (fun () -> "") (attempt p1) (attempt p)
    let many1Chars2Ref p1 p = many1FoldApply2 (fun (c: char) -> (new System.Text.StringBuilder()).Append(c)) (fun sb (c: char) -> sb.Append(c)) (fun sb -> sb.ToString()) (attempt p1) (attempt p)
    let skipManyChars2Ref p1 p = manyFoldApply2 (fun _ -> ()) (fun _ _ -> ()) (fun () -> ()) (fun () -> ()) (attempt p1) (attempt p)
    let skipMany1Chars2Ref p1 p = many1FoldApply2 (fun _ -> ()) (fun _ _ -> ()) (fun () -> ()) (attempt p1) (attempt p)

    let manySeq2 = seq { for p2 in ps2 do
                         for p3 in ps3 do
                             yield [p2; p3]}

    for p1 in ps1 do
        for ps in manySeq2 do
            let p_1, p_2, pr = seqParserAndReset2 ps

            checkParser (manyChars2 p1 p_1)      (manyChars2Ref p1 p_2) s0; pr()
            checkParser (many1Chars2 p1 p_1)     (many1Chars2Ref p1 p_2) s0; pr()
            checkParser (skipManyChars2 p1 p_1)  (skipManyChars2Ref p1 p_2) s0; pr()
            checkParser (skipMany1Chars2 p1 p_1) (skipMany1Chars2Ref p1 p_2) s0

    try manyChars (preturn ' ') s0 |> ignore; Fail()
    with Microsoft.FSharp.Core.Failure _ -> ()

    try skipManyChars (preturn ' ') s0 |> ignore; Fail()
    with Microsoft.FSharp.Core.Failure _ -> ()

    let sb = new System.Text.StringBuilder()
    for i = 1 to 200 do
        let s = sb.Append(char (i % 10)).ToString()
        manyChars anyChar |> ROkE s s.Length s (expectedError "any char")


    let eps1 = constantTestParsers 1 (expectedError "11")
    let eps2 = constantTestParsers 2 (expectedError "22")
    let eps3 = constantTestParsers 3 (expectedError "33")

    let manyCharsTillRef p endp = manyTillFoldApply (fun (c: char) -> (new System.Text.StringBuilder()).Append(c)) (fun sb (c: char) -> sb.Append(c)) (fun sb _ -> sb.ToString()) (fun _ -> "") p endp
    let skipManyCharsTillRef p endp = manyTillFoldApply  (fun _ -> ()) (fun _ _ -> ()) (fun _ _ -> ()) (fun _ -> ()) p endp

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
        checkParser (manyCharsTill     p_1 e_1) (manyCharsTillRef     p_2 e_2) s0; pr(); er()
        checkParser (skipManyCharsTill p_1 e_1) (skipManyCharsTillRef p_2 e_2) s0

    many1CharsTill2     hex digit (pchar '.') |> ROk "a23." 4 "a23"
    skipMany1CharsTill2 hex digit (pchar '.') |> ROk "a23." 4 ()

    try manyCharsTill (preturn ' ') (fail "t") s0 |> ignore; Fail()
    with Microsoft.FSharp.Core.Failure _ -> ()

    try skipManyCharsTill (preturn ' ') (fail "t") s0 |> ignore; Fail()
    with Microsoft.FSharp.Core.Failure _ -> ()


    let sps1  = constantTestParsers "1" (expectedError "1")
    let sps2  = constantTestParsers "2" (expectedError "2")
    let sps3  = constantTestParsers "3" (expectedError "3")
    let sps4  = constantTestParsers "4" (expectedError "4")
    let sps5  = constantTestParsers "5" (expectedError "5")
    let sps6  = constantTestParsers "6" (expectedError "6")
    let sps7  = constantTestParsers "7" (expectedError "7")

    let manyStringsRef p  = manyFoldApply (fun (s: string) -> (new System.Text.StringBuilder()).Append(s)) (fun sb (s: string) -> sb.Append(s)) (fun sb -> sb.ToString()) (fun () -> "") p
    let many1StringsRef p = many1FoldApply (fun (s: string) -> (new System.Text.StringBuilder()).Append(s)) (fun sb (s: string) -> sb.Append(s)) (fun sb -> sb.ToString()) p

    let manySeq7 = seq { for p1 in sps1.[1..] do
                         for p2 in sps2.[1..] do
                         for p3 in sps3.[1..] do
                         for p4 in sps4.[1..] do
                         for p5 in sps5.[1..] do
                         for p6 in sps6.[1..] do
                         for p7 in sps7.[1..] do
                             yield [p1;p2;p3;p4;p5;p6;p7]}

    for ps in manySeq7 do
        let p_1, p_2, pr = seqParserAndReset2 ps

        checkParser (manyStrings  p_1) (manyStringsRef  p_2) s0; pr()
        checkParser (many1Strings p_1) (many1StringsRef p_2) s0

    manyStrings2 (pstring "1") (pstring "2") |> ROkE "12223" 4 "1222" (expectedError "'2'")

    try manyStrings (preturn "1") s0 |> ignore; Fail()
    with Microsoft.FSharp.Core.Failure _ -> ()

let testSkipToString() =
    skipToString "abc" System.Int32.MaxValue |> RError "abbab" 5 (messageError "Could not find the string 'abc'.")
    skipToString "abc" System.Int32.MaxValue |> ROk "abc"    0 ()
    skipToString "abc" 0                     |> ROk "abc"    0 ()
    skipToString "abc" System.Int32.MaxValue |> ROk "abdabc" 3 ()
    skipToString "abc" 3                     |> ROk "abdabc" 3 ()
    skipToString "abc" 2 |> RError "abdabc" 2 (messageError "Could not find the string 'abc'.")

    skipToStringCI "AbC" System.Int32.MaxValue |> RError "abbab" 5 (messageError "Could not find the case-insensitive string 'AbC'.")
    skipToStringCI "AbC" System.Int32.MaxValue |> ROk "aBc"    0 ()
    skipToStringCI "AbC" 0                     |> ROk "abc"    0 ()
    skipToStringCI "AbC" System.Int32.MaxValue |> ROk "aBdaBc" 3 ()
    skipToStringCI "AbC" 3                     |> ROk "aBdaBc" 3 ()
    skipToStringCI "AbC" 2 |> RError "aBdaBc" 2 (messageError "Could not find the case-insensitive string 'AbC'.")

    charsTillString "abc" System.Int32.MaxValue |> RError "abbab" 5 (messageError "Could not find the string 'abc'.")
    charsTillString "abc" System.Int32.MaxValue |> ROk "abc"    3 ""
    charsTillString "abc" 0                     |> ROk "abc"    3 ""
    charsTillString "abc" System.Int32.MaxValue |> ROk "abdabc" 6 "abd"
    charsTillString "abc" 3                     |> ROk "abdabc" 6 "abd"
    charsTillString "abc" 2 |> RError "abdabc" 2 (messageError "Could not find the string 'abc'.")

    charsTillStringCI "AbC" System.Int32.MaxValue |> RError "abbab" 5 (messageError "Could not find the case-insensitive string 'AbC'.")
    charsTillStringCI "AbC" System.Int32.MaxValue |> ROk "aBc"    3 ""
    charsTillStringCI "AbC" 0                     |> ROk "abc"    3 ""
    charsTillStringCI "AbC" System.Int32.MaxValue |> ROk "aBdaBc" 6 "aBd"
    charsTillStringCI "AbC" 3                     |> ROk "aBdaBc" 6  "aBd"
    charsTillStringCI "AbC" 2 |> RError "aBdaBc" 2 (messageError "Could not find the case-insensitive string 'AbC'.")

    skipCharsTillString "abc" System.Int32.MaxValue |> RError "abbab" 5 (messageError "Could not find the string 'abc'.")
    skipCharsTillString "abc" System.Int32.MaxValue |> ROk "abc"    3 ()
    skipCharsTillString "abc" 0                     |> ROk "abc"    3 ()
    skipCharsTillString "abc" System.Int32.MaxValue |> ROk "abdabc" 6 ()
    skipCharsTillString "abc" 3                     |> ROk "abdabc" 6 ()
    skipCharsTillString "abc" 2                     |> RError "abdabc" 2 (messageError "Could not find the string 'abc'.")

    skipCharsTillStringCI "AbC" System.Int32.MaxValue |> RError "abbab" 5 (messageError "Could not find the case-insensitive string 'AbC'.")
    skipCharsTillStringCI "AbC" System.Int32.MaxValue |> ROk "aBc"    3 ()
    skipCharsTillStringCI "AbC" 0                     |> ROk "abc"    3 ()
    skipCharsTillStringCI "AbC" System.Int32.MaxValue |> ROk "aBdaBc" 6 ()
    skipCharsTillStringCI "AbC" 3                     |> ROk "aBdaBc" 6 ()
    skipCharsTillStringCI "AbC" 2                     |> RError "aBdaBc" 2 (messageError "Could not find the case-insensitive string 'AbC'.")

    try skipToString "1\r" 3 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try skipToStringCI "1\r" 3 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try charsTillString "1\r" 3 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try charsTillStringCI "1\r" 3 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try skipCharsTillString "1\r" 3 |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try skipCharsTillStringCI "1\r" 3 |> ignore; Fail()
    with :? System.ArgumentException -> ()


let testNumberParsers() =
    let ROkI   content i result parser = ROk content i result parser
    let ROk    content   result parser = ROk content (content.Length - 1) result parser

    let testNumberLiteral() =
        let all = ((enum) 0xffffffff) : NLO

        numberLiteral all "nl" |> RError "|" 0 (expectedError "nl")
        numberLiteral all "nl" |> ROk "0|"     (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "+0|"    (NumberLiteral("+0", NLF.HasPlusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "-0|"    (NumberLiteral("-0", NLF.HasMinusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0u|"    (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 1), 'u', EOS, EOS, EOS))
        numberLiteral all "nl" |> ROk "0az|"   (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 2), 'a', 'z', EOS, EOS))
        numberLiteral all "nl" |> ROk "0uAZ|"  (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 3), 'u', 'A', 'Z', EOS))
        numberLiteral all "nl" |> ROk "0ulLF|" (NumberLiteral("0", NLF.IsDecimal ||| NLF.HasIntegerPart ||| ((enum) 4), 'u', 'l', 'L', 'F'))

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

        numberLiteral all "nl" |> RError ".a"    1 (expectedError "digit")
        numberLiteral all "nl" |> RError ".ea"   1 (expectedError "digit")
        numberLiteral all "nl" |> RError ".1ea"  3 (expectedError "digit")
        numberLiteral all "nl" |> RError "-1ea"  3 (expectedError "digit")
        numberLiteral all "nl" |> RError "1.e-a" 4 (expectedError "digit")
        numberLiteral all "nl" |> RError "1e+a"  3 (expectedError "digit")

        numberLiteral all "nl" |> RError "0x.g"    3 (expectedError "hexadecimal digit")
        numberLiteral all "nl" |> RError "0x.pa"   3 (expectedError "hexadecimal digit")
        numberLiteral all "nl" |> RError "+0x.1pa" 6 (expectedError "digit")
        numberLiteral all "nl" |> RError "0x1pa"   4 (expectedError "digit")
        numberLiteral all "nl" |> RError "0x1.p-a" 6 (expectedError "digit")
        numberLiteral all "nl" |> RError "0x1p+a"  5 (expectedError "digit")

        numberLiteral all "nl" |> RError "0b3"   2 (expectedError "binary digit")
        numberLiteral all "nl" |> RError "-0b.0" 3 (expectedError "binary digit")
        numberLiteral all "nl" |> RError "+0ou"  3 (expectedError "octal digit")
        numberLiteral all "nl" |> RError "0o.0"  2 (expectedError "octal digit")

        numberLiteral (all ^^^ NLO.AllowPlusSign)  "nl" |> RError "+1|" 0 (expectedError "nl")
        numberLiteral (all ^^^ NLO.AllowPlusSign)  "nl" |> ROk    "-1|" (NumberLiteral("-1", NLF.HasMinusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowMinusSign) "nl" |> RError "-1|" 0 (expectedError "nl")
        numberLiteral (all ^^^ NLO.AllowMinusSign) "nl" |> ROk    "+1|" (NumberLiteral("+1", NLF.HasPlusSign ||| NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ (NLO.AllowPlusSign ||| NLO.AllowMinusSign)) "nl" |> ROk "1|" (NumberLiteral("1", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))

        numberLiteral (all ^^^ NLO.AllowFractionWOIntegerPart) "nl" |> RError ".0|"   0 (expectedError "nl")
        numberLiteral (all ^^^ NLO.AllowFractionWOIntegerPart) "nl" |> RError "0x.0|" 2 (expectedError "hexadecimal digit")

        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> ROk    "1."         (NumberLiteral("1", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> ROkI    "10.10E2" 2 (NumberLiteral("10", NLF.IsDecimal ||| NLF.HasIntegerPart, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> RError ".1"       0 (expectedError "nl")
        numberLiteral (all ^^^ NLO.AllowFraction) "nl" |> RError ".1"       0 (expectedError "nl")
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

        numberLiteral (all ^^^ NLO.AllowInfinity) "nl" |> RError  "Infinity|" 0 (expectedError "nl")
        numberLiteral (all ^^^ NLO.AllowInfinity) "nl" |> ROk     "NaN|" (NumberLiteral("NaN", NLF.IsNaN, EOS, EOS, EOS, EOS))
        numberLiteral (all ^^^ NLO.AllowNaN) "nl"      |> RError  "NaN|" 0 (expectedError "nl")
        numberLiteral (all ^^^ NLO.AllowNaN) "nl"      |> ROk     "Infinity|" (NumberLiteral("Infinity", NLF.IsInfinity, EOS, EOS, EOS, EOS))

    testNumberLiteral()

    let testPfloat() =
        pfloat |> RError "" 0 (expectedError "floating-point number")
        pfloat |> RError "-0x" 3 (expectedError "hexadecimal digit")
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
        pfloat |> RFatalError "1e99999|" 0 (messageError "This number is outside the allowable range for double precision floating-pointer numbers.")
        pfloat |> RFatalError "0x1p99999|" 0 (messageError "This number is outside the allowable range for double precision floating-pointer numbers.")
        pfloat |> ROk "-0x123cde.123afAcEp123|" (floatOfHexString "-0x123cde.123afAcEp123")
        pfloat |> ROk "-0x1.fffffffffffffp1023|"  -System.Double.MaxValue
        pfloat |> RFatalError "0x1.fffffffffffffp1024|" 0 (messageError "This number is outside the allowable range for double precision floating-pointer numbers.")
        pfloat |> ROk "Inf|" System.Double.PositiveInfinity
        pfloat |> ROk "-Infinity|" System.Double.NegativeInfinity
        pfloat >>$ 1 |> ROk  "NaN|" 1

    testPfloat()

    let testPuint64() =
        let expectedE = expectedError "integer number (64-bit, unsigned)"
        let overflowE = messageError "This number is outside the allowable range for 64-bit unsigned integers."

        puint64 |> RError "" 0 expectedE
        puint64 |> RError "+1" 0 expectedE
        puint64 |> RFatalError "18446744073709551616" 0 overflowE
        puint64 |> RFatalError "0000018446744073709551616" 0 overflowE
        puint64 |> RFatalError "111111111111111111111" 0 overflowE

        puint64 |> ROk "0|" 0UL
        puint64 |> ROk "000|" 0UL
        puint64 |> ROk "12345678901234567890|" 12345678901234567890UL
        puint64 |> ROk "18446744073709551615|" System.UInt64.MaxValue
        puint64 |> ROk "18446744073709551614|" (System.UInt64.MaxValue - 1UL)
        puint64 |> ROk "0000018446744073709551615|" System.UInt64.MaxValue

        puint64 |> RError "0x"  2 (expectedError "hexadecimal digit")
        puint64 |> RError "+0x1" 0 expectedE
        puint64 |> RFatalError "0x10000000000000000" 0 overflowE
        puint64 |> RFatalError "0x11111111111111111" 0 overflowE
        puint64 |> RFatalError "0xfffffffffffffffff" 0 overflowE

        puint64 |> ROk "0x0|" 0UL
        puint64 |> ROk "0x000|" 0UL
        puint64 |> ROk "0x1234567890abcdef|" 0x1234567890abcdefUL
        puint64 |> ROk "0X1234567890ABCDEF|" 0x1234567890abcdefUL
        puint64 |> ROk "0xffffffffffffffff|" System.UInt64.MaxValue
        puint64 |> ROk "0x00000ffffffffffffffff|" System.UInt64.MaxValue

        puint64 |> RFatalError "0o2000000000000000000000" 0 overflowE
        puint64 |> RFatalError "0o7777777777777777777777" 0 overflowE
        puint64 |> RFatalError "0o77777777777777777777777" 0 overflowE

        puint64 |> ROk "0o0|" 0UL
        puint64 |> ROk "0o000|" 0UL
        puint64 |> ROk "0o1234567123456701234567|" 0o1234567123456701234567UL
        puint64 |> ROk "0o1777777777777777777777|" System.UInt64.MaxValue
        puint64 |> ROk "0o1777777777777777777776|" (System.UInt64.MaxValue - 1UL)
        puint64 |> ROk "0o000001777777777777777777777|" System.UInt64.MaxValue

        puint64 |> RFatalError "0b10000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        puint64 |> RFatalError "0b11111111111111111111111111111111111111111111111111111111111111111" 0 overflowE

        puint64 |> ROk "0b0|" 0UL
        puint64 |> ROk "0b000|" 0UL
        puint64 |> ROk "0b1111111111111111111111111111111111111111111111111111111111111111|" System.UInt64.MaxValue
        puint64 |> ROk "0b000001111111111111111111111111111111111111111111111111111111111111111|" System.UInt64.MaxValue

    testPuint64()

    let testPint64() =
        let expectedE = expectedError "integer number (64-bit, signed)"
        let overflowE = messageError "This number is outside the allowable range for 64-bit signed integers."

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
        pint64 |> RFatalError "-0x0ffffffffffffffff" 0 overflowE

        pint64 |> RFatalError "0x8000000000000000" 0 overflowE
        pint64 |> RFatalError "+0x0008000000000000000" 0 overflowE
        pint64 |> RFatalError "-0x8000000000000001" 0 overflowE
        pint64 |> RFatalError "-0x0008000000000000001" 0 overflowE

        pint64 |> ROk "0x7fffffffffffffff|" System.Int64.MaxValue
        pint64 |> ROk "+0x000007fffffffffffffff|" System.Int64.MaxValue
        pint64 |> ROk "-0x8000000000000000|" System.Int64.MinValue
        pint64 |> ROk "-0x008000000000000000|" System.Int64.MinValue

        pint64 |> RFatalError "0o1777777777777777777776" 0 overflowE
        pint64 |> RFatalError "+0o1777777777777777777776" 0 overflowE
        pint64 |> RFatalError "-0o1777777777777777777776" 0 overflowE
        pint64 |> RFatalError "-0o0001777777777777777777776" 0 overflowE

        pint64 |> RFatalError "0o1000000000000000000000" 0 overflowE
        pint64 |> RFatalError "+0o001000000000000000000000" 0 overflowE
        pint64 |> RFatalError "-0o1000000000000000000001" 0 overflowE
        pint64 |> RFatalError "-0o001000000000000000000001" 0 overflowE

        pint64 |> ROk "0o777777777777777777777|" System.Int64.MaxValue
        pint64 |> ROk "+0o00777777777777777777777|" System.Int64.MaxValue
        pint64 |> ROk "-0o1000000000000000000000|" System.Int64.MinValue
        pint64 |> ROk "-0o001000000000000000000000|" System.Int64.MinValue

        pint64 |> RFatalError "+0b0001111111111111111111111111111111111111111111111111111111111111111" 0 overflowE
        pint64 |> RFatalError "-0b01111111111111111111111111111111111111111111111111111111111111111" 0 overflowE

        pint64 |> RFatalError "+0b1000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        pint64 |> RFatalError "0b0001000000000000000000000000000000000000000000000000000000000000000" 0 overflowE
        pint64 |> RFatalError "-0b0001000000000000000000000000000000000000000000000000000000000000001" 0 overflowE
        pint64 |> RFatalError "-0b1000000000000000000000000000000000000000000000000000000000000001" 0 overflowE

        pint64 |> ROk "0b111111111111111111111111111111111111111111111111111111111111111|" System.Int64.MaxValue
        pint64 |> ROk "+0b00111111111111111111111111111111111111111111111111111111111111111|" System.Int64.MaxValue
        pint64 |> ROk "-0b1000000000000000000000000000000000000000000000000000000000000000|"System.Int64.MinValue
        pint64 |> ROk "-0b0001000000000000000000000000000000000000000000000000000000000000000|" System.Int64.MinValue

    testPint64()

    let testPintOther() =
       let overflowInt32  = messageError "This number is outside the allowable range for 32-bit signed integers."
       let overflowUInt32 = messageError "This number is outside the allowable range for 32-bit unsigned integers."
       let overflowInt16  = messageError "This number is outside the allowable range for 16-bit signed integers."
       let overflowUInt16 = messageError "This number is outside the allowable range for 16-bit unsigned integers."
       let overflowInt8   = messageError "This number is outside the allowable range for 8-bit signed integers."
       let overflowUInt8  = messageError "This number is outside the allowable range for 8-bit unsigned integers."

       puint32 |> RError "+0|" 0 (expectedError "integer number (32-bit, unsigned)")
       puint32 |> RFatalError "4294967296|" 0 overflowUInt32
       puint32 |> RFatalError "00004294967296|" 0 overflowUInt32
       puint32 |> RFatalError "11111111111|" 0 overflowUInt32

       puint32 |> ROk "0|" 0u
       puint32 |> ROk "000|" 0u
       puint32 |> ROk "1234567890|" 1234567890u
       puint32 |> ROk "0001234567890|" 1234567890u
       puint32 |> ROk "4294967295|" System.UInt32.MaxValue
       puint32 |> ROk "0004294967295|" System.UInt32.MaxValue

       pint32 |> RError "+|" 0 (expectedError "integer number (32-bit, signed)")
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
    followedByChar '1' |> ROk "1" 0 ()
    followedByChar '1' |> RError "2" 0 (expectedError "'1'")
    followedByChar '\r' |> ROk "\r" 0 ()
    followedByChar '\r' |> ROk "\n" 0 ()
    followedByChar '\n' |> ROk "\r" 0 ()
    followedByChar '\n' |> ROk "\n" 0 ()

    notFollowedByChar '1'  |> ROk "2" 0 ()
    notFollowedByChar '1'  |> RError "1" 0 (unexpectedError "'1'")
    notFollowedByChar '\r' |> RError "\r" 0 (unexpectedError "newline")
    notFollowedByChar '\r' |> RError "\n" 0 (unexpectedError "newline")
    notFollowedByChar '\n' |> RError "\r" 0 (unexpectedError "newline")
    notFollowedByChar '\n' |> RError "\n" 0 (unexpectedError "newline")

    followedByString "123" |> ROk "123" 0 ()
    followedByString "123" |> RError "124" 0 (expectedError "'123'")
    notFollowedByString "123" |> ROk "124" 0 ()
    notFollowedByString "123" |> RError "123" 0 (unexpectedError "'123'")

    try followedByString "13\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try notFollowedByString "13\r" |> ignore; Fail()
    with :? System.ArgumentException -> ()

    nextCharSatisfies ((=) '2')  |> ROk    "12"  0 ()
    nextCharSatisfies ((=) '\n') |> ROk    "1\n" 0 ()
    nextCharSatisfies ((=) '\n') |> ROk    "1\r" 0 ()
    nextCharSatisfies ((=) '2')  |> RError "11"  0 NoErrorMessages
    nextCharSatisfies ((=) '2')  |> RError "1"   0 NoErrorMessages
    nextCharSatisfies ((=) '2')  |> RError ""    0 NoErrorMessages

    nextCharSatisfiesNot ((<>) '2')  |> ROk    "12"  0 ()
    nextCharSatisfiesNot ((<>) '\n') |> ROk    "1\n" 0 ()
    nextCharSatisfiesNot ((<>) '\n') |> ROk    "1\r" 0 ()
    nextCharSatisfiesNot ((<>) '2')  |> RError "11"  0 NoErrorMessages
    nextCharSatisfiesNot ((<>) '2')  |> ROk    "1"   0 ()
    nextCharSatisfiesNot ((<>) '2')  |> ROk    ""    0 ()

    currentCharSatisfies ((=) '2')  |> ROk    "2"  0 ()
    currentCharSatisfies ((=) '\n') |> ROk    "\n" 0 ()
    currentCharSatisfies ((=) '\n') |> ROk    "\r" 0 ()
    currentCharSatisfies ((=) '2')  |> RError "1"  0 NoErrorMessages
    currentCharSatisfies ((=) '2')  |> RError ""   0 NoErrorMessages

    currentCharSatisfiesNot ((<>) '2')  |> ROk    "2"  0 ()
    currentCharSatisfiesNot ((<>) '\n') |> ROk    "\n" 0 ()
    currentCharSatisfiesNot ((<>) '\n') |> ROk    "\r" 0 ()
    currentCharSatisfiesNot ((<>) '2')  |> RError "1"  0 NoErrorMessages
    currentCharSatisfiesNot ((<>) '2')  |> ROk    ""   0 ()

    (anyChar >>. previousCharSatisfies ((=) '1'))  |> ROk    "12"  1 ()
    (anyChar >>. previousCharSatisfies ((=) '\n')) |> ROkNL  "\n1" 1 ()
    (anyChar >>. previousCharSatisfies ((=) '\n')) |> ROkNL  "\r1" 1 ()
    (anyChar >>. previousCharSatisfies ((=) '1'))  |> RError "01"  1 NoErrorMessages
    (previousCharSatisfies ((=) '1'))  |> RError "1" 0 NoErrorMessages
    (previousCharSatisfies ((=) '1'))  |> RError "" 0 NoErrorMessages

    (anyChar >>. previousCharSatisfiesNot ((<>) '1'))  |> ROk   "12"  1 ()
    (anyChar >>. previousCharSatisfiesNot ((<>) '\n')) |> ROkNL "\n1" 1 ()
    (anyChar >>. previousCharSatisfiesNot ((<>) '\n')) |> ROkNL "\r1" 1 ()
    (anyChar >>. previousCharSatisfiesNot ((<>) '1'))  |> RError "01" 1 NoErrorMessages
    (previousCharSatisfiesNot ((<>) '1'))  |> ROk "1" 0 ()
    (previousCharSatisfiesNot ((<>) '1'))  |> ROk "" 0 ()

let run() =
    testCharParsers()
    testAnyNoneOf()
    testSpecialCharParsers()
    testStringParsers()
    testManySatisfy()
    testMany()
    testSkipToString()
    testNumberParsers()
    testFollowedBy()