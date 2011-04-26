// Copyright (c) Stephan Tolksdorf 2009-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.TextTests

open FParsec.Test.Test

type Text = FParsec.Text

let testFoldCase() =
    Text.FoldCase(null)   |> Equal null
    for s in [""; "a"; "aa"; "aaa"] do
        Text.FoldCase(s) |> ReferenceEqual s

    Text.FoldCase("A")    |> Equal "a"
    Text.FoldCase("aA")   |> Equal "aa"
    Text.FoldCase("aaA")  |> Equal "aaa"
    Text.FoldCase("abcAOUÄÖÜdef") |> Equal "abcaouäöüdef"

    let oneToOneMappings = getStaticField (typeof<FParsec.CharStream>.Assembly.GetType("FParsec.CaseFoldTable")) "oneToOneMappings" : string

    let mutable j = 0
    for i in 0..2..(oneToOneMappings.Length - 2) do
        let c = int oneToOneMappings.[i]
        for k = j to c - 1 do
            Text.FoldCase((char k).ToString()).[0] |> Equal (char k)
        Text.FoldCase((char c).ToString()).[0] |> Equal oneToOneMappings.[i + 1]
        j <- c + 1
    j |> Equal 0xff3b

let testNormalizeNewlines() =
    Text.NormalizeNewlines(null) |> Equal null
    Text.NormalizeNewlines("")   |> ReferenceEqual ""
    Text.NormalizeNewlines("ab") |> ReferenceEqual "ab"

    let check (cs: char[]) n =
        let str = new string(cs, 0, n)
        let nstr = str.Replace("\r\n", "\n").Replace("\r", "\n")
        let nstr2 = Text.NormalizeNewlines(str)
        Equal nstr nstr2

    let rec test (cs: char[]) n i =
        if i < n then
            cs.[i] <- '\r'
            test cs n (i + 1)
            cs.[i] <- '\n'
            test cs n (i + 1)
            cs.[i] <- '_'
            test cs n (i + 1)
        else
            check cs n

    let N = 10
    let cs = Array.zeroCreate N

    for n = 1 to 8 do
        // test all possible character sequences of length n consisting of '\r','\n' or '_' chars
        test cs n 0

    // make sure there is no size-specific copying problem
    for n = 1 to 24 do
        let s = new string('_', n)
        Text.NormalizeNewlines("\r" + s) |> Equal ("\n" + s)
        Text.NormalizeNewlines("\r\n" + s) |> Equal ("\n" + s)

    Text.NormalizeNewlines("_\n_\r\n_\r\r_\r\n_\r\n\n\r_") |> Equal "_\n_\n_\n\n_\n_\n\n\n_"

let testCountTextElements() =
    let countTextElementsRef s =
        let te = System.Globalization.StringInfo.GetTextElementEnumerator(s)
        let mutable count = 0
        while te.MoveNext() do count <- count + 1
        count

    let chars = [|"\u0020"; "\u007e"; "\U0001D41A";
                  "\u001F";" \u007F"; // control
                  "\u00AD"; "\U0001D173"; // format
                  string '\ud800'; // surrogate (uses string '...' to work around an fsc parser issue)
                  "\u0333"; "\U000101FD" // nonspacing mark
                  "\u0BBE"; "\U0001D166" // spacing combining mark
                  "\u20DD" // enclosing mark
                |]

    for c in chars do
        Text.CountTextElements(c) |> Equal (countTextElementsRef c)

    for c1 in chars do
        for c2 in chars do
            let s = c1 + c2
            Text.CountTextElements(s) |> Equal (countTextElementsRef s)

    let rand = System.Random(1234)
    let strings = Array.zeroCreate 5
    for i = 0 to 100000 do
        for j = 0 to strings.Length - 1 do
           strings.[j] <- chars.[rand.Next()%chars.Length]
           let s = System.String.Concat(strings)
           Text.CountTextElements(s) |> Equal (countTextElementsRef s)

let testIsSurrogate() =
    for c = 0 to 0xffff do
        let c = char c
        Text.IsSurrogate(c) |> Equal (System.Char.IsSurrogate(c))
        Text.IsLowSurrogate(c) |> Equal (System.Char.IsLowSurrogate(c))
        Text.IsHighSurrogate(c) |> Equal (System.Char.IsHighSurrogate(c))

let testIsWhitespace() =
    for c = 0 to 0xffff do
        Text.IsWhitespace(char c) |> Equal (System.Char.IsWhiteSpace(char c))


let run() =
    testNormalizeNewlines()
    testFoldCase()
    testCountTextElements()
    testIsWhitespace()
    testIsSurrogate()