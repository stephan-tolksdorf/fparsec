// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.CharSetTests

open FParsec.Test.Test

let basicTests() =
    let test s (sin: string) (sout: string) =
        let cs = FParsec.CharSet(s)
        for c in sin do
            cs.Contains(c) |> True
        for c in sout do
            cs.Contains(c) |> False

    test "" "" "a\u0000\uffff"
    test "a" "a" "\u0000\uffff"
    test "\u0000\uffffa" "a\u0000\uffffa" "b\u0001\ufffe"
    test "\u0002\u0001\u0399\u0400\u0401\u0399\u0400\u0401\uffffabc123" "\u0002\u0001\u0399\u0400\u0401\uffffabc123" "\u0000\u0398\u0402\ufffed0"


let moreTests() =
    let rand = new System.Random(12345)

    for j = 0 to 20000 do
        let n = rand.Next(1, 100)
        let cs = Array.zeroCreate n
        for i = 1 to n/2 do
            let r = rand.Next()
            cs.[i*2 - 2] <- char r
            cs.[i*2 - 1] <- char (r >>> 16)
        if n%2 = 1 then cs.[cs.Length - 1] <- char (rand.Next())

        let set = FParsec.CharSet(new string(cs))

        Array.sortInPlace cs

        let mutable c_1 = '\uffff'
        let mutable c = cs.[0]

        for i = 0 to n - 1 do
            set.Contains(c) |> True
            if c <> c_1 && int c - 1 <> int c_1 then
                set.Contains(char (int c - 1)) |> False
            if i + 1 < n then
                let c1 = cs.[i + 1]
                if c < '\uffff' && c <> c1 && int c + 1 <> int c1 then
                    set.Contains(char (int c + 1)) |> False
                c_1 <- c
                c <- c1

let run() =
    basicTests()
    moreTests()
