// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.StaticMappingTests

#if LOW_TRUST
#else

open FParsec
open FParsec.Range
open FParsec.StaticMapping

open FParsec.Test.Test

type EqualityComparer<'a> = System.Collections.Generic.EqualityComparer<'a>

let testCreateIndicatorFunction() =
    let test (ranges: Range[]) value (minValue: int) (maxValue: int) indicator =
        let mutable i = minValue
        for r in ranges do
            while i < r.Min do
                indicator i |> Equal (not value)
                i <- i + 1
            while i <= r.Max do
                indicator i |> Equal value
                i <- i + 1
        while i <= maxValue do
            indicator i |> Equal (not value)
            i <- i + 1

    FParsec.Emit.noBitVectorTests <- true

    // check all possible subsets of {1,2,...,N}
    let N = 11
    let set = new ResizeArray<_>(N)
    for n = 0 to (1 <<< N) - 1 do
        let mutable b = n
        let mutable i = 1
        while b <> 0 do
            if (b &&& 1) <> 0 then set.Add(i)
            b <- b >>> 1
            i <- i + 1

        let ranges = collectSortAndMergeRanges set
        let indicator = createStaticIntIndicatorFunctionImpl<int> 0 0. 0 (N + 1) false ranges
        test ranges true  0 (N + 1) indicator
        let indicator2 = createStaticIntIndicatorFunctionImpl<int> 0 0. 0 (N + 1) true ranges
        test ranges false 0 (N + 1) indicator2

        set.Clear()

    FParsec.Emit.noBitVectorTests <- false

    let rand = new System.Random(1234)

    // check some random subsets of {1,2,...,N}
    let N = 16000
    for n = 1 to 1000 do
        let p = rand.NextDouble()
        for i = 0 to N do
            if rand.NextDouble() <= p then
                set.Add(i)
        let invert = rand.NextDouble() <= 0.5
        let ranges = collectSortAndMergeRanges set
        let indicator = createStaticIntIndicatorFunctionImpl<int>
                            32 0.4 0 N invert ranges
        test ranges (not invert) 0 N indicator
        set.Clear()

    let () =
        let ranges = [|Range(1,1); Range(30,31)|]
        let indicator = createStaticIntIndicatorFunctionImpl<int> 0 0. 0 31 false ranges
        test ranges true 0 31 indicator

    let () =
        let ranges = [|Range(1,1); Range(30,32)|]
        let indicator = createStaticIntIndicatorFunctionImpl<int> 0 0. 0 32 false ranges
        test ranges true 0 32 indicator

    let () =
        let ranges = [|Range(1,1); Range(60,63)|]
        let indicator = createStaticIntIndicatorFunctionImpl<int> 0 0. 0 63 false ranges
        test ranges true 0 63 indicator

    let () =
        let ranges = [|Range(1,1); Range(60,64)|]
        let indicator = createStaticIntIndicatorFunctionImpl<int> 0 0. 0 64 false ranges
        test ranges true 0 64 indicator
    let () =
        let ranges = [|Range(1,1); Range(3,5); Range(6,7)|]
        let indicator = createStaticIntRangeIndicatorFunction false ranges
        test ranges true 0 9 indicator
        let indicator2 = createStaticIntIndicatorFunction false [1;3;4;5;6;7]
        test ranges true 0 9 indicator2

    let () =
        let indicator = createStaticCharIndicatorFunction false ['\u0000';'\ufffe';'\uffff']
        indicator '\u0000' |> Equal true
        indicator '\u0001' |> Equal false
        indicator '\ufffd' |> Equal false
        indicator '\ufffe' |> Equal true
        indicator '\uffff' |> Equal true
        let indicator2 = createStaticCharRangeIndicatorFunction false [Range(0,0); Range(0xfffe, 0xffff)]
        indicator2 '\u0000' |> Equal true
        indicator2 '\u0001' |> Equal false
        indicator2 '\ufffd' |> Equal false
        indicator2 '\ufffe' |> Equal true
        indicator2 '\uffff' |> Equal true


    let () =
        try createStaticCharRangeIndicatorFunction false [Range(0xfffe, 0xffff); Range(-1,0);] |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try createStaticCharRangeIndicatorFunction false [Range(0,0); Range(0xfffe, 0x10000)] |> ignore; Fail()
        with :? System.ArgumentException -> ()

    ()


type TestStruct(value: int) = struct end

[<CustomEquality; NoComparison>]
type TestStruct2 = struct
    val Field1: int64
    val Field2: int64
    new (value: int) = {Field1 = int64 value; Field2 = int64 value}
    override t.Equals(other: obj) =
        match other with
        | :? TestStruct2 as o -> t.Field1 = o.Field1 && t.Field2 = o.Field2
        | _ -> false
    override t.GetHashCode() = 0
end

let testCreateStaticIntMapping() =
    let test (ranges: Range[]) (values: 't[]) defaultValue (minKey: int) (maxKey: int) mapping =
        let mutable i = minKey
        for r, value in Seq.zip ranges values  do
            while i < r.Min do
                mapping i |> Equal defaultValue
                i <- i + 1
            while i <= r.Max do
                mapping i |> Equal value
                i <- i + 1
        while i <= maxKey do
            mapping i |> Equal defaultValue
            i <- i + 1

    let ranges = new ResizeArray<Range>()
    let rand = new System.Random(1234)
    let N = 16000
    for n = 0 to 2000 do
        ranges.Clear()

        let maxRangeLength = 1 + rand.Next(128)
        let mutable i = 0
        while i < N do
            let length = 1 + rand.Next(maxRangeLength)
            let i2 = min (i + length) N
            ranges.Add(Range(i, i2 - 1))
            i <- i2

        let ranges = ranges.ToArray()
        let values = Array.zeroCreate ranges.Length
        let mutable lastValue = 7
        for i = 0 to values.Length - 1 do
            // value in 0-7, but different from the last value
            lastValue <- (lastValue + 1 + rand.Next(7)) % 8
            values.[i] <- byte lastValue

        let ranges, values = filterOutDefaultValueRanges EqualityComparer<_>.Default ranges values 0uy
        let mapping = createStaticIntMappingImpl
                          //defaultMappingLengthCap defaultMappingDensityThreshold
                          16 0.90
                          0 (N - 1)
                          0uy ranges values
        test ranges values 0uy 0 (N - 1) mapping

    let test2 keyValues defaultValue =
        let mapping = createStaticIntMapping defaultValue keyValues
        for k,v in keyValues do
            mapping k |> Equal v

        mapping ((keyValues |> List.minBy (fun (k,v) -> k) |> fst) - 1) |> Equal defaultValue
        mapping ((keyValues |> List.maxBy (fun (k,v) -> k) |> fst) + 1) |> Equal defaultValue
        mapping System.Int32.MinValue |> Equal defaultValue

    test2 [0, 1; 1, 1; 2, 1;] 0

    test2 [1, true; 2, true; 3, true; 4, true] false
    test2 [1, '1'; 2, '2'; 3, '1'; 4, '0'] '0'
    test2 [1, 1y; 2, 2y; 3, 1y; 4, 0y] 0y
    test2 [1, 1uy; 2, 2uy; 3, 1uy; 4, 0uy] 0uy
    test2 [1, 1s; 2, 2s; 3, 1s; 4, 0s] 0s
    test2 [1, 1us; 2, 2us; 3, 1us; 4, 0us] 0us
    test2 [1, 1u; 2, 2u; 3, 1u; 4, 0u] 0u
    test2 [1, 1L; 2, 2L; 3, 1L; 4, 0L; 5, System.Int64.MaxValue] 0L
    test2 [1, 1UL; 2, 2UL; 3, 1UL; 4, 0UL; 5, System.UInt64.MaxValue] 0UL
    test2 [1, 1n; 2, 2n; 3, 1n; 4, 0n] 0n
    test2 [1, 1un; 2, 2un; 3, 1un; 4, 0un] 0un
    test2 [1, 1.f; 2, 2.f; 3, 1.f; 4, 0.f] 0.f
    test2 [1, 1.; 2, 2.; 3, 1.; 4, 0.] 0.
    test2 [1, "1"; 2, "2"; 3, "1"; 4, ""] ""
    test2 [1, "1"; 2, "2"; 3, "1"; 4, ""] null
    test2 [1, TestStruct(1); 2, TestStruct(2); 3, TestStruct(1); 4, TestStruct(0)] (TestStruct(0))
    test2 [1, TestStruct2(1); 2, TestStruct2(2); 3, TestStruct2(1); 4, TestStruct2(0)] (TestStruct2(0))
    test2 [1, FParsec.Associativity.Left; 2, FParsec.Associativity.Right; 3, FParsec.Associativity.Left; 4, FParsec.Associativity.None] FParsec.Associativity.None

    let () =
        let mapping = createStaticIntRangeMapping 0 [Range(1,1), 1; Range(3,3), 2; Range(4,5), 2; Range(6,6), 0]
        mapping 0 |> Equal 0
        mapping 1 |> Equal 1
        mapping 2 |> Equal 0
        mapping 3 |> Equal 2
        mapping 4 |> Equal 2
        mapping 5 |> Equal 2
        mapping 6 |> Equal 0
        mapping 7 |> Equal 0

    let () =
        try createStaticIntMapping 0 [1, 0; 1, 0] |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try createStaticIntRangeMapping 0 [Range(0, 1), 0; Range(1, 2), 0] |> ignore; Fail()
        with :? System.ArgumentException -> ()
    ()

let testCreateStaticStringMapping() =
    let testStringComparison() =
        for nn = 64 to 71 do
            let chars = [|for i = 1 to nn do yield char (32 + i)|]
            let str = new string(chars)
            for n = 0 to str.Length - 1 do
                let subStr = str.Substring(0, n)
                let mapping = createStaticStringMapping 0 [str, 1; subStr, 2]
                mapping str |> Equal 1
                mapping subStr |> Equal 2
                for i = 0 to chars.Length - 1 do
                    let c = chars.[i]
                    chars.[i] <- char (int c + 1)
                    mapping (new string(chars)) |> Equal 0
                    chars.[i] <- char 0
                    mapping (new string(chars)) |> Equal 0
                    chars.[i] <- c


    testStringComparison()

    let test defaultValue stringValues defaultTestStrings =
        let mapping = createStaticStringMapping defaultValue stringValues
        for str, value in stringValues do
            let v = mapping str
            v |> Equal value
        for str in defaultTestStrings do
            let v = mapping str
            v |> Equal defaultValue

    test 0 [] ["\u0000"]
    test 0 ["", 1] ["\u0000"]
    test 0 ["\u0000", 1] [""; "a"]
    test 0 ["", 1; "\u0000", 2] ["a"]

    test 0 ["\u0000", 1; "\u0001", 2] [""; "\u0002"]
    test 0 ["\u0000", 1; "\u0001", 2; "\u0002\u0003", 3] [""; "\u0002"]
    test 0 ["", 1; "\u0000", 2; "\u0001", 3] ["\u0002"]
    test 0 ["\u0001", 3; "\u0000", 2; "", 1] ["\u0002"]
    test 0 ["\u0001", 2; "\u0002\u0003", 3] [""; "\u0000"; "\u0002"]
    test 0 ["\u0001", 1; "\u0002", 2] [""; "\u0000"]
    test 0 ["", 1; "\u0001", 2; "\u0002", 3] ["\u0000"; "a"]
    test 0 ["\u0001", 1; "\u0002", 1] [""; "\u0000"; "a"]
    test 0 ["", 1; "\u0001", 2; "\u0002", 2] ["\u0000"; "a"]
    test 0 ["", 1; "\u0000", 2; "\u0001", 2] ["\u0002"; "a"]


    test 0 ["prefix1", 1; "prefix2", 2; "prefix3", 3] [""; "prefix"; "prefix\u0000"]
    test 0 ["prefix1", 2; "prefix2", 2; "prefix3", 2] [""; "prefix"; "prefix\u0000"]
    test 0 ["prefix1postfix", 2; "prefix2postfix", 2; "prefix3postfix.", 2] [""; "prefix"; "prefix\u0000"]
    test 0 ["", -1; "prefix", 1; "prefix1", 1; "prefix2", 2; "prefix3", 3] ["prefix\u0000"]

    test 0 ["", -1; "postfix1", 1; "postfix2", 2;
                    "test/postfix1", 1; "test/postfix2", 2;
                    "test/test/postfix1", 1; "test/test/postfix2", 2;
                    "test/test/test/postfix1", 1; "test/test/test/postfix2", 2;
                    "test/test/test|postfix1", 1; "test/test/test|postfix2", 2] []

    test 0
        [|"abstract", 1; "and", 2; "as", 3; "assert", 4;
          "base", 5; "begin", 6; "class", 7; "default", 8;
          "delegate", 9;  "done", 10; "downcast", 11; "downto", 12;
          "elif", 13; "else", 14; "end", 15; "exception", 16;
          "extern", 17; "finally", 18; "for", 19; "fun", 20;
          "function", 21; "if", 22; "in", 23; "inherit", 24;
          "inline", 25; "interface", 26; "internal", 27;  "lazy", 28;
          "match", 29; "member", 30; "module", 31; "mutable", 32;
          "namespace", 33; "new", 34; "of", 35; "open", 36;
          "or", 37; "override", 38; "private", 39; "public", 40;
          "rec", 41;  "static", 42; "struct", 43; "then", 44;
          "to", 45; "try", 46; "type", 47; "upcast", 48;
          "use", 49; "val", 50; "void", 51; "when", 52;
          "with", 53; "false", 54; "true", 55; "let", 56;
          "do", 57; "while", 58; "yield", 59; "return", 60;
          "asr", 61;" land", 61; "lor", 63; "lsl", 64;
          "lsr", 65; "lxor", 66; "mod", 67; "sig", 68;
          "atomic", 69; "break", 70; "checked", 71; "component", 72;
          "const", 73; "constraint", 74; "constructor", 75; "continue", 76;
          "eager", 77; "event", 78; "external", 79; "fixed", 80;
          "functor", 81; "global", 82; "include", 83; "method", 84;
          "mixin", 85; "object", 86; "parallel", 87; "process", 88;
          "protected", 89; "pure", 90; "sealed", 91; "tailcall", 92;
          "trait", 93; "virtual", 94; "volatile", 95|]
         []

    test "" ["1", "1"; "2", "2"; "221", "221"; "222", "222"; "3", "1"; "4", "4"] [""; "0"]
    test null ["1", "1"; "2", "2"; "221", "221"; "222", "222"; "3", "1"; "4", ""] [""; "0"]
    test (TestStruct(0)) ["1", TestStruct(1); "2", TestStruct(2); "221", TestStruct(221); "222", TestStruct(222); "3", TestStruct(1); "4", TestStruct(0)] [""; "0"]
    test (TestStruct2(0)) ["1", TestStruct2(1); "2", TestStruct2(2); "221", TestStruct2(221); "222", TestStruct2(222); "3", TestStruct2(1); "4", TestStruct2(0)] [""; "0"]
    test FParsec.Associativity.Left ["1", FParsec.Associativity.None; "2", FParsec.Associativity.Right; "3", FParsec.Associativity.None] [""; "4"]

    try createStaticStringMapping 0 [null, 1] |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try createStaticStringMapping 0 ["1", 1; null, 2] |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try createStaticStringMapping 0 ["", 1; "", 2] |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try createStaticStringMapping 0 ["1", 1; "", 2; "3", 3; "", 4] |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try createStaticStringMapping 0 ["1", 1; "1", 2] |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try createStaticStringMapping 0 ["0", 1; "1", 2; "3", 3; "1", 4] |> ignore; Fail()
    with :? System.ArgumentException -> ()

    try createStaticStringMapping 0 [] null |> ignore; Fail()
    with :? System.NullReferenceException
       | :? System.ArgumentNullException  -> ()

    try createStaticStringMapping 0 ["1", 1] null |> ignore; Fail()
    with :? System.NullReferenceException
       | :? System.ArgumentNullException  -> ()

    try createStaticStringMapping 0 ["1", 1; "2", 2] null |> ignore; Fail()
    with :? System.NullReferenceException
       | :? System.ArgumentNullException  -> ()

let run() =
    testCreateIndicatorFunction()
    testCreateStaticIntMapping()
    testCreateStaticStringMapping()

#endif
