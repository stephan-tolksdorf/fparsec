// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.RangeTests

#if LOW_TRUST
#else

open FParsec
open FParsec.Range

open FParsec.Test.Test

let int32Min = System.Int32.MinValue
let int32Max = System.Int32.MaxValue

let testLabel1, testLabel2 =
    let dm = new System.Reflection.Emit.DynamicMethod("__DummMethodForObtainingLabels", null, null)
    let ilg = dm.GetILGenerator()
    let label1, label2 = ilg.DefineLabel(), ilg.DefineLabel()
    ilg.Emit(System.Reflection.Emit.OpCodes.Ret);
    dm.CreateDelegate(typeof<System.Action>) |> ignore
    label1, label2

let testCheckRanges() =
    let l, l2 = testLabel1, testLabel2

#if DEBUG
    // in DEBUG builds one can't construct invalid ranges (due to an assert check)
#else
    try checkRangesAreValidSortedAndUnconnected [|Range(1, -1)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(1, -1)|] [|l|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(2, 1)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(2, 1)|] [|l; l2|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()
#endif

    try checkRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(0, 0)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(0, 0)|] [|l; l2|]|> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(1, 1)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected[|Range(0, 0); Range(1, 1)|] [|l; l|]|> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(2, 2); Range(3, 3);|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(0, 0); Range(2, 2); Range(3, 3);|] [|l; l; l|]|> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkRangesAreValidSortedAndUnconnected [|Range(1, 1); Range(0, 0)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(1, 1); Range(0, 0)|] [|l; l2|]|> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkRangesAreValidSortedAndUnconnected [|Range(int32Min, int32Max); Range(int32Max, int32Max)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(int32Min, int32Max); Range(int32Max, int32Max)|] [|l; l2|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkRangesAreValidSortedAndUnconnected [|Range(int32Min, int32Max); Range(int32Min, int32Min)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(int32Min, int32Max); Range(int32Min, int32Min)|] [|l; l2|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try checkLabelRangesAreValidSortedAndUnconnected [|Range(int32Min, int32Max)|] [|l; l2|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()


let testSortAndMergeRanges() =
    sortAndMergeRanges false [||] |> Equal [||]
    sortAndMergeRanges false [|Range(1, 1); Range(-1,-1)|] |> Equal [|Range(-1, -1); Range(1, 1)|]

    sortAndMergeRanges false [|Range(int32Min, int32Max)|] |> Equal [|Range(int32Min, int32Max)|]
    sortAndMergeRanges false [|Range(int32Min, 1); Range(2, int32Max)|] |> Equal [|Range(int32Min, int32Max)|]
    sortAndMergeRanges false [|Range(2, int32Max); Range(int32Min, 1)|] |> Equal [|Range(int32Min, int32Max)|]

    sortAndMergeRanges false [|Range(1, 2); Range(3, 4); Range(5, 6)|] |> Equal [|Range(1, 6)|]
    sortAndMergeRanges false [|Range(1, 2); Range(4, 4); Range(5, 6)|] |> Equal [|Range(1, 2); Range(4, 6)|]
    sortAndMergeRanges false [|Range(1, 2); Range(4, 4); Range(6, 6)|] |> Equal [|Range(1, 2); Range(4, 4); Range(6, 6)|]

    sortAndMergeRanges true [|Range(int32Min, int32Max); Range(int32Min + 1, int32Max)|] |> Equal [|Range(int32Min, int32Max)|]
    sortAndMergeRanges true [|Range(int32Min, int32Max); Range(int32Min, int32Max - 1)|] |> Equal [|Range(int32Min, int32Max)|]

    sortAndMergeRanges true [|Range(0,0); Range(int32Min, int32Max); Range(int32Min + 1, int32Max)|] |> Equal [|Range(int32Min, int32Max)|]
    sortAndMergeRanges true [|Range(0,0); Range(int32Min, int32Max); Range(int32Min, int32Max - 1)|] |> Equal [|Range(int32Min, int32Max)|]
    sortAndMergeRanges true [|Range(int32Min + 1, int32Max); Range(int32Min, int32Max - 1)|] |> Equal [|Range(int32Min, int32Max)|]

    sortAndMergeRanges true [|Range(1, 3); Range(3, 4); Range(5, 6)|] |> Equal [|Range(1, 6)|]
    sortAndMergeRanges true [|Range(1, 2); Range(3, 5); Range(5, 6)|] |> Equal [|Range(1, 6)|]
    sortAndMergeRanges true [|Range(1, 2); Range(2, 5); Range(5, 6)|] |> Equal [|Range(1, 6)|]
    sortAndMergeRanges true [|Range(1, 5); Range(3, 4); Range(5, 6)|] |> Equal [|Range(1, 6)|]
    sortAndMergeRanges true [|Range(1, 5); Range(5, 6); Range(3, 5)|] |> Equal [|Range(1, 6)|]
    sortAndMergeRanges true [|Range(1, 5); Range(3, 5); Range(5, 6); Range(1, 7)|] |> Equal [|Range(1, 7)|]

#if DEBUG
    // in DEBUG builds one can't construct invalid ranges (due to an assert check)
#else
    try sortAndMergeRanges false [|Range(1, -1);|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try sortAndMergeRanges false [|Range(0, 0); Range(2, 1)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()
#endif

    try sortAndMergeRanges false [|Range(0, 0); Range(0, 0)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try sortAndMergeRanges false [|Range(0, 0); Range(1, 2); Range(2, 2)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try sortAndMergeRanges false [|Range(int32Min, int32Max); Range(0, 0)|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

let testSortAndMergeKeyValueRanges() =
    let cmp = System.Collections.Generic.EqualityComparer<int>.Default
    sortAndMergeKeyValueRanges null [||] |> Equal ([||], [||])
    sortAndMergeKeyValueRanges null [|Range(1, 1), 0; Range(-1,-1), 1|] |> Equal ([|Range(-1, -1); Range(1, 1)|], [|1; 0|])
    sortAndMergeKeyValueRanges cmp  [|Range(1, 1), 0; Range(-1,-1), 1|] |> Equal ([|Range(-1, -1); Range(1, 1)|], [|1; 0|])
    sortAndMergeKeyValueRanges cmp  [|Range(1, 1), 0; Range(-1, 0), 0|] |> Equal ([|Range(-1, 1)|], [|0|])

    sortAndMergeKeyValueRanges null [|Range(2, int32Max), 0; Range(int32Min, 1), 0|] |> Equal ([|Range(int32Min, 1); Range(2, int32Max)|], [|0; 0|])
    sortAndMergeKeyValueRanges cmp  [|Range(2, int32Max), 0; Range(int32Min, 1), 0|] |> Equal ([|Range(int32Min, int32Max)|], [|0|])

    sortAndMergeKeyValueRanges cmp  [|Range(1, 2), 0; Range(3, 4), 0; Range(6, 6), 0|] |> Equal ([|Range(1, 4); Range(6, 6)|], [|0; 0|])
    sortAndMergeKeyValueRanges cmp  [|Range(1, 1), 0; Range(3, 4), 0; Range(5, 6), 0|] |> Equal ([|Range(1, 1); Range(3, 6)|], [|0; 0|])

#if DEBUG
    // in DEBUG builds one can't construct invalid ranges (due to an assert check)
#else
    try sortAndMergeKeyValueRanges null [|Range(1, -1), 0;|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try sortAndMergeKeyValueRanges null [|Range(0, 0), 0; Range(2, 1), 0|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()
#endif

    try sortAndMergeKeyValueRanges null [|Range(0, 0), 0; Range(0, 0), 1|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try sortAndMergeKeyValueRanges null [|Range(0, 0), 0; Range(1, 2), 1; Range(2, 2), 2|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try sortAndMergeKeyValueRanges null [|Range(int32Min, int32Max), 0; Range(0, 0), 1|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

let testMergeSortedKeyLabelRanges() =
    let l, l2 = testLabel1, testLabel2
    mergeSortedKeyLabelRanges [||] [||] |> Equal ([||], [||])
    mergeSortedKeyLabelRanges [|1|] [|l|] |> Equal ([|Range(1,1)|], [|l|])
    mergeSortedKeyLabelRanges [|1;2;3|] [|l;l;l|] |> Equal ([|Range(1,3)|], [|l|])
    mergeSortedKeyLabelRanges [|1;2;3|] [|l;l2;l|] |> Equal ([|Range(1,1);Range(2,2);Range(3,3)|], [|l;l2;l|])
    mergeSortedKeyLabelRanges [|1;2;3;5|] [|l;l;l;l|] |> Equal ([|Range(1,3); Range(5,5)|], [|l;l|])
    mergeSortedKeyLabelRanges [|1;2;3;5;6|] [|l;l;l;l;l|] |> Equal ([|Range(1,3); Range(5,6)|], [|l;l|])
    mergeSortedKeyLabelRanges [|1;2;3;5;6|] [|l;l2;l2;l;l|] |> Equal ([|Range(1,1); Range(2,3); Range(5,6)|], [|l;l2;l|])
    mergeSortedKeyLabelRanges [|1;3;5|] [|l;l;l|] |> Equal ([|Range(1, 1); Range(3, 3); Range(5,5)|], [|l;l;l|])
    mergeSortedKeyLabelRanges [|int32Max - 1; int32Max|] [|l;l|] |> Equal ([|Range(int32Max - 1, int32Max)|], [|l|])

    try mergeSortedKeyLabelRanges [||] [|l|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try mergeSortedKeyLabelRanges [|1; 1|] [|l; l|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

    try mergeSortedKeyLabelRanges [|1; 0|] [|l; l|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

let testCollectSortAndMergeRanges() =
    let cmp = System.Collections.Generic.EqualityComparer<int>.Default
    let rand = new System.Random(1234)

    // check all possible subsets of {1,2,...,N}
    let N = 8
    let set = new ResizeArray<_>(N)
    for n = 0 to (1 <<< N) - 1 do
        set.Clear()

        let mutable b = n
        let mutable i = 1
        while b <> 0 do
            if (b &&& 1) <> 0 then set.Add(i)
            b <- b >>> 1
            i <- i + 1
        let keys = set.ToArray()
        let ranges = keys |> Array.map (fun k -> Range(k, k))

        let mergedRanges1 = collectSortAndMergeRanges keys
        let mergedRanges2 = sortAndMergeRanges false ranges
        mergedRanges1 |> Equal mergedRanges2

        let mergedRanges1b, rangeValues = collectSortAndMergeKeyValueRanges
                                              cmp
                                              (keys |> Array.map (fun k -> k,0))
        mergedRanges1b |> Equal mergedRanges2
        rangeValues.Length |> Equal mergedRanges2.Length
        rangeValues |> Array.forall ((=) 0) |> True

        if n <> 0 then
            set.Add(keys.[rand.Next(keys.Length)])
            set.Add(keys.[rand.Next(keys.Length)])
            let keys2 = set.ToArray()
            shuffleArray rand keys2
            let mergedRanges3 = collectSortAndMergeRanges keys2
            mergedRanges3 |> Equal mergedRanges2

    collectSortAndMergeRanges [|int32Max; int32Min|] |> Equal [|Range(int32Min, int32Min); Range(int32Max, int32Max)|]
    collectSortAndMergeRanges [|int32Max - 1; int32Max; int32Min; int32Min + 1|] |> Equal [|Range(int32Min, int32Min + 1); Range(int32Max - 1, int32Max)|]

    collectSortAndMergeKeyValueRanges null [|2,0; 1,0|] |> Equal ([|Range(1, 1); Range(2, 2)|], [|0; 0|])

    try collectSortAndMergeKeyValueRanges null [|1,1;2,2;1,3|] |> ignore
        Fail()
    with :? System.ArgumentException -> ()

let testSumsOfLengths() =
    sumOfLengths [|Range(1,1)|] 0 1 |> Equal 1.
    sumOfLengths [|Range(-1,1); Range(2,2)|] 0 2 |> Equal 4.
    sumOfLengths [|Range(int32Min, int32Max)|] 0 1 |> Equal (double int32Max - double int32Min + 1.)
    sumOfLengths [|Range(int32Min, int32Max); Range(int32Min, 0); Range(1, int32Max); Range(int32Min, int32Max);|] 1 3 |> Equal (double int32Max - double int32Min + 1.)

    sumOfCappedLengths 1 [|Range(1,1)|] 0 1 |> Equal 1.
    sumOfCappedLengths 3 [|Range(-1,1); Range(2,2)|] 0 2 |> Equal 4.
    sumOfCappedLengths 1 [|Range(-1,1); Range(2,2)|] 0 2 |> Equal 2.

    sumOfCappedLengths int32Max [|Range(int32Min, int32Max)|] 0 1 |> Equal (double int32Max)
    sumOfCappedLengths 1 [|Range(int32Min, int32Max)|] 0 1 |> Equal 1.
    sumOfCappedLengths int32Max [|Range(int32Min, int32Max); Range(int32Min, 0); Range(1, int32Max); Range(int32Min, int32Max);|] 1 3 |> Equal (double int32Max + double int32Max)
    sumOfCappedLengths int32Max [|Range(int32Min, -2); Range(-1,0); Range(1, int32Max)|] 0 3 |> Equal (double int32Max - double int32Min + 1.)

let testFindPivot() =
    findPivot [|Range(1,1)|] 0 1 |> Equal (0, false)
    findPivot [|Range(1,1); Range(3,3)|] 0 2 |> Equal (0, true)
    findPivot [|Range(1,1); Range(3,3)|] 1 2 |> Equal (1, false)
    findPivot [|Range(1,1); Range(3,4)|] 0 2|> Equal (1, false)
    findPivot [|Range(0,1); Range(2,3); Range(5,5)|] 0 3 |> Equal (1, true)
    findPivot [|Range(0,1); Range(2,3); Range(5,5); Range(8, 10)|] 0 4 |> Equal (2, true)
    findPivot [|Range(1, 1); Range(3,3); Range(5,5); Range(7,7)|] 0 4 |> Equal (1, true)
    findPivot [|Range(1, 1); Range(2,3); Range(5,6); Range(7,7)|] 0 4 |> Equal (1, true)
    findPivot [|Range(1, 1); Range(2,5); Range(6,8)|] 0 3 |> Equal (1, true)

let run() =
    testCheckRanges()
    testSortAndMergeRanges()
    testSortAndMergeKeyValueRanges()
    testMergeSortedKeyLabelRanges()
    testCollectSortAndMergeRanges()
    testSumsOfLengths()
    testFindPivot()

#endif