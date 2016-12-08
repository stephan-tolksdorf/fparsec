// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

namespace FParsec

#if LOW_TRUST
    // we don't need the Range code in LOW_TRUST builds
#else

type Range = struct
    val Min: int
    val Max: int
    new (min, max) = assert (min <= max)
                     {Min = min; Max = max}
end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Range =
    open System.Collections.Generic
    open FParsec.Internals

    let int32Max = System.Int32.MaxValue

    let createInvalidRangeException() =
        System.ArgumentException("A range passed as an argument is invalid.")

    let checkRangesAreValidSortedAndUnconnected (ranges: Range[]) =
        if ranges.Length <> 0 then
            let r = ranges.[0]
            if r.Min > r.Max then raise (createInvalidRangeException())
            let mutable prevMax = r.Max
            for i = 1 to ranges.Length - 1 do
                let r = ranges.[i]
                if r.Min > r.Max then raise (createInvalidRangeException())
                if prevMax = int32Max || prevMax + 1 >= r.Min then
                    invalidArg "ranges" "The ranges must be sorted and neither overlapping nor immediately adjacent."
                prevMax <- r.Max

    let checkLabelRangesAreValidSortedAndUnconnected (ranges: Range[]) (labels: System.Reflection.Emit.Label[]) =
        if ranges.Length <> labels.Length then
            invalidArg "labels" "The range and label arrays must have the same lengths."
        if ranges.Length <> 0 then
            let r = ranges.[0]
            if r.Min > r.Max then raise (createInvalidRangeException())
            let mutable prevMax = r.Max
            for i = 1 to ranges.Length - 1 do
                let r = ranges.[i]
                if r.Min > r.Max then raise (createInvalidRangeException())
                if prevMax = int32Max then
                    invalidArg "ranges" "The ranges must be sorted and non-overlapping."
                if prevMax + 1 >= r.Min then
                    if prevMax + 1 = r.Min then
                        if labels.[i - 1].Equals(labels.[i]) then
                            raise (System.ArgumentException("Ranges with the same associated label must not be immediately adjacent."))
                    else
                        invalidArg "ranges" "The ranges must be sorted and non-overlapping."
                prevMax <- r.Max

    let rangeComparer = {new Comparer<Range>() with
                             member t.Compare(r1, r2) = compare r1.Min r2.Min}

    let sortAndMergeRanges allowOverlappingRanges (ranges: Range[]) =
        if ranges.Length = 0 then [||]
        else
            System.Array.Sort(ranges, rangeComparer)
            let mutable connected = 0
            let r = ranges.[0]
            if r.Min > r.Max then raise (createInvalidRangeException())
            let mutable prevMax = r.Max
            for i = 1 to ranges.Length - 1 do
                let r = ranges.[i]
                if r.Min > r.Max then raise (createInvalidRangeException())
                if prevMax < r.Min then
                    if prevMax + 1 = r.Min then
                        connected <- connected + 1
                    prevMax <- r.Max
                elif allowOverlappingRanges then
                    connected <- connected + 1
                    if prevMax < r.Max then
                        prevMax <- r.Max
                else
                    invalidArg "ranges" "The value ranges must be non-overlapping."

            if connected = 0 then ranges
            else
                let rs = Array.zeroCreate (ranges.Length - connected)
                let mutable j = 0
                for r in ranges do
                    if j = 0 || prevMax <> int32Max && prevMax + 1 < r.Min then
                        prevMax <- r.Max
                        rs.[j] <- r
                        j <- j + 1
                    elif prevMax < r.Max then
                        prevMax <- r.Max
                        rs.[j - 1] <- Range(rs.[j - 1].Min, r.Max)
                rs

    /// If the comparer is not null, adjacent ranges with the same value are merged.
    let sortAndMergeKeyValueRanges (cmp: EqualityComparer<'T>) (keyValueRanges: seq<Range*'T>) =
        // 'T could potentially be a large value type,
        // so we are trying to avoid copying 'T values where possible.
        let rvs = Array.ofSeq keyValueRanges
        if rvs.Length = 0 then [||], [||]
        else
            System.Array.Sort(rvs, {new Comparer<Range*'T>() with
                                        member t.Compare((r1, _), (r2, _)) = compare r1.Min r2.Min})
            let mutable connected = 0
            let (r, _) as rv = rvs.[0]
            if r.Min > r.Max then raise (createInvalidRangeException())
            let mutable prevMax = r.Max
            let mutable prevRV = rv
            for i = 1 to rvs.Length - 1 do
                let (r, _) as rv = rvs.[i]
                if r.Min > r.Max then raise (createInvalidRangeException())
                if prevMax >= r.Min then
                    invalidArg "keyValueRanges" "The ranges must be non-overlapping."
                if prevMax + 1 = r.Min && isNotNull cmp && cmp.Equals(snd prevRV, snd rv) then
                    connected <- connected + 1
                prevMax <- r.Max
                prevRV <- rv
            let n = rvs.Length - connected
            let rs, vs = Array.zeroCreate n, Array.zeroCreate n
            if connected = 0 then
                for i = 0 to rvs.Length - 1 do
                    let rv = rvs.[i]
                    rs.[i] <- fst rv
                    vs.[i] <- snd rv
            else
                let mutable j = 0
                for ((r, _) as rv) in rvs do
                    if j = 0 || not (prevMax + 1 = r.Min && cmp.Equals(snd prevRV, snd rv)) then
                        rs.[j] <- r
                        vs.[j] <- snd rv
                        j <- j + 1
                    else
                        rs.[j - 1] <- Range(rs.[j - 1].Min, r.Max)
                    prevMax <- r.Max
                    prevRV <- rv
            rs, vs

    let mergeSortedKeyLabelRanges (keys: int[]) (labels: System.Reflection.Emit.Label[])  =
        if keys.Length <> labels.Length then
            invalidArg "keys" "The key and label arrays must have the same lengths."
        if keys.Length = 0 then [||], [||]
        else
            let mutable prevKey = keys.[0]
            let mutable connected = 0
            for i = 1 to keys.Length - 1 do
                let key = keys.[i]
                if key <= prevKey then
                    invalidArg "keys" "The keys must be sorted and distinct."
                if key = prevKey + 1 && labels.[i] = labels.[i - 1] then
                    connected <- connected + 1
                prevKey <- key
            if connected = 0 then
                (keys |> Array.map (fun k -> Range(k, k))), labels
            else
                let ranges    = Array.zeroCreate (keys.Length - connected)
                let newLabels = Array.zeroCreate (keys.Length - connected)
                let mutable i = 0
                for j = 0 to ranges.Length - 1 do
                    let label = labels.[i]
                    newLabels.[j] <- label
                    let first = keys.[i]
                    let mutable last = first
                    i <- i + 1
                    while i < keys.Length && keys.[i] = last + 1
                                          && labels.[i] = label
                       do last <- last + 1
                          i <- i + 1
                    ranges.[j] <- Range(first, last)
                ranges, newLabels

    /// Duplicate values are allowed.
    let collectSortAndMergeRanges (values: seq<int>) =
        use iter = values.GetEnumerator()
        if not (iter.MoveNext()) then [||]
        else
            let ranges = ResizeArray<_>()
            let rec loop sorted min max =
                if iter.MoveNext() then
                    let k = iter.Current
                    if max <> int32Max && max + 1 = k then loop sorted min k
                    else
                        ranges.Add(Range(min, max))
                        loop (sorted && max < k) k k
                else
                    ranges.Add(Range(min, max))
                    sorted
            let value = iter.Current
            let sorted = loop true value value
            let ranges = ranges.ToArray()
            if sorted then ranges
            else sortAndMergeRanges true ranges

    /// ranges, values = collectSortAndMergeKeyValueRanges (cmp: EqualityComparer<'T>) (keyValues: seq<int*'T>)
    /// Duplicate keys are not allowed.
    /// If the comparer is not null, consecutive keys with the same value are combined.
    let collectSortAndMergeKeyValueRanges (cmp: EqualityComparer<'T>) (keyValues: seq<int*'T>)  =
        // 'T could potentially be a large value type,
        // so we are trying to avoid copying 'T values where possible.
        let kvs = Array.ofSeq keyValues
        System.Array.Sort(kvs, {new Comparer<int*'T>() with
                                    member t.Compare((k1, _), (k2,_)) = compare k1 k2})
        if kvs.Length = 0 then [||], [||]
        else
            let mutable prevKey, _ = kvs.[0]
            for i = 1 to kvs.Length - 1 do
                let k, _ = kvs.[i]
                if k = prevKey then
                    invalidArg "keyValues" "The sequence contains a duplicate key."
                prevKey <- k
            if isNull cmp then
                let ranges = Array.zeroCreate kvs.Length
                let values = Array.zeroCreate kvs.Length
                for i = 0 to kvs.Length - 1 do
                    let k, _ as kv = kvs.[i]
                    ranges.[i] <- Range(k, k)
                    values.[i] <- snd kv
                ranges, values
            else
                let ranges = ResizeArray<_>()
                let mutable kv = kvs.[0]
                let mutable i = 0
                while i < kvs.Length do
                    let kv0 = kv
                    let mutable k = fst kv
                    i <- i + 1
                    while i < kvs.Length && (kv <- kvs.[i]
                                             k + 1 = fst kv && cmp.Equals(snd kv0, snd kv))
                       do k <- k + 1
                          i <- i + 1
                    ranges.Add(Range(fst kv0, k))
                let ranges = ranges.ToArray()
                let values = Array.zeroCreate ranges.Length
                let mutable j = 0
                for i = 0 to ranges.Length - 1 do
                    let r = ranges.[i]
                    values.[i] <- snd kvs.[j]
                    j <- j + (r.Max - r.Min + 1)
                ranges, values

    /// sumOfLengths (ranges: Range[]) (iBegin: int) (iEnd: int)
    /// precondition: iBegin < iEnd, ranges must be sorted and non-overlapping
    let sumOfLengths (ranges: Range[]) iBegin iEnd =
        assert (iBegin < iEnd)
        // since the ranges are sorted non-overlapping, their sum is <= UInt32.MaxValue + 1
        let mutable n = uint32 (iEnd - iBegin)
        for i = iBegin to iEnd - 1 do
            let r = ranges.[i]
            n <- n + uint32 (r.Max - r.Min)
        if n <> 0u then double n
        else double System.UInt32.MaxValue + 1. // n has overflown by exactly 1

    /// sumOfCappedLengths (lengthCap: int32) (ranges: Range[]) (iBegin: int) (iEnd: int)
    /// precondition: iBegin < iEnd, ranges must be sorted and non-overlapping
    /// a lengthCap <= 0 is interpreted as a lengthCap of 2^32
    let sumOfCappedLengths lengthCap (ranges: Range[]) iBegin iEnd =
        assert (iBegin < iEnd)
        // since the ranges are sorted non-overlapping, their sum is <= UInt32.MaxValue + 1
        let lengthCapM1 = if lengthCap > 0 then uint32 (lengthCap - 1) else System.UInt32.MaxValue
        let mutable n = uint32 (iEnd - iBegin)
        for i = iBegin to iEnd - 1 do
            let r = ranges.[i]
            n <- n + min (uint32 (r.Max - r.Min)) lengthCapM1
        if n <> 0u then double n
        else double System.UInt32.MaxValue + 1. // n has overflown by exactly 1

    /// density lengthCap (ranges: Range[]) iBegin iEnd
    /// precondition: iBegin < iEnd, ranges must be sorted and non-overlapping
    let density lengthCap (ranges: Range[]) iBegin iEnd =
        assert (iBegin < iEnd)
        let n = sumOfCappedLengths lengthCap ranges iBegin iEnd
        let d = double ranges.[iEnd - 1].Max - double ranges.[iBegin].Min + 1.
        n/d

    /// rangeIndex, pivotAroundRangeMax = findPivot (ranges: Range[]) iBegin iEnd
    /// precondition: iBegin < iEnd, ranges must be sorted and non-overlapping
    let findPivot (ranges: Range[]) iBegin iEnd =
        assert (iBegin < iEnd)
        // the pivot heuristic is based on Korobeynikov (2007), http://llvm.org/pubs/2007-05-31-Switch-Lowering.pdf
        let mutable first, last = double ranges.[iBegin].Min, double ranges.[iEnd - 1].Max
        let mutable pivot, pivotAroundPreviousRangeMax = iBegin, false
        let mutable sumLeft, sumRight = 0., sumOfLengths ranges iBegin iEnd
        let sumHalf = sumRight*0.5
        let mutable maxQuality, maxDistanceToMiddle = -1., sumRight
        let r = ranges.[iBegin]
        let mutable nextMin, nextMax = double r.Min, double r.Max
        for i = iBegin + 1 to iEnd - 1 do
            let prevMax = nextMax
            let prevLength = nextMax - nextMin + 1.
            sumLeft  <- sumLeft  + prevLength
            sumRight <- sumRight - prevLength
            let r = ranges.[i]
            nextMin <- double r.Min
            nextMax <- double r.Max
            let logDistance = System.Math.Log(nextMin - prevMax)
            let leftDensity  = sumLeft/(prevMax - first + 2.) // add 2 instead of 1 to decrease the quality of
            let rightDensity = sumRight/(last - nextMin + 2.) // of the two most extreme possible pivot points
            let quality = (leftDensity + rightDensity)*logDistance
            if quality >= maxQuality then
                let distanceToMiddle = System.Math.Abs(sumLeft - sumHalf);
                if quality > maxQuality || distanceToMiddle < maxDistanceToMiddle then
                    maxQuality <- quality
                    maxDistanceToMiddle <- distanceToMiddle
                    pivot <- i
                    pivotAroundPreviousRangeMax <- sumLeft >= sumRight
        if pivotAroundPreviousRangeMax then
            (pivot - 1), true
        else
            pivot, false

    let rec findInSortedNonOverlappingRanges (ranges: Range[]) value =
        let rec loop iFirst iLast =
            if iFirst <= iLast then
                let middle = int ((uint32 (iFirst + iLast))/2u)
                let middleRange = ranges.[middle]
                if value < middleRange.Min then loop iFirst (middle - 1)
                elif value > middleRange.Max then loop (middle + 1) iLast
                else middle
            else ~~~iFirst
        loop 0 (ranges.Length - 1)

#endif

