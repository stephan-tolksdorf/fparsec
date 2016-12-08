// Copyright (c) Stephan Tolksdorf 2010-2012
// License: Simplified BSD License. See accompanying documentation.

module FParsec.StaticMapping

#if LOW_TRUST
#else
open System.Reflection
open System.Reflection.Emit
open System.Runtime.Serialization
open System.Diagnostics
open System.Collections.Generic
open System.Threading

open FParsec
open FParsec.Internals
open FParsec.Range
open FParsec.Emit

/// Unsafe because it doesn't constrain the type argument to reference types.
let private UnsafeReferenceEqualityComparer<'T> =
    { new EqualityComparer<'T>() with
         override t.Equals(x, y) = obj.ReferenceEquals(x, y)
         override t.GetHashCode(x) = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
    }

type PhysicalEqualityComparer<'T> private () =
    static let instanceOrNull =
        let t = typeof<'T>
        if not t.IsValueType then
            UnsafeReferenceEqualityComparer<'T>
        elif t.IsEnum || typeof<System.IEquatable<'T>>.IsAssignableFrom(t) then
            EqualityComparer<'T>.Default
        else
            null

    static member InstanceOrNull = instanceOrNull

let mutable private staticMappingCounter = 0

let private createStaticMappingTypeBuilder<'TIn,'TOut>() =
    let name = "StaticMapping" + (string (Interlocked.Increment(&staticMappingCounter)))
    let tb = createTypeBuilder
                 name
                 (TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Class)
                 typeof<Microsoft.FSharp.Core.FSharpFunc<'TIn,'TOut>> null
    let mb = tb.DefineMethod("Invoke",
                             MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual,
                             CallingConventions.HasThis,
                             typeof<'TOut>, [|typeof<'TIn>|])
    tb, mb.GetILGenerator()

let createStaticMappingAssertException() =
    System.Exception("An internal assert check in FParsec.StaticMapping failed. Please report this error to fparsec@quanttec.com. (The Data member of the exception object contains the information needed to reproduce the error.)")

let internal defaultMappingLengthCap = 32
let internal defaultMappingDensityThreshold = 0.4
let internal defaultIndicatorLengthCap = 32*8
let internal defaultIndicatorDensityThreshold = 0.4/32.

let internal createStaticIntIndicatorFunctionImpl<'TInt when 'TInt : struct>
                 lengthCap densityThreshold minValue maxValue invert ranges : ('TInt -> bool) =

    if not (typeof<'TInt> = typeof<int> || typeof<'TInt> = typeof<char>) then
        failwith "Only char and int are supported as input types."

    let tb, ilg = createStaticMappingTypeBuilder<'TInt, bool>()

    let resultLocal = ilg.DeclareLocal(typeof<bool>) // local 0
    emitSetMembershipTest ilg
                          (fun ilg -> ilg.Emit(OpCodes.Ldarg_1)) // loads var
                          (fun ilg -> ilg.Emit(OpCodes.Stloc_0)) // stores result
                          (TempLocals(ilg))
                          lengthCap densityThreshold
                          minValue maxValue
                          invert ranges
    ilg.Emit(OpCodes.Ldloc_0)
    ilg.Emit(OpCodes.Ret)

    let t = tb.CreateType()
    let indicator = FormatterServices.GetUninitializedObject(t) :?> ('TInt -> bool)

#if DEBUG_STATIC_MAPPING
    // saveEmitAssembly "FParsec.Emitted.dll"

    let raiseException key : unit =
        let e = createStaticMappingAssertException()
        e.Data.["Argument"]   <- key
        e.Data.["IsInverted"] <- invert
        e.Data.["Ranges"]     <- ranges
        raise e

    let findKeyinRanges =
        (if typeof<'TInt> = typeof<char> then
             (box (fun (key: char) -> findInSortedNonOverlappingRanges ranges (int key)))
         else
             (box (findInSortedNonOverlappingRanges ranges))
        ) :?> ('TInt -> int)

    fun key ->
        let b1 = indicator key
        let b2_ = findKeyinRanges key >= 0
        let b2 = if invert then not b2_ else b2_
        if b1 <> b2 then raiseException key
        b1
#else
    indicator
#endif

let createStaticCharIndicatorFunction invert (charsInSet: seq<char>) =
    let ranges = collectSortAndMergeRanges (charsInSet |> Seq.map (fun c -> int c))
    createStaticIntIndicatorFunctionImpl<char>
        defaultIndicatorLengthCap defaultIndicatorDensityThreshold
        0 0xffff
        invert ranges

let createStaticCharRangeIndicatorFunction invert (rangesInSet: seq<Range>) =
    let ranges = sortAndMergeRanges true (Array.ofSeq rangesInSet)
    if ranges.Length <> 0 && ranges.[0].Min < 0 || ranges.[ranges.Length - 1].Max > 0xffff then
        invalidArg "charRanges" "A range contains values outside the range of valid UTF-16 char values (0 - 0xffff)."
    createStaticIntIndicatorFunctionImpl<char>
        defaultIndicatorLengthCap defaultIndicatorDensityThreshold
        0 0xffff
        invert ranges

let createStaticIntIndicatorFunction invert (valuesInSet: seq<int>) =
    let ranges = collectSortAndMergeRanges valuesInSet
    createStaticIntIndicatorFunctionImpl<int>
        defaultIndicatorLengthCap defaultIndicatorDensityThreshold
        System.Int32.MinValue System.Int32.MaxValue
        invert ranges

let createStaticIntRangeIndicatorFunction invert (rangesInSet: seq<Range>) =
    let ranges = sortAndMergeRanges true (Array.ofSeq rangesInSet)
    createStaticIntIndicatorFunctionImpl<int>
        defaultIndicatorLengthCap defaultIndicatorDensityThreshold
        System.Int32.MinValue System.Int32.MaxValue
        invert ranges


let internal createStaticIntMappingImpl
                 lengthCap densityThreshold
                 minKey maxKey
                 (defaultValue: 'T) (ranges: Range[]) (values: 'T[]) : (int -> 'T) =
    assert (ranges.Length = values.Length)

    if ranges.Length = 0 then fun _ -> defaultValue
    else
        let physicalEqualityComparer = PhysicalEqualityComparer<'T>.InstanceOrNull
        let T = typeof<'T>
        if T = typeof<bool> then
            let values = box values :?> bool[]
            let defaultValue = box defaultValue :?> bool
            box (createStaticIntIndicatorFunctionImpl
                    (lengthCap*(defaultIndicatorLengthCap/defaultMappingLengthCap))
                    (densityThreshold*(defaultIndicatorDensityThreshold/defaultMappingDensityThreshold))
                    minKey maxKey
                    defaultValue ranges) :?> (int -> 'T)
        else
            let tb, ilg = createStaticMappingTypeBuilder<int, 'T>()

            let isPrimitive = T.IsPrimitive || T.IsEnum
            let loadConstant = if isPrimitive then createLoaderForPrimitiveConstants ilg
                               else Unchecked.defaultof<_>
            // local 0
            let resultOrIndexLocal = ilg.DeclareLocal(if isPrimitive then T else typeof<int>)

            let defaultLabel = ilg.DefineLabel()
            let returnLabel = ilg.DefineLabel()

            let labels = Array.zeroCreate ranges.Length

            let mutable needToEmit = null
            let mutable needToEmitCount = 0
            let physicalEqualityComparer = PhysicalEqualityComparer<'T>.InstanceOrNull

            if isNull physicalEqualityComparer then
                for i = 0 to labels.Length - 1 do
                    labels.[i] <- ilg.DefineLabel()
            else
                // we don't need to emit multiple case handlers for identical values
                needToEmit <- Array.zeroCreate values.Length
                let valueLabels = Dictionary<'T,Label>(values.Length, physicalEqualityComparer)
                for i = 0 to values.Length - 1 do
                    let value = values.[i]
                    let mutable label = Unchecked.defaultof<_>
                    if not (valueLabels.TryGetValue(value, &label)) then
                        needToEmit.[i] <- true
                        label <- ilg.DefineLabel()
                        valueLabels.Add(value, label)
                    labels.[i] <- label
                needToEmitCount <- valueLabels.Count
                if needToEmitCount = values.Length then
                    needToEmit <- null

            emitSwitch ilg
                       (fun ilg -> ilg.Emit(OpCodes.Ldarg_1)) // loads key
                       (TempLocals(ilg))
                       lengthCap densityThreshold
                       minKey maxKey
                       defaultLabel ranges labels

            let returnedValues = if isPrimitive || isNull needToEmit then null
                                 else Array.zeroCreate needToEmitCount
            let mutable returnedValuesCount = 0

            for i = 0 to labels.Length - 1 do
                if isNull needToEmit || needToEmit.[i] then
                    ilg.MarkLabel(labels.[i])
                    if isPrimitive then
                        loadConstant (values.[i])
                    else
                        if isNotNull returnedValues then
                            returnedValues.[returnedValuesCount] <- values.[i]
                        loadI4 ilg returnedValuesCount
                        returnedValuesCount <- returnedValuesCount + 1
                    ilg.Emit(OpCodes.Stloc_0)
                    ilg.Emit(OpCodes.Br, returnLabel)

            // return default value
            let defaultValueIsNull = not T.IsValueType && isNull (box defaultValue)
            ilg.MarkLabel(defaultLabel)
            if isPrimitive then
                loadConstant defaultValue
                ilg.Emit(OpCodes.Stloc_0)
            else
                if defaultValueIsNull then
                    ilg.Emit(OpCodes.Ldnull)
                else
                    ilg.Emit(OpCodes.Ldarg_0)
                    ilg.Emit(OpCodes.Ldfld, tb.DefineField("DefaultValue", T, FieldAttributes.Public))
                ilg.Emit(OpCodes.Ret)

            // return result
            ilg.MarkLabel(returnLabel)
            if isPrimitive then
                ilg.Emit(OpCodes.Ldloc_0)
            else
                // We could store all the values in individual fields to avoid the bounds check
                // and indirect load, but that probably wouldn't be worth the additional
                // code generation (and garbage collection?) costs (except for tiny mappings).
                ilg.Emit(OpCodes.Ldarg_0)
                ilg.Emit(OpCodes.Ldfld, tb.DefineField("Values", values.GetType(), FieldAttributes.Public))
                ilg.Emit(OpCodes.Ldloc_0)
                ilg.Emit(OpCodes.Ldelem, T)
            ilg.Emit(OpCodes.Ret)

            let t = tb.CreateType()
            let mapping = FormatterServices.GetUninitializedObject(t) :?> (int -> 'T)
            if not isPrimitive then
                // we can't use the previously used Fieldbuilders here, because SetValue is not implemented in FieldBuilders
                if not defaultValueIsNull then t.GetField("DefaultValue").SetValue(mapping, defaultValue)
                t.GetField("Values").SetValue(mapping, if isNotNull returnedValues then returnedValues else values)

        #if DEBUG_STATIC_MAPPING
            //saveEmitAssembly "FParsec.Emitted.dll"

            if isNull physicalEqualityComparer then mapping
            else
                let raiseException key : unit =
                    let e = createStaticMappingAssertException()
                    e.Data.["Argument"]     <- key
                    e.Data.["Ranges"]       <- ranges
                    e.Data.["Values"]       <- values
                    e.Data.["DefaultValue"] <- defaultValue
                    raise e

                fun key ->
                    let value = mapping key
                    let index = findInSortedNonOverlappingRanges ranges key
                    if index >= 0 then
                        if not (physicalEqualityComparer.Equals(value, values.[index])) then raiseException key
                    else
                        if not (physicalEqualityComparer.Equals(value, defaultValue)) then raiseException key
                    value
        #else
            mapping
        #endif


let internal filterOutDefaultValueRanges (comparer: EqualityComparer<_>) (ranges: Range[]) (values: _[]) defaultValue =
    if isNull comparer then ranges, values
    else
        let mutable n = 0
        for v in values do
            if comparer.Equals(v, defaultValue) then n <- n + 1
        if n = 0 then ranges, values
        else
            let N = values.Length - n
            let newRanges, newValues = Array.zeroCreate N, Array.zeroCreate N
            let mutable j = 0
            for i = 0 to values.Length - 1 do
                let v = values.[i]
                if not (comparer.Equals(v, defaultValue)) then
                    newValues.[j] <- v
                    newRanges.[j] <- ranges.[i]
                    j <- j + 1
            newRanges, newValues

// we need to use #seq instead of seq here to prevent the F# compiler
// from unnecessarily wrapping the returned function value

let createStaticIntMapping (defaultValue: 'T) (keyValues: #seq<int*'T>) =
    let valueComparer = PhysicalEqualityComparer<'T>.InstanceOrNull
    let ranges, values = collectSortAndMergeKeyValueRanges valueComparer keyValues
    let ranges, values = filterOutDefaultValueRanges valueComparer ranges values defaultValue
    createStaticIntMappingImpl
        defaultMappingLengthCap defaultMappingDensityThreshold
        System.Int32.MinValue System.Int32.MaxValue
        defaultValue ranges values

let createStaticIntRangeMapping (defaultValue: 'T) (keyValues: #seq<Range*'T>) =
    let valueComparer = PhysicalEqualityComparer<'T>.InstanceOrNull
    let ranges, values = sortAndMergeKeyValueRanges valueComparer keyValues
    let ranges, values = filterOutDefaultValueRanges valueComparer ranges values defaultValue
    createStaticIntMappingImpl
        defaultMappingLengthCap defaultMappingDensityThreshold
        System.Int32.MinValue System.Int32.MaxValue
        defaultValue ranges values

type private IntType = U2
                     | U4
                     | U8

[<NoEquality; NoComparison>]
type Subtree(stringIndex: int, index: int, count: int) = struct
    member t.StringIndex = stringIndex
    member t.Index = index
    member t.Count = count // must be greater 0
end

type SubtreeEqualityComparer<'T>(stringValues: (string*'T)[], valueComparer: EqualityComparer<'T>) =
    inherit EqualityComparer<Subtree>()

    override t.Equals(subtree1: Subtree, subtree2: Subtree) =
        let aligned = subtree1.StringIndex%2 = subtree2.StringIndex%2 // our string comparison code assumes an identical 4-byte-alignment
        let count = subtree1.Count
        count = subtree2.Count
        && (let mutable i = 0
            while uint32 i < uint32 count do
                let string1, value1 = stringValues.[subtree1.Index + i]
                let string2, value2 = stringValues.[subtree2.Index + i]
                let remaining = string1.Length - subtree1.StringIndex
                if  remaining = string2.Length - subtree2.StringIndex
                    && (aligned || remaining <= 1)
                    && valueComparer.Equals(value1, value2)
                    && System.String.CompareOrdinal(string1, subtree1.StringIndex,
                                                    string2, subtree2.StringIndex, remaining) = 0
                then i <- i + 1
                else i <- System.Int32.MinValue // break
            i = count)

    override t.GetHashCode(subtree: Subtree) =
        subtree.Count ^^^ valueComparer.GetHashCode(snd stringValues.[subtree.Index])

let createStaticStringMapping (defaultValue: 'T) (keyValues: #seq<string*'T>) : (string -> 'T) =
    let T = typeof<'T>

    let physicalEqualityComparer = PhysicalEqualityComparer<'T>.InstanceOrNull

    let kvs = Array.ofSeq keyValues
    System.Array.Sort(kvs, {new Comparer<string*'T>() with
                                member t.Compare((k1, _), (k2, _)) = System.String.CompareOrdinal(k1, k2)})

    let mutable previousKey = null
    for (key, _) in kvs do
        if isNull key then invalidArg "keyValues" "The string keys must not be null."
        if key = previousKey then invalidArg "keyValues" "The strings keys must be different."
        previousKey <- key

    match kvs.Length with
    | 0 -> fun str ->
               let throwIfStringIsNull = str.Length
               defaultValue
    | 1 -> let key, value = kvs.[0]
           fun str ->
               let throwIfStringIsNull = str.Length
               if str = key then value else defaultValue
    | _ ->
        let mutable i0 = if fst kvs.[0] = "" then 1 else 0

        let getMinMaxLength iBegin iEnd =
            assert (iBegin < iEnd)
            let firstKey, _ = kvs.[iBegin]
            let mutable minLength = firstKey.Length
            let mutable maxLength = minLength
            for i = iBegin + 1 to iEnd - 1 do
                let key, _ = kvs.[i]
                let length = key.Length
                minLength <- min length minLength
                maxLength <- max length maxLength
            minLength, maxLength

        let minLength, maxLength = getMinMaxLength i0 kvs.Length

        let findIndexOfFirstCharAfterCommonPrefix startIndex iBegin iEnd minKeyLength =
            let rec loop index =
                if index = minKeyLength then index
                else
                    let c = (fst kvs.[iBegin]).[index]
                    let rec keysEqualAtX i =
                        if i = iEnd then true
                        elif (fst kvs.[i]).[index] <> c then false
                        else keysEqualAtX (i + 1)
                    if not (keysEqualAtX (iBegin + 1)) then index
                    else loop (index + 1)
            loop startIndex

        let prefixLength = findIndexOfFirstCharAfterCommonPrefix 0 i0 kvs.Length minLength

        // sort by first char after common prefix, then by length, then lexicographical
        System.Array.Sort(kvs, {new Comparer<string*'T>() with
                                    member t.Compare((k1, _), (k2, _)) =
                                        if k1.Length > prefixLength && k2.Length > prefixLength then
                                            let d = int k1.[prefixLength] - int k2.[prefixLength]
                                            if d <> 0 then d
                                            else
                                                let d = k1.Length - k2.Length
                                                if d <> 0 then d
                                                else System.String.CompareOrdinal(k1, k2)
                                        else
                                            k1.Length - k2.Length})

        let tb, ilg = createStaticMappingTypeBuilder<string, 'T>()

        let isPrimitive = T.IsPrimitive || T.IsEnum

        let physicalEqualityComparer = PhysicalEqualityComparer<'T>.InstanceOrNull
        let loadConstant = if isPrimitive then createLoaderForPrimitiveConstants ilg
                            else Unchecked.defaultof<_>

        let lengthLocal = ilg.DeclareLocal(typeof<int>)
        let loadLength() = ilg.Emit(OpCodes.Ldloc_0)
        let storeLength() = ilg.Emit(OpCodes.Stloc_0)

        let charPointerType = typeof<char>.MakePointerType()
        let charPointerLocal = ilg.DeclareLocal(charPointerType)
        let loadPtr() = ilg.Emit(OpCodes.Ldloc_1)
        let storePtr() = ilg.Emit(OpCodes.Stloc_1)

        // Declaring the following local as int instead of char improves
        // code generation on the 64-bit JIT.
        let chLocal = ilg.DeclareLocal(typeof<int (*char*)>)
        let loadCh = fun (_: ILGenerator) -> ilg.Emit(OpCodes.Ldloc_2)
        let storeCh() = ilg.Emit(OpCodes.Stloc_2)

        let resultOrIndexLocal = ilg.DeclareLocal(if isPrimitive then T else typeof<int>)
        let loadResult() = ilg.Emit(OpCodes.Ldloc_3)
        let storeResult() = ilg.Emit(OpCodes.Stloc_3)

        let stringLocal = ilg.DeclareLocal(typeof<string>, true) // pinned string
        let storeString() = ilg.Emit(OpCodes.Stloc_S, 4uy)

        // set up local variables
        ilg.Emit(OpCodes.Ldarg_1) // load string argument
        ilg.Emit(OpCodes.Dup)
        ilg.Emit(OpCodes.Dup)
        storeString() // pins string
        // accessing .Length triggers null reference exception if string is null
        ilg.EmitCall(OpCodes.Call, typeof<string>.GetMethod("get_Length"), null)
        storeLength()
        ilg.Emit(OpCodes.Conv_I)
        ilg.EmitCall(OpCodes.Call, typeof<System.Runtime.CompilerServices.RuntimeHelpers>.GetMethod("get_OffsetToStringData"), null)
        ilg.Emit(OpCodes.Add)
        storePtr()

        let defaultLabel = ilg.DefineLabel()
        let returnLabel = ilg.DefineLabel()

        // some helper functions

        let dereferenceAndIncrementPtr intType doIncrement =
            loadPtr()
            if doIncrement then
                ilg.Emit(OpCodes.Dup)
                loadI4 ilg (match intType with
                            | U2 -> 1*sizeof<char>
                            | U4 -> 2*sizeof<char>
                            | U8 -> 4*sizeof<char>)
                ilg.Emit(OpCodes.Add)
                storePtr()
            match intType with
            | U2 -> ilg.Emit(OpCodes.Ldind_U2)
            | U4 -> ilg.Emit(OpCodes.Ldind_U4)
            | U8 -> ilg.Emit(OpCodes.Ldind_I8)

        let incrementPtrByNumberOfChars i =
            loadPtr()
            loadI4 ilg (i*sizeof<char>)
            ilg.Emit(OpCodes.Add)
            storePtr()

        let returnedValueIndices = if isPrimitive then null else ResizeArray<_>(kvs.Length)
        let returnValue i =
            if isPrimitive then
                loadConstant (snd kvs.[i])
            else
                loadI4 ilg (returnedValueIndices.Count)
                returnedValueIndices.Add(i)
            storeResult()
            ilg.Emit(OpCodes.Br, returnLabel)

        let longKeyData = ref (new ResizeArray<_>(), null, null, null)

        /// Emit a call to FParsec.Buffer.Equal helper function to compare
        /// a long segment of the input string.
        let emitLongStringComparison dataIndex dataLength isFinal =
            let data, fieldBuilder, methodInfo, pinnedDataLocal = !longKeyData
            let mutable f, m, pdl = fieldBuilder, methodInfo, pinnedDataLocal
            if isNull f then
                f <- tb.DefineField("longKeyData", typeof<uint32[]>, FieldAttributes.Public)
                let ptrType = typeof<uint32>.MakePointerType()
                m <- typeof<FParsec.Buffer>.GetMethod("Equals", [|ptrType; ptrType; typeof<uint32>|])
                pdl <- ilg.DeclareLocal(typeof<uint32[]>, true)
                longKeyData:= (data, f, m, pdl)

            ilg.Emit(OpCodes.Ldarg_0)
            ilg.Emit(OpCodes.Ldfld, f)
            ilg.Emit(OpCodes.Dup)
            ilg.Emit(OpCodes.Stloc_S, pdl) // pin data array
            loadI4 ilg dataIndex
            ilg.Emit(OpCodes.Ldelema, typeof<uint32>)
            ilg.Emit(OpCodes.Conv_I)

            loadPtr()
            if not isFinal then
                incrementPtrByNumberOfChars (dataLength*2)
            loadI4 ilg dataLength
            ilg.EmitCall(OpCodes.Call, m, null)
            ilg.Emit(OpCodes.Ldnull)
            ilg.Emit(OpCodes.Stloc_S, pdl) // unpin data array
            ilg.Emit(OpCodes.Brfalse, defaultLabel)

        let emitStringComparison (key: string) idx length isFinal =
            if length > 0 then
                let mutable idx, length = idx, length
                if idx%2 = 1 then
                    // align ptr to 4-byte boundary
                    // (this assumes that the first char in a string is aligned)
                    dereferenceAndIncrementPtr U2 (not isFinal || length > 1)
                    loadI4 ilg (int key.[idx])
                    ilg.Emit(OpCodes.Bne_Un, defaultLabel)
                    idx <- idx + 1
                    length <- length - 1

                if length > sizeof<unativeint>*4 then
                    // store string data into longStringData
                    let data, _, _, _ = !longKeyData
                    let dataIndex = data.Count
                    while length >= 2 do
                        // if necessary we will swap the byte order of the whole data array
                        // when we assign it to the longKeyData field
                        let v = uint32 key.[idx] ||| (uint32 key.[idx + 1] <<< 16)
                        data.Add(v)
                        idx <- idx + 2
                        length <- length - 2
                    if isFinal && length = 1 then
                        data.Add(uint32 key.[idx])
                        length <- 0
                    // emit call to string comparison function
                    emitLongStringComparison dataIndex (data.Count - dataIndex) isFinal
                else
                #if UNALIGNED_READS
                    if sizeof<unativeint> = 8 then
                        while length >= 4 || (isFinal && length = 3) do
                            dereferenceAndIncrementPtr U8 (not isFinal || length > 4)
                            let v =    (uint64 key.[idx]           )
                                   ||| (uint64 key.[idx + 1] <<< 16)
                                   ||| (uint64 key.[idx + 2] <<< 32)
                                   ||| (if length > 3 then uint64 key.[idx + 3] <<< 48 else 0UL)
                            let v = if System.BitConverter.IsLittleEndian then v
                                    else Buffer.SwapByteOrder(v)
                            loadU8 ilg v
                            ilg.Emit(OpCodes.Bne_Un, defaultLabel)
                            idx <- idx + 4
                            length <- length - 4
                #endif
                    while length >= 2 || (isFinal && length = 1) do
                        dereferenceAndIncrementPtr U4 (not isFinal || length > 2)
                        let v = if length = 1 then int key.[idx]
                                else int key.[idx] ||| (int key.[idx + 1] <<< 16)
                        let v = if System.BitConverter.IsLittleEndian then v
                                else int (Buffer.SwapByteOrder(uint32 v))
                        loadI4 ilg v
                        ilg.Emit(OpCodes.Bne_Un, defaultLabel)
                        idx <- idx + 2
                        length <- length - 2
                if length > 0 then
                    Debug.Assert(not isFinal)
                    dereferenceAndIncrementPtr U2 true
                    loadI4 ilg (int key.[idx])
                    ilg.Emit(OpCodes.Bne_Un, defaultLabel)

        let subtreeLabels = if isNull physicalEqualityComparer then null
                            else System.Collections.Generic.Dictionary<Subtree, Label>(SubtreeEqualityComparer<'T>(kvs, physicalEqualityComparer))

        // Partitions the key pairs iBegin..(iEnd - 1) into branches with identical "branch-key".
        // Returns [|iBegin, i2, ..., iN, iEnd], [|fst kvs.[iBegin], fst kvs.[i2], ..., fst kvs.[iN]|]
        // where iBegin .. indexN are the indices where the branches start.
        let getBranchIndicesAndKeys (iBegin: int) iEnd getBranchKey =
            let mutable n = 0
            let indices, keys = new ResizeArray<int>(iEnd - iBegin), new ResizeArray<int>(iEnd - iBegin)
            indices.Add(iBegin)
            let mutable prevKey : int = getBranchKey (fst kvs.[iBegin])
            keys.Add(prevKey)
            for i = iBegin + 1 to iEnd - 1 do
                let key = getBranchKey (fst kvs.[i])
                if key <> prevKey then
                    prevKey <- key
                    indices.Add(i)
                    keys.Add(key)
            indices.Add(iEnd) // the indices array has one element more
            indices.ToArray(), keys.ToArray()

        // Returns labels for the subtrees given by the branchIndices and the subtreeStringIndex,
        // and an array with bools indicating whether the respective label was newly created.
        // If the dictionary already contains a label for an equivalent subtree, that label is returned;
        // otherwise, a new label is created.
        let getBranchLabels (subtreeLabels: Dictionary<Subtree, Label>) subtreeStringIndex (branchIndices: int[]) =
            assert (branchIndices.Length >= 2 && branchIndices.[0] < branchIndices.[1])
            let n = branchIndices.Length - 1
            let isNewLabel = Array.zeroCreate n
            let labels = Array.zeroCreate n
            if isNull subtreeLabels then
                for i = 0 to n - 1 do
                    isNewLabel.[i] <- true
                    labels.[i] <- ilg.DefineLabel()
            else
                let mutable iBegin = branchIndices.[0]
                for j = 1 to branchIndices.Length - 1 do
                    let iEnd = branchIndices.[j]
                    let subtree = Subtree(subtreeStringIndex, iBegin, iEnd - iBegin)
                    iBegin <- iEnd

                    let b = j - 1
                    let mutable label = Unchecked.defaultof<_>
                    if subtreeLabels.TryGetValue(subtree, &label) then
                        labels.[b] <- label
                    else
                        isNewLabel.[b] <- true
                        let label = ilg.DefineLabel()
                        labels.[b] <- label
                        subtreeLabels.Add(subtree, label)
            labels, isNewLabel

        let tempLocals = new TempLocals(ilg)

        // Assumes keys in iBegin..(iEnd - 1) are sorted by the branch-key returned by getBranchKey.
        let switch getBranchKey loadVar minVarValue maxVarValue subtreeLabels iBegin iEnd subtreeStringIndex emitBranchIter =
            let branchIndices, branchKeys = getBranchIndicesAndKeys iBegin iEnd getBranchKey
            let branchLabels, isNewLabel = getBranchLabels subtreeLabels subtreeStringIndex branchIndices
            let switchRanges, switchLabels = mergeSortedKeyLabelRanges branchKeys branchLabels
            emitSwitch ilg loadVar tempLocals
                       defaultMappingLengthCap defaultMappingDensityThreshold
                       minVarValue maxVarValue
                       defaultLabel switchRanges switchLabels
            for i = 0 to isNewLabel.Length - 1 do
                if isNewLabel.[i] then
                    ilg.MarkLabel(branchLabels.[i])
                    emitBranchIter branchIndices.[i] branchIndices.[i + 1]

        let subtreeEqualityComparer = if isNull physicalEqualityComparer then Unchecked.defaultof<_>
                                      else SubtreeEqualityComparer<'T>(kvs, physicalEqualityComparer)
        let subtreeLabels = if isNull physicalEqualityComparer then null
                            else Dictionary<Subtree, Label>(subtreeEqualityComparer)

        let rec emitSubtree length idx iBegin iEnd =
            assert (   iBegin < iEnd
                    && kvs.[iBegin..(iEnd - 1)]
                       |> Array.map (fun (k,_) -> k.Length)
                       |> Array.forall ((=) length))
            let idx1 = findIndexOfFirstCharAfterCommonPrefix idx iBegin iEnd length
            if idx <> idx1 then
                emitStringComparison (fst kvs.[iBegin]) idx (idx1 - idx) (idx1 = length)
            if idx1 = length then
                assert (iBegin + 1 = iEnd)
                returnValue iBegin
            else
                let mutable emit = true
                if idx <> idx1 && isNotNull subtreeLabels then
                    let subtree = Subtree(idx1, iBegin, iEnd - iBegin)
                    let mutable label = Unchecked.defaultof<_>
                    if subtreeLabels.TryGetValue(subtree, &label) then
                        // an equivalent subtree has already been handled elsewhere
                        ilg.Emit(OpCodes.Br, label) // jump to that code
                        emit <- false
                    else
                        let label = ilg.DefineLabel()
                        ilg.MarkLabel(label)
                        subtreeLabels.Add(subtree, label)

                if emit then
                    dereferenceAndIncrementPtr U2 (idx1 + 1 < length)
                    storeCh()
                    switch (fun str -> int str.[idx1]) loadCh 0 0xffff
                           (if idx1 + 1 < length || isNull subtreeLabels then subtreeLabels // we want to keep the switch branches local
                            else Dictionary<Subtree, Label>(subtreeEqualityComparer))       // when they only contain a return statement
                           iBegin iEnd
                           (idx1 + 1) (emitSubtree length (idx1 + 1))

        let emitMaxLengthSubtree stringIndex iBegin iEnd =
            loadLength()
            loadI4 ilg maxLength
            ilg.Emit(OpCodes.Bne_Un, defaultLabel)
            emitSubtree maxLength stringIndex iBegin iEnd

        Debug.Assert(i0 < kvs.Length)

        if i0 <> 0 then // first key is empty
            let label = ilg.DefineLabel()
            loadLength()
            ilg.Emit(OpCodes.Brtrue, label)
            returnValue 0
            ilg.MarkLabel(label)

        if minLength = maxLength then
            emitMaxLengthSubtree 0 i0 kvs.Length
        else // at least two non-empty keys with different lengths
            let checkMinLength() =
                loadLength()
                loadI4 ilg minLength
                ilg.Emit(OpCodes.Blt, defaultLabel)

            if prefixLength <> 0 then
                checkMinLength()
                emitStringComparison (fst kvs.[i0]) 0 prefixLength false
                if prefixLength = minLength then
                    let label = ilg.DefineLabel()
                    loadLength()
                    loadI4 ilg minLength
                    ilg.Emit(OpCodes.Bne_Un, label)
                    returnValue i0
                    ilg.MarkLabel(label)
                    i0 <- i0 + 1

            else // prefixLength = 0
                if i0 = 0 && (fst kvs.[0]).[0] = '\u0000' then
                    // If a key contains a zero as the first char, we can't avoid
                    // the following length check (which we otherwise don't need for
                    // the switch because of the null termination of strings).
                    checkMinLength()

            if prefixLength + 1 = maxLength then // prefixLength <> 0
                emitMaxLengthSubtree prefixLength i0 kvs.Length
            else
                let topLevelTreeLabels = if isNull subtreeEqualityComparer then null
                                         else Dictionary<Subtree, Label>(subtreeEqualityComparer)
                // switch over char after prefix
                dereferenceAndIncrementPtr U2 (prefixLength + 1 < maxLength)
                storeCh()
                switch (fun str -> int str.[prefixLength]) loadCh 0 0xffff
                        topLevelTreeLabels
                        i0 kvs.Length
                        (prefixLength + 1)
                        (fun iBegin iEnd ->
                            // switch over length
                            switch (fun str -> str.Length) (fun ilg -> loadLength()) 0 System.Int32.MaxValue
                                    subtreeLabels
                                    iBegin iEnd
                                    (prefixLength + 1)
                                    (fun iBegin iEnd ->
                                        emitSubtree (fst kvs.[iBegin]).Length (prefixLength + 1) iBegin iEnd))

        // return default value
        let defaultValueIsNull = not T.IsValueType && isNull (box defaultValue)
        ilg.MarkLabel(defaultLabel)
        if isPrimitive then
            loadConstant defaultValue
            storeResult()
        else
            if defaultValueIsNull then
                ilg.Emit(OpCodes.Ldnull)
            else
                ilg.Emit(OpCodes.Ldarg_0)
                ilg.Emit(OpCodes.Ldfld, tb.DefineField("DefaultValue", T, FieldAttributes.Public))
            ilg.Emit(OpCodes.Ret)

        // return result
        ilg.MarkLabel(returnLabel)
        if isPrimitive then
            loadResult()
        else
            // We could store all the values in individual fields to avoid the bounds check
            // and indirect load, but that probably wouldn't be worth the additional
            // code generation (and garbage collection?) costs (except for tiny mappings).
            ilg.Emit(OpCodes.Ldarg_0)
            ilg.Emit(OpCodes.Ldfld, tb.DefineField("Values", typeof<'T[]>, FieldAttributes.Public))
            loadResult()
            ilg.Emit(OpCodes.Ldelem, T)
        ilg.Emit(OpCodes.Ret)

        // compile type
        let t = tb.CreateType()
        // instantiate type
        let mapping = FormatterServices.GetUninitializedObject(t) :?> (string -> 'T)
        if not isPrimitive then
            // we can't use the previously used Fieldbuilders here, because SetValue is not implemented in FieldBuilders
            if not defaultValueIsNull then t.GetField("DefaultValue").SetValue(mapping, defaultValue)
            let values = Array.zeroCreate returnedValueIndices.Count
            let mutable j = 0
            for i in returnedValueIndices do
                values.[j] <- snd kvs.[i]
                j <- j + 1
            t.GetField("Values").SetValue(mapping, values)

        let data, _, _, _ = !longKeyData
        if data.Count <> 0 then
            let dataArray = data.ToArray()
            if not (System.BitConverter.IsLittleEndian) then
                FParsec.Buffer.SwapByteOrder(dataArray)
            t.GetField("longKeyData").SetValue(mapping, dataArray)


    #if DEBUG_STATIC_MAPPING
        // saveEmitAssembly "FParsec.Emitted.dll"

        if isNull physicalEqualityComparer then mapping
        else
            let dict = new System.Collections.Generic.Dictionary<string,'T>(kvs.Length)
            for k, v in kvs do
                dict.Add(k, v)
            let errorHandler (key: string) : unit =
                let e = new System.Exception("An internal assert check in FParsec.StaticMapping.createStringMapping failed. Please report this error to fparsec@quanttec.com. (The Data member of the exception object contains the information needed to reproduce the error.)")
                e.Data.["Argument"] <- key
                e.Data.["KeysValues"] <- dict
                e.Data.["DefaultValue"]  <- defaultValue
                raise e

            fun key ->
                let mutable value = Unchecked.defaultof<_>
                if not (dict.TryGetValue(key, &value)) then value <- defaultValue
                let value2 = mapping key
                if not (physicalEqualityComparer.Equals(value, value2)) then errorHandler key
                value
    #else
        mapping
    #endif

#endif