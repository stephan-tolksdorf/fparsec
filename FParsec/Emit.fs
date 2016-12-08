// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

module internal FParsec.Emit

#if LOW_TRUST
#else

open System.Diagnostics
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

open Microsoft.FSharp.NativeInterop

open FParsec.Internals
open FParsec.Range

#nowarn "9" // "Uses of this construct may result in the generation of unverifiable .NET IL code."

let mutable private assemblyBuilder = null
let mutable private moduleBuilder = null
let private createTypeBuilderSyncRoot = new obj()

let createTypeBuilder name args parent (interfaces : System.Type[]) =
    lock createTypeBuilderSyncRoot (fun _ ->
        if isNull moduleBuilder then
            let assemblyName = new AssemblyName("FParsec.Emitted")
            let access =
                        #if DEBUG
                            AssemblyBuilderAccess.RunAndSave
                        #else
                            AssemblyBuilderAccess.Run
                        #endif
            assemblyBuilder <- System.Threading.Thread.GetDomain().DefineDynamicAssembly(assemblyName, access)
            moduleBuilder <- assemblyBuilder.DefineDynamicModule("FParsec.Emitted"
                                                                 #if DEBUG
                                                                     , "FParsec.Emitted.dll"
                                                                 #else
                                                                 #endif
                                                                 )
        moduleBuilder.DefineType("FParsec.Emitted." + name, args, parent, interfaces)
    )

#if DEBUG
let saveEmitAssembly fileName = assemblyBuilder.Save(fileName)
#endif

// Does anyone have an idea why the .NET System.Reflection.Emit.OpCode
// is implemented as a gigantic struct? (It has a size of ~36 bytes!)


let loadI4 (ilg: ILGenerator) (i: int32) =
    // For run-time-only code generation it probably makes little difference
    // whether we optimize the size of the IL, but we do it anyway.
    match i with
    | -1 -> ilg.Emit(OpCodes.Ldc_I4_M1)
    | 0  -> ilg.Emit(OpCodes.Ldc_I4_0)
    | 1  -> ilg.Emit(OpCodes.Ldc_I4_1)
    | 2  -> ilg.Emit(OpCodes.Ldc_I4_2)
    | 3  -> ilg.Emit(OpCodes.Ldc_I4_3)
    | 4  -> ilg.Emit(OpCodes.Ldc_I4_4)
    | 5  -> ilg.Emit(OpCodes.Ldc_I4_5)
    | 6  -> ilg.Emit(OpCodes.Ldc_I4_6)
    | 7  -> ilg.Emit(OpCodes.Ldc_I4_7)
    | 8  -> ilg.Emit(OpCodes.Ldc_I4_8)
    | _  ->
        let i1 = int8 i
        if i <> int32 i1 then ilg.Emit(OpCodes.Ldc_I4, i)
        else ilg.Emit(OpCodes.Ldc_I4_S, i1)

let loadI8 (ilg: ILGenerator) (i: int64) =
    let i4 = int32 i
    if i <> int64 i4 then ilg.Emit(OpCodes.Ldc_I8, i)
    else
        loadI4 ilg i4
        ilg.Emit(OpCodes.Conv_I8)

let loadU8 (ilg: ILGenerator) (i: uint64) =
    if i > uint64 System.UInt32.MaxValue then ilg.Emit(OpCodes.Ldc_I8, int64 i)
    else
        loadI4 ilg (int32 i)
        ilg.Emit(OpCodes.Conv_U8)

let loadI (ilg: ILGenerator) (i: nativeint) =
    if sizeof<nativeint> = 4 then
        ilg.Emit(OpCodes.Ldc_I4, int32 i)
    else
        ilg.Emit(OpCodes.Ldc_I8, int64 i)
    ilg.Emit(OpCodes.Conv_I)

let loadU (ilg: ILGenerator) (i: unativeint) =
    if sizeof<unativeint> = 4 then
        ilg.Emit(OpCodes.Ldc_I4, int32 i)
    else
        ilg.Emit(OpCodes.Ldc_I8, int64 i)
    ilg.Emit(OpCodes.Conv_U)

let private createLoaderForPrimitiveConstantsImpl (ty: System.Type) (ilg: ILGenerator) : ('T -> unit) =
    let ty = if ty.IsEnum then System.Enum.GetUnderlyingType(ty) else ty

    if   ty = typeof< int32> then fun x -> loadI4 ilg (box x :?> int32)
    elif ty = typeof<uint32> then fun x -> loadI4 ilg (int32 (box x :?> uint32))
    elif ty = typeof<int64>  then fun x -> loadI8 ilg (box x :?> int64)
    elif ty = typeof<uint64> then fun x -> loadU8 ilg (box x :?> uint64)
    elif ty = typeof< int16> then fun x -> loadI4 ilg (int32 (box x :?> int16))
    elif ty = typeof<uint16> then fun x -> loadI4 ilg (int32 (box x :?> uint16))
    elif ty = typeof<char>   then fun x -> loadI4 ilg (int32 (box x :?> char))
    elif ty = typeof< int8>  then fun x -> loadI4 ilg (int32 (box x :?> int8))
    elif ty = typeof<uint8>  then fun x -> loadI4 ilg (int32 (box x :?> uint8))
    elif ty = typeof<bool>   then
        fun x -> ilg.Emit(if box x :?> bool then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
    elif ty = typeof<double> then fun x -> ilg.Emit(OpCodes.Ldc_R8, (box x :?> double))
    elif ty = typeof<float32> then fun x -> ilg.Emit(OpCodes.Ldc_R4, (box x :?> float32))
    elif ty = typeof<nativeint> then fun x -> loadI ilg (box x :?> nativeint)
    elif ty = typeof<unativeint> then fun x -> loadU ilg (box x :?> unativeint)
    else invalidArg "ty" "Invalid type argument."

let createLoaderForPrimitiveConstants<'T> ilg : ('T -> unit) =
    createLoaderForPrimitiveConstantsImpl typeof<'T> ilg

let createLoaderForBoxedPrimitiveConstants (ty: System.Type) ilg : (obj -> unit) =
    createLoaderForPrimitiveConstantsImpl ty ilg


let emitRangeCheck branchIfInRange (ilg: ILGenerator) (label: Label) minValue maxValue (range: Range)  =
    Debug.Assert(minValue <= range.Min && range.Max <= maxValue)
    if minValue = range.Min && range.Max = maxValue then
        ilg.Emit(OpCodes.Pop)
        if branchIfInRange then
            ilg.Emit(OpCodes.Br, label)
    elif range.Min = range.Max then
        loadI4 ilg range.Min
        if branchIfInRange then
            ilg.Emit(OpCodes.Beq, label)
        else
            ilg.Emit(OpCodes.Bne_Un, label)
    elif minValue = range.Min then
        // we only have to check the right bound
        loadI4 ilg range.Max
        if branchIfInRange then
            ilg.Emit(OpCodes.Ble, label)
        else
            ilg.Emit(OpCodes.Bgt, label)
    elif range.Max = maxValue then
        // we only have to check the left bound
        loadI4 ilg range.Min
        if branchIfInRange then
            ilg.Emit(OpCodes.Bge, label)
        else
            ilg.Emit(OpCodes.Blt, label)
    else
        // we have to check both bounds
        if range.Min <> 0 then
            loadI4 ilg range.Min
            ilg.Emit(OpCodes.Sub)
        loadI4 ilg (range.Max - range.Min)
        if branchIfInRange then
            ilg.Emit(OpCodes.Ble_Un, label) // unsigned comparison
        else
            ilg.Emit(OpCodes.Bgt_Un, label) // unsigned comparison

let emitBranchIfOutOfRange ilg label minValue maxValue range =
    emitRangeCheck false ilg label minValue maxValue range

let emitBranchIfInRange ilg label minValue maxValue range =
    emitRangeCheck true ilg label minValue maxValue range

let emitRangeTest pushFalseIfInRange (ilg: ILGenerator) minValue maxValue (range: Range) =
    Debug.Assert(minValue <= range.Min && range.Max <= maxValue)

    let emitNot() =
        ilg.Emit(OpCodes.Ldc_I4_0)
        ilg.Emit(OpCodes.Ceq)

    if minValue = range.Min && range.Max = maxValue then
        ilg.Emit(OpCodes.Pop)
        if pushFalseIfInRange then
            ilg.Emit(OpCodes.Ldc_I4_0)
        else
            ilg.Emit(OpCodes.Ldc_I4_1)
    elif range.Min = range.Max then
        loadI4 ilg range.Min
        ilg.Emit(OpCodes.Ceq)
        if pushFalseIfInRange then emitNot()
    elif minValue = range.Min then
        // we only have to check the right bound
        loadI4 ilg range.Max
        ilg.Emit(OpCodes.Cgt)
        if not pushFalseIfInRange then emitNot()
    elif range.Max = maxValue then
        // we only have to check the left bound
        loadI4 ilg range.Min
        ilg.Emit(OpCodes.Clt)
        if not pushFalseIfInRange then emitNot()
    else
        // we have to check both bounds
        if range.Min <> 0 then
            loadI4 ilg range.Min
            ilg.Emit(OpCodes.Sub)
        loadI4 ilg (range.Max - range.Min)
        ilg.Emit(OpCodes.Cgt_Un) // unsigned comparison
        if not pushFalseIfInRange then emitNot()

let emitTwoRangeTest (ilg: ILGenerator) (loadVar: ILGenerator -> unit) inverse minValue maxValue (range1: Range) (range2: Range) =
    assert (range1.Max < range2.Min && range1.Max + 1 < range2.Min)
    let needOuterRangeCheck = minValue < range1.Min || range2.Max < maxValue
    let w = sizeof<unativeint>*8
    if needOuterRangeCheck && (maxValue - minValue < w) then
        // use a simple bit vector test:
        // (bits >> (var - off)) & 1
        let off = if minValue > 0 && maxValue < w then 0 else minValue
        let mutable bits = if inverse then unativeint -1n else 0un
        for r in [range1; range2] do
            for i in r.Min .. r.Max do
                let b = i - off
                if inverse then
                    bits <- bits ^^^ (1un <<< b)
                else
                    bits <- bits ||| (1un <<< b)
        loadU ilg bits
        loadVar ilg
        if off <> 0 then
            loadI4 ilg off
            ilg.Emit(OpCodes.Sub)
        ilg.Emit(OpCodes.Shr_Un)
        ilg.Emit(OpCodes.Ldc_I4_1)
        ilg.Emit(OpCodes.And)
    elif not needOuterRangeCheck
         || (range1.Max + 2 = range2.Min && range1.Min <> range1.Max && range2.Min <> range2.Max)
    then
        if needOuterRangeCheck then
            loadVar ilg
            emitRangeTest inverse ilg minValue maxValue (Range(range1.Min, range2.Max))
        loadVar ilg
        emitRangeTest (not inverse) ilg minValue maxValue (Range(range1.Max + 1, range2.Min - 1))
        if needOuterRangeCheck then
            if inverse then
                ilg.Emit(OpCodes.Or)
            else
                ilg.Emit(OpCodes.And)
    else
        loadVar ilg
        emitRangeTest inverse ilg minValue maxValue range1
        loadVar ilg
        emitRangeTest inverse ilg minValue maxValue range2
        if inverse then
            ilg.Emit(OpCodes.And)
        else
            ilg.Emit(OpCodes.Or)


type TempLocals(ilg: ILGenerator) =
    let mutable intLocal = null
    let mutable boolLocal = null

    /// used by emitSetMembershipTest (and indirectly by emitSwitch)
    member t.GetIntLocal() =
        if isNull intLocal then
            intLocal <- ilg.DeclareLocal(typeof<int32>)
        intLocal

    /// used by emitSwitch
    member t.GetBoolLocal() =
        if isNull boolLocal then
            boolLocal <- ilg.DeclareLocal(typeof<bool>)
        boolLocal

/// flag used for testing purposes
let mutable noBitVectorTests = false

let emitSetMembershipTest (ilg: ILGenerator)
                          (loadVar: ILGenerator -> unit) (storeResult: ILGenerator -> unit)
                          (temps: TempLocals)
                          lengthCap densityThreshold
                          minValue maxValue
                          inverse (ranges: Range[]) =

    checkRangesAreValidSortedAndUnconnected ranges

    let endLabel = ilg.DefineLabel()
    let outOfRangeLabel = ilg.DefineLabel()

    let emitBitVectorTest minValue maxValue iBegin iEnd =
        let first, last = ranges.[iBegin].Min, ranges.[iEnd - 1].Max
        // set up bit vector in unmanaged memory
        let w = sizeof<unativeint>*8
        // save a subtraction if it doesn't cost too much memory
        let off = if first > 0 && (last < w || (first < 3*w && (last >= first + w))) then 0
                  else first

        let lastMinusOff = uint32 (last - off)
        if lastMinusOff > uint32 System.Int32.MaxValue then
           raise (System.ArgumentException("The ranges span width is too large."))

        let length = int (lastMinusOff/uint32 w + 1u)
        if  uint32 length * uint32 w > uint32 System.Int32.MaxValue then
            raise (System.ArgumentException("The ranges span width is too large."))

        let mutable stackVar = 0un
        let ptr = if length = 1 then NativePtr.ofNativeInt (NativePtr.toNativeInt &&stackVar)
                  else NativePtr.ofNativeInt (UnmanagedMemoryPool.Allocate(length*sizeof<unativeint>))

        // fill bit vector ptr.[0..length - 1]
        let r = ranges.[iBegin]
        let mutable rMin, rMax = r.Min - off, r.Max - off
        let mutable i = iBegin + 1
        if not inverse then
            for j = 0 to length - 1 do
                let mutable n = 0un
                let j1w = (j + 1)*w
                while rMin < j1w do
                    n <- n ||| (1un <<< rMin%w)
                    if rMin < rMax then rMin <- rMin + 1
                    elif i < iEnd then
                        let r = ranges.[i]
                        rMin <- r.Min - off; rMax <- r.Max - off
                        i <- i + 1
                    else rMin <- System.Int32.MaxValue // break
                NativePtr.set ptr j n
        else
            for j = 0 to length - 1 do
                let mutable n = unativeint -1n
                let j1w = (j + 1)*w
                while rMin < j1w do
                    n <- n ^^^ (1un <<< rMin%w)
                    if rMin < rMax then rMin <- rMin + 1
                    elif i < iEnd then
                        let r = ranges.[i]
                        rMin <- r.Min - off; rMax <- r.Max - off
                        i <- i + 1
                    else rMin <- System.Int32.MaxValue // break
                NativePtr.set ptr j n

        let intTemp = temps.GetIntLocal()

        // t = (uint32)(x - off)
        loadVar ilg
        if off <> 0 then
            loadI4 ilg off
            ilg.Emit(OpCodes.Sub)
        ilg.Emit(OpCodes.Stloc, intTemp)

        // if (t > (uint32)(last - off)) goto outOfRangeLabel
        if minValue < off || length*w <= maxValue - off then
            ilg.Emit(OpCodes.Ldloc, intTemp)
            loadI4 ilg (last - off)
            ilg.Emit(OpCodes.Bgt_Un, outOfRangeLabel)

        if length = 1 then
            // x = *ptr
            loadU ilg stackVar
        else
            // x = *(ptr + t/w)
            loadU ilg (unativeint (NativePtr.toNativeInt ptr))
            ilg.Emit(OpCodes.Ldloc, intTemp)
            loadI4 ilg w
            ilg.Emit(OpCodes.Div_Un)
            loadI4 ilg sizeof<unativeint>
            ilg.Emit(OpCodes.Mul)
            ilg.Emit(OpCodes.Add)
            ilg.Emit(OpCodes.Ldind_I)

        // result = (x >> t%w) & 1
        ilg.Emit(OpCodes.Ldloc, intTemp)
        if length > 1 then
            loadI4 ilg w
            ilg.Emit(OpCodes.Rem_Un)
        ilg.Emit(OpCodes.Shr_Un)
        ilg.Emit(OpCodes.Ldc_I4_1)
        ilg.Emit(OpCodes.And)
        storeResult ilg
        ilg.Emit(OpCodes.Br, endLabel)

    let emitRangeTest inverse minValue maxValue (range: Range) =
        loadVar ilg
        emitRangeTest inverse ilg minValue maxValue range
        storeResult ilg
        ilg.Emit(OpCodes.Br, endLabel)

    let emitTwoRangeTest inverse minValue maxValue range1 range2 =
        emitTwoRangeTest ilg loadVar inverse minValue maxValue range1 range2
        storeResult ilg
        ilg.Emit(OpCodes.Br, endLabel)

    let rec emitRegion minValue maxValue iBegin iEnd =
        Debug.Assert(iBegin < iEnd && minValue <= ranges.[iBegin].Min && ranges.[iEnd - 1].Max <= maxValue)

        match iEnd - iBegin with
        | 0 -> failwith "emitSetMembershipTest.emitRegion"
        | 1 -> emitRangeTest inverse minValue maxValue ranges.[iBegin]
        | 2 -> emitTwoRangeTest inverse minValue maxValue ranges.[iBegin] ranges.[iBegin + 1]
        | _ -> // at least 3 ranges
            if not noBitVectorTests
               && density lengthCap ranges iBegin iEnd >= densityThreshold
            then
                emitBitVectorTest minValue maxValue iBegin iEnd
            else
                let i, pivotAroundRangeMax = findPivot ranges iBegin iEnd
                let label = ilg.DefineLabel()
                let r = ranges.[i]
                loadVar ilg
                if pivotAroundRangeMax then
                    loadI4 ilg r.Max
                    ilg.Emit(OpCodes.Bgt, label)
                    emitRegion  minValue   r.Max     iBegin (i + 1)
                    ilg.MarkLabel(label)
                    emitRegion (r.Max + 1) maxValue (i + 1)  iEnd
                else
                    loadI4 ilg r.Min
                    ilg.Emit(OpCodes.Blt, label)
                    emitRegion r.Min     maxValue   i      iEnd
                    ilg.MarkLabel(label)
                    emitRegion minValue (r.Min - 1) iBegin i

    if ranges.Length <> 0 then
        emitRegion minValue maxValue 0 ranges.Length

    ilg.MarkLabel(outOfRangeLabel)
    if inverse then
        ilg.Emit(OpCodes.Ldc_I4_1)
    else
        ilg.Emit(OpCodes.Ldc_I4_0)
    storeResult ilg
    ilg.MarkLabel(endLabel)



let emitSwitch (ilg: ILGenerator) (loadVar: ILGenerator -> unit) (temps: TempLocals)
               lengthCap densityThreshold
               minValue maxValue
               (defaultLabel: Label) (ranges: Range[]) (labels: Label[])  =
    Debug.Assert(ranges.Length = labels.Length)
    checkLabelRangesAreValidSortedAndUnconnected ranges labels

    let emitJumpTable (* minValue maxValue *) iBegin iEnd =
        // We can't optimize the range check of the switch statement,
        // so we have no use for minValue and maxValue arguments.
        // (In LLVM we could use the 'unreachable' instruction for optimizing the range check.)
        Debug.Assert(iBegin + 2 <= iEnd)
        let first = ranges.[iBegin].Min
        let off = first
        let length =
            let last = ranges.[iEnd - 1].Max
            let lastMinusOff = last - off
            if uint32 lastMinusOff >= uint32 System.Int32.MaxValue then
                raise (System.ArgumentException("The ranges span width is too large."))
            lastMinusOff + 1 // length <= Int32.MaxValue

        let jt = Array.zeroCreate length
        let mutable j = 0
        for i = iBegin to iEnd - 1 do
            let r = ranges.[i]
            let rMin, rMax = r.Min - off, r.Max - off
            while j < rMin do
                jt.[j] <- defaultLabel
                j <- j + 1
            let label = labels.[i]
            while j <= rMax do
                jt.[j] <- label
                j <- j + 1

        loadVar ilg
        if off <> 0 then
            loadI4 ilg off
            ilg.Emit(OpCodes.Sub)
        ilg.Emit(OpCodes.Switch, jt)
        ilg.Emit(OpCodes.Br, defaultLabel)

    let emitBranchIfInRange2 label (defaultLabel: Label) minValue maxValue (range: Range) =
        if minValue < range.Min || range.Max < maxValue then
            loadVar ilg
            emitBranchIfInRange ilg label minValue maxValue range
            ilg.Emit(OpCodes.Br, defaultLabel)
        else
            ilg.Emit(OpCodes.Br, label)

    let emitBranchIfInRange label minValue maxValue (range: Range) =
        loadVar ilg
        emitBranchIfInRange ilg label minValue maxValue range

    let emitBranchIfOutOfRange label minValue maxValue (range: Range) =
        if minValue < range.Min || range.Max < maxValue then
            loadVar ilg
            emitBranchIfOutOfRange ilg label minValue maxValue range

    let rec emitRegion minValue maxValue iBegin iEnd =
        Debug.Assert(iBegin < iEnd && minValue <= ranges.[iBegin].Min && ranges.[iEnd - 1].Max <= maxValue)

        let pivotAroundRange i pivotAroundRangeMax =
            let label = ilg.DefineLabel()
            let r = ranges.[i]
            loadVar ilg
            if pivotAroundRangeMax then
                loadI4 ilg r.Max
                ilg.Emit(OpCodes.Bgt, label)
                emitRegion  minValue   r.Max     iBegin (i + 1)
                ilg.MarkLabel(label)
                emitRegion (r.Max + 1) maxValue (i + 1)  iEnd
            else
                loadI4 ilg r.Min
                ilg.Emit(OpCodes.Blt, label)
                emitRegion r.Min     maxValue   i      iEnd
                ilg.MarkLabel(label)
                emitRegion minValue (r.Min - 1) iBegin i

        match iEnd - iBegin with
        | 0 ->
            failwith "emitSwitch.emitRegion"
        | 1 ->
            emitBranchIfInRange2 labels.[iBegin] defaultLabel minValue maxValue ranges.[iBegin]
        | 2 ->
            let r1, r2 = ranges.[iBegin], ranges.[iBegin + 1]
            let l1, l2 = labels.[iBegin], labels.[iBegin + 1]
            if l1 = l2 then
                Debug.Assert(r1.Max + 1 < r2.Min)
                //emitBranchIfOutOfRange defaultLabel minValue maxValue (Range(r1.Min, r2.Max))
                //emitBranchIfInRange2 defaultLabel l1 r1.Min r2.Max (Range(r1.Max + 1, r2.Min - 1))
                emitTwoRangeTest ilg loadVar false minValue maxValue r1 r2
                ilg.Emit(OpCodes.Brtrue, l1)
                ilg.Emit(OpCodes.Br, defaultLabel)
            else
                let rangesAreConnected = r1.Max + 1 = r2.Min
                let checkLeft, checkRight = minValue < r1.Min, r2.Max < maxValue
                if rangesAreConnected && ((checkLeft && checkRight) || (not checkLeft && not checkRight)) then
                    emitBranchIfOutOfRange defaultLabel minValue maxValue (Range(r1.Min, r2.Max))
                    // If 64-bit .NET JIT can substitute both of the branches emitted below with
                    // the code at the destination, it chooses to substitute the first. Hence,
                    // we put the more likely case first (assuming that values are
                    // uniformly distributed on {minValue ... maxValue}).
                    // (The 32-bit .NET JIT (version 4) doesn't yet seem to seriously attempt
                    //  a code block reordering optimization.)
                    if uint32 (r1.Max - r1.Min) >= uint32 (r2.Max - r2.Min) then
                        emitBranchIfInRange2 l1 l2 r1.Min r2.Max r1
                    else
                        emitBranchIfInRange2 l2 l1 r1.Min r2.Max r2
                else
                    if (if rangesAreConnected then checkRight (* not checkLeft *)
                        else uint32 (r1.Max - r1.Min) >= uint32 (r2.Max - r2.Min))
                    then
                        emitBranchIfInRange l1 minValue maxValue r1
                        let minRightValue = if checkLeft then minValue else r1.Max + 1
                        emitBranchIfInRange2 l2 defaultLabel minRightValue maxValue r2
                    else
                        emitBranchIfInRange l2 minValue maxValue r2
                        let maxLeftValue = if checkRight then maxValue else r2.Min - 1
                        emitBranchIfInRange2 l1 defaultLabel minValue maxLeftValue r1
        | _ -> // at least 3 ranges
            let allLabelsAreIdentical =
                let label = labels.[iBegin]
                let mutable i = iBegin + 1
                while i < iEnd && label.Equals(labels.[i]) do i <- i + 1
                i = iEnd
            if allLabelsAreIdentical then
                let bl = temps.GetBoolLocal()
                // emitSetMembershipTest doesn't use GetBoolLocal itself
                emitSetMembershipTest ilg loadVar (fun ilg -> ilg.Emit(OpCodes.Stloc, bl)) temps
                                      (lengthCap*8) (densityThreshold/32.)
                                      minValue maxValue
                                      false ranges.[iBegin..(iEnd - 1)]
                ilg.Emit(OpCodes.Ldloc, bl)
                ilg.Emit(OpCodes.Brtrue, labels.[iBegin])
                ilg.Emit(OpCodes.Br, defaultLabel)
            elif density lengthCap ranges iBegin iEnd >= densityThreshold then
                emitJumpTable iBegin iEnd
            else
                let i, pivotAroundRangeMax = findPivot ranges iBegin iEnd
                pivotAroundRange i pivotAroundRangeMax

    if ranges.Length <> 0 then
        emitRegion minValue maxValue 0 ranges.Length
    else
        ilg.Emit(OpCodes.Br, defaultLabel)

#endif