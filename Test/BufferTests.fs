// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.BufferTests

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open FParsec.Test.Test

#nowarn "9" // "Uses of this construct may result in the generation of unverifiable .NET IL code."

type Buffer = FParsec.Buffer

let testSwapByteOrder() =
    Buffer.SwapByteOrder(0xffffffffu) |> Equal 0xffffffffu
    Buffer.SwapByteOrder(0x00000000u) |> Equal 0x00000000u
    Buffer.SwapByteOrder(0x12345678u) |> Equal 0x78563412u
    Buffer.SwapByteOrder(0xffffffffffffffffUL) |> Equal 0xffffffffffffffffUL
    Buffer.SwapByteOrder(0x0000000000000000UL) |> Equal 0x0000000000000000UL
    Buffer.SwapByteOrder(0x123456789abcdef0UL) |> Equal 0xf0debc9a78563412UL
#if LOW_TRUST
    let array = [|0x12345678u; 0x9abcdef0u; 0x12345678u|]
    Buffer.SwapByteOrder(array)
    array |> Equal [|0x78563412u; 0xf0debc9au; 0x78563412u;|]
#else
    let p = NativePtr.stackalloc 3
    NativePtr.set p 0 0x12345678u
    NativePtr.set p 1 0x9abcdef0u
    NativePtr.set p 2 0x12345678u
    Buffer.SwapByteOrder(p, 3u)
    NativePtr.get p 0 |> Equal 0x78563412u
    NativePtr.get p 1 |> Equal 0xf0debc9au
    NativePtr.get p 2 |> Equal 0x78563412u
#endif
    Buffer.SwapByteOrder(0x01020304u) |> Equal 0x04030201u

#if LOW_TRUST
#else

let testCopy() =
    let n = 64
    let bytes = Array.init n (fun i -> byte i)
    let buffer1 = Array.zeroCreate n : byte[]
    let buffer2 = Array.zeroCreate n : byte[]
    let handle = GCHandle.Alloc(buffer2, GCHandleType.Pinned)
    let buffer2Ptr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject()) : nativeptr<byte>

    for iSrc = 0 to n do
        for iDst = 0 to n do
            for size = 0 to min (n - iSrc) (n - iDst) do
                Array.blit bytes 0 buffer1 0 n
                Array.blit bytes 0 buffer2 0 n
                System.Buffer.BlockCopy(buffer1, iSrc, buffer1, iDst, size)
                Buffer.Copy(NativePtr.add buffer2Ptr iDst,
                            NativePtr.add buffer2Ptr iSrc, size)
                if buffer1 <> buffer2 then
                    Fail()

    try Buffer.Copy(NativePtr.ofNativeInt 0n, NativePtr.ofNativeInt 0n, -1)
        Fail()
    with :? System.ArgumentOutOfRangeException -> ()

let testEqual() =
    let n = 16
    let buffer1 = NativePtr.stackalloc n
    let buffer2 = NativePtr.stackalloc n
    for i = 0 to n - 1 do
        NativePtr.set buffer1 i (uint32 i)
        NativePtr.set buffer2 i (uint32 i)

    for length = 0 to n do
        for i = 0 to length - 1 do
            Buffer.Equals(buffer1, buffer2, uint32 length) |> True
            NativePtr.set buffer2 i 0xffffffffu
            Buffer.Equals(buffer1, buffer2, uint32 length) |> False
            NativePtr.set buffer2 i (uint32 i)


#endif

let run() =
    testSwapByteOrder()
#if LOW_TRUST
#else
    testCopy()
    testEqual()
#endif
