// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.BufferTests

open System
open System.Runtime.InteropServices
open Xunit
open FsCheck
open Microsoft.FSharp.NativeInterop
open FParsec.Test.Test
open FsCheck.Xunit

#nowarn "9" // "Uses of this construct may result in the generation of unverifiable .NET IL code."

type Buffer = FParsec.Buffer

[<FParsecProperty>]
let ``SwapByteOrder swaps 32bit unsigned integers`` (i: uint32) =
    let expected =
        BitConverter.GetBytes(i)
        |> Array.rev
        |> Convert.ToHexString
    
    let actual =
        Buffer.SwapByteOrder i
        |> BitConverter.GetBytes
        |> Convert.ToHexString
    
    expected === actual

[<FParsecProperty>]
let ``SwapByteOrder swaps 64bit unsigned integers`` (i: uint64) =
    let expected =
        BitConverter.GetBytes(i)
        |> Array.rev
        |> Convert.ToHexString
    
    let actual =
        Buffer.SwapByteOrder i
        |> BitConverter.GetBytes
        |> Convert.ToHexString
    
    expected === actual

#if LOW_TRUST
[<FParsecProperty>]
let ``SwapByteOrder swaps 32bit unsigned integer arrays`` (i: uint32 array) =
    let expected =
        i
        |> Array.collect (BitConverter.GetBytes >> Array.rev)
        |> Convert.ToHexString
    
    Buffer.SwapByteOrder i
    
    let actual =
        i
        |> Array.collect BitConverter.GetBytes
        |> Convert.ToHexString
    
    expected === actual
#else

let inline createPointer (data: uint32 array) =
    let length = data.Length
    let intPtr = NativePtr.stackalloc length
    data |> Array.iteri (NativePtr.set intPtr)
    intPtr

[<FParsecProperty>]
let ``SwapByteOrder swaps 32bit unsigned integer spans`` (i: uint32 array) =
    let expected =
        i
        |> Array.collect (BitConverter.GetBytes >> Array.rev)
        |> Convert.ToHexString
    
    let actual =
        let length = i.Length
        let p = createPointer i
        
        Buffer.SwapByteOrder(Span<_>(NativePtr.toVoidPtr p, length))
        i
        |> Array.mapi (fun index _ -> NativePtr.get p index)
        |> Array.collect BitConverter.GetBytes
        |> Convert.ToHexString
    
    expected === actual

[<FParsecProperty>]
let ``Copy copies bytes between arrays`` (data: byte array) (seed: int) =
    let rnd = Random(seed)
    let length = data.Length
    let iSrc = rnd.Next(0, length)
    let iDst = rnd.Next(0, length)
    let size = rnd.Next(0, min (length - iSrc) (length - iDst))
    
    let expected =
        let buffer = Array.copy data
        System.Buffer.BlockCopy(buffer, iSrc, buffer, iDst, size)
        buffer
    
    let actual =
        let buffer = Array.copy data
        let handle = GCHandle.Alloc(buffer, GCHandleType.Pinned)
        let bufferPtr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject()) : nativeptr<byte>
        Buffer.Copy(NativePtr.add bufferPtr iDst, NativePtr.add bufferPtr iSrc, size)
        buffer
        
    expected === actual

[<Fact>]
let ``Copy throws an ArgumentOutOfRangeException when using a negative size`` () =
    Assert.Throws<ArgumentOutOfRangeException>(fun () -> Buffer.Copy(NativePtr.ofNativeInt 0n, NativePtr.ofNativeInt 0n, -1))

[<FParsecProperty>]
let ``Equal returns true for array pointers with the same data`` (data: uint32 array) (seed: int) =
    let rnd = Random(seed)
    let comparisonLength = rnd.Next(0, data.Length)
    let buffer1 = createPointer data
    let buffer2 = createPointer data
    
    Buffer.Equals(buffer1, buffer2, comparisonLength)

[<FParsecProperty>]
let ``Equal returns false for array pointers with different data`` (NonEmptyArray data) (seed: int) =
    let rnd = Random(seed)
    let i = rnd.Next(0, data.Length-1)
    let buffer1 = createPointer data
    let buffer2 = createPointer data
    NativePtr.set buffer2 i 0xffffffffu
    
    not <| Buffer.Equals(buffer1, buffer2, i + 1)

#endif