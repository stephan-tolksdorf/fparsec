// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.HexFloat32Tests

open System
open Microsoft.FSharp.Core
open Xunit
open FParsec.Test.Test

let float32ToHexString = FParsec.CharParsers.float32ToHexString
let float32OfHexString = FParsec.CharParsers.float32OfHexString
let eps    = (float32) (Math.Pow(2.0, -24.0))
let min    = (float32) (Math.Pow(2.0, -126.0)) // smallest normal number
let minmin = (float32) (Math.Pow(2.0, -149.0)) // smallest subnormal number

[<FParsecProperty(MaxTest = 100000)>]
let ``float32ToHexString output matches test oracle implementation`` (f: float32) =
    float32ToHexString f === Float32.toHexString f

[<FParsecProperty>]
let ``float32ToHexString returns strings in the correct format`` (FiniteFloat32 f) =
    float32ToHexString f .* @"(-?0x[01].[0-9a-fA-F]*p-?[0-9]*$)"

[<FParsecProperty(MaxTest = 100000)>]
let ``float32ToHexString and floatOfHexString functions are reciprocal`` (FiniteFloat32 f) =
    (float32ToHexString f |> float32OfHexString) === f

[<FParsecProperty>]
let ``float32ToHexString returns strings that start with '0' for positive finite values`` (PositiveFiniteFloat32 f) =
    let expected = '0'
    
    let actual = (float32ToHexString f)[0]
    
    expected === actual

[<FParsecProperty>]
let ``Hex String starts with '-' for negative finite values`` (NegativeFiniteFloat32 f) =
    let expected = '-'
    
    let actual = (float32ToHexString f)[0]
    
    expected === actual

[<Fact>]
let ``float32ToHexString returns 'Infinity' for a positive infinity value`` () =
    Assert.Equal("Infinity", float32ToHexString Single.PositiveInfinity)

[<Fact>]
let ``float32ToHexString returns '-Infinity' for a negative infinity value`` () =
    Assert.Equal("-Infinity", float32ToHexString Single.NegativeInfinity)

[<Fact>]
let ``float32ToHexString returns 'NaN' for an invalid float value`` () =
    Assert.Equal("NaN", float32ToHexString Single.NaN)

[<Fact>]
let ``Positive Zero exponent`` () =
    Assert.Equal("0x0.0p0", float32ToHexString 0.0f)

[<Fact>]
let ``Negative Zero exponent`` () =
    Assert.Equal("-0x0.0p0", float32ToHexString -0.0f)

[<Theory>]
[<InlineData("")>]
[<InlineData(".")>]
[<InlineData("p1")>]
[<InlineData(".p1")>]
[<InlineData("1x1")>]
[<InlineData("x1")>]
[<InlineData("0xx1")>]
[<InlineData("0x/")>]
[<InlineData("0x:")>]
[<InlineData("0x@")>]
[<InlineData("0xG")>]
[<InlineData("0x`")>]
[<InlineData("0xg")>]
[<InlineData("0.1pp1")>]
[<InlineData("0.1p+")>]
[<InlineData("0.1p-")>]
[<InlineData("0.fg")>]
[<InlineData("1.0 ")>]
[<InlineData("1..")>]
[<InlineData("1.0.")>]
let ``floatOfHexString correctly rejects malformed hex strings`` str =
    Assert.Throws<FormatException>(fun () -> float32OfHexString str |> ignore)

[<Fact>]
let ``floatOfHexString correctly rejects a null string`` () =
    Assert.Throws<ArgumentNullException>(fun () -> float32OfHexString null |> ignore)

[<Theory>]
[<InlineData("Inf", Double.PositiveInfinity)>]
[<InlineData("iNf", Double.PositiveInfinity)>]
[<InlineData("Infinity", Double.PositiveInfinity)>]
[<InlineData("+InFinITy", Double.PositiveInfinity)>]
[<InlineData("-Inf", Double.NegativeInfinity)>]
[<InlineData("-InFinITy", Double.NegativeInfinity)>]
[<InlineData("NaN", Double.NaN)>]
[<InlineData("-nAn", Double.NaN)>]
[<InlineData("+Nan", Double.NaN)>]
let ``floatOfHexString correctly handles Infinity and NaN strings`` str expected =
    Assert.Equal(expected, float32OfHexString str)

[<Theory>]
[<InlineData("001")>]
[<InlineData("1.")>]
[<InlineData("1.0")>]
[<InlineData("0x1")>]
[<InlineData("0X1")>]
[<InlineData("0x0001")>]
[<InlineData("0x1.")>]
[<InlineData("0x1.0")>]
[<InlineData("0x001.0")>]
[<InlineData("1.0p0")>]
[<InlineData("1.0P0")>]
[<InlineData("001.00p+000")>]
[<InlineData(".100p+004")>]
[<InlineData(".0100p+008")>]
[<InlineData("00.100p+004")>]
[<InlineData("00.0100p+008")>]
[<InlineData("0010.0p-004")>]
[<InlineData("0x1.0p0")>]
[<InlineData("0X1.0P0")>]
[<InlineData("0x001.00p+000")>]
[<InlineData("0x00.100p+004")>]
[<InlineData("0x.100p+004")>]
[<InlineData("0x0010.0p-004")>]
[<InlineData("+001")>]
[<InlineData("+1.")>]
[<InlineData("+1.0")>]
[<InlineData("+.100p+004")>]
[<InlineData("+0x0010.0p-004")>]
let ``floatOfHexString handles different formats of 1.0`` str =
    Assert.Equal(1.0f, float32OfHexString str)

[<Theory>]
[<InlineData("-001")>]
[<InlineData("-1.")>]
[<InlineData("-1.0")>]
[<InlineData("-0x1")>]
[<InlineData("-0X1")>]
[<InlineData("-0x0001")>]
[<InlineData("-0x1.")>]
[<InlineData("-0x1.0")>]
[<InlineData("-0x001.0")>]
[<InlineData("-1.0p0")>]
[<InlineData("-1.0P0")>]
[<InlineData("-001.00p+000")>]
[<InlineData("-.100p+004")>]
[<InlineData("-.0100p+008")>]
[<InlineData("-00.100p+004")>]
[<InlineData("-00.0100p+008")>]
[<InlineData("-0010.0p-004")>]
[<InlineData("-0x1.0p0")>]
[<InlineData("-0X1.0P0")>]
[<InlineData("-0x001.00p+000")>]
[<InlineData("-0x00.100p+004")>]
[<InlineData("-0x.100p+004")>]
[<InlineData("-0x0010.0p-004")>]
let ``floatOfHexString handles different formats of -1.0`` str =
    Assert.Equal(-1.0f, float32OfHexString str)

[<Theory>]
[<InlineData("0")>]
[<InlineData("0.")>]
[<InlineData("0.0")>]
[<InlineData("00.0")>]
[<InlineData("00.000")>]
[<InlineData("00.000p0")>]
[<InlineData("00.000p99999999")>]
[<InlineData("0x0")>]
[<InlineData("0x0.")>]
[<InlineData("0x0.0")>]
[<InlineData("0x00.0")>]
[<InlineData("0x00.000")>]
[<InlineData("0x00.000p0")>]
[<InlineData("0x00.000p99999999")>]
[<InlineData("100P-2147483639")>]
[<InlineData("100P-2147483640")>]
[<InlineData("100P-2147483647")>]
[<InlineData("100P-9999999999999999999999999")>]
[<InlineData("0.001P-2147483639")>]
[<InlineData("0.001P-2147483640")>]
[<InlineData("0.001P-2147483647")>]
[<InlineData("0.001P-9999999999999999999999999")>]
let ``floatOfHexString generates floats that are binary equivalent to positive 0`` str =
    let actual = float32OfHexString str
    Assert.Equal(BitConverter.SingleToInt32Bits(0.0f), BitConverter.SingleToInt32Bits(actual))

[<Theory>]
[<InlineData("-0")>]
[<InlineData("-0.")>]
[<InlineData("-0.0")>]
[<InlineData("-00.0")>]
[<InlineData("-00.000")>]
[<InlineData("-00.000p0")>]
[<InlineData("-00.000p99999999")>]
[<InlineData("-0x0")>]
[<InlineData("-0x0.")>]
[<InlineData("-0x0.0")>]
[<InlineData("-0x00.0")>]
[<InlineData("-0x00.000")>]
[<InlineData("-0x00.000p0")>]
[<InlineData("-0x00.000p0")>]
[<InlineData("-0x00.000p99999999")>]
[<InlineData("-100P-2147483639")>]
[<InlineData("-100P-2147483640")>]
[<InlineData("-100P-2147483647")>]
[<InlineData("-100P-9999999999999999999999999")>]
[<InlineData("-0.001P-2147483639")>]
[<InlineData("-0.001P-2147483640")>]
[<InlineData("-0.001P-2147483647")>]
[<InlineData("-0.001P-9999999999999999999999999")>]
let ``floatOfHexString generates floats that are binary equivalent to negative 0`` str =
    let actual = float32OfHexString str
    Assert.Equal(BitConverter.SingleToInt32Bits(-0.0f), BitConverter.SingleToInt32Bits(actual))

[<Theory>]
[<InlineData("0x0123", 0x0123)>]
[<InlineData("0x4567", 0x4567)>]
[<InlineData("0x89ab", 0x89ab)>]
[<InlineData("0x89AB", 0x89ab)>]
[<InlineData("0xcdef", 0xcdef)>]
[<InlineData("0xCDEF", 0xcdef)>]
let ``Not sure what's being tested here`` str expected =
    let actual = float32OfHexString str
    Assert.Equal(single expected, actual)

[<Theory>]
[<InlineData("0x123.456e00p-8")>]
[<InlineData("0x91.a2b700p-7")>]
[<InlineData("0x48.d15b80p-6")>]
[<InlineData("0x24.68adc0p-5")>]
[<InlineData("0x12.3456e0p-4")>]
[<InlineData("0x9.1a2b70p-3")>]
[<InlineData("0x4.8d15b8p-2")>]
[<InlineData("0x2.468adcp-1")>]
[<InlineData("0x.91a2b70p+1")>]
[<InlineData("0x.48d15b8p+2")>]
[<InlineData("0x.2468adcp+3")>]
[<InlineData("0x.123456ep+4")>]
[<InlineData("0x.091a2b70p+5")>]
[<InlineData("0x.048d15b8p+6")>]
[<InlineData("0x.02468adcp+7")>]
[<InlineData("0x.0123456ep+8")>]
let ``Not sure what's being tested here also`` str =
    let expected = float32OfHexString "0x1.23456e"
    let actual = float32OfHexString str
    
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0x1.fffffep127")>]
[<InlineData("0x.1fffffep131")>]
[<InlineData("0x.01fffffep135")>]
[<InlineData("0x1f.ffffep123")>]
[<InlineData("0x1fffffe.p103")>]
[<InlineData("0x0.ffffff5p128")>]
[<InlineData("0x0.ffffff6p128")>]
[<InlineData("0x0.ffffff7p128")>]
[<InlineData("0x0.ffffff7ffffffffp128")>]
let ``floatOfHexString generates max value floats for positive values near max value`` str =
    let actual = float32OfHexString str
    Assert.Equal(Single.MaxValue, actual)

[<Theory>]
[<InlineData("-0x1fffffe000.p91")>]
let ``floatOfHexString generates max value floats for negative values near max value`` str =
    let actual = float32OfHexString str
    Assert.Equal(-Single.MaxValue, actual)

[<Theory>]
[<InlineData("0x0.ffffff8p128")>]
[<InlineData("0x0.ffffff800000p128")>]
[<InlineData("0x0.fffffffp128")>]
[<InlineData("0x1p128")>]
[<InlineData("100P2147483639")>]
[<InlineData("100P2147483640")>]
[<InlineData("100P2147483647")>]
[<InlineData("100P9999999999999999999999999")>]
[<InlineData("0.001P2147483639")>]
[<InlineData("0.001P2147483640")>]
[<InlineData("0.001P2147483647")>]
[<InlineData("0.001P9999999999999999999999999")>]
let ``floatOfHexString throws a System Overflow exception when passed values that are too large`` str =
    Assert.Throws<OverflowException>(fun () -> float32OfHexString str |> ignore)

[<Theory>]
[<InlineData("0x1.000001e")>]
[<InlineData("0x1.000001c")>]
[<InlineData("0x1.000001a")>]
[<InlineData("0x1.0000018")>]
[<InlineData("0x2.0000024p-1")>]
[<InlineData("0x4.0000048p-2")>]
[<InlineData("0x8.0000090p-3")>]
[<InlineData("0x1.0000016")>]
[<InlineData("0x1.0000014")>]
[<InlineData("0x2.0000028p-1")>]
[<InlineData("0x4.0000050p-2")>]
[<InlineData("0x8.00000a0p-3")>]
[<InlineData("0x1.0000012")>]
[<InlineData("0x2.0000024p-1")>]
[<InlineData("0x4.0000048p-2")>]
[<InlineData("0x8.0000090p-3")>]
[<InlineData("0x1.00000110")>]
[<InlineData("0x2.00000220p-1")>]
[<InlineData("0x4.00000440p-2")>]
[<InlineData("0x8.00000880p-3")>]
[<InlineData("0x1.00000108")>]
[<InlineData("0x2.00000210p-1")>]
[<InlineData("0x4.00000420p-2")>]
[<InlineData("0x8.00000840p-3")>]
[<InlineData("0x1.00000104")>]
[<InlineData("0x2.00000208p-1")>]
[<InlineData("0x4.0000041p-2")>]
[<InlineData("0x8.0000082p-3")>]
let ``floatOfHexString handles values near 1`` str =
    let expected = 1.0f + 2.0f*eps
    
    let actual = float32OfHexString str
    
    Assert.Equal(expected, actual)

let nearestValueData : obj[] list =
    [
        [|"0x1.000001" ; 1.f |]
        [|"0x2.000002p-1" ; 1.f |]
        [|"0x4.000004p-2" ; 1.f |]
        [|"0x8.00000p-3" ; 1.f |]
        [|"0x1.00000100000010000"; 1.f + 2.f*eps |]
        [|"0x1.000000ffffffffff"; 1.f |]
        [|"0x1.000000e"; 1.f |]
        [|"0x1.000000c"; 1.f |]
        [|"0x1.000000a"; 1.f |]
        [|"0x1.0000008"; 1.f |]
        [|"0x1.0000006"; 1.f |]
        [|"0x1.0000004"; 1.f |]
        [|"0x1.0000002"; 1.f |]
        [|"0x1.0000000"; 1.f |]
        [|"0x1.fffffffffP-1"; 1.f |]
        [|"0x1.ffffffeP-1"; 1.f |]
        [|"0x1.ffffffcP-1"; 1.f |]
        [|"0x1.ffffffaP-1"; 1.f |]
        [|"0x1.ffffff8P-1"; 1.f |]
        [|"0x1.ffffff6P-1"; 1.f |]
        [|"0x1.ffffff4P-1"; 1.f |]
        [|"0x1.ffffff2P-1"; 1.f |]
        [|"0x1.ffffffP-1"; 1.f |] // round towards even
        [|"0x1.ffffff0000P-1"; 1.f |]
        [|"0x1.fffffefffffP-1"; 1.f - eps |]
        [|"0x1.fffffee0001P-1"; 1.f - eps |]
        [|"0x1.fffffecP-1"; 1.f - eps |]
        [|"0x1.fffffeaP-1"; 1.f - eps |]
        [|"0x1.fffffe8P-1"; 1.f - eps |]
        [|"0x1.fffffe6P-1"; 1.f - eps |]
        [|"0x1.fffffe4P-1"; 1.f - eps |]
        [|"0x1.fffffe2P-1"; 1.f - eps |]
        [|"0x1.fffffe0P-1"; 1.f - eps |]
        [|"0x1.fffffd001P-1"; 1.f - eps |]
        [|"0x0.8000008P-125"; min |]
        [|"0x0.80000080P-125"; min |]
        [|"0x0.8000007ffffP-125"; min |]
        [|"0x0.8000007P-125"; min |]
        [|"0x0.8000006P-125"; min |]
        [|"0x0.8000005P-125"; min |]
        [|"0x0.8000004P-125"; min |]
        [|"0x0.8000003P-125"; min |]
        [|"0x0.8000002P-125"; min |]
        [|"0x0.8000001P-125"; min |]
        [|"0x0.8000000P-125"; min |]
        [|"0x0.7ffffffP-125"; min |]
        [|"0x0.7fffffeP-125"; min |]
        [|"0x0.7fffffdP-125"; min |]
        [|"0x0.7fffffcP-125"; min |]
        [|"0x0.7fffffbP-125"; min |]
        [|"0x0.7fffffaP-125"; min |]
        [|"0x0.7fffff9P-125"; min |]
        [|"0x0.7fffff8P-125"; min |]
        [|"0x0.7fffff7fffP-125"; min - minmin |]
        [|"0x0.7fffff7P-125"; min - minmin |]
        [|"0x0.7fffff6P-125"; min - minmin |]
        [|"0x0.7fffff5P-125"; min - minmin |]
        [|"0x0.7fffff4P-125"; min - minmin |]
        [|"0x0.7fffff3P-125"; min - minmin |]
        [|"0x0.7fffff2P-125"; min - minmin |]
        [|"0x0.7fffff1P-125"; min - minmin |]
        [|"0x0.7fffff0P-125"; min - minmin |]
        [|"0x0.7ffffefP-125"; min - minmin |]
        [|"0x0.7ffffeeP-125"; min - minmin |]
        [|"0x0.7ffffedP-125"; min - minmin |]
        [|"0x0.7ffffecP-125"; min - minmin |]
        [|"0x0.7ffffebP-125"; min - minmin |]
        [|"0x0.7ffffeaP-125"; min - minmin |]
        [|"0x0.7ffffe9P-125"; min - minmin |]
        [|"0x0.7ffffe8001P-125"; min - minmin |]
        [|"0x0.7ffffe8P-125"; min - 2.f*minmin |]
        [|"0x0.7ffffe80P-125"; min - 2.f*minmin |]
        [|"0x0.7ffffe7ffP-125"; min - 2.f*minmin |]
        [|"0x0.0000019P-125"; 2.f*minmin |]
        [|"0x0.0000018P-125"; 2.f*minmin |]
        [|"0x0.0000017ffP-125"; minmin |]
        [|"0x0.000001P-125"; minmin |]
        [|"0x0.0000010P-125"; minmin |]
        [|"0x0.0000008001P-125"; minmin |]
        [|"0x0.0000008P-125"; 0.f |]
        [|"0x0.0000007ffffP-125"; 0.f |]
        [|"0x1P-150"; 0.f |]
    ]

[<Theory>]
[<MemberData(nameof(nearestValueData))>]
let ``floatOfHexString rounds to the nearest even value`` str expected =
    let actual = float32OfHexString str
    
    Assert.Equal(expected, actual)

let zeroValueData : obj[] list =
    [
        [|"0x1.fffffdP-1"   ; 1.f - 2.f*eps |] // round towards zero
        [|"0x1.fffffd0P-1"  ; 1.f - 2.f*eps |]
        [|"0x1.fffffcfffP-1"; 1.f - 2.f*eps |]
        [|"0x1.fffffce0P-1" ; 1.f - 2.f*eps |]
        [|"0x1.fffffc20P-1" ; 1.f - 2.f*eps |]
        [|"0x0.800000fP-125"; min + minmin |]
        [|"0x0.800000eP-125"; min + minmin |]
        [|"0x0.800000dP-125"; min + minmin |]
        [|"0x0.800000cP-125"; min + minmin |]
        [|"0x0.800000bP-125"; min + minmin |]
        [|"0x0.800000aP-125"; min + minmin |]
        [|"0x0.8000009P-125"; min + minmin |]
        [|"0x0.8000008001P-125"; min + minmin |]
    ]

[<Theory>]
[<MemberData(nameof(zeroValueData))>]
let ``floatOfHexString rounds towards zero value`` str expected =
    let actual = float32OfHexString str
    
    Assert.Equal(expected, actual)