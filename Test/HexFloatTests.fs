// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.HexFloatTests

open System
open Microsoft.FSharp.Core
open Xunit

let floatToHexString   = FParsec.CharParsers.floatToHexString
let floatOfHexString   = FParsec.CharParsers.floatOfHexString
let eps    = Math.Pow(2.0, -53.0)
let min    = Math.Pow(2.0, -1022.0)  // smallest normal number
let minmin = Math.Pow(2.0, -1074.0) // smallest subnormal number


[<FParsecProperty(MaxTest = 100000)>]
let ``floatToHexString output matches test oracle implementation`` (f: float) =
    floatToHexString f === Float.toHexString f

[<FParsecProperty>]
let ``floatToHexString returns strings in the correct format`` (FiniteFloat f) =
    floatToHexString f .* @"(-?0x[01].[0-9a-fA-F]*p-?[0-9]*$)"

[<FParsecProperty(MaxTest = 100000)>]
let ``floatToHexString and floatOfHexString functions are reciprocal`` (FiniteFloat f) =
    (floatToHexString f |> floatOfHexString) === f

[<FParsecProperty>]
let ``floatToHexString returns strings that start with '0' for positive finite values`` (PositiveFiniteFloat f) =
    let expected = '0'
    
    let actual = (floatToHexString f)[0]
    
    expected === actual

[<FParsecProperty>]
let ``Hex String starts with '-' for negative finite values`` (NegativeFiniteFloat f) =
    let expected = '-'
    
    let actual = (floatToHexString f)[0]
    
    expected === actual

[<Fact>]
let ``floatToHexString returns 'Infinity' for a positive infinity value`` () =
    Assert.Equal("Infinity", floatToHexString Double.PositiveInfinity)

[<Fact>]
let ``floatToHexString returns '-Infinity' for a negative infinity value`` () =
    Assert.Equal("-Infinity", floatToHexString Double.NegativeInfinity)

[<Fact>]
let ``floatToHexString returns 'NaN' for an invalid float value`` () =
    Assert.Equal("NaN", floatToHexString Double.NaN)

[<Fact>]
let ``Positive Zero exponent`` () =
    Assert.Equal("0x0.0p0", floatToHexString 0.0)

[<Fact>]
let ``Negative Zero exponent`` () =
    Assert.Equal("-0x0.0p0", floatToHexString -0.0)

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
    Assert.Throws<FormatException>(fun () -> floatOfHexString str |> ignore)

[<Fact>]
let ``floatOfHexString correctly rejects a null string`` () =
    Assert.Throws<ArgumentNullException>(fun () -> floatOfHexString null |> ignore)

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
    Assert.Equal(expected, floatOfHexString str)

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
    Assert.Equal(1.0, floatOfHexString str)

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
    Assert.Equal(-1.0, floatOfHexString str)

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
    let actual = floatOfHexString str
    Assert.Equal(BitConverter.DoubleToInt64Bits(0.0), BitConverter.DoubleToInt64Bits(actual))

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
    let actual = floatOfHexString str
    Assert.Equal(BitConverter.DoubleToInt64Bits(-0.0), BitConverter.DoubleToInt64Bits(actual))

[<Theory>]
[<InlineData("0x0123", 0x0123)>]
[<InlineData("0x4567", 0x4567)>]
[<InlineData("0x89ab", 0x89ab)>]
[<InlineData("0x89AB", 0x89ab)>]
[<InlineData("0xcdef", 0xcdef)>]
[<InlineData("0xCDEF", 0xcdef)>]
let ``Not sure what's being tested here`` str expected =
    let actual = floatOfHexString str
    Assert.Equal(double expected, actual)

[<Theory>]
[<InlineData("0x123.456789abcde00p-8")>]
[<InlineData("0x91.a2b3c4d5e6f00p-7")>]
[<InlineData("0x48.d159e26af3780p-6")>]
[<InlineData("0x24.68acf13579bc0p-5")>]
[<InlineData("0x12.3456789abcde0p-4")>]
[<InlineData("0x9.1a2b3c4d5e6fp-3")>]
[<InlineData("0x4.8d159e26af378p-2")>]
[<InlineData("0x2.468acf13579bcp-1")>]
[<InlineData("0x.91a2b3c4d5e6f0p+1")>]
[<InlineData("0x.48d159e26af378p+2")>]
[<InlineData("0x.2468acf13579bcp+3")>]
[<InlineData("0x.123456789abcdep+4")>]
[<InlineData("0x.091a2b3c4d5e6f0p+5")>]
[<InlineData("0x.048d159e26af378p+6")>]
[<InlineData("0x.02468acf13579bcp+7")>]
[<InlineData("0x.0123456789abcdep+8")>]
let ``Not sure what's being tested here also`` str =
    let expected = floatOfHexString "0x1.23456789abcde"
    let actual = floatOfHexString str
    
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("0x1.fffffffffffffp1023")>]
[<InlineData("0x.1fffffffffffffp1027")>]
[<InlineData("0x.01fffffffffffffp1031")>]
[<InlineData("0x1f.ffffffffffffp1019")>]
[<InlineData("0x1fffffffffffff.p971")>]
[<InlineData("0x1.fffffffffffff5p1023")>]
[<InlineData("0x1.fffffffffffff6p1023")>]
[<InlineData("0x1.fffffffffffff7p1023")>]
[<InlineData("0x1.fffffffffffff7ffffffffp1023")>]
let ``floatOfHexString generates max value floats for positive values near max value`` str =
    let actual = floatOfHexString str
    Assert.Equal(Double.MaxValue, actual)

[<Theory>]
[<InlineData("-0x1fffffffffffff000.p959")>]
let ``floatOfHexString generates max value floats for negative values near max value`` str =
    let actual = floatOfHexString str
    Assert.Equal(-Double.MaxValue, actual)

[<Theory>]
[<InlineData("0x1.fffffffffffff8p1023")>]
[<InlineData("0x1.fffffffffffff800000p1023")>]
[<InlineData("0x1.ffffffffffffffp1023")>]
[<InlineData("0x1p1024")>]
[<InlineData("100P2147483639")>]
[<InlineData("100P2147483640")>]
[<InlineData("100P2147483647")>]
[<InlineData("100P9999999999999999999999999")>]
[<InlineData("0.001P2147483639")>]
[<InlineData("0.001P2147483640")>]
[<InlineData("0.001P2147483647")>]
[<InlineData("0.001P9999999999999999999999999")>]
let ``floatOfHexString throws a System Overflow exception when passed values that are too large`` str =
    Assert.Throws<OverflowException>(fun () -> floatOfHexString str |> ignore)

[<Theory>]
[<InlineData("0x1.0000000000000f")>]
[<InlineData("0x1.0000000000000e")>]
[<InlineData("0x1.0000000000000d")>]
[<InlineData("0x1.0000000000000c")>]
[<InlineData("0x2.00000000000018p-1")>] 
[<InlineData("0x4.0000000000003p-2")>]
[<InlineData("0x8.0000000000006p-3")>]
[<InlineData("0x1.0000000000000b")>]
[<InlineData("0x1.0000000000000a")>]
[<InlineData("0x2.00000000000014p-1")>]
[<InlineData("0x4.00000000000028p-2")>]
[<InlineData("0x8.0000000000005p-3")>]
[<InlineData("0x1.00000000000009")>]
[<InlineData("0x2.00000000000012p-1")>]
[<InlineData("0x4.00000000000024p-2")>]
[<InlineData("0x8.00000000000048p-3")>]
[<InlineData("0x1.000000000000088")>]
[<InlineData("0x2.00000000000011p-1")>]
[<InlineData("0x4.00000000000022p-2")>]
[<InlineData("0x8.00000000000044p-3")>]
[<InlineData("0x1.000000000000084")>]
[<InlineData("0x2.000000000000108p-1")>]
[<InlineData("0x4.00000000000021p-2")>]
[<InlineData("0x8.00000000000042p-3")>]
[<InlineData("0x1.000000000000082")>]
[<InlineData("0x2.000000000000104p-1")>]
[<InlineData("0x4.000000000000208p-2")>]
[<InlineData("0x8.00000000000041p-3")>]
[<InlineData("0x1.0000000000000800000010000")>]
let ``floatOfHexString handles values near 1`` str =
    let expected = 1.0 + 2.*eps
    
    let actual = floatOfHexString str
    
    Assert.Equal(expected, actual)

let nearestValueData : obj[] list =
    [
        [| "0x1.00000000000008"   ; 1.0 |]
        [| "0x2.0000000000001p-1" ; 1.0 |]
        [| "0x4.0000000000002p-2" ; 1.0 |]
        [| "0x8.0000000000004p-3" ; 1.0 |]
        [| "0x1.00000000000007ffffffffff"; 1.0 |]
        [| "0x1.00000000000007"; 1.0 |]
        [| "0x1.00000000000006"; 1.0 |]
        [| "0x1.00000000000005"; 1.0 |]
        [| "0x1.00000000000004"; 1.0 |]
        [| "0x1.00000000000003"; 1.0 |]
        [| "0x1.00000000000002"; 1.0 |]
        [| "0x1.00000000000001"; 1.0 |]
        [| "0x1.00000000000000"; 1.0 |]
        [| "0x1.ffffffffffffffffP-1"; 1.0 |]
        [| "0x1.fffffffffffffeP-1"; 1.0 |]
        [| "0x1.fffffffffffffdP-1"; 1.0 |]
        [| "0x1.fffffffffffffcP-1"; 1.0 |]
        [| "0x1.fffffffffffffbP-1"; 1.0 |]
        [| "0x1.fffffffffffffaP-1"; 1.0 |]
        [| "0x1.fffffffffffff9P-1"; 1.0 |]
        [| "0x1.fffffffffffff8P-1"; 1.0 |]
        [| "0x1.fffffffffffff800P-1"; 1.0 |]
        [| "0x1.fffffffffffff7ffffffP-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff7000001P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff7P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff6P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff5P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff4P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff3P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff2P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff1P-1"; 1.0 - eps |]
        [| "0x1.fffffffffffff0P-1"; 1.0 - eps |]
        [| "0x1.ffffffffffffefP-1"; 1.0 - eps |]
        [| "0x1.ffffffffffffe8001P-1"; 1.0 - eps |]
        [| "0x1.ffffffffffffe8P-1"; 1.0 - 2.*eps |]
        [| "0x1.ffffffffffffe80P-1"; 1.0 - 2.*eps |]
        [| "0x1.ffffffffffffe7ffffP-1"; 1.0 - 2.*eps |]
        [| "0x1.ffffffffffffe70P-1"; 1.0 - 2.*eps |]
        [| "0x1.ffffffffffffe1P-1"; 1.0 - 2.*eps |]
        [| "0x1.0000000000000fP-1022"; min + minmin |]
        [| "0x1.0000000000000eP-1022"; min + minmin |]
        [| "0x1.0000000000000dP-1022"; min + minmin |]
        [| "0x1.0000000000000cP-1022"; min + minmin |]
        [| "0x1.0000000000000bP-1022"; min + minmin |]
        [| "0x1.0000000000000aP-1022"; min + minmin |]
        [| "0x1.00000000000009P-1022"; min + minmin |]
        [| "0x1.00000000000008001P-1022"; min + minmin |]
        [| "0x1.00000000000008P-1022"; min |]
        [| "0x1.000000000000080P-1022"; min |]
        [| "0x1.00000000000007ffffP-1022"; min |]
        [| "0x1.00000000000007P-1022"; min |]
        [| "0x1.00000000000006P-1022"; min |]
        [| "0x1.00000000000005P-1022"; min |]
        [| "0x1.00000000000004P-1022"; min |]
        [| "0x1.00000000000003P-1022"; min |]
        [| "0x1.00000000000002P-1022"; min |]
        [| "0x1.00000000000001P-1022"; min |]
        [| "0x1.00000000000000P-1022"; min |]
        [| "0x0.ffffffffffffffP-1022"; min |]
        [| "0x0.fffffffffffffeP-1022"; min |]
        [| "0x0.fffffffffffffdP-1022"; min |]
        [| "0x0.fffffffffffffcP-1022"; min |]
        [| "0x0.fffffffffffffbP-1022"; min |]
        [| "0x0.fffffffffffffaP-1022"; min |]
        [| "0x0.fffffffffffff9P-1022"; min |]
        [| "0x0.fffffffffffff8P-1022"; min |]
        [| "0x0.fffffffffffff7fffP-1022"; min - minmin |]
        [| "0x0.fffffffffffff7P-1022"; min - minmin |]
        [| "0x0.fffffffffffff6P-1022"; min - minmin |]
        [| "0x0.fffffffffffff5P-1022"; min - minmin |]
        [| "0x0.fffffffffffff4P-1022"; min - minmin |]
        [| "0x0.fffffffffffff3P-1022"; min - minmin |]
        [| "0x0.fffffffffffff2P-1022"; min - minmin |]
        [| "0x0.fffffffffffff1P-1022"; min - minmin |]
        [| "0x0.fffffffffffff0P-1022"; min - minmin |]
        [| "0x0.ffffffffffffefP-1022"; min - minmin |]
        [| "0x0.ffffffffffffeeP-1022"; min - minmin |]
        [| "0x0.ffffffffffffedP-1022"; min - minmin |]
        [| "0x0.ffffffffffffecP-1022"; min - minmin |]
        [| "0x0.ffffffffffffebP-1022"; min - minmin |]
        [| "0x0.ffffffffffffeaP-1022"; min - minmin |]
        [| "0x0.ffffffffffffe9P-1022"; min - minmin |]
        [| "0x0.ffffffffffffe8001P-1022"; min - minmin |]
        [| "0x0.ffffffffffffe8P-1022"; min - 2.*minmin |]
        [| "0x0.ffffffffffffe80P-1022"; min - 2.*minmin |]
        [| "0x0.ffffffffffffe7ffP-1022"; min - 2.*minmin |]
        [| "0x0.00000000000019P-1022"; 2.*minmin |]
        [| "0x0.00000000000018P-1022"; 2.*minmin |]
        [| "0x0.00000000000017ffP-1022"; minmin |]
        [| "0x0.0000000000001P-1022"; minmin |]
        [| "0x0.00000000000010P-1022"; minmin |]
        [| "0x0.00000000000008001P-1022"; minmin |]
        [| "0x0.00000000000008P-1022"; 0. |]
        [| "0x0.00000000000007ffffP-1022"; 0. |]
        [| "0x1.P-1075"; 0. |]
    ]

[<Theory>]
[<MemberData(nameof(nearestValueData))>]
let ``floatOfHexString rounds to the nearest even value`` str expected =
    let actual = floatOfHexString str
    
    Assert.Equal(expected, actual)