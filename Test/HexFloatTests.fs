// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.HexFloatTests

open FParsec.Test.Test

let floatToHexString   = FParsec.CharParsers.floatToHexString
let floatOfHexString   = FParsec.CharParsers.floatOfHexString
let float32ToHexString = FParsec.CharParsers.float32ToHexString
let float32OfHexString = FParsec.CharParsers.float32OfHexString

let testDoubleHexFloat() =
    /// bitwise equal
    let BEqual a b =
        Equal (System.BitConverter.DoubleToInt64Bits(a)) (System.BitConverter.DoubleToInt64Bits(b))

    // float32ToHexString
    ///////////////////

    let max    = System.Double.MaxValue
    let eps    = System.Math.Pow(2.0, -53.0)
    let min    = System.Math.Pow(2.0, -1022.0)  // smallest normal number
    let minmin = System.Math.Pow(2.0, -1074.0) // smallest subnormal number

    floatToHexString 0.0 |> Equal "0x0.0p0"
    floatToHexString -0.0 |> Equal "-0x0.0p0"
    floatToHexString 1.0 |> Equal "0x1.0p0"
    floatToHexString -1.0 |> Equal "-0x1.0p0"
    floatToHexString (1.0 + 4.*eps) |> Equal "0x1.0000000000002p0"
    floatToHexString (1.0 + 2.*eps) |> Equal "0x1.0000000000001p0"
    floatToHexString (1.0 - eps) |> Equal "0x1.fffffffffffffp-1"
    floatToHexString (1.0 - 2.*eps) |> Equal "0x1.ffffffffffffep-1"
    floatToHexString min |> Equal "0x1.0p-1022"
    floatToHexString (min + minmin) |> Equal "0x1.0000000000001p-1022"
    floatToHexString (min - minmin) |> Equal "0x0.fffffffffffffp-1022"
    floatToHexString (min - 2.*minmin) |> Equal "0x0.ffffffffffffep-1022"
    floatToHexString (minmin) |> Equal "0x0.0000000000001p-1022"
    floatToHexString max |> Equal "0x1.fffffffffffffp1023"

    floatToHexString System.Double.PositiveInfinity |> Equal "Infinity"
    floatToHexString System.Double.NegativeInfinity |> Equal "-Infinity"
    floatToHexString System.Double.NaN |> Equal "NaN"


    // floatOfHexString
    ///////////////////

    try floatOfHexString null |> ignore; Fail()
    with :? System.ArgumentNullException -> ()

    let checkFormatError s =
        try floatOfHexString s |> ignore; Fail ()
        with :? System.FormatException -> ()

    checkFormatError ""
    checkFormatError "."
    checkFormatError "p1"
    checkFormatError ".p1"
    checkFormatError "1x1"
    checkFormatError "x1"
    checkFormatError "0xx1"
    checkFormatError "0x/"
    checkFormatError "0x:"
    checkFormatError "0x@"
    checkFormatError "0xG"
    checkFormatError "0x`"
    checkFormatError "0xg"
    checkFormatError "0.1pp1"
    checkFormatError "0.1p+"
    checkFormatError "0.1p-"
    checkFormatError "0.fg"
    checkFormatError "1.0 "
    checkFormatError "1.."
    checkFormatError "1.0."

    floatOfHexString "Inf"      |> Equal System.Double.PositiveInfinity

    floatOfHexString "iNf"      |> Equal System.Double.PositiveInfinity
    floatOfHexString "Infinity" |> Equal System.Double.PositiveInfinity
    floatOfHexString "+InFinITy" |> Equal System.Double.PositiveInfinity
    floatOfHexString "-Inf"      |> Equal (-System.Double.PositiveInfinity)
    floatOfHexString "-InFinITy" |> Equal (-System.Double.PositiveInfinity)
    floatOfHexString "NaN" |> BEqual System.Double.NaN
    floatOfHexString "-nAn" |> BEqual System.Double.NaN
    floatOfHexString "+Nan" |> BEqual System.Double.NaN

    floatOfHexString "001"           |> Equal 1.0
    floatOfHexString "1."            |> Equal 1.0
    floatOfHexString "1.0"           |> Equal 1.0
    floatOfHexString "0x1"           |> Equal 1.0
    floatOfHexString "0X1"           |> Equal 1.0
    floatOfHexString "0x0001"        |> Equal 1.0
    floatOfHexString "0x1."          |> Equal 1.0
    floatOfHexString "0x1.0"         |> Equal 1.0
    floatOfHexString "0x001.0"       |> Equal 1.0
    floatOfHexString "1.0p0"         |> Equal 1.0
    floatOfHexString "1.0P0"         |> Equal 1.0
    floatOfHexString "001.00p+000"   |> Equal 1.0
    floatOfHexString ".100p+004"     |> Equal 1.0
    floatOfHexString ".0100p+008"    |> Equal 1.0
    floatOfHexString "00.100p+004"   |> Equal 1.0
    floatOfHexString "00.0100p+008"  |> Equal 1.0
    floatOfHexString "0010.0p-004"   |> Equal 1.0
    floatOfHexString "0x1.0p0"       |> Equal 1.0
    floatOfHexString "0X1.0P0"       |> Equal 1.0
    floatOfHexString "0x001.00p+000" |> Equal 1.0
    floatOfHexString "0x00.100p+004" |> Equal 1.0
    floatOfHexString "0x.100p+004"   |> Equal 1.0
    floatOfHexString "0x0010.0p-004" |> Equal 1.0

    floatOfHexString "-001"           |> Equal -1.0
    floatOfHexString "-1."            |> Equal -1.0
    floatOfHexString "-1.0"           |> Equal -1.0
    floatOfHexString "-0x1"           |> Equal -1.0
    floatOfHexString "-0X1"           |> Equal -1.0
    floatOfHexString "-0x0001"        |> Equal -1.0
    floatOfHexString "-0x1."          |> Equal -1.0
    floatOfHexString "-0x1.0"         |> Equal -1.0
    floatOfHexString "-0x001.0"       |> Equal -1.0
    floatOfHexString "-1.0p0"         |> Equal -1.0
    floatOfHexString "-1.0P0"         |> Equal -1.0
    floatOfHexString "-001.00p+000"   |> Equal -1.0
    floatOfHexString "-.100p+004"     |> Equal -1.0
    floatOfHexString "-.0100p+008"    |> Equal -1.0
    floatOfHexString "-00.100p+004"   |> Equal -1.0
    floatOfHexString "-00.0100p+008"  |> Equal -1.0
    floatOfHexString "-0010.0p-004"   |> Equal -1.0
    floatOfHexString "-0x1.0p0"       |> Equal -1.0
    floatOfHexString "-0X1.0P0"       |> Equal -1.0
    floatOfHexString "-0x001.00p+000" |> Equal -1.0
    floatOfHexString "-0x00.100p+004" |> Equal -1.0
    floatOfHexString "-0x.100p+004"   |> Equal -1.0
    floatOfHexString "-0x0010.0p-004" |> Equal -1.0

    floatOfHexString "+001"           |> Equal 1.0
    floatOfHexString "+1."            |> Equal 1.0
    floatOfHexString "+1.0"           |> Equal 1.0
    floatOfHexString "+.100p+004"     |> Equal 1.0
    floatOfHexString "+0x0010.0p-004" |> Equal 1.0

    floatOfHexString "0"        |> BEqual 0.
    floatOfHexString "0."       |> BEqual 0.
    floatOfHexString "0.0"      |> BEqual 0.
    floatOfHexString "00.0"     |> BEqual 0.
    floatOfHexString "00.000"   |> BEqual 0.
    floatOfHexString "00.000p0" |> BEqual 0.
    floatOfHexString "00.000p99999999" |> BEqual 0.
    floatOfHexString "0x0"        |> BEqual 0.
    floatOfHexString "0x0."       |> BEqual 0.
    floatOfHexString "0x0.0"      |> BEqual 0.
    floatOfHexString "0x00.0"     |> BEqual 0.
    floatOfHexString "0x00.000"   |> BEqual 0.
    floatOfHexString "0x00.000p0" |> BEqual 0.
    floatOfHexString "0x00.000p99999999" |> BEqual 0.
    floatOfHexString "100P-2147483639"   |> BEqual 0.
    floatOfHexString "100P-2147483640"   |> BEqual 0.
    floatOfHexString "100P-2147483647"   |> BEqual 0.
    floatOfHexString "100P-9999999999999999999999999"   |> BEqual 0.
    floatOfHexString "0.001P-2147483639" |> BEqual 0.
    floatOfHexString "0.001P-2147483640" |> BEqual 0.
    floatOfHexString "0.001P-2147483647" |> BEqual 0.
    floatOfHexString "0.001P-9999999999999999999999999" |> BEqual 0.

    floatOfHexString "-0"        |> BEqual -0.0
    floatOfHexString "-0."       |> BEqual -0.0
    floatOfHexString "-0.0"      |> BEqual -0.0
    floatOfHexString "-00.0"     |> BEqual -0.0
    floatOfHexString "-00.000"   |> BEqual -0.0
    floatOfHexString "-00.000p0" |> BEqual -0.
    floatOfHexString "-00.000p99999999" |> BEqual -0.
    floatOfHexString "-0x0"        |> BEqual -0.0
    floatOfHexString "-0x0."       |> BEqual -0.0
    floatOfHexString "-0x0.0"      |> BEqual -0.0
    floatOfHexString "-0x00.0"     |> BEqual -0.0
    floatOfHexString "-0x00.000"   |> BEqual -0.0
    floatOfHexString "-0x00.000p0" |> BEqual -0.0
    floatOfHexString "-0x00.000p0" |> BEqual -0.
    floatOfHexString "-0x00.000p99999999" |> BEqual -0.
    floatOfHexString "-100P-2147483639"   |> BEqual -0.
    floatOfHexString "-100P-2147483640"   |> BEqual -0.
    floatOfHexString "-100P-2147483647"   |> BEqual -0.
    floatOfHexString "-100P-9999999999999999999999999"   |> BEqual -0.
    floatOfHexString "-0.001P-2147483639" |> BEqual -0.
    floatOfHexString "-0.001P-2147483640" |> BEqual -0.
    floatOfHexString "-0.001P-2147483647" |> BEqual -0.
    floatOfHexString "-0.001P-9999999999999999999999999" |> BEqual -0.

    floatOfHexString "0x0123" |> Equal (double 0x0123)
    floatOfHexString "0x4567" |> Equal (double 0x4567)
    floatOfHexString "0x89ab" |> Equal (double 0x89ab)
    floatOfHexString "0x89AB" |> Equal (double 0x89ab)
    floatOfHexString "0xcdef" |> Equal (double 0xcdef)
    floatOfHexString "0xCDEF" |> Equal (double 0xcdef)

    let v = floatOfHexString "0x1.23456789abcde"

    floatOfHexString "0x123.456789abcde00p-8" |> Equal v
    floatOfHexString "0x91.a2b3c4d5e6f00p-7" |> Equal v
    floatOfHexString "0x48.d159e26af3780p-6" |> Equal v
    floatOfHexString "0x24.68acf13579bc0p-5" |> Equal v
    floatOfHexString "0x12.3456789abcde0p-4" |> Equal v
    floatOfHexString "0x9.1a2b3c4d5e6fp-3" |> Equal v
    floatOfHexString "0x4.8d159e26af378p-2" |> Equal v
    floatOfHexString "0x2.468acf13579bcp-1" |> Equal v
    floatOfHexString "0x.91a2b3c4d5e6f0p+1" |> Equal v
    floatOfHexString "0x.48d159e26af378p+2" |> Equal v
    floatOfHexString "0x.2468acf13579bcp+3" |> Equal v
    floatOfHexString "0x.123456789abcdep+4" |> Equal v
    floatOfHexString "0x.091a2b3c4d5e6f0p+5" |> Equal v
    floatOfHexString "0x.048d159e26af378p+6" |> Equal v
    floatOfHexString "0x.02468acf13579bcp+7" |> Equal v
    floatOfHexString "0x.0123456789abcdep+8" |> Equal v

    // near max
    floatOfHexString "0x1.fffffffffffffp1023"  |> Equal max
    floatOfHexString "0x.1fffffffffffffp1027"  |> Equal max
    floatOfHexString "0x.01fffffffffffffp1031" |> Equal max
    floatOfHexString "0x1f.ffffffffffffp1019" |> Equal max
    floatOfHexString "0x1fffffffffffff.p971" |> Equal max
    floatOfHexString "-0x1fffffffffffff000.p959" |> Equal (-max)

    floatOfHexString "0x1.fffffffffffff5p1023"  |> Equal max
    floatOfHexString "0x1.fffffffffffff6p1023"  |> Equal max
    floatOfHexString "0x1.fffffffffffff7p1023"  |> Equal max
    floatOfHexString "0x1.fffffffffffff7ffffffffp1023"  |> Equal max

    let checkOverflow s =
        try floatOfHexString s |> ignore; Fail ()
        with :? System.OverflowException -> ()

    checkOverflow "0x1.fffffffffffff8p1023"
    checkOverflow "0x1.fffffffffffff800000p1023"
    checkOverflow "0x1.ffffffffffffffp1023"
    checkOverflow "0x1p1024"
    checkOverflow "100P2147483639"
    checkOverflow "100P2147483640"
    checkOverflow "100P2147483647"
    checkOverflow "100P9999999999999999999999999"
    checkOverflow "0.001P2147483639"
    checkOverflow "0.001P2147483640"
    checkOverflow "0.001P2147483647"
    checkOverflow "0.001P9999999999999999999999999"

    // near 1
    floatOfHexString "0x1.0000000000000f" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x1.0000000000000e" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x1.0000000000000d" |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.0000000000000c" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x2.00000000000018p-1" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x4.0000000000003p-2"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x8.0000000000006p-3"  |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.0000000000000b" |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.0000000000000a" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x2.00000000000014p-1" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x4.00000000000028p-2" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x8.0000000000005p-3"  |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.00000000000009"   |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x2.00000000000012p-1"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x4.00000000000024p-2"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x8.00000000000048p-3" |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.000000000000088"    |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x2.00000000000011p-1"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x4.00000000000022p-2"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x8.00000000000044p-3"  |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.000000000000084"    |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x2.000000000000108p-1"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x4.00000000000021p-2"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x8.00000000000042p-3"  |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.000000000000082"    |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x2.000000000000104p-1"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x4.000000000000208p-2"  |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x8.00000000000041p-3"  |> Equal (1.0 + 2.*eps)

    floatOfHexString "0x1.00000000000008"    |> Equal (1.0)  // round towards even
    floatOfHexString "0x2.0000000000001p-1"  |> Equal (1.0)
    floatOfHexString "0x4.0000000000002p-2"  |> Equal (1.0)
    floatOfHexString "0x8.0000000000004p-3"  |> Equal (1.0)

    floatOfHexString "0x1.0000000000000800000010000" |> Equal (1.0 + 2.*eps)
    floatOfHexString "0x1.00000000000007ffffffffff" |> Equal (1.0)
    floatOfHexString "0x1.00000000000007" |> Equal (1.0)
    floatOfHexString "0x1.00000000000006" |> Equal (1.0)
    floatOfHexString "0x1.00000000000005" |> Equal (1.0)
    floatOfHexString "0x1.00000000000004" |> Equal (1.0)
    floatOfHexString "0x1.00000000000003" |> Equal (1.0)
    floatOfHexString "0x1.00000000000002" |> Equal (1.0)
    floatOfHexString "0x1.00000000000001" |> Equal (1.0)
    floatOfHexString "0x1.00000000000000" |> Equal (1.0)
    floatOfHexString "0x1.ffffffffffffffffP-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffffeP-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffffdP-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffffcP-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffffbP-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffffaP-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffff9P-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffff8P-1" |> Equal (1.0) // round towards even
    floatOfHexString "0x1.fffffffffffff800P-1" |> Equal (1.0)
    floatOfHexString "0x1.fffffffffffff7ffffffP-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff7000001P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff7P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff6P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff5P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff4P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff3P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff2P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff1P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.fffffffffffff0P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.ffffffffffffefP-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.ffffffffffffe8001P-1" |> Equal (1.0 - eps)
    floatOfHexString "0x1.ffffffffffffe8P-1" |> Equal (1.0 - 2.*eps) // round towards even
    floatOfHexString "0x1.ffffffffffffe80P-1" |> Equal (1.0 - 2.*eps)
    floatOfHexString "0x1.ffffffffffffe7ffffP-1" |> Equal (1.0 - 2.*eps)
    floatOfHexString "0x1.ffffffffffffe70P-1" |> Equal (1.0 - 2.*eps)
    floatOfHexString "0x1.ffffffffffffe1P-1" |> Equal (1.0 - 2.*eps)

    floatOfHexString "0x1.0000000000000fP-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.0000000000000eP-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.0000000000000dP-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.0000000000000cP-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.0000000000000bP-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.0000000000000aP-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.00000000000009P-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.00000000000008001P-1022" |> Equal (min + minmin)
    floatOfHexString "0x1.00000000000008P-1022" |> Equal (min) // round towards even
    floatOfHexString "0x1.000000000000080P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000007ffffP-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000007P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000006P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000005P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000004P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000003P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000002P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000001P-1022" |> Equal (min)
    floatOfHexString "0x1.00000000000000P-1022" |> Equal (min)
    floatOfHexString "0x0.ffffffffffffffP-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffffeP-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffffdP-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffffcP-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffffbP-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffffaP-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffff9P-1022" |> Equal (min)
    floatOfHexString "0x0.fffffffffffff8P-1022" |> Equal (min) // round towards even
    floatOfHexString "0x0.fffffffffffff7fffP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff7P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff6P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff5P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff4P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff3P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff2P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff1P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.fffffffffffff0P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffefP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffeeP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffedP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffecP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffebP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffeaP-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffe9P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffe8001P-1022" |> Equal (min - minmin)
    floatOfHexString "0x0.ffffffffffffe8P-1022" |> Equal (min - 2.*minmin) // round towards even
    floatOfHexString "0x0.ffffffffffffe80P-1022" |> Equal (min - 2.*minmin) // round towards even
    floatOfHexString "0x0.ffffffffffffe7ffP-1022" |> Equal (min - 2.*minmin)

    floatOfHexString "0x0.00000000000019P-1022" |> Equal (2.*minmin)
    floatOfHexString "0x0.00000000000018P-1022" |> Equal (2.*minmin) // round towards even
    floatOfHexString "0x0.00000000000017ffP-1022" |> Equal (minmin)
    floatOfHexString "0x0.0000000000001P-1022" |> Equal (minmin)
    floatOfHexString "0x0.00000000000010P-1022" |> Equal (minmin)
    floatOfHexString "0x0.00000000000008001P-1022" |> Equal (minmin)
    floatOfHexString "0x0.00000000000008P-1022" |> Equal (0.) // round towards even
    floatOfHexString "0x0.00000000000007ffffP-1022" |> Equal (0.)
    floatOfHexString "0x1.P-1075" |> Equal (0.)

    // round trip checking
    //////////////////////
    let rand = System.Random(123)
    let buffer = Array.zeroCreate 8
    let randomFloat() =
        rand.NextBytes(buffer)
        System.BitConverter.ToDouble(buffer, 0)

    for i = 0 to 100000 do
        let f = randomFloat()
        let s = floatToHexString f
        let f2 = floatOfHexString s
        True (f = f2 || f <> f)


let testSingleHexFloat() =

    /// bitwise equal
    let BEqual (a: float32) (b: float32) =
        Equal (System.BitConverter.GetBytes(a)) (System.BitConverter.GetBytes(b))


    // float32ToHexString
    ///////////////////

    let max    = System.Single.MaxValue
    let eps    = (float32) (System.Math.Pow(2.0, -24.0))
    let min    = (float32) (System.Math.Pow(2.0, -126.0)) // smallest normal number
    let minmin = (float32) (System.Math.Pow(2.0, -149.0)) // smallest subnormal number

    float32ToHexString 0.0f |> Equal "0x0.0p0"
    float32ToHexString -0.0f |> Equal "-0x0.0p0"
    float32ToHexString 1.0f |> Equal "0x1.0p0"
    float32ToHexString -1.0f |> Equal "-0x1.0p0"
    float32ToHexString (1.0f + 4.f*eps) |> Equal "0x1.000004p0"
    float32ToHexString (1.0f + 2.f*eps) |> Equal "0x1.000002p0"
    float32ToHexString (1.0f - eps)     |> Equal "0x1.fffffep-1"
    float32ToHexString (1.0f - 2.f*eps) |> Equal "0x1.fffffcp-1"
    float32ToHexString min |> Equal "0x1.0p-126"
    float32ToHexString (min + minmin)     |> Equal "0x1.000002p-126"
    float32ToHexString (min - minmin)     |> Equal "0x0.fffffep-126"
    float32ToHexString (min - 2.f*minmin) |> Equal "0x0.fffffcp-126"
    float32ToHexString (minmin)           |> Equal "0x0.000002p-126"
    float32ToHexString max |> Equal "0x1.fffffep127"

    float32ToHexString System.Single.PositiveInfinity |> Equal "Infinity"
    float32ToHexString System.Single.NegativeInfinity |> Equal "-Infinity"
    float32ToHexString System.Single.NaN |> Equal "NaN"


    // float32OfHexString
    ///////////////////

    try float32OfHexString null |> ignore; Fail()
    with :? System.ArgumentNullException -> ()

    let checkFormatError s =
        try float32OfHexString s |> ignore; Fail ()
        with :? System.FormatException -> ()

    checkFormatError ""
    checkFormatError "."
    checkFormatError "p1"
    checkFormatError ".p1"
    checkFormatError "1x1"
    checkFormatError "x1"
    checkFormatError "0xx1"
    checkFormatError "0x/"
    checkFormatError "0x:"
    checkFormatError "0x@"
    checkFormatError "0xG"
    checkFormatError "0x`"
    checkFormatError "0xg"
    checkFormatError "0.1pp1"
    checkFormatError "0.1p+"
    checkFormatError "0.1p-"
    checkFormatError "0.fg"
    checkFormatError "1.0 "
    checkFormatError "1.."
    checkFormatError "1.0."

    float32OfHexString "Inf"      |> Equal System.Single.PositiveInfinity

    float32OfHexString "iNf"      |> Equal System.Single.PositiveInfinity
    float32OfHexString "Infinity" |> Equal System.Single.PositiveInfinity
    float32OfHexString "+InFinITy" |> Equal System.Single.PositiveInfinity
    float32OfHexString "-Inf"      |> Equal (-System.Single.PositiveInfinity)
    float32OfHexString "-InFinITy" |> Equal (-System.Single.PositiveInfinity)
    float32OfHexString "NaN" |> BEqual System.Single.NaN
    float32OfHexString "-nAn" |> BEqual System.Single.NaN
    float32OfHexString "+Nan" |> BEqual System.Single.NaN

    float32OfHexString "001"           |> Equal 1.0f
    float32OfHexString "1."            |> Equal 1.0f
    float32OfHexString "1.0"           |> Equal 1.0f
    float32OfHexString "0x1"           |> Equal 1.0f
    float32OfHexString "0X1"           |> Equal 1.0f
    float32OfHexString "0x0001"        |> Equal 1.0f
    float32OfHexString "0x1."          |> Equal 1.0f
    float32OfHexString "0x1.0"         |> Equal 1.0f
    float32OfHexString "0x001.0"       |> Equal 1.0f
    float32OfHexString "1.0p0"         |> Equal 1.0f
    float32OfHexString "1.0P0"         |> Equal 1.0f
    float32OfHexString "001.00p+000"   |> Equal 1.0f
    float32OfHexString ".100p+004"     |> Equal 1.0f
    float32OfHexString ".0100p+008"    |> Equal 1.0f
    float32OfHexString "00.100p+004"   |> Equal 1.0f
    float32OfHexString "00.0100p+008"  |> Equal 1.0f
    float32OfHexString "0010.0p-004"   |> Equal 1.0f
    float32OfHexString "0x1.0p0"       |> Equal 1.0f
    float32OfHexString "0X1.0P0"       |> Equal 1.0f
    float32OfHexString "0x001.00p+000" |> Equal 1.0f
    float32OfHexString "0x00.100p+004" |> Equal 1.0f
    float32OfHexString "0x.100p+004"   |> Equal 1.0f
    float32OfHexString "0x0010.0p-004" |> Equal 1.0f

    float32OfHexString "-001"           |> Equal -1.0f
    float32OfHexString "-1."            |> Equal -1.0f
    float32OfHexString "-1.0"           |> Equal -1.0f
    float32OfHexString "-0x1"           |> Equal -1.0f
    float32OfHexString "-0X1"           |> Equal -1.0f
    float32OfHexString "-0x0001"        |> Equal -1.0f
    float32OfHexString "-0x1."          |> Equal -1.0f
    float32OfHexString "-0x1.0"         |> Equal -1.0f
    float32OfHexString "-0x001.0"       |> Equal -1.0f
    float32OfHexString "-1.0p0"         |> Equal -1.0f
    float32OfHexString "-1.0P0"         |> Equal -1.0f
    float32OfHexString "-001.00p+000"   |> Equal -1.0f
    float32OfHexString "-.100p+004"     |> Equal -1.0f
    float32OfHexString "-.0100p+008"    |> Equal -1.0f
    float32OfHexString "-00.100p+004"   |> Equal -1.0f
    float32OfHexString "-00.0100p+008"  |> Equal -1.0f
    float32OfHexString "-0010.0p-004"   |> Equal -1.0f
    float32OfHexString "-0x1.0p0"       |> Equal -1.0f
    float32OfHexString "-0X1.0P0"       |> Equal -1.0f
    float32OfHexString "-0x001.00p+000" |> Equal -1.0f
    float32OfHexString "-0x00.100p+004" |> Equal -1.0f
    float32OfHexString "-0x.100p+004"   |> Equal -1.0f
    float32OfHexString "-0x0010.0p-004" |> Equal -1.0f

    float32OfHexString "+001"           |> Equal 1.0f
    float32OfHexString "+1."            |> Equal 1.0f
    float32OfHexString "+1.0"           |> Equal 1.0f
    float32OfHexString "+.100p+004"     |> Equal 1.0f
    float32OfHexString "+0x0010.0p-004" |> Equal 1.0f

    float32OfHexString "0"        |> BEqual 0.f
    float32OfHexString "0."       |> BEqual 0.f
    float32OfHexString "0.0"      |> BEqual 0.f
    float32OfHexString "00.0"     |> BEqual 0.f
    float32OfHexString "00.000"   |> BEqual 0.f
    float32OfHexString "00.000p0" |> BEqual 0.f
    float32OfHexString "00.000p99999999" |> BEqual 0.f
    float32OfHexString "0x0"        |> BEqual 0.f
    float32OfHexString "0x0."       |> BEqual 0.f
    float32OfHexString "0x0.0"      |> BEqual 0.f
    float32OfHexString "0x00.0"     |> BEqual 0.f
    float32OfHexString "0x00.000"   |> BEqual 0.f
    float32OfHexString "0x00.000p0" |> BEqual 0.f
    float32OfHexString "0x00.000p99999999" |> BEqual 0.f
    float32OfHexString "100P-2147483639"   |> BEqual 0.f
    float32OfHexString "100P-2147483640"   |> BEqual 0.f
    float32OfHexString "100P-2147483647"   |> BEqual 0.f
    float32OfHexString "100P-9999999999999999999999999"   |> BEqual 0.f
    float32OfHexString "0.001P-2147483639" |> BEqual 0.f
    float32OfHexString "0.001P-2147483640" |> BEqual 0.f
    float32OfHexString "0.001P-2147483647" |> BEqual 0.f
    float32OfHexString "0.001P-9999999999999999999999999" |> BEqual 0.f

    float32OfHexString "-0"        |> BEqual -0.0f
    float32OfHexString "-0."       |> BEqual -0.0f
    float32OfHexString "-0.0"      |> BEqual -0.0f
    float32OfHexString "-00.0"     |> BEqual -0.0f
    float32OfHexString "-00.000"   |> BEqual -0.0f
    float32OfHexString "-00.000p0" |> BEqual -0.f
    float32OfHexString "-00.000p99999999" |> BEqual -0.f
    float32OfHexString "-0x0"        |> BEqual -0.0f
    float32OfHexString "-0x0."       |> BEqual -0.0f
    float32OfHexString "-0x0.0"      |> BEqual -0.0f
    float32OfHexString "-0x00.0"     |> BEqual -0.0f
    float32OfHexString "-0x00.000"   |> BEqual -0.0f
    float32OfHexString "-0x00.000p0" |> BEqual -0.0f
    float32OfHexString "-0x00.000p0" |> BEqual -0.f
    float32OfHexString "-0x00.000p99999999" |> BEqual -0.f
    float32OfHexString "-100P-2147483639"   |> BEqual -0.f
    float32OfHexString "-100P-2147483640"   |> BEqual -0.f
    float32OfHexString "-100P-2147483647"   |> BEqual -0.f
    float32OfHexString "-100P-9999999999999999999999999"   |> BEqual -0.f
    float32OfHexString "-0.001P-2147483639" |> BEqual -0.f
    float32OfHexString "-0.001P-2147483640" |> BEqual -0.f
    float32OfHexString "-0.001P-2147483647" |> BEqual -0.f
    float32OfHexString "-0.001P-9999999999999999999999999" |> BEqual -0.f

    float32OfHexString "0x0123" |> Equal (single 0x0123)
    float32OfHexString "0x4567" |> Equal (single 0x4567)
    float32OfHexString "0x89ab" |> Equal (single 0x89ab)
    float32OfHexString "0x89AB" |> Equal (single 0x89ab)
    float32OfHexString "0xcdef" |> Equal (single 0xcdef)
    float32OfHexString "0xCDEF" |> Equal (single 0xcdef)

    let v = float32OfHexString "0x1.23456e"

    float32OfHexString "0x123.456e00p-8" |> Equal v
    float32OfHexString "0x91.a2b700p-7" |> Equal v
    float32OfHexString "0x48.d15b80p-6" |> Equal v
    float32OfHexString "0x24.68adc0p-5" |> Equal v
    float32OfHexString "0x12.3456e0p-4" |> Equal v
    float32OfHexString "0x9.1a2b70p-3" |> Equal v
    float32OfHexString "0x4.8d15b8p-2" |> Equal v
    float32OfHexString "0x2.468adcp-1" |> Equal v
    float32OfHexString "0x.91a2b70p+1" |> Equal v
    float32OfHexString "0x.48d15b8p+2" |> Equal v
    float32OfHexString "0x.2468adcp+3" |> Equal v
    float32OfHexString "0x.123456ep+4" |> Equal v
    float32OfHexString "0x.091a2b70p+5" |> Equal v
    float32OfHexString "0x.048d15b8p+6" |> Equal v
    float32OfHexString "0x.02468adcp+7" |> Equal v
    float32OfHexString "0x.0123456ep+8" |> Equal v


    // near max
    float32OfHexString "0x1.fffffep127"  |> Equal max
    float32OfHexString "0x.1fffffep131"  |> Equal max
    float32OfHexString "0x.01fffffep135" |> Equal max
    float32OfHexString "0x1f.ffffep123" |> Equal max
    float32OfHexString "0x1fffffe.p103" |> Equal max
    float32OfHexString "-0x1fffffe000.p91" |> Equal (-max)

    float32OfHexString "0x0.ffffff5p128"  |> Equal max
    float32OfHexString "0x0.ffffff6p128"  |> Equal max
    float32OfHexString "0x0.ffffff7p128"  |> Equal max
    float32OfHexString "0x0.ffffff7ffffffffp128" |> Equal max

    let checkOverflow s =
        try float32OfHexString s |> ignore; Fail ()
        with :? System.OverflowException -> ()

    checkOverflow "0x0.ffffff8p128"
    checkOverflow "0x0.ffffff800000p128"
    checkOverflow "0x0.fffffffp128"
    checkOverflow "0x1p128"
    checkOverflow "100P2147483639"
    checkOverflow "100P2147483640"
    checkOverflow "100P2147483647"
    checkOverflow "100P9999999999999999999999999"
    checkOverflow "0.001P2147483639"
    checkOverflow "0.001P2147483640"
    checkOverflow "0.001P2147483647"
    checkOverflow "0.001P9999999999999999999999999"

    // near 1
    float32OfHexString "0x1.000001e" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x1.000001c" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x1.000001a" |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.0000018"    |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x2.0000024p-1" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x4.0000048p-2" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x8.0000090p-3" |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.0000016" |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.0000014" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x2.0000028p-1" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x4.0000050p-2" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x8.00000a0p-3"  |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.0000012"   |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x2.0000024p-1"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x4.0000048p-2"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x8.0000090p-3" |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.00000110"    |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x2.00000220p-1"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x4.00000440p-2"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x8.00000880p-3"  |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.00000108"    |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x2.00000210p-1"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x4.00000420p-2"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x8.00000840p-3"  |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.00000104"    |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x2.00000208p-1" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x4.0000041p-2"  |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x8.0000082p-3"  |> Equal (1.f + 2.f*eps)

    float32OfHexString "0x1.000001"    |> Equal (1.f)  // round towards even
    float32OfHexString "0x2.000002p-1"  |> Equal (1.f)
    float32OfHexString "0x4.000004p-2"  |> Equal (1.f)
    float32OfHexString "0x8.00000p-3"  |> Equal (1.f)

    float32OfHexString "0x1.00000100000010000" |> Equal (1.f + 2.f*eps)
    float32OfHexString "0x1.000000ffffffffff" |> Equal (1.f)
    float32OfHexString "0x1.000000e" |> Equal (1.f)
    float32OfHexString "0x1.000000c" |> Equal (1.f)
    float32OfHexString "0x1.000000a" |> Equal (1.f)
    float32OfHexString "0x1.0000008" |> Equal (1.f)
    float32OfHexString "0x1.0000006" |> Equal (1.f)
    float32OfHexString "0x1.0000004" |> Equal (1.f)
    float32OfHexString "0x1.0000002" |> Equal (1.f)
    float32OfHexString "0x1.0000000" |> Equal (1.f)
    float32OfHexString "0x1.fffffffffP-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffffeP-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffffcP-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffffaP-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffff8P-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffff6P-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffff4P-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffff2P-1" |> Equal (1.f)
    float32OfHexString "0x1.ffffffP-1" |> Equal (1.f) // round towards even
    float32OfHexString "0x1.ffffff0000P-1" |> Equal (1.f)
    float32OfHexString "0x1.fffffefffffP-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffee0001P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffecP-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffeaP-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffe8P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffe6P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffe4P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffe2P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffe0P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffd001P-1" |> Equal (1.f - eps)
    float32OfHexString "0x1.fffffdP-1"    |> Equal (1.f - 2.f*eps) // round towards zero
    float32OfHexString "0x1.fffffd0P-1"   |> Equal (1.f - 2.f*eps)
    float32OfHexString "0x1.fffffcfffP-1" |> Equal (1.f - 2.f*eps)
    float32OfHexString "0x1.fffffce0P-1"  |> Equal (1.f - 2.f*eps)
    float32OfHexString "0x1.fffffc20P-1"  |> Equal (1.f - 2.f*eps)

    float32OfHexString "0x0.800000fP-125" |> Equal (min + minmin)
    float32OfHexString "0x0.800000eP-125" |> Equal (min + minmin)
    float32OfHexString "0x0.800000dP-125" |> Equal (min + minmin)
    float32OfHexString "0x0.800000cP-125" |> Equal (min + minmin)
    float32OfHexString "0x0.800000bP-125" |> Equal (min + minmin)
    float32OfHexString "0x0.800000aP-125" |> Equal (min + minmin)
    float32OfHexString "0x0.8000009P-125" |> Equal (min + minmin)
    float32OfHexString "0x0.8000008001P-125" |> Equal (min + minmin)
    float32OfHexString "0x0.8000008P-125" |> Equal (min) // round towards even
    float32OfHexString "0x0.80000080P-125" |> Equal (min)
    float32OfHexString "0x0.8000007ffffP-125" |> Equal (min)
    float32OfHexString "0x0.8000007P-125" |> Equal (min)
    float32OfHexString "0x0.8000006P-125" |> Equal (min)
    float32OfHexString "0x0.8000005P-125" |> Equal (min)
    float32OfHexString "0x0.8000004P-125" |> Equal (min)
    float32OfHexString "0x0.8000003P-125" |> Equal (min)
    float32OfHexString "0x0.8000002P-125" |> Equal (min)
    float32OfHexString "0x0.8000001P-125" |> Equal (min)
    float32OfHexString "0x0.8000000P-125" |> Equal (min)
    float32OfHexString "0x0.7ffffffP-125" |> Equal (min)
    float32OfHexString "0x0.7fffffeP-125" |> Equal (min)
    float32OfHexString "0x0.7fffffdP-125" |> Equal (min)
    float32OfHexString "0x0.7fffffcP-125" |> Equal (min)
    float32OfHexString "0x0.7fffffbP-125" |> Equal (min)
    float32OfHexString "0x0.7fffffaP-125" |> Equal (min)
    float32OfHexString "0x0.7fffff9P-125" |> Equal (min)
    float32OfHexString "0x0.7fffff8P-125" |> Equal (min) // round towards even
    float32OfHexString "0x0.7fffff7fffP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff7P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff6P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff5P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff4P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff3P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff2P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff1P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7fffff0P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffefP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffeeP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffedP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffecP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffebP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffeaP-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffe9P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffe8001P-125" |> Equal (min - minmin)
    float32OfHexString "0x0.7ffffe8P-125" |> Equal (min - 2.f*minmin) // round towards even
    float32OfHexString "0x0.7ffffe80P-125" |> Equal (min - 2.f*minmin) // round towards even
    float32OfHexString "0x0.7ffffe7ffP-125" |> Equal (min - 2.f*minmin)

    float32OfHexString "0x0.0000019P-125" |> Equal (2.f*minmin)
    float32OfHexString "0x0.0000018P-125" |> Equal (2.f*minmin) // round towards even
    float32OfHexString "0x0.0000017ffP-125" |> Equal (minmin)
    float32OfHexString "0x0.000001P-125" |> Equal (minmin)
    float32OfHexString "0x0.0000010P-125" |> Equal (minmin)
    float32OfHexString "0x0.0000008001P-125" |> Equal (minmin)
    float32OfHexString "0x0.0000008P-125" |> Equal (0.f) // round towards even
    float32OfHexString "0x0.0000007ffffP-125" |> Equal (0.f)
    float32OfHexString "0x1P-150" |> Equal (0.f)

    // round trip checking
    //////////////////////
    let rand = System.Random(123)
    let buffer = Array.zeroCreate 4
    let randomFloat32() =
        rand.NextBytes(buffer)
        System.BitConverter.ToSingle(buffer, 0)

    for i = 0 to 100000 do
        let f = randomFloat32()
        let s = float32ToHexString f
        let f2 = float32OfHexString s
        True (f = f2 || f <> f)

let run() =
    testDoubleHexFloat()
    testSingleHexFloat()