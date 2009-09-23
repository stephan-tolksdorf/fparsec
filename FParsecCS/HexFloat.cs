// Copyright (c) Stephan Tolksdorf 2008-2009
// License: Simplified BSD License. See accompanying documentation.

using System;

namespace FParsec {

internal class HexFloat {
// see the documentation of floatToHexString and floatOfHexString for more info

#pragma warning disable 0429 // unreachable expression code
#pragma warning disable 0162 // unreachable code

// The non-LOW_TRUST code in this class relies on the endianness of floating-point
// numbers in memory being the same as the normal platform endianness,
// i.e. on *((uint*)(&s)) and *((ulong*)(&d)) returning the correct IEEE-754 bit
// representation of the single and double precision numbers s and d.
// I'm not aware of any .NET/Mono platform where this is not the case.
// In the unlikely event anyone ever runs this code on a platform where
// this is not the case the unit tests will detect the problem.

#if !LOW_TRUST
    unsafe
#endif
public static string DoubleToHexString(double x) {
    const int expBits = 11;  // bits for biased exponent
    const int maxBits = 53;  // significant bits (including implicit bit)
    const int maxChars = 24; // "-0x1.fffffffffffffp-1022"

    const int maxBiasedExp = (1 << expBits) - 1;
    const int maxExp       = 1 << (expBits - 1); // max n for which 0.5*2^n is a double
    const int bias = maxExp - 1;

    const int maxFractNibbles = (maxBits - 1)/4 + (((maxBits - 1)%4) == 0 ? 0 : 1);
    const ulong mask  = (1UL << (maxBits - 1)) - 1; // mask for lower (maxBits - 1) bits

#if LOW_TRUST
    ulong xn = unchecked((ulong)BitConverter.DoubleToInt64Bits(x));
#else
    ulong xn  = *((ulong*)(&x)); // reinterpret double as ulong
#endif
    int sign = (int)(xn >> (maxBits - 1 + expBits));
    int e = (int)((xn >> (maxBits - 1)) & maxBiasedExp); // the biased exponent
    ulong s  = xn & mask; // the significand (without the implicit bit)
    if (e < maxBiasedExp) {
        if (e == 0 && s == 0) return sign == 0 ? "0x0.0p0" : "-0x0.0p0";
        char[] str = new char[maxChars];
        int i = 0;
        if (sign != 0) str[i++] = '-';
        str[i++] = '0'; str[i++] = 'x';
        str[i++] = e > 0 ? '1' : '0';
        str[i++] = '.';
        if ((maxBits - 1)%4 > 0) { // normalize fraction to multiple of 4 bits
            s <<= 4 - (maxBits - 1)%4;
        }
        int lastNonNull = i;
        for (int j = 0; j < maxFractNibbles; ++j) {
            int h = unchecked((int) (s >> ((maxFractNibbles - 1 - j) << 2))) & 0xf;
            if (h != 0) lastNonNull = i;
            str[i++] = "0123456789abcdef"[h];
        }
        i = lastNonNull + 1;
        str[i++] = 'p';
        if (e >= bias) e -= bias;
        else {
            str[i++] = '-';
            e = e > 0 ? -(e - bias) : bias - 1;
        }
        // e holds absolute unbiased exponent
        int li = e < 10 ? 1 : (e < 100 ? 2 : (e < 1000 ? 3 : 4)); // floor(log(10, e))) + 1
        i += li;
        do {
            int r = e%10; e = e/10;
            str[--i] = (char) (48 + r);
        } while (e > 0);
        i += li;
        return new String(str, 0, i);
    } else {
        if (s == 0) return sign == 0 ? "Infinity" : "-Infinity";
        else return "NaN";
    }
}

#if !LOW_TRUST
    unsafe
#endif
public static string SingleToHexString(float x) {
    const int expBits = 8;   // bits for biased exponent
    const int maxBits = 24;  // significant bits (including implicit bit)
    const int maxChars = 16; // "-0x1.fffffep-126"

    const int maxBiasedExp = (1 << expBits) - 1;
    const int maxExp       = 1 << (expBits - 1); // max n for which 0.5*2^n is a double
    const int bias = maxExp - 1;

    const int maxFractNibbles = (maxBits - 1)/4 + (((maxBits - 1)%4) == 0 ? 0 : 1);
    const uint mask = (1U << (maxBits - 1)) - 1; // mask for lower (maxBits - 1) bits

#if LOW_TRUST
    uint xn = BitConverter.ToUInt32(BitConverter.GetBytes(x), 0);
#else
    uint xn = *((uint*)(&x)); // reinterpret float as ulong
#endif
    int sign = (int)(xn >> (maxBits - 1 + expBits));
    int e = (int)((xn >> (maxBits - 1)) & maxBiasedExp); // the biased exponent
    uint s = xn & mask; // the significand (without the implicit bit)
    if (e < maxBiasedExp) {
        if (e == 0 && s == 0) return sign == 0 ? "0x0.0p0" : "-0x0.0p0";
        char[] str = new char[maxChars];
        int i = 0;
        if (sign != 0) str[i++] = '-';
        str[i++] = '0'; str[i++] = 'x';
        str[i++] = e > 0 ? '1' : '0';
        str[i++] = '.';
        int lastNonNull = i;
        if ((maxBits - 1)%4 > 0) { // normalize fraction to multiple of 4 bits
            s <<= 4 - (maxBits - 1)%4;
        }
        for (int j = 0; j < maxFractNibbles; ++j) {
            int h = (int)(s >> ((maxFractNibbles - 1 - j) << 2)) & 0xf;
            if (h != 0) lastNonNull = i;
            str[i++] = "0123456789abcdef"[h];
        }
        i = lastNonNull + 1;
        str[i++] = 'p';
        if (e >= bias) e -= bias;
        else {
            str[i++] = '-';
            e = e > 0 ? -(e - bias) : bias - 1;
        }
        // e holds absolute unbiased exponent
        int li = e < 10 ? 1 : (e < 100 ? 2 : 3); // floor(log(10, e))) + 1
        i += li;
        do {
            int r = e%10; e = e/10;
            str[--i] = (char)(48 + r);
        } while (e > 0);
        i += li;
        return new String(str, 0, i);
    } else {
        if (s == 0) return sign == 0 ? "Infinity" : "-Infinity";
        else return "NaN";
    }
}

#pragma warning restore 0429
#pragma warning restore 0162

#if !LOW_TRUST
    unsafe
#endif
public static double DoubleFromHexString(string str) {
    const int expBits = 11;    // bits for exponent
    const int maxBits = 53;    // significant bits (including implicit bit)

    const int maxExp = 1 << (expBits - 1); // max n for which 0.5*2^n is a double
    const int minExp = -maxExp + 3; // min n for which 0.5*2^n is a normal double
    const int minSExp = minExp - (maxBits - 1); // min n for which 0.5*2^n is a subnormal double

    const int maxBits2 = maxBits + 2;
    const ulong mask  = (1UL << (maxBits - 1)) - 1; // mask for lower (maxBits - 1) bits

    if (str == null) throw new ArgumentNullException("string");
    int n = str.Length;
    if (n == 0) goto InvalidFormat;

    if (n > (int.MaxValue + minSExp - 1 - 9)/4) {// n*4 <= Int32.MaxValue protects against an nBits overflow
        throw new System.FormatException("The given hexadecimal string representation of a double precision float is too long.");
    }
    int sign = 0;   // 0 == positive, 1 == negative
    ulong xn = 0;    // integer significand with up to maxBits + 2 bits, where the (maxBits + 2)th bit
                    // (the least significant bit) is the logical OR of the (maxBits + 2)th and all following input bits
    int nBits = -1; // number of bits in xn, not counting leading zeros
    int exp = 0;    // the base-2 exponent
#if LOW_TRUST
    var s = str;
#else
    fixed (char* s = str) {
#endif
        int i = 0;
        // sign
        if (s[0] == '+') i = 1;
        else if (s[0] == '-') {
            i = 1;
            sign = 1;
        }
        // "0x" prefix
        if (i + 1 < n && (s[i + 1] == 'x' || s[i + 1] == 'X')) {
            if (s[i] != '0') goto InvalidFormat;
            i += 2;
        }
        bool pastDot = false;
        for (;;) {
            if (i == n) {
                if (!pastDot) exp = nBits;
                if (nBits >= 0) break;
                else goto InvalidFormat;
            }
            char c = s[i++];
            if (c <= '9' ? c >= '0' : (c <= 'f' && (c >= 'a' || (c >= 'A' && c <= 'F')))) {
                int h = c;
                h = (h & 15) + (h >> 6)*9; // converts hex char to int
                if (nBits <= 0 ) {
                    xn |= (uint)h;
                    nBits = 0;
                    while (h > 0) {
                        ++nBits;
                        h >>= 1;
                    }
                    if (pastDot) exp -= 4 - nBits;
                } else if (nBits <= maxBits2 - 4) {
                    xn <<= 4;
                    xn |= (uint)h;
                    nBits += 4;
                } else if (nBits < maxBits2) {
                    int nRemBits = maxBits2 - nBits;
                    int nSurplusBits = 4 - nRemBits;
                    int surplusBits = h & (0xf >> nRemBits);
                    surplusBits = (0xfffe >> surplusBits) & 1; // == surplusBits != 0 ? 1 : 0
                    xn <<= nRemBits;
                    xn |= (uint)((h >> nSurplusBits) | (surplusBits));
                    nBits += 4;
                } else {
                    xn |= (uint)((0xfffe >> h) & 1); // (0xfffe >> h) & 1 == h != 0 ? 1 : 0
                    nBits += 4;
                }
            } else if (c == '.') {
                if (pastDot) goto InvalidFormat;
                pastDot = true;
                exp = nBits >= 0 ? nBits : 0; // exponent for integer part of float
            } else if ((c == 'p' || c == 'P') && nBits >= 0) {
                if (!pastDot) exp = nBits;
                int eSign = 1;
                if (i < n && (s[i] == '-' || s[i] == '+')) {
                    if (s[i] == '-') eSign = -1;
                    ++i;
                }
                if (i == n) goto InvalidFormat;
                int e = 0;
                do {
                    c = s[i++];
                    if ('0' <= c && c <= '9') {
                        if (e <= (int.MaxValue - 9)/10) e = e*10 + (c - '0');
                        else e = int.MaxValue - 8;
                    } else goto InvalidFormat;
                } while (i < n);
                e*= eSign;
                // |either e is exact, or |e| >= int.MaxValue - 8
                // |exp| <= n*4 <= Int32.MaxValue - |minSExp - 1| - 9
                // either e is exact or |exp + e| >= |minSExp - 1| + 1
                try {
                    exp += e;
                } catch (System.OverflowException) {
                    exp = e < 0 ? int.MinValue : int.MaxValue;
                }
                break;
            } else {
                --i;
                if (nBits == -1 && i + 3 <= n) {
                    if (   (s[i    ] == 'i' || s[i]     == 'I')
                        && (s[i + 1] == 'n' || s[i + 1] == 'N')
                        && (s[i + 2] == 'f' || s[i + 2] == 'F')
                        && (i + 3 == n
                            || (i + 8 == n && (s[i + 3] == 'i' || s[i + 3] == 'I')
                                           && (s[i + 4] == 'n' || s[i + 4] == 'N')
                                           && (s[i + 5] == 'i' || s[i + 5] == 'I')
                                           && (s[i + 6] == 't' || s[i + 6] == 'T')
                                           && (s[i + 7] == 'y' || s[i + 7] == 'Y'))))
                    {
                        return sign == 0 ? Double.PositiveInfinity : Double.NegativeInfinity;
                    } else if (i + 3 == n && (s[i]     == 'n' || s[i]     == 'N')
                                          && (s[i + 1] == 'a' || s[i + 1] == 'A')
                                          && (s[i + 2] == 'n' || s[i + 2] == 'N'))
                    {
                        return Double.NaN;
                    }
                }
                goto InvalidFormat;
            }
        } // for
#if !LOW_TRUST
    } // fixed
#endif
    if (nBits == 0) return sign == 0 ? 0.0 : -0.0;
    if (exp <= maxExp) {
        if (exp >= minExp && nBits <= maxBits) {
            // not subnormal and no rounding is required
            if (nBits < maxBits) xn <<= maxBits - nBits; // normalize significand to maxBits
            xn &= mask; // mask out lower (maxBits - 1) bits, the most significant bit is encoded in exp
        } else {
            if (nBits < maxBits2) xn <<= maxBits2 - nBits; // normalize significand to (maxBits + 2) bits
            int isSubnormal = 0;
            if (exp < minExp) {
                if (exp < minSExp - 1) return sign == 0 ? 0.0 : -0.0; // underflow (minSExp - 1 could still be rounded to minSExp)
                isSubnormal = 1;
                do {
                    xn = (xn >> 1) | (xn & 1);
                } while (++exp < minExp);
                if (xn <= 2) return sign == 0 ? 0.0 : -0.0; // underflow
            }
            int r = unchecked((int)xn) & 0x7; // (lsb, bit below lsb, logical OR of all bits below the bit below lsb)
            xn >>= 2; // truncate to maxBits
            if (r >= 6 || r == 3) {
                xn++;
                xn &= mask;
                if (xn == 0) { // rounded to a power of two
                    exp += 1;
                    if (exp > maxExp) goto Overflow;
                }
            } else {
                xn &= mask;
            }
            exp -= isSubnormal;
        }
        exp -= minExp - 1; // add bias
        xn = (((ulong)sign) << ((maxBits - 1) + expBits)) | (((ulong)exp) << (maxBits - 1)) | xn;
    #if LOW_TRUST
        return BitConverter.Int64BitsToDouble(unchecked((long)xn));
    #else
        return *((double*)(&xn));
    #endif
    }

Overflow:
    string msg = n < 32 ? "The given string (\"" + str + "\") represents a value either too large or too small for a double precision float."
                        : "The given string represents a value either too large or too small for a double precision float.";
    throw new System.OverflowException(msg);

InvalidFormat:
    string errmsg = n < 32 ? "The given hexadecimal string representation of a double precision float (\"" + str + "\") is invalid."
                           : "The given hexadecimal string representation of a double precision float is invalid.";
    throw new System.FormatException(errmsg);
}

#if !LOW_TRUST
    unsafe
#endif
public static float SingleFromHexString(string str) {
    const int expBits = 8;  // bits for exponent
    const int maxBits = 24; // significant bits (including implicit bit)

    const int maxExp = 1 << (expBits - 1); // max n for which 0.5*2^n is a double
    const int minExp = -maxExp + 3; // min n for which 0.5*2^n is a normal double
    const int minSExp = minExp - (maxBits - 1); // min n for which 0.5*2^n is a subnormal Single

    const int maxBits2 = maxBits + 2;
    const int mask  = (1 << (maxBits - 1)) - 1; // mask for lower (maxBits - 1) bits

    if (str == null) throw new ArgumentNullException("string");
    int n = str.Length;
    if (n == 0) goto InvalidFormat;

    if (n > (int.MaxValue + minSExp - 1 - 9)/4) {// n*4 <= Int32.MaxValue protects against an nBits overflow
        throw new System.FormatException("The given hexadecimal string representation of a single precision float is too long.");
    }
    int sign = 0;   // 0 == positive, 1 == negative
    int xn = 0;     // integer significand with up to maxBits + 2 bits, where the (maxBits + 2)th bit
                    // (the least significant bit) is the logical OR of the (maxBits + 2)th and all following input bits
    int nBits = -1; // number of bits in xn, not counting leading zeros
    int exp = 0;    // the base-2 exponent
#if LOW_TRUST
    var s = str;
#else
    fixed (char* s = str) {
#endif
        int i = 0;
        // sign
        if (s[0] == '+') i = 1;
        else if (s[0] == '-') {
            i = 1;
            sign = 1;
        }
        // "0x" prefix
        if (i + 1 < n && (s[i + 1] == 'x' || s[i + 1] == 'X')) {
            if (s[i] != '0') goto InvalidFormat;
            i += 2;
        }
        bool pastDot = false;
        for (;;) {
            if (i == n) {
                if (!pastDot) exp = nBits;
                if (nBits >= 0) break;
                else goto InvalidFormat;
            }
            char c = s[i++];
            if (c <= '9' ? c >= '0' : (c <= 'f' && (c >= 'a' || (c >= 'A' && c <= 'F')))) {
                int h = c;
                h = (h & 15) + (h >> 6)*9; // converts hex char to int
                if (nBits <= 0 ) {
                    xn |= h;
                    nBits = 0;
                    while (h > 0) {
                        ++nBits;
                        h >>= 1;
                    }
                    if (pastDot) exp -= 4 - nBits;
                } else if (nBits <= maxBits2 - 4) {
                    xn <<= 4;
                    xn |= h;
                    nBits += 4;
                } else if (nBits < maxBits2) {
                    int nRemBits = maxBits2 - nBits;
                    int nSurplusBits = 4 - nRemBits;
                    int surplusBits = h & (0xf >> nRemBits);
                    surplusBits = (0xfffe >> surplusBits) & 1; // == surplusBits != 0 ? 1 : 0
                    xn <<= nRemBits;
                    xn |= (h >> nSurplusBits) | (surplusBits);
                    nBits += 4;
                } else {
                    xn |= (0xfffe >> h) & 1; // (0xfffe >> h) & 1 == h != 0 ? 1 : 0
                    nBits += 4;
                }
            } else if (c == '.') {
                if (pastDot) goto InvalidFormat;
                pastDot = true;
                exp = nBits >= 0 ? nBits : 0; // exponent for integer part of float
            } else if ((c == 'p' || c == 'P') && nBits >= 0) {
                if (!pastDot) exp = nBits;
                int eSign = 1;
                if (i < n && (s[i] == '-' || s[i] == '+')) {
                    if (s[i] == '-') eSign = -1;
                    ++i;
                }
                if (i == n) goto InvalidFormat;
                int e = 0;
                do {
                    c = s[i++];
                    if ('0' <= c && c <= '9') {
                        if (e <= (int.MaxValue - 9)/10) e = e*10 + (c - '0');
                        else e = int.MaxValue - 8;
                    } else goto InvalidFormat;
                } while (i < n);
                e*= eSign;
                // |either e is exact, or |e| >= int.MaxValue - 8
                // |exp| <= n*4 <= Int32.MaxValue - |minSExp - 1| - 9
                // either e is exact or |exp + e| >= |minSExp - 1| + 1
                try {
                    exp += e;
                } catch (System.OverflowException) {
                    exp = e < 0 ? int.MinValue : int.MaxValue;
                }
                break;
            } else {
                --i;
                if (nBits == -1 && i + 3 <= n) {
                    if (   (s[i    ] == 'i' || s[i]     == 'I')
                        && (s[i + 1] == 'n' || s[i + 1] == 'N')
                        && (s[i + 2] == 'f' || s[i + 2] == 'F')
                        && (i + 3 == n
                            || (i + 8 == n && (s[i + 3] == 'i' || s[i + 3] == 'I')
                                           && (s[i + 4] == 'n' || s[i + 4] == 'N')
                                           && (s[i + 5] == 'i' || s[i + 5] == 'I')
                                           && (s[i + 6] == 't' || s[i + 6] == 'T')
                                           && (s[i + 7] == 'y' || s[i + 7] == 'Y'))))
                    {
                        return sign == 0 ? Single.PositiveInfinity : Single.NegativeInfinity;
                    } else if (i + 3 == n && (s[i]     == 'n' || s[i]     == 'N')
                                          && (s[i + 1] == 'a' || s[i + 1] == 'A')
                                          && (s[i + 2] == 'n' || s[i + 2] == 'N'))
                    {
                        return Single.NaN;
                    }
                }
                goto InvalidFormat;
            }
        } // for
#if !LOW_TRUST
    } // fixed
#endif
    if (nBits == 0) return sign == 0 ? 0.0f : -0.0f;
    if (exp <= maxExp) {
        if (exp >= minExp && nBits <= maxBits) {
            // not subnormal and no rounding is required
            if (nBits < maxBits) xn <<= maxBits - nBits; // normalize significand to maxBits
            xn &= mask; // mask out lower (maxBits - 1) bits, the most significant bit is encoded in exp
        } else {
            if (nBits < maxBits2) xn <<= maxBits2 - nBits; // normalize significand to (maxBits + 2) bits
            int isSubnormal = 0;
            if (exp < minExp) {
                if (exp < minSExp - 1) return sign == 0 ? 0.0f : -0.0f; // underflow (minSExp - 1 could still be rounded to minSExp)
                isSubnormal = 1;
                do {
                    xn = (xn >> 1) | (xn & 1);
                } while (++exp < minExp);
                if (xn <= 2) return sign == 0 ? 0.0f : -0.0f; // underflow
            }
            int r = xn & 0x7; // (lsb, bit below lsb, logical OR of all bits below the bit below lsb)
            xn >>= 2; // truncate to maxBits
            if (r >= 6 || r == 3) {
                xn++;
                xn &= mask;
                if (xn == 0) { // rounded to a power of two
                    exp += 1;
                    if (exp > maxExp) goto Overflow;
                }
            } else {
                xn &= mask;
            }
            exp -= isSubnormal;
        }
        exp -= minExp - 1; // add bias
        xn = (sign << ((maxBits - 1) + expBits)) | (exp << (maxBits - 1)) | xn;
    #if LOW_TRUST
        return BitConverter.ToSingle(BitConverter.GetBytes(xn), 0);
    #else
        return *((float*)(&xn));
    #endif
    }

Overflow:
    string msg = n < 32 ? "The given string (\"" + str + "\") represents a value either too large or too small for a single precision float."
                        : "The given string represents a value either too large or too small for a single precision float.";
    throw new System.OverflowException(msg);

InvalidFormat:
    string errmsg = n < 32 ? "The given hexadecimal string representation of a single precision float (\"" + str + "\") is invalid."
                           : "The given hexadecimal string representation of a single precision float is invalid.";
    throw new System.FormatException(errmsg);
}

} // class HexFloat

}
