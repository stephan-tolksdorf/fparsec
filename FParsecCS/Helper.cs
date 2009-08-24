// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace FParsec {

public sealed class Helper {

internal static bool ContainsNewlineChar(string str) {
    foreach (char c in str) {
        if (c > '\r') continue;
        if (c == '\r' || c == '\n') goto ReturnTrue;
    }
    return false;
ReturnTrue:
    return true;
}

internal unsafe static T RunParserOnSubstream<T,TUserState,TSubStreamUserState>(
                             Microsoft.FSharp.Core.FastFunc<State<TSubStreamUserState>,T> parser,
                             TSubStreamUserState userState,
                             State<TUserState> stateBeforeSubStream, State<TUserState> stateAfterSubStream)
{
    var s0 = stateBeforeSubStream;
    var s1 = stateAfterSubStream;
    CharStream.Anchor* anchor = s0.Iter.Anchor;
    if (anchor != s1.Iter.Anchor)
        throw new ArgumentException("The states are associated with different CharStreams.");

    if (anchor->LastBlock == 0) {
        // the CharStream has only one block, so its safe to
        // construct a new CharStream from a pointer into the original buffer
        char* ptr = s0.Iter.Ptr;
        if (ptr == null) ptr = anchor->BufferEnd;
        char* end = s1.Iter.Ptr;
        if (end == null) end = anchor->BufferEnd;
        if (end < ptr) throw new ArgumentException("The position of the second state lies before the position of the first state.");
        int length = CharStream.PositiveDistance(ptr, end);
        CharStream stream = (CharStream)anchor->StreamHandle.Target;
        string buffer = stream.BufferString;
        using (var subStream = buffer != null
                               ? new CharStream(buffer, ptr, CharStream.PositiveDistance(anchor->BufferBegin, ptr) + stream.BufferStringIndex, length, s0.Index)
                               : new CharStream(ptr, length, s0.Index, 0))
        {
            var state = new State<TSubStreamUserState>(subStream, s0.Pos, userState);
            return parser.Invoke(state);
        }
    } else if (s0.Iter.Block == s1.Iter.Block && anchor->Block == s1.Iter.Block) {
        char* ptr = s0.Iter.Ptr;
        char* end = s1.Iter.Ptr;
        if (end < ptr) throw new ArgumentException("The position of the second state lies before the position of the first state.");
        int length = CharStream.PositiveDistance(ptr, end);
        string subString = new String(ptr, 0, length);
        fixed (char* pSubString = subString)
        using (var subStream = new CharStream(subString, pSubString, 0, length, s0.Index)) {
            var state = new State<TSubStreamUserState>(subStream, s0.Pos, userState);
            return parser.Invoke(state);
        }
    } else {
        long index1 = s0.Iter.Index;
        long index2 = s1.Iter.Index;
        if (index2 < index1) throw new ArgumentException("The position of the second state lies before the position of the first state.");
        int length = (int) Math.Min(index2 - index1, (long) System.Int32.MaxValue);
        // length >= Int32.MaxValue will trigger an exception anyway (because the string is too large)
        string subString = new String('\u0000', length);
        fixed (char* pSubString = subString) {
            s0.Iter.Read(pSubString, length);
            using (var subStream = new CharStream(subString, pSubString, 0, length, s0.Index)) {
                var state = new State<TSubStreamUserState>(subStream, s0.Pos, userState);
                return parser.Invoke(state);
            }
        }
    }
}

public sealed unsafe class CharSet {
    private const int wordSize = 32;
    private const int log2WordSize = 5;

    private int    min;
    private int    max;
    private int    tableMin;
    private int[]  table;
    private string charsNotInTable; // We use a string here instead of a char[] because the JIT
                                    // produces better code for loops involving strings.

    public unsafe CharSet(string chars) : this(chars, 32) {}
    // because of mandatory bounds checking, we wouldn't get any advantage from a fixed size table

    public unsafe CharSet(string chars, int maxTableSize) {
        if (chars.Length == 0) {
            tableMin = min = 0x10000;
            max = -1;
            table = new int[0];
            charsNotInTable = null;
            return;
        }
        if (maxTableSize < 4) maxTableSize = 4;
        else if (maxTableSize > 0x10000/wordSize) maxTableSize  = 0x10000/wordSize;
        int maxTableBits = maxTableSize*wordSize;

        char prevChar = chars[0];
        min = prevChar;
        max = prevChar;
        tableMin = -1;
        int tableMax = -1;
        int nCharsNotInTable = 0;
        for (int i = 1; i < chars.Length; ++i) {
            char c = chars[i];
            if (c == prevChar) continue; // filter out repeated chars
            prevChar = c;
            int prevMin = min;
            int prevMax = max;
            min = Math.Min(c, min);
            max = Math.Max(c, max);
            if (tableMin < 0) {
                // the first time the table range is exceeded the tableMin is set
                if (max - min >= maxTableBits) {
                    tableMin = prevMin; // stays fixed
                    tableMax = prevMax; // will be updated later
                    nCharsNotInTable = 1;
                }
            } else if (c < tableMin || c >= tableMin + maxTableBits) {
                ++nCharsNotInTable;
            } else {
                tableMax = Math.Max(c, tableMax);
            }
        }
        if (tableMin < 0) {
            tableMin = min;
            tableMax = max;
        }
        int tableSize =   tableMax - tableMin + 1 < maxTableBits
                        ? (tableMax - tableMin + 1)/wordSize + ((tableMax - tableMin + 1)%wordSize != 0 ? 1 : 0)
                        : maxTableSize;
        table = new int[tableSize];
        charsNotInTable = nCharsNotInTable > 0 ? new string('\u0000', nCharsNotInTable) : null;
        fixed (char* pCharsNotInTable = charsNotInTable) {
            prevChar = chars[0] != 'x' ? 'x' : 'y';
            int n = 0;
            for (int i = 0; i < chars.Length; ++i) {
                char c = chars[i];
                if (c == prevChar) continue;
                prevChar = c;
                int off = c - tableMin;
                int idx = off >> log2WordSize;
                if ((uint)idx < (uint)table.Length) {
                    table[idx] |= 1 << off; // we don't need to mask off because C#'s operator<< does that for us
                } else {
                    pCharsNotInTable[n++] = c;
                }
            }
            Debug.Assert(n == nCharsNotInTable);
        }
    }

    public bool Contains(char c) {
        int off = c - tableMin;
        int idx = off >> log2WordSize;
        if ((uint)idx < (uint)table.Length) {
            return ((table[idx] >> off) & 1) != 0; // we don't need to mask off because C#'s operator>> does that for us
        }
        if (charsNotInTable == null) return false;
        if (c >= min && c <= max) {
            foreach (char cc in charsNotInTable) {
                if (cc == c) goto ReturnTrue;
            }
        }
        return false;
    ReturnTrue:
        return true;
    }
}

// see the documentation of floatToHexString and floatOfHexString for more info

#pragma warning disable 0429 // unreachable expression code
#pragma warning disable 0162 // unreachable code

public static unsafe string DoubleToHexString(double x) {
    const int expBits = 11;  // bits for biased exponent
    const int maxBits = 53;  // significant bits (including implicit bit)
    const int maxChars = 24; // "-0x1.fffffffffffffp-1022"

    const int maxBiasedExp = (1 << expBits) - 1;
    const int maxExp       = 1 << (expBits - 1); // max n for which 0.5*2^n is a double
    const int bias = maxExp - 1;

    const int maxFractNibbles = (maxBits - 1)/4 + (((maxBits - 1)%4) == 0 ? 0 : 1);
    const ulong mask  = (1UL << (maxBits - 1)) - 1; // mask for lower (maxBits - 1) bits

    ulong xn  = *((ulong*)(&x)); // reinterpret double as ulong
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
            int h = ((int) (s >> ((maxFractNibbles - 1 - j) << 2))) & 0xf;
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

public static unsafe string SingleToHexString(float x) {
    const int expBits = 8;   // bits for biased exponent
    const int maxBits = 24;  // significant bits (including implicit bit)
    const int maxChars = 16; // "-0x1.fffffep-126"

    const int maxBiasedExp = (1 << expBits) - 1;
    const int maxExp       = 1 << (expBits - 1); // max n for which 0.5*2^n is a double
    const int bias = maxExp - 1;

    const int maxFractNibbles = (maxBits - 1)/4 + (((maxBits - 1)%4) == 0 ? 0 : 1);
    const uint mask = (1U << (maxBits - 1)) - 1; // mask for lower (maxBits - 1) bits

    uint xn = *((uint*)(&x)); // reinterpret float as ulong
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
            int h = ((int) (s >> ((maxFractNibbles - 1 - j) << 2))) & 0xf;
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
            str[--i] = (char) (48 + r);
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

public static unsafe double DoubleFromHexString(string str) {
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
    fixed (char* s = str) {
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
                    xn |= (uint) h;
                    nBits += 4;
                } else if (nBits < maxBits2) {
                    int nRemBits = maxBits2 - nBits;
                    int nSurplusBits = 4 - nRemBits;
                    int surplusBits = h & (0xf >> nRemBits);
                    surplusBits = (0xfffe >> surplusBits) & 1; // == surplusBits != 0 ? 1 : 0
                    xn <<= nRemBits;
                    xn |= (uint) ((h >> nSurplusBits) | (surplusBits));
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
    } // fixed
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
            int r = (int)(((uint)xn) & 0x7u); // (lsb, bit below lsb, logical OR of all bits below the bit below lsb)
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
        xn = (((ulong) sign) << ((maxBits - 1) + expBits)) | (((ulong) exp) << (maxBits - 1)) | xn;
        return *((double*)(&xn));
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

public static unsafe float SingleFromHexString(string str) {
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
    fixed (char* s = str) {
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
    } // fixed
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
            int r = (int)(((uint)xn) & 0x7u); // (lsb, bit below lsb, logical OR of all bits below the bit below lsb)
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
        return *((float*)(&xn));
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

} // class Helper

}
