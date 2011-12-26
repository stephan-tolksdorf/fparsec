// Copyright (c) Stephan Tolksdorf 2009-2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Globalization;
using System.Text;
using System.Diagnostics;
using System.Runtime.InteropServices;

using Microsoft.FSharp.Core;

using FParsec;

namespace FParsec {

public static class Text {

/// <summary>Detects the presence of an encoding preamble in the first count bytes of the byte buffer.
/// If detectEncoding is false, this function only searches for the preamble of the given default encoding,
/// otherwise also for any of the standard unicode byte order marks (UTF-8, UTF-16 LE/BE, UTF-32 LE/BE).
/// If an encoding different from the given default encoding is detected, the new encoding
/// is assigned to the encoding reference.
/// Returns the number of bytes in the detected preamble, or 0 if no preamble is detected.
/// </summary>
internal static int DetectPreamble(byte[] buffer, int count, ref Encoding encoding, bool detectEncoding) {
    Debug.Assert(count >= 0);
    if (detectEncoding && count >= 2) {
        switch (buffer[0]) {
        case 0xEF:
            if (buffer[1] == 0xBB && count > 2 && buffer[2] == 0xBF) {
            #if !SILVERLIGHT
                if (encoding.CodePage != 65001)
            #else
                if (encoding.WebName != "utf-8")
            #endif
                    encoding = Encoding.UTF8;
                return 3;
            }
        break;
        case 0xFE:
            if (buffer[1] == 0xFF) {
            #if !SILVERLIGHT
                if (encoding.CodePage != 1201)
            #else
                if (encoding.WebName != "utf-16BE")
            #endif
                    encoding = Encoding.BigEndianUnicode;
                return 2;
            }
        break;
        case 0xFF:
            if (buffer[1] == 0xFE) {
                if (count >= 4 && buffer[2] == 0x00 && buffer[3] == 0x00) {
                #if !SILVERLIGHT
                    if (encoding.CodePage != 12000)
                        encoding = Encoding.UTF32; // UTF-32 little endian
                #else
                    // TODO: check at run time if encoding is available
                    if (encoding.WebName != "utf-32")
                        throw new NotSupportedException("An UTF-32 input encoding was detected, which is not natively supported under Silverlight.");
                #endif
                    return 4;
                } else {
                #if !SILVERLIGHT
                    if (encoding.CodePage != 1200)
                #else
                    if (encoding.WebName != "utf-16")
                #endif
                        encoding = Encoding.Unicode; // UTF-16 little endian
                    return 2;
                }
            }
        break;
        case 0x00:
            if (buffer[1] == 0x00 && count >= 4 && buffer[2] == 0xFE && buffer[3] == 0xFF) {
            #if !SILVERLIGHT
                if (encoding.CodePage != 12001)
                    encoding = new UTF32Encoding(true, true); // UTF-32 big endian
            #else
                if (encoding.WebName != "utf-32BE")
                    throw new NotSupportedException("An UTF-32 (big endian) input encoding was detected, which is not natively supported under Silverlight.");
            #endif
                return 4;
            }
        break;
        }
    }
    byte[] preamble = encoding.GetPreamble();
    if (preamble.Length > 0 && count >= preamble.Length) {
        int i = 0;
        while (buffer[i] == preamble[i]) {
            if (++i == preamble.Length) return preamble.Length;
        }
    }
    return 0;
}

#if !LOW_TRUST
/// <summary>Reads all remaining chars into the given buffer. If the remaining stream
/// content holds more than the given maximum number of chars, an exception will be thrown.</summary>
internal unsafe static int ReadAllRemainingCharsFromStream(char* buffer, int maxCount, byte[] byteBuffer, int byteBufferIndex, int byteBufferCount, System.IO.Stream stream, long streamPosition, Decoder decoder, bool flush) {
    Debug.Assert(maxCount > 0 && byteBufferIndex >= 0 && byteBufferIndex < byteBufferCount);
    fixed (byte* pByteBuffer = byteBuffer) {
        int bufferCount = 0;
        for (;;) {
            try {
                bufferCount += decoder.GetChars(pByteBuffer + byteBufferIndex, byteBufferCount - byteBufferIndex,
                                                buffer + bufferCount, maxCount - bufferCount, flush);
            } catch (DecoderFallbackException e) {
                e.Data.Add("Stream.Position", streamPosition - (byteBufferCount - byteBufferIndex) + e.Index);
                throw;
            }
            if (flush) break;
            byteBufferIndex = 0; // GetChars consumed all bytes in the byte buffer
            byteBufferCount = stream.Read(byteBuffer, 0, byteBuffer.Length);
            streamPosition += byteBufferCount;
            flush = byteBufferCount == 0;
        }
        return bufferCount;
    }
}
#endif


/// <summary>Returns a case-folded copy of the string argument. All chars are mapped
/// using the (non-Turkic) 1-to-1 case folding mappings (v. 6.0) for Unicode code
/// points in the Basic Multilingual Plane, i.e. code points below 0x10000.
/// If the argument is null, null is returned.</summary>
#if LOW_TRUST
static public string FoldCase(string str) {
    char[] cftable = CaseFoldTable.FoldedChars;
    if (str != null) {
        for (int i = 0; i < str.Length; ++i) {
            char c   = str[i];
            char cfc = cftable[c];
            if (c != cfc) {
                StringBuilder sb = new StringBuilder(str);
                sb[i++] = cfc;
                for (; i < str.Length; ++i) {
                    c   = str[i];
                    cfc = cftable[c];
                    if (c != cfc) sb[i] = cfc;
                }
                return sb.ToString();
            }
        }
    }
    return str;
}
#else
static unsafe public string FoldCase(string str) {
    if (str != null) {
        fixed (char* src0 = str) {
            char* end = src0 + str.Length;
            char* cftable = CaseFoldTable.FoldedChars;
            char* src = src0;
            for (;;) { // src is null-terminated, so we can always read one char
                char c = *src;
                if (c == cftable[c]) {
                    if (++src >= end) break;
                } else {
                    string newString = new String('\u0000', str.Length);
                    fixed (char* dst_ = newString) {
                        src = src0;
                        char* dst = dst_;
                        do {
                            *dst = cftable[*src];
                             ++src; ++dst;
                        } while (src != end);
                    }
                    return newString;
                }
            }
        }
    }
    return str;
}
#endif

#if !LOW_TRUST
    unsafe
#endif
static public char FoldCase(char ch) {
    return CaseFoldTable.FoldedChars[ch];
}

internal static int FindNewlineOrEOSChar(string str) {
    int i;
    for (i = 0; i < str.Length; ++i) {
        char c = str[i];
        // '\n' = '\u000A', '\r' = '\u000D'
        if (unchecked((uint)c - 0xEu) < 0xFFFFu - 0xEu) continue;
        if (c == '\n' || c == '\r' || c == '\uffff') goto Return;
    }
    i = -1;
Return:
    return i;
}

/// <summary>Returns the given string with all occurrences of "\r\n" and "\r" replaced
/// by "\n". If the argument is null, null is returned.</summary>
#if LOW_TRUST
static public string NormalizeNewlines(string str) {
    if (str == null || str.Length == 0) return str;
    int nCR   = 0;
    int nCRLF = 0;
    for (int i = 0; i < str.Length; ++i) {
        if (str[i] == '\r') {
            if (i + 1 < str.Length && str[i + 1] == '\n') ++nCRLF;
            else ++nCR;
        }
    }
    if (nCRLF == 0) {
        return nCR == 0 ? str : str.Replace('\r', '\n');
    } else {
        return CopyWithNormalizedNewlines(str, 0, str.Length, nCRLF, nCR);
    }
}
static internal string CopyWithNormalizedNewlines(string src, int index, int length, int nCRLF, int nCR) {
    Debug.Assert(length > 0 && nCRLF >= 0 && nCR >= 0 && (nCRLF | nCR) != 0);
    if (nCRLF != 0) {
        StringBuilder sb = new StringBuilder(length - nCRLF);
        int end = index + length;
        int i0 = index;
        if (nCR == 0) {
            int nn = nCRLF;
            int i = index;
            for (;;) {
                char c = src[i++];
                if (c == '\r') {
                    sb.Append(src, i0, i - i0 - 1).Append('\n');
                    ++i; // skip over the '\n' in "\r\n"
                    i0 = i;
                    if (--nn == 0) break;
                }
            }
        } else {
            int nn = nCRLF + nCR;
            int i = index;
            for (;;) {
                char c = src[i++];
                if (c == '\r') {
                    sb.Append(src, i0, i - i0 - 1).Append('\n');
                    if (i < end && src[i] == '\n') ++i; // skip over the '\n' in "\r\n"
                    i0 = i;
                    if (--nn == 0) break;
                }
            }
        }
        if (i0 < end) sb.Append(src, i0, end - i0);
        return sb.ToString();
    } else {
        return new StringBuilder(src, index, length, length).Replace('\r', '\n').ToString();
    }
}
#else
static unsafe public string NormalizeNewlines(string str) {
    int length;
    if (str == null || (length = str.Length) == 0) return str;
    fixed (char* src = str) { // the char buffer is guaranteed to be null-terminated (C# language specification on fixed statement)
        int nCR   = 0;
        int nCRLF = 0;
        for (int i = 0; i < length; ++i) {
            if (src[i] == '\r') {
                if (src[i + 1] == '\n') ++nCRLF; // relies on null-termination
                else ++nCR;
            }
        }
        if (nCRLF == 0) {
            return nCR == 0 ? str : str.Replace('\r', '\n');
        } else {
            return CopyWithNormalizedNewlines(src, length, nCRLF, nCR);
        }
    }
}
static unsafe internal string CopyWithNormalizedNewlines(char* src, int length, int nCRLF, int nCR) {
    Debug.Assert(length > 0 && nCRLF >= 0 && nCR >= 0 && (nCRLF | nCR) != 0);
    string newString = new String('\n', length - nCRLF);
    fixed (char* dst_ = newString) {
        char* dst = dst_;
        char* end = src + length;
        if (nCRLF != 0) {
            if (nCR == 0) {
                int nn = nCRLF;
                for (;;) {
                    char c = *src;
                    ++src;
                    if (c != '\r') {
                        *dst = c;
                        ++dst;
                    } else {
                        ++src; // skip over the '\n' in "\r\n"
                        *dst = '\n';
                        ++dst;;
                        if (--nn == 0) break;
                    }
                }
            } else {
                int nn = nCRLF + nCR;
                for (;;) {
                    char c = *src;
                    ++src;
                    if (c != '\r') {
                        *dst = c;
                        ++dst;
                    } else {
                        if (*src == '\n') ++src; // skip over the '\n' in "\r\n" (relies on null-termination)
                        *dst = '\n';
                        ++dst;
                        if (--nn == 0) break;
                    }
                }
            }
        } else {
            int nn = nCR;
            for (;;) {
                char c = *src;
                ++src;
                if (c != '\r') {
                    *dst = c;
                    ++dst;
                } else {
                    *dst = '\n';
                    ++dst;
                    if (--nn == 0) break;
                }
            }
        }
        // copy remaining chars
        #if UNALIGNED_READS
            if (src != end) {
                uint len = Buffer.PositiveDistance(src, end);
                if ((unchecked((int)dst) & 2) != 0) { // align dest
                    *dst = *src;
                    ++src; ++dst; --len;
                }
                while (len >= 8) {
                    ((int*)dst)[0] = ((int*)src)[0];
                    ((int*)dst)[1] = ((int*)src)[1];
                    ((int*)dst)[2] = ((int*)src)[2];
                    ((int*)dst)[3] = ((int*)src)[3];
                    src += 8; dst += 8; len -= 8;
                }
                if ((len & 4) != 0) {
                    ((int*)dst)[0] = ((int*)src)[0];
                    ((int*)dst)[1] = ((int*)src)[1];
                    src += 4; dst += 4;
                }
                if ((len & 2) != 0) {
                    ((int*)dst)[0] = ((int*)src)[0];
                    src += 2; dst += 2;
                }
                if ((len & 1) != 0) {
                    *dst = *src;
                }
            }
        #else
            while (src < end) {
                *dst = *src;
                ++src; ++dst;
            }
        #endif
    }
    return newString;
}
#endif


/// <summary>A faster implementation of System.Globalization.StringInfo(str).LengthInTextElements.</summary>
public static int CountTextElements(string str) {
    int count = 0;
    int end = str.Length;
    int i = 0;
    for (;;) {
    SkipBaseCharacter:
        if (i >= end) break;
        char c = str[i];
        ++i;
        ++count;
        if (c < ' ') continue; // control char
        if (c > '~') {
            var uc = CharUnicodeInfo.GetUnicodeCategory(c);
        Switch:
            switch (uc) {
            case UnicodeCategory.Surrogate:
                uc = Char.GetUnicodeCategory(str, i - 1);
                if (uc == UnicodeCategory.Surrogate) continue;
                ++i;
                goto Switch;
            case UnicodeCategory.NonSpacingMark:
            case UnicodeCategory.SpacingCombiningMark:
            case UnicodeCategory.EnclosingMark:
            case UnicodeCategory.Control:
            case UnicodeCategory.Format:
            case UnicodeCategory.OtherNotAssigned:
                continue;
            // adding these cases to the default branch prevents
            // the MS C# compiler from splitting the jump table
            case UnicodeCategory.OtherNumber:
            case UnicodeCategory.DashPunctuation:
            case UnicodeCategory.ClosePunctuation:
            case UnicodeCategory.InitialQuotePunctuation:
            case UnicodeCategory.OtherPunctuation:
            case UnicodeCategory.ModifierSymbol:
            default:
                break; // exits the switch, not the loop
            }
        }
    // SkipMoreBaseCharactersOrCombiningMarks:
        for (;;) {
            if (i >= end) break;
            c = str[i];
            ++i;
            if (c >= ' ') {
                if (c <= '~') {
                    ++count;
                    continue;
                }
            } else { // control char
                ++count;
                goto SkipBaseCharacter;
            }
            var uc = CharUnicodeInfo.GetUnicodeCategory(c);
        Switch:
            switch (uc) {
            case UnicodeCategory.NonSpacingMark:
            case UnicodeCategory.SpacingCombiningMark:
            case UnicodeCategory.EnclosingMark:
                continue;
            case UnicodeCategory.Surrogate:
                uc = Char.GetUnicodeCategory(str, i - 1);
                if (uc != UnicodeCategory.Surrogate) {
                    ++i;
                    goto Switch;
                }
                ++count;
                goto SkipBaseCharacter;
            case UnicodeCategory.Control:
            case UnicodeCategory.Format:
            case UnicodeCategory.OtherNotAssigned:
                ++count;
                goto SkipBaseCharacter;
            // adding these cases to the default branch prevents
            // the MS C# compiler from splitting the jump table
            case UnicodeCategory.OtherNumber:
            case UnicodeCategory.DashPunctuation:
            case UnicodeCategory.ClosePunctuation:
            case UnicodeCategory.InitialQuotePunctuation:
            case UnicodeCategory.OtherPunctuation:
            case UnicodeCategory.ModifierSymbol:
            default:
                ++count;
                continue;
            }
        }
        break;
    }
    return count;
}

// Apparently System.Char.Is(High|Low)Surrogate is not safe for consumption by Silverlight developers

public static bool IsSurrogate(char ch)     { return (ch & 0xF800) == 0xD800; }
public static bool IsHighSurrogate(char ch) { return (ch & 0xFC00) == 0xD800; }
public static bool IsLowSurrogate(char ch)  { return (ch & 0xFC00) == 0xDC00; }

#if LOW_TRUST

public static bool IsWhitespace(char ch) {
    return System.Char.IsWhiteSpace(ch);
}

#else

internal unsafe struct IsWhitespaceHelper {
    // we use the same data structure and algorithm as for IdentifierValidator

    private static readonly byte[] DataArray = {
        0,1,1,1,1,1,1,1,1,1,1,2,3,1,1,1,4,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,

        0,1,2,2,3,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        5,6,7,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,

        0,62,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,64,0,0,255,7,0,0,0,131,0,0,0,0,0,128,
    };

    private const int Table1Offset = 0;
    private const int Table1Size = 128;
    private const int Table1Log2Length = 7;
    private const int Table2Offset = 128;
    private const int Table2Size = 96;
    private const int Table2Log2BlockLength = 4;
    private const int Table3Offset = Table2Offset + Table2Size;
    private const int Table3Size = 32;
    private const int Table3Log2BlockLength = 5;

#if LOW_TRUST
    private static readonly byte[] Table1 = BufferHelpers.CopySubarray(DataArray, 0, Table2Offset);
    private static readonly byte[] Table2 = BufferHelpers.CopySubarray(DataArray, Table2Offset, Table2Size);
    private static readonly uint[] Table3 = BufferHelpers.CopyUIntsStoredInLittleEndianByteArray(DataArray, Table3Offset, Table3Size);
#else
    private static readonly byte* Data   = LoadDataArrayIntoFixedBuffer();
    private static readonly byte* Table1 = Data + Table1Offset;
    private static readonly byte* Table2 = Data + Table2Offset;
    private static readonly uint* Table3 = (uint*)(Data + Table3Offset);

    private static byte* LoadDataArrayIntoFixedBuffer() {
        var buffer = UnmanagedMemoryPool.Allocate(DataArray.Length);
        Marshal.Copy(DataArray, 0, buffer, DataArray.Length);
        Debug.Assert(Table3Size%sizeof(uint) == 0);
        if (!System.BitConverter.IsLittleEndian)
            Buffer.SwapByteOrder((uint*)((byte*)buffer + Table3Offset), Table3Size/sizeof(uint));
        return (byte*)buffer;
    }
#endif

    public static uint IsWhitespace_(char ch) {
        uint cp = (uint)ch;
        uint idx1 = cp >> (Table2Log2BlockLength + Table3Log2BlockLength);
        const uint f2 = 1u << Table2Log2BlockLength;
        const uint m2 = f2 - 1;
        uint idx2 = Table1[idx1]*f2 + ((cp >> Table3Log2BlockLength) & m2);
        uint idx3 = Table2[idx2];
        return Table3[idx3] >> (int)(cp /* & 0x1fu */); // C#'s operator>> masks with 0x1fu, no matter whether we do too
    }
}

/// <summary>A faster implementation of System.Char.IsWhiteSpace.</summary>
public static bool IsWhitespace(char ch) { // should get inlined
    return (IsWhitespaceHelper.IsWhitespace_(ch) & 1u) != 0;
}

#endif

#if !LOW_TRUST
    unsafe
#endif
internal static string HexEscape(char c) {
#if LOW_TRUST
    char[] cs = new char[6];
#else
    char* cs = stackalloc char[6];
#endif
    cs[0] = '\\';
    cs[1] = 'u';
    int n = c;
    for (int j = 0; j < 4; ++j) {
        cs[5 - j] = "0123456789abcdef"[n & 0xf];
        n >>= 4;
    }
    return new string(cs, 0, 6);
}

internal static string EscapeChar(char c) {
    switch (c) {
    case '\\': return "\\\\";
    case '\'': return "\\\'";
    case '\"': return "\\\"";
    case '\r': return "\\r";
    case '\n': return "\\n";
    case '\t': return "\\t";
    case '\f': return "\\f";
    case '\v': return "\\v";
    case '\a': return "\\a";
    case '\b': return "\\b";
    default:   return HexEscape(c);
    }
}

#if !LOW_TRUST
    unsafe
#endif
internal static string Concat(string str0, string str1, string str2, string str3, string str4) {
#if LOW_TRUST
    return str0 + str1 + str2 + str3 + str4;
#else
    int length = str0.Length + str1.Length + str2.Length + str3.Length + str4.Length;
    var str = new string('\u0000', length);
    fixed (char* pStr = str) {
        int i = 0;
        for (int j = 0; j < str0.Length; ++i, ++j) pStr[i] = str0[j];
        for (int j = 0; j < str1.Length; ++i, ++j) pStr[i] = str1[j];
        for (int j = 0; j < str2.Length; ++i, ++j) pStr[i] = str2[j];
        for (int j = 0; j < str3.Length; ++i, ++j) pStr[i] = str3[j];
        for (int j = 0; j < str4.Length; ++i, ++j) pStr[i] = str4[j];
    }
    return str;
#endif
}

internal static string Escape(string str, string prefix1, string prefix2, string postfix1, string postfix2, char escapedQuoteChar) {
    Debug.Assert(str != null && prefix1 != null && prefix2 != null && postfix1 != null && postfix2 != null);
    StringBuilder sb = null;
    int i0 = 0;
    int i = 0;
    for (;;) {
        if (i >= str.Length) break;
        char c = str[i];
        ++i;
        if (c > '\'' && c < '\u007f') {
            if (c != '\\') continue;
        } else if (c == ' ' || (   !Char.IsControl(c) && c != escapedQuoteChar
                                && (c < '\u2028' || c > '\u2029'))) continue;
        if ((object)sb == null) {
            sb = new StringBuilder(str.Length + prefix1.Length + prefix2.Length + postfix1.Length + postfix2.Length + 8);
            sb.Append(prefix1).Append(prefix2);
        }
        int n = i - i0 - 1;
        if (n != 0) sb.Append(str, i0, n);
        i0 = i;
        sb.Append(EscapeChar(c));
    }
    if ((object)sb == null) return Concat(prefix1, prefix2, str, postfix1, postfix2);
    if (i0 != i) sb.Append(str, i0, i - i0);
    return sb.Append(postfix1).Append(postfix2).ToString();
}

internal static string AsciiEscape(string str, string prefix1, string prefix2, string postfix1, string postfix2, char escapedQuoteChar) {
    Debug.Assert(str != null && prefix1 != null && prefix2 != null && postfix1 != null && postfix2 != null);
    StringBuilder sb = null;
    int i0 = 0;
    int i = 0;
    for (;;) {
        if (i >= str.Length) break;
        char c = str[i];
        ++i;
        if (c > '\'' && c < '\u007f') {
            if (c != '\\') continue;
        } else if (c == ' ' || (c >= ' ' &&  c <= '\'' && c != escapedQuoteChar)) continue;
        if ((object)sb == null) {
            sb = new StringBuilder(str.Length + prefix1.Length + prefix2.Length + postfix1.Length + postfix2.Length + 8);
            sb.Append(prefix1).Append(prefix2);
        }
        int n = i - i0 - 1;
        if (n != 0) sb.Append(str, i0, n);
        i0 = i;
        sb.Append(EscapeChar(c));
    }
    if ((object)sb == null) return Concat(prefix1, prefix2, str, postfix1, postfix2);
    if (i0 != i) sb.Append(str, i0, i - i0);
    return sb.Append(postfix1).Append(postfix2).ToString();
}

internal static string SingleQuote(string str) {
    return Escape(str, "", "'", "'", "", '\'');
}

internal static string SingleQuote(string prefix, string str, string postfix) {
    return Escape(str, prefix, "'", "'", postfix, '\'');
}

internal static string DoubleQuote(string str) {
    return Escape(str, "", "\"", "\"", "", '"');
}

internal static string DoubleQuote(string prefix, string str, string postfix) {
    return Escape(str, prefix, "\"", "\"", postfix, '"');
}


} // class Text

}