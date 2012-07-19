// Copyright (c) Stephan Tolksdorf 2010-2012
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Text;
using System.Diagnostics;
using System.Runtime.InteropServices;

using Microsoft.FSharp.Core;

namespace FParsec {

#if !LOW_TRUST
    unsafe
#endif
public sealed class IdentifierValidator {

    internal enum IdentifierCharFlags : byte {
        None                = 0,

        Continue            = 1,
        NonContinue         = 2,
        //Start       = NonContinue | Continue,

        // the following two values are used by the FParsec identifier parser, not this class
        PreCheckContinue    = 4,
        PreCheckNonContinue = 8,
    }

#if !SILVERLIGHT
    public NormalizationForm NormalizationForm { get; set; }
    public bool NormalizeBeforeValidation { get; set; }
#endif
    public bool AllowJoinControlCharsAsIdContinueChars { get; set; }
    private readonly IdentifierCharFlags[] AsciiCharOptions;

    private void CheckAscii(char asciiChar) {
        if (asciiChar == 0 || asciiChar >= 128)
            throw new ArgumentOutOfRangeException("asciiChar", "The identifier char settings can only be read or set for non-zero ASCII chars, i.e. chars in the range '\u0001'-'\u007f'.");
    }

    public void SetIsAsciiNoIdChar(char asciiChar)       { CheckAscii(asciiChar); AsciiCharOptions[asciiChar] = 0; }
    public void SetIsAsciiIdStartChar(char asciiChar)    { CheckAscii(asciiChar); AsciiCharOptions[asciiChar] = IdentifierCharFlags.NonContinue | IdentifierCharFlags.Continue; }
    public void SetIsAsciiIdNonStartChar(char asciiChar) { CheckAscii(asciiChar); AsciiCharOptions[asciiChar] = IdentifierCharFlags.Continue; }

    public IdentifierValidator() {
        var ascii = new IdentifierCharFlags[128];
        var start = IdentifierCharFlags.NonContinue | IdentifierCharFlags.Continue;
        // defaults as defined by XID_START/XID_CONTINUE
        for (int c = 'A'; c <= 'Z'; ++c) ascii[c] = start;
        for (int c = 'a'; c <= 'z'; ++c) ascii[c] = start;
        for (int c = '0'; c <= '9'; ++c) ascii[c] = IdentifierCharFlags.Continue;
        ascii['_'] = IdentifierCharFlags.Continue;
        AsciiCharOptions = ascii;
    }

    internal IdentifierValidator(IdentifierCharFlags[] asciiCharOptions) {
        Debug.Assert(asciiCharOptions.Length == 128);
        AsciiCharOptions = asciiCharOptions;
    }

    /// <summary>Returns the normalized string, or null in case an invalid identifier
    /// character is found. If an invalid character is found, the string index of the
    /// invalid character is assigned to the out parameter, otherwise -1.</summary>
    public string ValidateAndNormalize(string str, out int errorPosition) {
        // Pinning str and asciiOptions to avoid redundant bounds checks would actually
        // slow down the code for small to medium size identifiers because of the
        // (unnecessarily) high overhead associated with C#'s fixed statement. One
        // issue is that the .NET C# compiler emits null and 0-length checks even
        // though the C# standard leaves the respective behaviour undefined and
        // one hence can't rely on them. Another, more severe issue is that the
        // C# compiler puts the whole code inside the scope of the fixed statement
        // into a try-finally block, even if the whole function has no exception
        // handlers. The try-finally block in turn inhibits certain optimizations
        // by the JIT, in particular it seems to prevent the 32-bit .NET JIT from
        // compiling gotos into straighforward jumps.
        var asciiOptions = AsciiCharOptions;
        bool isSecondRound = false;
        bool isOnlyAscii  = true;
        int i = 1;
        int length = str.Length; // throws if str is null
        if (length == 0) goto ReturnWithError; // check could be avoided for null-terminated buffer

        // Even if NormalizeBeforeValidation is set we first try to validate the
        // identifier without normalization. If we don't get an error, we normalize
        // after validation. If we get an error, we normalize and try
        // to validate the identifier a second time. This doesn't change results
        // because XID identifiers are "closed under normalization".

    IdStart:
        char c = str[0];
        if (c < 128) {
            if ((asciiOptions[c] & IdentifierCharFlags.NonContinue) == 0) goto Error;
        } else {
            isOnlyAscii = false;
            if (!Text.IsSurrogate(c)) {
                if (!IsXIdStartOrSurrogate(c)) goto Error;
            } else {
                if (i == length) goto Error; // check could be avoided for null-terminated buffer
                char c1 = str[1];
                if (c > 0xDBFF || !Text.IsLowSurrogate(c1)) goto ReturnWithError;
                int cp = (c - 0xD800)*0x400 + c1 - 0xDC00; // codepoint minus 0x10000
                if (!IsXIdStartSmp(cp)) goto Error;
                ++i;
            }
        }
        if (i < length) {
            if (!AllowJoinControlCharsAsIdContinueChars) {
                for (;;) {
                    c = str[i];
                    ++i;
                    if (c < 128) {
                        if ((asciiOptions[c] & IdentifierCharFlags.Continue) == 0) goto Error;
                        if (i == length) break;
                    } else {
                        isOnlyAscii = false;
                        if (!Text.IsSurrogate(c)) {
                            if (!IsXIdContinueOrSurrogate(c)) goto Error;
                            if (i == length) break;
                        } else {
                            if (i == length) goto Error; // check could be avoided for null-terminated buffer
                            char c1 = str[i];
                            if (c > 0xDBFF || !Text.IsLowSurrogate(c1)) goto ReturnWithError;
                            int cp = (c - 0xD800)*0x400 + c1 - 0xDC00; // codepoint minus 0x10000
                            if (!IsXIdContinueSmp(cp)) goto Error;
                            if (++i >= length) break;
                        }
                    }
                }
            } else { // duplicates the code from the previous case, the only difference being the (*) line
                for (;;) {
                    c = str[i];
                    ++i;
                    if (c < 128) {
                        if ((asciiOptions[c] & IdentifierCharFlags.Continue) == 0) goto Error;
                        if (i == length) break;
                    } else {
                        isOnlyAscii = false;
                        if (!Text.IsSurrogate(c)) {
                            if (!IsXIdContinueOrJoinControlOrSurrogate(c)) goto Error; // (*)
                            if (i == length) break;
                        } else {
                            if (i == length) goto Error; // check could be avoided for null-terminated buffer
                            char c1 = str[i];
                            if (c > 0xDBFF || !Text.IsLowSurrogate(c1)) goto ReturnWithError;
                            int cp = (c - 0xD800)*0x400 + c1 - 0xDC00; // codepoint minus 0x10000
                            if (!IsXIdContinueSmp(cp)) goto Error;
                            if (++i >= length) break;
                        }
                    }
                }
            }
        }
        errorPosition = -1;
#if SILVERLIGHT
        return str; // Silverlight does not support Unicode normalization
    Error:
#else
        if (NormalizationForm == 0 || (isOnlyAscii | isSecondRound)) return str;
        return str.Normalize(NormalizationForm);
    Error:
        if (NormalizeBeforeValidation && NormalizationForm != 0 && !(isOnlyAscii | isSecondRound)) {
            string nstr;
            try { nstr = str.Normalize(NormalizationForm); } // throws for invalid unicode characters
            catch (ArgumentException) { nstr = str; }
            if ((object)nstr != (object)str) {
                str = nstr;
                length = nstr.Length;
                isSecondRound = true;
                i = 1;
                goto IdStart;
            }
        }
#endif
    ReturnWithError:
        errorPosition = i - 1;
        return null;
    }

    private class IsIdStartCharOrSurrogateFSharpFunc : FSharpFunc<char, bool> {
        private IdentifierCharFlags[] AsciiCharOptions;
        public IsIdStartCharOrSurrogateFSharpFunc(IdentifierCharFlags[] asciiCharOptions) { AsciiCharOptions = asciiCharOptions; }

        public override bool Invoke(char ch) {
            if (ch < 128) return (AsciiCharOptions[ch] & IdentifierCharFlags.NonContinue) != 0;
            return IsXIdStartOrSurrogate(ch);
        }
    }

    private class IsIdContinueCharOrSurrogateFSharpFunc : FSharpFunc<char, bool> {
        private IdentifierCharFlags[] AsciiCharOptions;
        public IsIdContinueCharOrSurrogateFSharpFunc(IdentifierCharFlags[] asciiCharOptions) { AsciiCharOptions = asciiCharOptions; }

        public override bool Invoke(char ch) {
            if (ch < 128) return (AsciiCharOptions[ch] & IdentifierCharFlags.Continue) != 0;
            return IsXIdContinueOrSurrogate(ch);
        }
    }

    private class IsIdContinueCharOrJoinControlOrSurrogateFSharpFunc : FSharpFunc<char, bool> {
        private IdentifierCharFlags[] AsciiCharOptions;
        public IsIdContinueCharOrJoinControlOrSurrogateFSharpFunc(IdentifierCharFlags[] asciiCharOptions) { AsciiCharOptions = asciiCharOptions; }

        public override bool Invoke(char ch) {
            if (ch < 128) return (AsciiCharOptions[ch] & IdentifierCharFlags.Continue) != 0;
            return IsXIdContinueOrJoinControlOrSurrogate(ch);
        }
    }

    private FSharpFunc<char, bool> isIdStartOrSurrogateFunc;
    public  FSharpFunc<char, bool> IsIdStartOrSurrogateFunc { get {
        return     isIdStartOrSurrogateFunc
               ?? (isIdStartOrSurrogateFunc = new IsIdStartCharOrSurrogateFSharpFunc(AsciiCharOptions));
    } }

    private FSharpFunc<char, bool> isIdContinueOrSurrogateFunc;
    public  FSharpFunc<char, bool> IsIdContinueOrSurrogateFunc { get {
        return     isIdContinueOrSurrogateFunc
               ?? (isIdContinueOrSurrogateFunc = new IsIdContinueCharOrSurrogateFSharpFunc(AsciiCharOptions));
    } }

    private FSharpFunc<char, bool> isIdContinueOrJoinControlOrSurrogateFunc;
    public  FSharpFunc<char, bool> IsIdContinueOrJoinControlOrSurrogateFunc { get {
        return     isIdContinueOrJoinControlOrSurrogateFunc
               ?? (isIdContinueOrJoinControlOrSurrogateFunc = new IsIdContinueCharOrJoinControlOrSurrogateFSharpFunc(AsciiCharOptions));
    } }

    // The XID_START/XID_CONTINUE property data is stored in two multiple-stage lookup tables:
    // the BMP codepoints (0 - 0xFFFF) are stored in a two-stage table and the SMP codepoints (0x10000 - 0x10FFFF)
    // are stored in a three-stage table.
    //
    // Each two-stage table consists of an integer index arrays and one bit array.
    // Each three-stage table consists of two integer index arrays and one bit array.
    //
    // The first stage array is divided into multiple parts: one for XID_START, one for XID_CONTINUE
    // and -- only for the BMP table -- one in which in addition to the XID_CONTINUE chars the two
    // JOIN_CONTROL chars "zero-width non-joiner" (ZWNJ, '\u200C') and "zero-width joiner"
    // (ZWJ, '\u200D') are marked.
    // All codepoints in the BMP reserved for surrogates are marked as XID_START and XID_CONTINUE.
    //
    // The bits in the last stage array are stored in 32-bit words, where each 32-bit word
    // is stored in the platform byte order.
    //
    // To determine whether a codepoint has a property in a three-stage table,
    // three indices are computed:
    // idx1 = the (log_2 table1Length) most significant bits of the codepoint
    // idx2 = table1(START|CONTINUE|CONTINUE_OR_JOIN_CONTROL)[idx]*table2BlockLength
    //        + the following (log_2 table2BlockLength) bits of the codepoint
    // idx3 = table2[idx2]*table3BlockLength + the least significant (log_2 table3BlockLength) bits of the codepoint
    // If the bit in table3 at the bit index idx3 is set, the codepoint has the property, otherwise not.

    public static bool IsXIdStartOrSurrogate(char bmpCodePoint) { // should get inlined
        return (IsXIdStartOrSurrogate_(bmpCodePoint) & 1u) != 0;
    }
    private static uint IsXIdStartOrSurrogate_(char bmpCodePoint) {
        uint cp = (uint)bmpCodePoint;
        uint idx1 = cp >> XIdBmpTable2Log2BitBlockLength;
        const uint f2 = 1u << (XIdBmpTable2Log2BitBlockLength - 5);
        const uint m2 = f2 - 1;
        uint idx2 = XIdStartBmpTable1[idx1]*f2 + ((cp >> 5) & m2);
        return XIdBmpTable2[idx2] >> (int)(cp /* & 0x1fu */); // C#'s operator>> masks with 0x1fu, no matter whether we do too
    }

    public static bool IsXIdContinueOrSurrogate(char bmpCodePoint) { // should get inlined
         return (IsXIdContinueOrSurrogate_(bmpCodePoint) & 1u) != 0u;
    }
    private static uint IsXIdContinueOrSurrogate_(char bmpCodePoint) {
        uint cp = (uint)bmpCodePoint;
        uint idx1 = cp >> XIdBmpTable2Log2BitBlockLength;
        const uint f2 = 1u << (XIdBmpTable2Log2BitBlockLength - 5);
        const uint m2 = f2 - 1;
        uint idx2 = XIdContinueBmpTable1[idx1]*f2 + ((cp >> 5) & m2);
        return XIdBmpTable2[idx2] >> (int)(cp /* & 0x1fu */); // C#'s operator>> masks with 0x1fu, no matter whether we do too
    }

    public static bool IsXIdContinueOrJoinControlOrSurrogate(char bmpCodePoint) { // should get inlined
         return (IsXIdContinueOrJoinControlOrSurrogate_(bmpCodePoint) & 1u) != 0u;
    }
    private static uint IsXIdContinueOrJoinControlOrSurrogate_(char bmpCodePoint) {
        uint cp = (uint)bmpCodePoint;
        uint idx1 = cp >> XIdBmpTable2Log2BitBlockLength;
        const uint f2 = 1u << (XIdBmpTable2Log2BitBlockLength - 5);
        const uint m2 = f2 - 1;
        uint idx2 = XIdContinueOrJoinerBmpTable1[idx1]*f2 + ((cp >> 5) & m2);
        return XIdBmpTable2[idx2] >> (int)(cp /* & 0x1fu */); // C#'s operator>> masks with 0x1fu, no matter whether we do too
    }

    public static bool IsXIdStartSmp(int smpCodePointMinus0x10000) { // should get inlined
        return (IsXIdStartSmp_(smpCodePointMinus0x10000) & 1u) != 0;
    }
    private static uint IsXIdStartSmp_(int smpCodePointMinus0x10000) {
        uint cp = unchecked((uint)smpCodePointMinus0x10000);
        uint idx1 = cp >> (XIdSmpTable2Log2BlockLength + XIdSmpTable3Log2BlockLength);
        const uint f2 = 1u << XIdSmpTable2Log2BlockLength,
                   f3 = 1u << (XIdSmpTable3Log2BlockLength - 5);
        const uint m2 = f2 - 1, m3 = f3 - 1;
    #if !LOW_TRUST
        if ((idx1 & (0xffffffffu << XIdSmpTable1Log2Length)) != 0) throw new IndexOutOfRangeException();
    #endif
        uint idx2 = XIdStartSmpTable1[idx1]*f2 + ((cp >> XIdSmpTable3Log2BlockLength) & m2);
        uint idx3 = XIdSmpTable2[idx2]*f3 + ((cp >> 5) & m3);
        return XIdSmpTable3[idx3] >> (int)(cp /* & 0x1fu */); // C#'s operator>> masks with 0x1fu, no matter whether we do too
    }

    public static bool IsXIdContinueSmp(int smpCodePointMinus0x10000) { // should get inlined
        return (IsXIdContinueSmp_(smpCodePointMinus0x10000) & 1u) != 0;
    }
    private static uint IsXIdContinueSmp_(int smpCodePointMinus0x10000) {
        uint cp = unchecked((uint)smpCodePointMinus0x10000);
        uint idx1 = cp >> (XIdSmpTable2Log2BlockLength + XIdSmpTable3Log2BlockLength);
        const uint f2 = 1u << XIdSmpTable2Log2BlockLength,
                   f3 = 1u << (XIdSmpTable3Log2BlockLength - 5);
        const uint m2 = f2 - 1, m3 = f3 - 1;
    #if !LOW_TRUST
        if ((idx1 & (0xffffffffu << XIdSmpTable1Log2Length)) != 0) throw new IndexOutOfRangeException();
    #endif
        uint idx2 = XIdContinueSmpTable1[idx1]*f2 + ((cp >> XIdSmpTable3Log2BlockLength) & m2);
        uint idx3 = XIdSmpTable2[idx2]*f3 + ((cp >> 5) & m3);
        return XIdSmpTable3[idx3] >> (int)(cp /* & 0x1fu */); // C#'s operator>> masks with 0x1fu, no matter whether we do too
    }

    // tables for Unicode 6.1.0

    private const int XIdStartBmpTable1Offset = 0;
    private const int XIdContinueBmpTable1Offset = 256;
    private const int XIdContinueOrJoinerBmpTable1Offset = 512;
    private const int XIdBmpTable1Size = 256;
    private const int XIdBmpTable1Log2Length = 8;
    private const int XIdBmpTable2Offset = 768;
    private const int XIdBmpTable2Size = 2816;
    private const int XIdBmpTable2Log2BitBlockLength = 8;

    private const int XIdStartSmpTable1Offset = 3584;
    private const int XIdContinueSmpTable1Offset = 3840;
    private const int XIdSmpTable1Size = 256;
    private const int XIdSmpTable1Log2Length = 8;
    private const int XIdSmpTable2Offset = 4096;
    private const int XIdSmpTable2Size = 576;
    private const int XIdSmpTable2Log2BlockLength = 5;
    private const int XIdSmpTable3Offset = 4672;
    private const int XIdSmpTable3Size = 832;
    private const int XIdSmpTable3Log2BlockLength = 7;

    private static readonly byte[] DataArray = new byte[] {
        0,2,3,4,6,8,10,12,14,16,18,20,22,24,26,28,30,2,32,33,
        35,2,36,37,39,41,43,45,47,49,2,51,52,55,56,56,56,56,56,56,
        56,56,56,56,57,59,56,56,61,63,56,56,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,64,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,65,
        2,2,2,2,66,2,67,69,70,72,74,76,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,78,2,2,2,2,
        2,2,2,2,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
        56,56,56,56,56,56,56,56,56,2,79,80,82,83,84,86,1,2,3,5,
        7,9,11,13,15,17,19,21,23,25,27,29,31,2,32,34,35,2,36,38,
        40,42,44,46,48,50,2,51,53,55,56,56,56,56,56,56,56,56,56,56,
        58,60,56,56,62,63,56,56,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,64,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,65,2,2,2,2,
        66,2,68,69,71,73,75,77,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,78,2,2,2,2,2,2,2,2,
        56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
        56,56,56,56,56,2,79,81,82,83,85,87,1,2,3,5,7,9,11,13,
        15,17,19,21,23,25,27,29,31,2,32,34,35,2,36,38,40,42,44,46,
        48,50,2,51,54,55,56,56,56,56,56,56,56,56,56,56,58,60,56,56,
        62,63,56,56,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,64,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,65,2,2,2,2,66,2,68,69,
        71,73,75,77,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,78,2,2,2,2,2,2,2,2,56,56,56,56,
        56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
        56,2,79,81,82,83,85,87,0,0,0,0,0,0,0,0,254,255,255,7,
        254,255,255,7,0,0,0,0,0,4,32,4,255,255,127,255,255,255,127,255,
        0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,
        0,4,160,4,255,255,127,255,255,255,127,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,195,255,3,0,31,80,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,223,56,64,215,255,255,251,255,255,255,
        255,255,255,255,255,255,191,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,223,56,192,215,255,255,251,255,255,255,255,255,255,255,255,255,191,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,3,252,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,251,252,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,0,254,255,255,255,127,2,254,255,255,255,
        255,0,0,0,0,0,0,0,0,0,255,255,255,7,7,0,255,255,255,255,
        255,0,254,255,255,255,127,2,254,255,255,255,255,0,254,255,255,255,255,191,
        182,0,255,255,255,7,7,0,0,0,0,0,255,255,255,255,255,7,0,0,
        0,192,254,255,255,255,255,255,255,255,255,255,255,255,47,0,96,192,0,156,
        0,0,255,7,255,255,255,255,255,255,255,255,255,195,255,255,255,255,255,255,
        255,255,255,255,255,255,239,159,255,253,255,159,0,0,253,255,255,255,0,0,
        0,224,255,255,255,255,255,255,255,255,255,255,63,0,2,0,0,252,255,255,
        255,7,48,4,0,0,255,255,255,255,255,255,255,231,255,255,255,255,255,255,
        255,255,255,255,255,255,3,0,255,255,255,255,255,255,63,4,255,255,63,4,
        16,1,0,0,255,255,255,1,0,0,0,0,0,0,0,0,253,31,0,0,
        0,0,0,0,0,0,0,0,255,255,255,255,255,63,0,0,255,255,255,15,
        0,0,0,0,0,0,0,0,253,31,0,0,0,0,0,0,240,255,255,127,
        240,255,255,255,255,255,255,35,0,0,1,255,3,0,254,254,224,159,249,255,
        255,253,197,35,0,64,0,176,3,0,3,0,255,255,255,255,255,255,255,255,
        255,255,255,255,207,255,254,254,238,159,249,255,255,253,197,243,159,121,128,176,
        207,255,3,0,224,135,249,255,255,253,109,3,0,0,0,94,0,0,28,0,
        224,191,251,255,255,253,237,35,0,0,1,0,3,0,0,0,238,135,249,255,
        255,253,109,211,135,57,2,94,192,255,63,0,238,191,251,255,255,253,237,243,
        191,59,1,0,207,255,0,0,224,159,249,255,255,253,237,35,0,0,0,176,
        3,0,2,0,232,199,61,214,24,199,255,3,0,0,1,0,0,0,0,0,
        238,159,249,255,255,253,237,243,159,57,192,176,207,255,2,0,236,199,61,214,
        24,199,255,195,199,61,129,0,192,255,0,0,224,223,253,255,255,253,239,35,
        0,0,0,3,3,0,0,0,224,223,253,255,255,253,239,35,0,0,0,64,
        3,0,6,0,238,223,253,255,255,253,239,227,223,61,96,3,207,255,0,0,
        236,223,253,255,255,253,239,243,223,61,96,64,207,255,6,0,224,223,253,255,
        255,255,255,39,0,64,0,0,3,0,0,252,224,255,127,252,255,255,251,47,
        127,0,0,0,0,0,0,0,236,223,253,255,255,255,255,231,223,125,128,0,
        207,255,0,252,236,255,127,252,255,255,251,47,127,132,95,255,0,0,12,0,
        254,255,255,255,255,255,5,0,127,0,0,0,0,0,0,0,150,37,240,254,
        174,236,5,32,95,0,0,240,0,0,0,0,254,255,255,255,255,255,255,7,
        255,127,255,3,0,0,0,0,150,37,240,254,174,236,255,59,95,63,255,243,
        0,0,0,0,1,0,0,0,0,0,0,0,255,254,255,255,255,31,0,0,
        0,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,3,
        255,3,160,194,255,254,255,255,255,31,254,255,223,255,255,254,255,255,255,31,
        64,0,0,0,0,0,0,0,255,255,255,255,255,7,0,128,0,0,63,60,
        98,192,225,255,3,64,0,0,255,255,255,255,191,32,255,255,255,255,255,247,
        255,255,255,255,255,255,255,255,255,3,255,255,255,255,255,255,255,255,255,63,
        255,255,255,255,191,32,255,255,255,255,255,247,255,255,255,255,255,255,255,255,
        255,61,127,61,255,255,255,255,255,61,255,255,255,255,61,127,61,255,127,255,
        255,255,255,255,255,255,61,255,255,255,255,255,255,255,255,7,0,0,0,0,
        255,255,0,0,255,255,255,255,255,255,255,255,255,255,31,0,255,255,61,255,
        255,255,255,255,255,255,255,231,0,254,3,0,255,255,0,0,255,255,255,255,
        255,255,255,255,255,255,31,0,254,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,159,255,255,254,255,255,7,
        255,255,255,255,255,255,255,255,255,199,1,0,255,223,3,0,255,255,3,0,
        255,255,3,0,255,223,1,0,255,255,255,255,255,255,15,0,0,0,128,16,
        0,0,0,0,255,223,31,0,255,255,31,0,255,255,15,0,255,223,13,0,
        255,255,255,255,255,255,255,255,255,255,143,48,255,3,0,0,0,0,0,0,
        255,255,255,255,255,255,255,255,255,255,255,0,255,255,255,255,255,5,255,255,
        255,255,255,255,255,255,63,0,0,56,255,3,255,255,255,255,255,255,255,255,
        255,255,255,0,255,255,255,255,255,7,255,255,255,255,255,255,255,255,63,0,
        255,255,255,31,0,0,0,0,0,0,255,255,255,63,31,0,255,255,255,255,
        255,15,0,0,254,0,0,0,0,0,0,0,255,255,255,31,255,15,255,15,
        192,255,255,255,255,63,31,0,255,255,255,255,255,15,255,255,255,3,255,7,
        0,0,0,0,255,255,127,0,255,255,255,255,255,255,31,0,0,0,0,0,
        0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,255,255,255,15,
        255,255,255,255,255,255,255,127,255,255,255,159,255,3,255,3,128,0,0,0,
        0,0,0,0,0,0,0,0,224,255,255,255,255,255,15,0,224,15,0,0,
        0,0,0,0,248,255,255,255,1,192,0,252,255,255,255,255,63,0,0,0,
        255,255,255,255,255,255,255,255,255,15,255,3,0,248,15,0,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,15,0,255,255,255,255,15,0,0,0,
        0,224,0,252,255,255,255,63,0,0,0,0,0,0,0,0,0,0,0,0,
        0,222,99,0,255,255,255,255,255,255,255,0,255,227,255,255,255,255,255,63,
        0,0,0,0,0,0,0,0,0,0,247,255,255,255,127,0,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        0,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,127,0,0,240,
        255,255,63,63,255,255,255,255,63,63,255,170,255,255,255,63,255,255,255,255,
        255,255,223,95,220,31,207,15,255,31,220,31,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,2,128,0,0,255,31,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,128,1,0,16,0,0,0,2,128,
        0,0,255,31,0,0,0,0,0,0,255,31,226,255,1,0,0,48,0,0,
        0,0,0,128,1,0,16,0,0,0,2,128,0,0,255,31,0,0,0,0,
        0,0,255,31,226,255,1,0,132,252,47,63,80,253,255,243,224,67,0,0,
        255,255,255,255,255,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,255,255,255,255,255,127,255,255,
        255,255,255,127,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        31,120,12,0,255,255,255,255,255,127,255,255,255,255,255,127,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,31,248,15,0,255,255,255,255,
        191,32,255,255,255,255,255,255,255,128,0,0,255,255,127,0,127,127,127,127,
        127,127,127,127,0,0,0,0,255,255,255,255,191,32,255,255,255,255,255,255,
        255,128,0,128,255,255,127,0,127,127,127,127,127,127,127,127,255,255,255,255,
        224,0,0,0,254,3,62,31,254,255,255,255,255,255,255,255,255,255,127,224,
        254,255,255,255,255,255,255,255,255,255,255,247,224,0,0,0,254,255,62,31,
        254,255,255,255,255,255,255,255,255,255,127,230,254,255,255,255,255,255,255,255,
        255,255,255,247,224,255,255,255,255,63,254,255,255,255,255,255,255,255,255,255,
        255,127,0,0,255,255,255,7,0,0,0,0,0,0,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,63,0,
        0,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,31,0,0,0,0,0,0,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,31,0,0,
        0,0,0,0,0,0,255,255,255,255,255,63,255,31,255,255,0,12,0,0,
        255,255,255,255,255,127,0,128,255,255,255,0,255,255,255,255,255,255,255,255,
        255,255,0,0,255,31,255,255,255,15,0,0,255,255,255,255,255,255,240,191,
        255,255,255,128,255,255,255,255,255,255,255,255,255,255,3,0,0,0,128,255,
        252,255,255,255,255,255,255,255,255,255,255,255,255,121,15,0,255,7,0,0,
        0,0,0,0,0,0,0,255,187,247,255,255,7,0,0,0,255,255,255,255,
        255,255,15,0,252,255,255,255,255,255,15,0,0,0,0,0,0,0,252,8,
        255,255,255,255,255,0,0,0,255,255,255,255,255,255,15,0,255,255,255,255,
        255,255,255,255,31,0,255,3,255,255,255,8,0,252,255,255,63,0,255,255,
        127,0,0,0,255,255,255,31,240,255,255,255,255,255,7,0,0,128,0,0,
        0,0,0,0,255,255,255,255,255,63,255,255,255,255,15,0,255,255,255,31,
        255,255,255,255,255,255,255,255,1,128,255,3,0,0,0,0,255,255,255,255,
        255,1,0,0,247,15,0,0,255,255,127,4,255,255,255,255,255,255,98,62,
        5,0,0,56,255,7,28,0,255,255,255,255,255,255,127,0,255,63,255,3,
        255,255,127,12,255,255,255,255,255,255,255,255,7,0,0,56,255,255,124,0,
        126,126,126,0,127,127,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,255,255,255,255,7,0,0,0,126,126,126,0,127,127,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,255,255,
        255,55,255,3,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,15,0,255,255,127,248,255,255,255,255,255,15,255,255,255,255,
        255,255,255,255,255,255,255,255,255,63,255,255,255,255,255,255,255,255,255,255,
        255,255,255,3,0,0,0,0,127,0,248,160,255,253,127,95,219,255,255,255,
        255,255,255,255,255,255,255,255,255,255,3,0,0,0,248,255,255,255,255,255,
        127,0,248,224,255,253,127,95,219,255,255,255,255,255,255,255,255,255,255,255,
        255,255,3,0,0,0,248,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,63,240,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,63,0,0,255,255,255,255,255,255,
        255,255,252,255,255,255,255,255,255,0,0,0,0,0,255,3,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,138,170,255,255,255,255,255,255,255,255,
        255,255,255,255,255,255,255,31,255,255,0,0,127,0,24,0,0,224,0,0,
        0,0,138,170,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,31,
        0,0,0,0,254,255,255,7,254,255,255,7,192,255,255,255,255,255,255,63,
        255,255,255,127,252,252,252,28,0,0,0,0,0,0,255,3,254,255,255,135,
        254,255,255,7,192,255,255,255,255,255,255,255,255,255,255,127,252,252,252,28,
        0,0,0,0,
        0,2,4,5,6,6,7,6,6,6,6,9,6,10,12,6,13,13,13,13,
        13,13,13,13,13,13,14,15,6,6,6,16,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,1,3,4,5,
        6,6,8,6,6,6,6,9,6,11,12,6,13,13,13,13,13,13,13,13,
        13,13,14,15,6,6,6,16,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,17,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,0,1,2,3,3,5,6,7,
        8,9,3,3,3,3,3,3,11,3,12,13,14,3,16,3,17,3,3,3,
        3,3,3,3,0,1,2,4,3,5,6,7,8,10,3,3,3,3,3,3,
        11,3,12,13,15,3,16,3,17,3,3,3,3,3,3,3,18,20,22,24,
        3,3,3,3,3,3,3,3,3,26,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,19,21,23,25,3,3,3,3,3,3,3,3,
        3,27,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        8,8,8,8,8,8,28,3,29,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,8,8,8,8,8,8,8,8,
        30,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,8,8,8,8,31,3,3,3,
        3,3,3,3,3,3,32,34,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,8,8,8,8,31,3,3,3,3,3,3,3,3,3,33,35,
        36,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        40,41,42,8,8,43,44,45,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,37,38,39,3,3,3,40,41,42,8,8,43,44,46,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,47,48,3,3,8,8,8,8,8,8,8,8,8,8,8,8,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        8,8,8,8,8,8,8,8,8,8,8,8,8,49,8,8,8,8,8,8,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        8,8,8,8,8,8,50,8,9,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        8,8,8,8,9,3,3,3,3,3,3,3,3,3,3,3,3,3,8,51,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        3,3,3,3,3,3,3,3,255,239,255,255,127,255,255,183,255,63,255,63,
        0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,7,
        0,0,0,0,0,0,0,0,255,255,255,255,255,255,31,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,32,255,255,255,31,255,255,255,255,255,255,1,0,
        0,0,0,0,255,255,255,127,0,0,255,255,255,7,0,0,0,0,0,0,
        255,255,255,63,255,255,255,255,15,255,62,0,0,0,0,0,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,63,0,0,0,0,
        0,0,0,0,0,0,0,0,255,255,255,63,255,3,0,0,0,0,0,0,
        0,0,0,0,63,253,255,255,255,255,191,145,255,255,63,0,0,0,0,0,
        255,255,63,0,255,255,255,3,0,0,0,0,0,0,0,0,255,255,255,255,
        255,255,255,192,0,0,0,0,0,0,0,0,1,0,239,254,255,255,15,0,
        0,0,0,0,255,255,255,31,111,240,239,254,255,255,15,135,0,0,0,0,
        255,255,255,31,255,255,255,255,255,255,63,0,255,255,63,0,255,255,7,0,
        255,255,255,255,255,255,255,255,255,1,0,0,0,0,0,0,248,255,255,255,
        255,255,255,0,0,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,
        127,0,0,0,192,255,0,0,248,255,255,255,255,255,0,0,0,0,255,255,
        255,1,0,0,255,255,255,255,255,255,255,7,0,0,255,255,255,1,255,3,
        248,255,255,255,127,0,0,0,0,0,0,0,0,0,0,0,255,255,255,255,
        255,255,223,255,0,0,0,0,0,0,0,0,248,255,255,255,255,255,7,0,
        30,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,31,0,255,3,
        0,0,0,0,255,255,255,255,255,7,0,0,0,0,0,0,0,0,0,0,
        255,255,255,255,255,255,255,0,255,3,0,0,0,0,0,0,255,255,255,255,
        255,255,255,255,255,255,255,255,255,127,0,0,255,255,255,255,255,255,255,255,
        255,255,255,255,7,0,0,0,255,255,255,255,255,127,0,0,0,0,0,0,
        0,0,0,0,255,255,255,255,255,255,255,1,0,0,0,0,0,0,0,0,
        255,255,255,255,255,255,255,255,31,0,1,0,0,0,0,0,255,255,255,255,
        255,255,255,255,31,0,255,255,255,255,255,127,0,0,248,255,0,0,0,0,
        0,0,0,0,0,0,0,0,0,128,255,255,0,0,0,0,0,0,0,0,
        0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,224,227,7,248,231,15,0,0,
        0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        28,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,255,255,223,255,
        255,255,255,255,255,255,255,223,100,222,255,235,239,255,255,255,255,255,255,255,
        191,231,223,223,255,255,255,123,95,252,253,255,255,255,255,255,255,255,255,255,
        63,255,255,255,253,255,255,247,255,255,255,247,255,255,223,255,255,255,223,255,
        255,127,255,255,255,127,255,255,255,253,255,255,255,253,255,255,247,15,0,0,
        0,0,0,0,255,253,255,255,255,253,255,255,247,207,255,255,255,255,255,255,
        239,255,255,255,150,254,247,10,132,234,150,170,150,247,247,94,255,251,255,15,
        238,251,255,15,0,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,
        255,255,127,0,0,0,0,0,255,255,255,255,255,255,31,0,255,255,255,255,
        255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,0,0,
    };

#if LOW_TRUST
    private static readonly byte[] XIdStartBmpTable1 = Buffer.CopySubarray(DataArray, XIdStartBmpTable1Offset, XIdBmpTable1Size);
    private static readonly byte[] XIdContinueBmpTable1 = Buffer.CopySubarray(DataArray, XIdContinueBmpTable1Offset, XIdBmpTable1Size);
    private static readonly byte[] XIdContinueOrJoinerBmpTable1 = Buffer.CopySubarray(DataArray, XIdContinueOrJoinerBmpTable1Offset, XIdBmpTable1Size);
    private static readonly uint[] XIdBmpTable2 = Buffer.CopyUIntsStoredInLittleEndianByteArray(DataArray, XIdBmpTable2Offset, XIdBmpTable2Size);

    private static readonly byte[] XIdStartSmpTable1 = Buffer.CopySubarray(DataArray, XIdStartSmpTable1Offset, XIdSmpTable1Size);
    private static readonly byte[] XIdContinueSmpTable1 = Buffer.CopySubarray(DataArray, XIdContinueSmpTable1Offset, XIdSmpTable1Size);
    private static readonly byte[] XIdSmpTable2 = Buffer.CopySubarray(DataArray, XIdSmpTable2Offset, XIdSmpTable2Size);
    private static readonly uint[] XIdSmpTable3 = Buffer.CopyUIntsStoredInLittleEndianByteArray(DataArray, XIdSmpTable3Offset, XIdSmpTable3Size);
#else
    private static byte* Data = LoadDataArrayIntoFixedBuffer();

    private static readonly byte* XIdStartBmpTable1 = Data + XIdStartBmpTable1Offset;
    private static readonly byte* XIdContinueBmpTable1 = Data + XIdContinueBmpTable1Offset;
    private static readonly byte* XIdContinueOrJoinerBmpTable1 = Data + XIdContinueOrJoinerBmpTable1Offset;
    private static readonly uint* XIdBmpTable2 = (uint*)(Data + XIdBmpTable2Offset);

    private static readonly byte* XIdStartSmpTable1 = Data + XIdStartSmpTable1Offset;
    private static readonly byte* XIdContinueSmpTable1 = Data + XIdContinueSmpTable1Offset;
    private static readonly byte* XIdSmpTable2 = Data + XIdSmpTable2Offset;
    private static readonly uint* XIdSmpTable3 = (uint*)(Data + XIdSmpTable3Offset);

    private static byte* LoadDataArrayIntoFixedBuffer() {
        IntPtr buffer = UnmanagedMemoryPool.Allocate(DataArray.Length);
        Marshal.Copy(DataArray, 0, buffer, DataArray.Length);
        Debug.Assert(XIdBmpTable2Size%sizeof(uint) == 0);
        Debug.Assert(XIdSmpTable3Size%sizeof(uint) == 0);
        if (!System.BitConverter.IsLittleEndian) {
            Buffer.SwapByteOrder((uint*)((byte*)buffer + XIdBmpTable2Offset), XIdBmpTable2Size/sizeof(uint));
            Buffer.SwapByteOrder((uint*)((byte*)buffer + XIdSmpTable3Offset), XIdSmpTable3Size/sizeof(uint));
        }
        return (byte*)buffer;
    }
#endif

}
}