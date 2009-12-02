// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Diagnostics;
using System.Text;
using System.Globalization;
using System.Runtime.InteropServices;

namespace FParsec {

public static class Helper {

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
                if (encoding.CodePage != 65001) encoding = Encoding.UTF8;
                return 3;
            }
        break;
        case 0xFE:
            if (buffer[1] == 0xFF) {
                if (encoding.CodePage != 1201) encoding = Encoding.BigEndianUnicode;
                return 2;
            }
        break;
        case 0xFF:
            if (buffer[1] == 0xFE) {
                if (count >= 4 && buffer[2] == 0x00 && buffer[3] == 0x00) {
                    if (encoding.CodePage != 12000) encoding = Encoding.UTF32; // UTF-32 little endian
                    return 4;
                } else {
                    if (encoding.CodePage != 1200) encoding = Encoding.Unicode; // UTF-16 little endian
                    return 2;
                }
            }
        break;
        case 0x00:
            if (buffer[1] == 0x00 && count >= 4 && buffer[2] == 0xFE && buffer[3] == 0xFF) {
                if (encoding.CodePage != 12001) encoding = new UTF32Encoding(true, true); // UTF-32 big endian
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

internal static bool ContainsNewlineChar(string str) {
    foreach (char c in str) {
        if (c > '\r') continue;
        if (c == '\r' || c == '\n') goto ReturnTrue;
    }
    return false;
ReturnTrue:
    return true;
}

#if !LOW_TRUST
// This is about the fastest way to run a parser on a string.
// (If you need something faster, you'll have to recycle CharStream and (first) State instances;
//  but that's too application specific to be efficiently implementable in this library.)
[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:Dispose objects before losing scope", Justification="The CharStream is manually disposed.")]
internal unsafe static T RunParserOnString<T,TParser,TUserState>(
                             string str, int index, int length,
                             Microsoft.FSharp.Core.FSharpFunc<TParser,Microsoft.FSharp.Core.FSharpFunc<State<TUserState>,T>> applyParser,
                             TParser parser,
                             TUserState userState,
                             string streamName)
{
    CharStream.Anchor anchor;
    fixed (char* pStr = str) {
        var stream = new CharStream(str, pStr, pStr + index, length, 0, &anchor);
        try {
            var data0  = new State<TUserState>.Data(1, 0, userState, streamName);
            var state0 = new State<TUserState>(stream.Begin, data0);
            var applyParserOpt = applyParser as Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc<TParser, State<TUserState>, T>;
            if (applyParserOpt != null)
                return applyParserOpt.Invoke(parser, state0);
            else
                return applyParser.Invoke(parser).Invoke(state0);
        } finally { // manually dispose stream
            if (stream.anchor != null) {
                stream.anchor = null;
                anchor.StreamHandle.Free();
            }
        }
    }
}
#endif

#if LOW_TRUST
internal static T RunParserOnSubstream<T,TUserState,TSubStreamUserState>(
                             Microsoft.FSharp.Core.FSharpFunc<State<TSubStreamUserState>,T> parser,
                             TSubStreamUserState userState,
                             State<TUserState> stateBeforeSubStream, State<TUserState> stateAfterSubStream)
{
    CharStream stream = stateBeforeSubStream.Iter.Stream;
    if (stream != stateAfterSubStream.Iter.Stream)
        throw new ArgumentException("The states are associated with different CharStreams.");

    int idx0 = stateBeforeSubStream.Iter.Idx;
    int idx1 = stateAfterSubStream.Iter.Idx;
    if (idx0 < 0) idx0 = stream.IndexEnd;
    if (idx1 < 0) idx1 = stream.IndexEnd;
    if (idx0 > idx1)
        throw new ArgumentException("The position of the second state lies before the position of the first state.");
    var subStream = new CharStream(stream.String, idx0, idx1 - idx0, (idx0 - stream.IndexBegin) + stream.StreamIndexOffset);
    var data0 = stateBeforeSubStream.data;
    var data = new State<TSubStreamUserState>.Data(data0.Line, data0.LineBegin, userState, data0.StreamName);
    var state = new State<TSubStreamUserState>(subStream.Begin, data);
    return parser.Invoke(state);
}
#else
internal unsafe static T RunParserOnSubstream<T,TUserState,TSubStreamUserState>(
                             Microsoft.FSharp.Core.FSharpFunc<State<TSubStreamUserState>,T> parser,
                             TSubStreamUserState userState,
                             State<TUserState> stateBeforeSubStream, State<TUserState> stateAfterSubStream)
{
    CharStream.Anchor subStreamAnchor;
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
        using (var subStream = new CharStream(stream.BufferString, stream.BufferStringPointer, ptr, length, s0.Index, &subStreamAnchor)) {
            var data0 = stateBeforeSubStream.data;
            var data = new State<TSubStreamUserState>.Data(data0.Line, data0.LineBegin, userState, data0.StreamName);
            var state = new State<TSubStreamUserState>(subStream.Begin, data);
            return parser.Invoke(state);
        }
    } else if (s0.Iter.Block == s1.Iter.Block && anchor->Block == s1.Iter.Block) {
        char* ptr = s0.Iter.Ptr;
        char* end = s1.Iter.Ptr;
        if (end < ptr) throw new ArgumentException("The position of the second state lies before the position of the first state.");
        int length = CharStream.PositiveDistance(ptr, end);
        string subString = new String(ptr, 0, length);
        fixed (char* pSubString = subString)
        using (var subStream = new CharStream(subString, pSubString, pSubString, length, s0.Index, &subStreamAnchor)) {
            var data0 = stateBeforeSubStream.data;
            var data = new State<TSubStreamUserState>.Data(data0.Line, data0.LineBegin, userState, data0.StreamName);
            var state = new State<TSubStreamUserState>(subStream.Begin, data);
            return parser.Invoke(state);
        }
    } else {
        ulong index1 = (ulong)s0.Iter.Index;
        ulong index2 = (ulong)s1.Iter.Index;
        if (index2 < index1) throw new ArgumentException("The position of the second state lies before the position of the first state.");
        ulong length_ = index2 - index1;
        // length >= Int32.MaxValue will trigger an exception anyway (because the string is too large)
        int length = length_ > (uint)System.Int32.MaxValue ? System.Int32.MaxValue : (int)length_;
        string subString = new String('\u0000', length);
        fixed (char* pSubString = subString) {
            s0.Iter.Read(pSubString, length);
            using (var subStream = new CharStream(subString, pSubString, pSubString, length, s0.Index, &subStreamAnchor)) {
                var data0 = stateBeforeSubStream.data;
                var data = new State<TSubStreamUserState>.Data(data0.Line, data0.LineBegin, userState, data0.StreamName);
                var state = new State<TSubStreamUserState>(subStream.Begin, data);
                return parser.Invoke(state);
            }
        }
    }
}
#endif

#if !LOW_TRUST
    unsafe
#endif
public sealed class CharSet {
    private const int wordSize = 32;
    private const int log2WordSize = 5;

    private int    min;
    private int    max;
    private int    tableMin;
    private int[]  table;
    private string charsNotInTable; // We use a string here instead of a char[] because the JIT
                                    // produces better code for loops involving strings.

    public CharSet(string chars) : this(chars, 32) {}
    // because of mandatory bounds checking, we wouldn't get any advantage from a fixed size table

    public CharSet(string chars, int maxTableSize) {
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

    #if LOW_TRUST
        var notInTable = nCharsNotInTable > 0 ? new char[nCharsNotInTable] : null;
    #else
        charsNotInTable = nCharsNotInTable > 0 ? new string('\u0000', nCharsNotInTable) : null;
        fixed (char* notInTable = charsNotInTable) {
    #endif
            prevChar = chars[0] != 'x' ? 'x' : 'y';
            int n = 0;
            for (int i = 0; i < chars.Length; ++i) {
                char c = chars[i];
                if (c == prevChar) continue;
                prevChar = c;
                int off = c - tableMin;
                int idx = off >> log2WordSize;
                if (unchecked((uint)idx) < (uint)table.Length) {
                    table[idx] |= 1 << off; // we don't need to mask off because C#'s operator<< does that for us
                } else {
                    notInTable[n++] = c;
                }
            }
            Debug.Assert(n == nCharsNotInTable);
    #if !LOW_TRUST
        }
    #else
       if (nCharsNotInTable > 0) charsNotInTable = new string(notInTable);
    #endif
    }

    public bool Contains(char value) {
        int off = value - tableMin;
        int idx = off >> log2WordSize;
        if (unchecked((uint)idx) < (uint)table.Length) {
            return ((table[idx] >> off) & 1) != 0; // we don't need to mask off because C#'s operator>> does that for us
        }
        if (charsNotInTable == null) return false;
        if (value >= min && value <= max) {
            foreach (char c in charsNotInTable) {
                if (c == value) goto ReturnTrue;
            }
        }
        return false;
    ReturnTrue:
        return true;
    }
}

/// <summary>A faster implementation of System.Globalization.StringInfo(str).LengthInTextElements.</summary>
internal static int CountTextElements(string str) {
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
            if (uc == UnicodeCategory.Surrogate) {
                uc = System.Char.GetUnicodeCategory(str, i - 1);
                if (uc != UnicodeCategory.Surrogate) ++i;
            }
            switch (uc) {
            case UnicodeCategory.NonSpacingMark:
            case UnicodeCategory.SpacingCombiningMark:
            case UnicodeCategory.EnclosingMark:
            case UnicodeCategory.Control:
            case UnicodeCategory.Format:
            case UnicodeCategory.Surrogate:
            case UnicodeCategory.OtherNotAssigned:
                continue;
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
            if (uc == UnicodeCategory.Surrogate) {
                uc = System.Char.GetUnicodeCategory(str, i - 1);
                if (uc != UnicodeCategory.Surrogate) ++i;
            }
            switch (uc) {
            case UnicodeCategory.NonSpacingMark:
            case UnicodeCategory.SpacingCombiningMark:
            case UnicodeCategory.EnclosingMark:
                continue;
            case UnicodeCategory.Control:
            case UnicodeCategory.Format:
            case UnicodeCategory.Surrogate:
            case UnicodeCategory.OtherNotAssigned:
                ++count;
                goto SkipBaseCharacter;
            default:
                ++count;
                continue;
            }
        }
        break;
    }
    return count;
}

} // class Helper

}
