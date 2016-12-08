// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Diagnostics;

namespace FParsec {

#if !LOW_TRUST
    unsafe
#endif
internal sealed class CharSet {
    private const int WordSize = 32;
    private const int Log2WordSize = 5;

    private int    Min;
    private int    Max;
    private int    BitTableMin;
    private int[]  BitTable;
    private string CharsNotInBitTable; // We use a string here instead of a char[] because the
                                       // .NET JITs tend to  produce better code for loops involving strings.

    public CharSet(string chars) : this(chars, 32) {}
    // because of mandatory bounds checking, we wouldn't get any advantage from a fixed size table

    public CharSet(string chars, int maxTableSize) {
        if (chars.Length == 0) {
            BitTableMin = Min = 0x10000;
            Max = -1;
            BitTable = new int[0];
            // charsNotInTable = null;
            return;
        }
        if (maxTableSize < 4) maxTableSize = 4;
        else if (maxTableSize > 0x10000/WordSize) maxTableSize  = 0x10000/WordSize;
        int maxTableBits = maxTableSize*WordSize;

        char prevChar = chars[0];
        Min = prevChar;
        Max = prevChar;
        BitTableMin = -1;
        int bitTableMax = -1;
        int nCharsNotInTable = 0;
        for (int i = 1; i < chars.Length; ++i) {
            char c = chars[i];
            if (c == prevChar) continue; // filter out repeated chars
            prevChar = c;
            int prevMin = Min;
            if (c < Min) Min = c;
            int prevMax = Max;
            if (c > Max) Max = c;
            if (BitTableMin < 0) {
                // the first time the table range is exceeded the tableMin is set
                if (Max - Min >= maxTableBits) {
                    BitTableMin = prevMin; // stays fixed
                    bitTableMax = prevMax; // will be updated later
                    nCharsNotInTable = 1;
                }
            } else if (c < BitTableMin || c >= BitTableMin + maxTableBits) {
                ++nCharsNotInTable;
            } else {
                bitTableMax = Math.Max(c, bitTableMax);
            }
        }
        if (BitTableMin < 0) {
            BitTableMin = Min;
            bitTableMax = Max;
        }
        int tableSize =   bitTableMax - BitTableMin + 1 < maxTableBits
                        ? (bitTableMax - BitTableMin + 1)/WordSize + ((bitTableMax - BitTableMin + 1)%WordSize != 0 ? 1 : 0)
                        : maxTableSize;
        BitTable = new int[tableSize];

    #if LOW_TRUST
        var notInTable = nCharsNotInTable > 0 ? new char[nCharsNotInTable] : null;
    #else
        CharsNotInBitTable = nCharsNotInTable > 0 ? new string('\u0000', nCharsNotInTable) : "";
        fixed (char* notInTable = CharsNotInBitTable) {
    #endif
            prevChar = chars[0] != 'x' ? 'x' : 'y';
            int n = 0;
            for (int i = 0; i < chars.Length; ++i) {
                char c = chars[i];
                if (c == prevChar) continue;
                prevChar = c;
                int off = c - BitTableMin;
                int idx = off >> Log2WordSize;
                if (unchecked((uint)idx) < (uint)BitTable.Length) {
                    BitTable[idx] |= 1 << off; // we don't need to mask off because C#'s operator<< does that for us
                } else {
                    notInTable[n++] = c;
                }
            }
            Debug.Assert(n == nCharsNotInTable);
    #if !LOW_TRUST
        }
    #else
       if (nCharsNotInTable > 0) CharsNotInBitTable = new string(notInTable);
    #endif
    }

    public bool Contains(char value) {
        int off = value - BitTableMin;
        int idx = off >> Log2WordSize;
        if (unchecked((uint)idx) < (uint)BitTable.Length) {
            return ((BitTable[idx] >> off) & 1) != 0; // we don't need to mask off because C#'s operator>> does that for us
        }
        if (CharsNotInBitTable == null) return false;
        if (value >= Min && value <= Max) {
            foreach (char c in CharsNotInBitTable) {
                if (c == value) goto ReturnTrue;
            }
        }
        return false;
    ReturnTrue:
        return true;
    }
}

}