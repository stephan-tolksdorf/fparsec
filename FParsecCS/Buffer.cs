// Copyright (c) Stephan Tolksdorf 2007-2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Buffers.Binary;
using System.Diagnostics;

namespace FParsec {

public static class Buffer {

#if !LOW_TRUST

/// <summary>Calculates: end - begin. <br />
/// Precondition: 2^31 > end - begin >= 0.</summary>
internal static unsafe uint PositiveDistance(char* begin, char* end) {
    return (uint)((byte*)end - (byte*)begin)/2;
}

/// <summary>Calculates: end - begin. <br />
/// Precondition: end - begin >= 0.</summary>
internal static unsafe long PositiveDistance64(char* begin, char* end) {
    return (long)((ulong)((byte*)end - (byte*)begin)/2);
}

/// <summary>Copies size bytes from src to dst. Correctly handles overlapped memory blocks.</summary>
static internal unsafe void Copy(byte* dst, byte* src, int size) {
    if (size < 0) throw new ArgumentOutOfRangeException("size", "The size must be non-negative.");

    System.Buffer.MemoryCopy(src, dst, size, size);
}

#endif

internal static uint SwapByteOrder(uint value) => BinaryPrimitives.ReverseEndianness(value);

internal static ulong SwapByteOrder(ulong value) => BinaryPrimitives.ReverseEndianness(value);

internal static void SwapByteOrder(Span<uint> array) {
    for (int i = 0; i < array.Length; ++i) {
        array[i] = BinaryPrimitives.ReverseEndianness(array[i]);
    }
}

#if LOW_TRUST

internal static byte[] CopySubarray(byte[] array, int index, int length) {
    var subArray = new byte[length];
    System.Buffer.BlockCopy(array, index, subArray, 0, length);
    return subArray;
}

internal static uint[] CopyUIntsStoredInLittleEndianByteArray(byte[] src, int srcIndex, int srcLength) {
    Debug.Assert(srcLength%sizeof(uint) == 0);
    var subArray = new uint[srcLength/sizeof(uint)];
    System.Buffer.BlockCopy(src, srcIndex, subArray, 0, srcLength);
    if (!BitConverter.IsLittleEndian) SwapByteOrder(subArray);
    return subArray;
}
#endif

#if !LOW_TRUST
// used by StaticMapping.createStaticStringMapping
public static unsafe bool Equals(uint* ptr1, uint* ptr2, uint length) {
    Debug.Assert(length >= 0);
    for (; length >= 4; length -= 4) {
        if (   ptr1[0] != ptr2[0]
            || ptr1[1] != ptr2[1]
            || ptr1[2] != ptr2[2]
            || ptr1[3] != ptr2[3]) goto ReturnFalse;
        ptr1 += 4;
        ptr2 += 4;
    }
    if ((length & 2) != 0) {
        if (   ptr1[0] != ptr2[0]
            || ptr1[1] != ptr2[1]) goto ReturnFalse;
        ptr1 += 2;
        ptr2 += 2;
    }
    if ((length & 1) != 0) {
        if (ptr1[0] != ptr2[0]) goto ReturnFalse;
    }
    return true;
ReturnFalse:
    return false;
}
#endif

}

}