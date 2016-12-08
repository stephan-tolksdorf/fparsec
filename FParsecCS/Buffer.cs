// Copyright (c) Stephan Tolksdorf 2007-2010
// License: Simplified BSD License. See accompanying documentation.

using System;

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

// Probably for pedagogical reasons there is no System.Buffer.BlockCopy
// that takes pointers, hence we are forced to write our own version.

/// <summary>Copies size bytes from src to dst. Correctly handles overlapped memory blocks.</summary>
static internal unsafe void Copy(byte* dst, byte* src, int size) {
    if (size < 0) throw new ArgumentOutOfRangeException("size", "The size must be non-negative.");

    // C# doesn't support native ints and the 32-bit .NET JIT can't optimize the
    // 64-comparison into a single 32-bit one, so we have to get our hands dirty...

    // goto Reverse if src < dst && dst - src < size
    if (sizeof(IntPtr) == 4) {
        if (unchecked((uint)(dst - src)) < (uint)size) goto Reverse;
    } else {
        if (unchecked((ulong)(dst - src)) < (ulong)size) goto Reverse;
    }

#if UNALIGNED_READS
    // with UNALIGNED_READS we don't require identical 2-byte alignment
    if (((uint)dst & 1) == ((uint)src & 1)) {
#else
    if (((uint)dst & 3) == ((uint)src & 3)) {
#endif
        // the pointers have identical byte (and 2-byte) alignment

        // align dst
        if (((uint)dst & 1) != 0 && size != 0) {
            *dst = *src;
            ++src; ++dst; --size;
        }
        if (((uint)dst & 2) != 0 && size >= 2) {
            *((short*)dst) = *((short*)src);
            src += 2; dst += 2;; size -= 2;
        }

        for (; size >= 16; size -= 16) {
            ((int*)dst)[0] = ((int*)src)[0];
            ((int*)dst)[1] = ((int*)src)[1];
            ((int*)dst)[2] = ((int*)src)[2];
            ((int*)dst)[3] = ((int*)src)[3];
            src += 16; dst += 16;
        }
        if ((size != 0)) {
            if ((size & 8) != 0) {
                ((int*)dst)[0] = ((int*)src)[0];
                ((int*)dst)[1] = ((int*)src)[1];
                src += 8; dst += 8;
            }
            if ((size & 4) != 0) {
                *((int*)dst) = *((int*)src);
                src += 4; dst += 4;
            }
            if ((size & 2) != 0) {
                *((short*)dst) = *((short*)src);
                src += 2; dst += 2;
            }
            if ((size & 1) != 0) {
                *dst = *src;
            }
        }
        return;
    } else {
        // backup path for pointers with different byte (or 2-byte) alignment
        for (; size != 0; --size) {
            *dst = *src;
            ++src; ++dst;
        }
        return;
    }

Reverse:
    src += size; dst += size;
#if UNALIGNED_READS
    // with UNALIGNED_READS we don't require identical 2-byte alignment
    if (((uint)dst & 1) == ((uint)src & 1)) {
#else
    if (((uint)dst & 3) == ((uint)src & 3)) {
#endif
        // the pointers have identical byte (and 2-byte) alignment

        // align dst
        if (((uint)dst & 1) != 0 && size != 0) {
            --src; --dst; --size;
            *dst = *src;
        }
        if (((uint)dst & 2) != 0 && size >= 2) {
            src -= 2; dst -= 2; size -= 2;
            *((short*)dst) = *((short*)src);
        }
        for (; size >= 16; size -= 16) {
            src -= 16; dst -= 16;
            ((int*)dst)[3] = ((int*)src)[3];
            ((int*)dst)[2] = ((int*)src)[2];
            ((int*)dst)[1] = ((int*)src)[1];
            ((int*)dst)[0] = ((int*)src)[0];
        }
        if ((size & 0xf) != 0) {
            if ((size & 8) != 0) {
                src -= 8; dst -= 8;
                ((int*)dst)[1] = ((int*)src)[1];
                ((int*)dst)[0] = ((int*)src)[0];
            }
            if ((size & 4) != 0) {
                src -= 4; dst -= 4;
                *((int*)dst) = *((int*)src);
            }
            if ((size & 2) != 0) {
                src -= 2; dst -= 2;
                *((short*)dst) = *((short*)src);
            }
            if ((size & 1) != 0) {
                src -= 1; dst -= 1;
                *dst = *src;
            }
        }
        return;
    } else {
        // backup path for pointers with different byte (or 2-byte) alignment
        for (; size != 0; --size) {
            --src; --dst;
            *dst = *src;
        }
        return;
    }
}

#endif

internal static uint SwapByteOrder(uint value) {
    return    (((value << 24) | (value >>  8)) & 0xff00ff00U)
            | (((value <<  8) | (value >> 24)) & 0x00ff00ffU);
}

internal static ulong SwapByteOrder(ulong value) {
    return   (((value << 56) | (value >>  8)) & 0xff000000ff000000UL)
           | (((value <<  8) | (value >> 56)) & 0x000000ff000000ffUL)
           | (((value << 40) | (value >> 24)) & 0x00ff000000ff0000UL)
           | (((value << 24) | (value >> 40)) & 0x0000ff000000ff00UL);
}

internal static void SwapByteOrder(uint[] array) {
    for (int i = 0; i < array.Length; ++i) {
        var v = array[i];
        array[i] =   (((v << 24) | (v >>  8)) & 0xff00ff00U)
                   | (((v <<  8) | (v >> 24)) & 0x00ff00ffU);
    }
}

#if !LOW_TRUST

internal static unsafe void SwapByteOrder(uint* buffer, uint length) {
    for (int i = 0; i < length; ++i) {
        var v = buffer[i];
        buffer[i] =   (((v << 24) | (v >>  8)) & 0xff00ff00U)
                    | (((v <<  8) | (v >> 24)) & 0x00ff00ffU);
    }
}

#endif

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