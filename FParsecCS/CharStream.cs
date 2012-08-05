// Copyright (c) Stephan Tolksdorf 2007-2012
// License: Simplified BSD License. See accompanying documentation.

#if !LOW_TRUST

using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.Serialization;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

using Microsoft.FSharp.Core;

using FParsec.Cloning;

namespace FParsec {

/// <summary>An opaque representation of a CharStream index.</summary>
public unsafe struct CharStreamIndexToken {
#if DEBUG
    internal readonly CharStream CharStream;
    private long Index { get { return GetIndex(CharStream); } }
#endif
    internal readonly char* Ptr;
    private readonly int BlockPlus1;
    /// <summary>Returns -1 if the IndexToken was zero-initialized.</summary>
    internal int Block { get { return unchecked(BlockPlus1 - 1); } }

    internal CharStreamIndexToken(
                              #if DEBUG
                                  CharStream charStream,
                              #endif
                                  char* ptr,
                                  int block)
    {
    #if DEBUG
        CharStream = charStream;
    #endif
        Ptr = ptr;
        BlockPlus1 = unchecked(block + 1);
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void ThrowInvalidIndexToken() {
        throw new InvalidOperationException("The CharStreamIndexToken is invalid.");
    }

    public long GetIndex(CharStream charStreamFromWhichIndexTokenWasRetrieved) {
        int block = Block;
        if (block < 0) ThrowInvalidIndexToken(); // tests for a zero-initialized IndexToken
    #if DEBUG
        Debug.Assert(CharStream == charStreamFromWhichIndexTokenWasRetrieved);
    #endif
        return charStreamFromWhichIndexTokenWasRetrieved.GetIndex(Ptr, block);
    }
}

public struct TwoChars : IEquatable<TwoChars> {
    private uint Chars;

    internal TwoChars(uint chars) {
        Chars = chars;
    }
    public TwoChars(char char0, char char1) {
        Chars = ((uint)char1 << 16) | (uint)char0;
    }

    public char Char0 { get { return unchecked((char)Chars); } }
    public char Char1 { get { return (char)(Chars >> 16); } }

    public override bool Equals(object obj) { return (obj is TwoChars) && Chars == ((TwoChars) obj).Chars; }
    public bool Equals(TwoChars other) { return Chars == other.Chars; }
    public override int GetHashCode()  { return unchecked((int)Chars); }
    public static bool operator==(TwoChars left, TwoChars right) { return left.Chars == right.Chars; }
    public static bool operator!=(TwoChars left, TwoChars right) { return left.Chars != right.Chars; }
}

/// <summary>Provides read‐access to a sequence of UTF‐16 chars.</summary>
public unsafe class CharStream : IDisposable {

    // In order to facilitate efficient backtracking we divide the stream into overlapping
    // blocks with equal number of chars. The blocks are overlapping, so that
    // backtracking over short distances at a block boundary doesn't trigger a reread of the
    // previous block.
    //
    //              Block 0
    //
    //    -----------------|--------  Block 1
    //                       Overlap
    //                      --------|--------|--------  Block 2
    //                                        Overlap
    //                                        --------|--------|--------
    //                                                                  (...)
    //  a '-' symbolizes a char, a '|' a block boundary.
    //
    //
    // In general there's no fixed relationship between the number of input bytes and the
    // number of input chars. Worse, the encoding can be stateful, which makes it necessary
    // to persist the decoder state over block boundaries. If we later want to
    // be able to reread a certain block, we therefore need to keep record of various
    // bits of information describing the state of the input stream at the beginning of a block:

    private class BlockInfo {
        /// <summary>the byte stream index of the first char in the block after the OverhangCharsAtBlockBegin</summary>
        public long ByteIndex;
        /// <summary>the value of the CharStream's ByteBufferIndex before the block is read</summary>
        public int ByteBufferIndex;

        /// <summary>the number of bytes in the stream from ByteIndex to the first char after the OverhangCharsAfterOverlap</summary>
        public int NumberOfBytesInOverlap;

        /// <summary>the last char in the overlap with the previous block (used for integrity checking)</summary>
        public char LastCharInOverlap;

        /// <summary>chars at the block begin that were already read together with chars of the last block before the overlap</summary>
        public string OverhangCharsAtBlockBegin;
        /// <summary>chars after the overlap with the previous block that were already read together with the overlap chars</summary>
        public string OverhangCharsAfterOverlap;

        // Unfortunately the Decoder API has no explicit methods for managing the state,
        // which forces us to use the comparatively inefficient serialization API
        // (via FParsec.Cloning) for this purpose.
        // The absence of explicit state management or at least a cloning method in the
        // Decoder interface is almost as puzzling to me as the absence of such methods
        // in System.Random.

        public CloneImage DecoderImageAtBlockBegin;
        public CloneImage DecoderImageAfterOverlap;

        public BlockInfo(long byteIndex, int byteBufferIndex,
                         int nBytesInOverlapCount, char lastCharInOverlap,
                         string overhangCharsAtBlockBegin, CloneImage decoderImageAtBlockBegin,
                         string overhangCharsAfterOverlap, CloneImage decoderImageAfterOverlap)
        {
            ByteIndex = byteIndex;
            ByteBufferIndex = byteBufferIndex;
            NumberOfBytesInOverlap = nBytesInOverlapCount;
            LastCharInOverlap = lastCharInOverlap;
            OverhangCharsAtBlockBegin = overhangCharsAtBlockBegin;
            OverhangCharsAfterOverlap = overhangCharsAfterOverlap;
            DecoderImageAtBlockBegin = decoderImageAtBlockBegin;
            DecoderImageAfterOverlap = decoderImageAfterOverlap;
        }
    }

    private const int DefaultBlockSize = 3*(1 << 16); // 3*2^16 = 200k
    private const int DefaultByteBufferLength = (1 << 12);
    private static int MinimumByteBufferLength = 128; // must be larger than longest detectable preamble (we can only guess here)
    private const char EOS = '\uFFFF';

    public const char EndOfStreamChar = EOS;

    /// <summary>Points to the current char in Buffer,
    /// or is null if the end of the stream has been reached.</summary>
    internal char* Ptr;
    /// <summary>Equals Ptr == null ? null : BufferBegin.</summary>
    internal char* PtrBegin;
    /// <summary>Equals Ptr == null ? null : BufferEnd.</summary>
    internal char* PtrEnd;

    /// <summary>Begin of the used part of the char buffer. Is constant. Is null if the CharStream is empty.</summary>
    internal char* BufferBegin;
    /// <summary>End of the used part of the char buffer. Varies for a multi-block stream. Is null if the CharStream is empty.</summary>
    internal char* BufferEnd;

    /// <summary>The block currently loaded in the buffer.</summary>
    internal int Block;

    /// <summary>Any CharStream method or property setter increments this value when it changes the CharStream state.
    /// Backtracking to an old state also restores the old value of the StateTag.</summary>
    public
#if SMALL_STATETAG
           int
#else
           long
#endif
                 StateTag;

    internal long IndexOfFirstCharInBlock;

    internal long _IndexOfFirstChar;
    /// <summary>The index of the first char in the stream.</summary>
    public long IndexOfFirstChar { get { return _IndexOfFirstChar; } }

    internal long _Line;
    /// <summary>The line number for the next char. (The line count starts with 1.)</summary>
    public long Line { get { return _Line; } }
    public void SetLine_WithoutCheckAndWithoutIncrementingTheStateTag(long line) {
        _Line = line;
    }

    internal long _LineBegin;
    /// <summary>The stream index of the first char of the line that also contains the next char.</summary>
    public long LineBegin { get { return _LineBegin; } }
    public void SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag(long lineBegin) {
        _LineBegin = lineBegin;
    }

    /// <summary>The UTF‐16 column number of the next char, i.e. Index ‐ LineBegin  + 1.</summary>
    public long Column { get { return Index - LineBegin + 1; } }

    internal string _Name;
    public string Name {
        get { return _Name; }
        set { _Name = value; ++StateTag; }
    }

    /// <summary>The Encoding that is used for decoding the underlying byte stream, or
    /// System.Text.UnicodeEncoding in case the stream was directly constructed
    /// from a string or char buffer.</summary>
    public Encoding Encoding { get; private set; }

    // If the CharStream is constructed from a binary stream, we use a managed string as the char
    // buffer. This allows us to apply regular expressions directly to the input.
    // In the case of multi-block CharStreams we thus have to mutate the buffer string through pointers.
    // This is safe as long as we use a newly constructed string and we don't pass a reference
    // to the internal buffer string to the "outside world". (The one instance where we have to pass
    // a reference to the buffer string is regex matching. See the docs for Match(regex) for more info.)
    //
    // Apart from Match(regex) we access the internal buffer only through a pinned pointer.
    // This way we avoid the overhead of redundant bounds checking and can support strings, char arrays
    // and unmanaged char buffers through the same interface.
    //
    // Pinning a string or char array makes life more difficult for the GC. However, as long as
    // the buffer is only short-lived or large enough to be allocated on the large object heap,
    // there shouldn't be a problem. Furthermore, the buffer strings for CharStreams constructed
    // from a binary stream are allocated through the StringBuffer interface and hence always live
    // on the large object heap. Thus, the only scenario to really worry about (and which the
    // documentation explicitly warns about) is when a large number of small CharStreams
    // are constructed directly from strings or char arrays and are used for an extended period of time.

    /// <summary>The string holding the char buffer, or null if the buffer is not part of a .NET string.</summary>
    internal string BufferString;
    /// <summary>A pointer to the beginning of BufferString, or null if BufferString is null.</summary>
    internal char* BufferStringPointer;

    /// <summary>Holds the GCHandle for CharStreams directly constructed from strings or char arrays.</summary>
    private GCHandle BufferHandle;
    /// <summary>Holds the StringBuffer for CharStreams constructed from a binary stream.</summary>
    private StringBuffer StringBuffer;

#if DEBUG
    internal FSharpRef<int> SubstreamCount = new FSharpRef<int>(0);
    internal FSharpRef<int> ParentSubstreamCount = null;
#endif

    private MultiBlockData BlockData;
    internal bool IsSingleBlockStream { get { return BlockData == null; } }

    /// <summary>Contains the data and methods needed in case the input byte stream
    /// is large enough to span multiple blocks of the CharStream.</summary>
    private partial class MultiBlockData {
        public CharStream CharStream;

        public long IndexOfLastCharPlus1;

        /// <summary>The index of the last block of the stream, or Int32.MaxValue if the end of stream has not yet been detected.</summary>
        public int LastBlock;

        public Stream Stream;
        // we keep a separate record of the Stream.Position, so that we don't need to require Stream.CanSeek
        public long StreamPosition;
        // we use StreamLength to avoid calling Read() again on a non-seekable stream after it returned 0 once (see ticket #23)
        public long StreamLength;
        public bool LeaveOpen;

        public int MaxCharCountForOneByte;
        public Decoder Decoder;
        public bool DecoderIsSerializable;

        public int BlockSize;
        public int BlockOverlap;
        /// <summary>BufferBegin + BlockSize - minRegexSpace</summary>
        public char* RegexSpaceThreshold;

        /// <summary>The byte stream index of the first unused byte in the ByteBuffer.</summary>
        public long ByteIndex { get { return StreamPosition - (ByteBufferCount - ByteBufferIndex); } }

        public List<BlockInfo> Blocks;

        public byte[] ByteBuffer;
        public int ByteBufferIndex;
        public int ByteBufferCount;
    }

    public long IndexOfLastCharPlus1 { get {
        return BlockData != null ? BlockData.IndexOfLastCharPlus1
                                 : IndexOfFirstChar + Buffer.PositiveDistance(BufferBegin, BufferEnd);
    } }

    public int BlockOverlap { get {
        return BlockData == null ? 0 : BlockData.BlockOverlap;
    } }

    public int MinRegexSpace {
        get {
            return BlockData == null
                   ? 0
                   : (int)Buffer.PositiveDistance(BlockData.RegexSpaceThreshold,
                                                  BufferBegin + BlockData.BlockSize);
        }
        set {
            if (BlockData != null) {
                if (value < 0 || value > BlockData.BlockOverlap) throw new ArgumentOutOfRangeException("value", "The MinRegexSpace value must be non-negative and not greater than the BlockOverlap.");
                BlockData.RegexSpaceThreshold = BufferBegin + BlockData.BlockSize - value;
            }
        }
    }

    public bool IsBeginOfStream { get { return Ptr == BufferBegin && Block == 0; } }
    public bool IsEndOfStream { get { return Ptr == null; } }


    public long Index {
    #if CLR45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
    #endif
        get {
            if (Ptr != null) {
                Debug.Assert(BufferBegin <= Ptr && Ptr < BufferEnd);
                if (sizeof(System.IntPtr) != 8) // the JIT removes the inactive branch
                    return Buffer.PositiveDistance(PtrBegin, Ptr) + IndexOfFirstCharInBlock;
                else
                    return Buffer.PositiveDistance64(PtrBegin, Ptr) + IndexOfFirstCharInBlock;
            }
            Debug.Assert(BlockData == null || BlockData.IndexOfLastCharPlus1 != Int64.MaxValue);
            return IndexOfLastCharPlus1;
        }
    }

    internal long GetIndex(char* ptr, int block) {
        if (ptr != null) {
            if (block == Block) {
                Debug.Assert(BufferBegin <= ptr && ptr < BufferEnd);
                if (sizeof(System.IntPtr) != 8)
                    return Buffer.PositiveDistance(BufferBegin, ptr) + IndexOfFirstCharInBlock;
                else
                    return Buffer.PositiveDistance64(BufferBegin, ptr) + IndexOfFirstCharInBlock;
            } else {
                Debug.Assert(BlockData != null && BufferBegin <= ptr && ptr < BufferBegin + BlockData.BlockSize);
                int blockSizeMinusOverlap = BlockData.BlockSize - BlockData.BlockOverlap;
                long indexOfBlockBegin = IndexOfFirstChar + Math.BigMul(block, blockSizeMinusOverlap);
                if (sizeof(System.IntPtr) != 8)
                    return Buffer.PositiveDistance(BufferBegin, ptr) + indexOfBlockBegin;
                else
                    return Buffer.PositiveDistance64(BufferBegin, ptr) + indexOfBlockBegin;
            }
        }
        Debug.Assert(BlockData == null || BlockData.IndexOfLastCharPlus1 != Int64.MaxValue);
        return IndexOfLastCharPlus1;
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public Position Position { get {
        long index = Index;
        return new Position(_Name, index, Line, index - LineBegin + 1);
    } }

    // we don't have a public constructor that only takes a string to avoid potential confusion with a filepath constructor
    internal CharStream(string chars) {
        Debug.Assert(chars != null);
        BufferString = chars;
        BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
        char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject();
        BufferStringPointer = bufferBegin;
        CharConstructorContinue(bufferBegin, chars.Length);
    }

    public CharStream(string chars, int index, int length) : this(chars, index, length, 0) {}
    public CharStream(string chars, int index, int length, long streamIndexOffset) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (index < 0) throw new ArgumentOutOfRangeException("index", "index is negative.");
        if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "index or length is out of range.");
        if (streamIndexOffset < 0 || streamIndexOffset >= (1L << 60)) throw new ArgumentOutOfRangeException("streamIndexOffset", "streamIndexOffset must be non-negative and less than 2^60.");
        IndexOfFirstCharInBlock = streamIndexOffset;
        _IndexOfFirstChar = streamIndexOffset;
        _LineBegin = streamIndexOffset;

        BufferString = chars;
        BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
        char* pBufferString = (char*)BufferHandle.AddrOfPinnedObject();
        BufferStringPointer = pBufferString;
        CharConstructorContinue(pBufferString + index, length);
    }

    public CharStream(char[] chars, int index, int length) : this(chars, index, length, 0) { }
    public CharStream(char[] chars, int index, int length, long streamIndexOffset) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (index < 0) throw new ArgumentOutOfRangeException("index", "index is negative.");
        if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "index or length is out of range.");
        if (streamIndexOffset < 0 || streamIndexOffset >= (1L << 60)) throw new ArgumentOutOfRangeException("streamIndexOffset", "streamIndexOffset must be non-negative and less than 2^60.");
        IndexOfFirstCharInBlock = streamIndexOffset;
        _IndexOfFirstChar = streamIndexOffset;
        _LineBegin = streamIndexOffset;

        BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
        char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject() + index;
        if (bufferBegin < unchecked(bufferBegin + length + 1)) { // a pedantic check ...
            CharConstructorContinue(bufferBegin, length);
        } else {
            // ... for a purely theoretic case
            BufferHandle.Free();
            throw new ArgumentOutOfRangeException("length", "The char array may not be allocated directly below the end of the address space.");
        }

    }

    public CharStream(char* chars, int length) : this(chars, length, 0) {}
    public CharStream(char* chars, int length, long streamIndexOffset) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
        if (chars >= unchecked(chars + length + 1)) // chars + length + 1 must not overflow (the + 1 is needed for some methods below)
            throw new ArgumentOutOfRangeException("length", "length is too large.");
        if (streamIndexOffset < 0 || streamIndexOffset >= (1L << 60)) throw new ArgumentOutOfRangeException("streamIndexOffset", "streamIndexOffset must be non-negative and less than 2^60.");
        IndexOfFirstCharInBlock = streamIndexOffset;
        _IndexOfFirstChar = streamIndexOffset;
        _LineBegin = streamIndexOffset;
        CharConstructorContinue(chars, length);
    }

    private void CharConstructorContinue(char* bufferBegin, int length) {
        Debug.Assert((bufferBegin != null || length == 0) && length >= 0
                     && bufferBegin < unchecked(bufferBegin + length + 1)); // the + 1 is needed for some methods below

        if (length != 0) {
            BufferBegin = bufferBegin;
            BufferEnd = bufferBegin + length;
            Ptr = bufferBegin;
            PtrBegin = bufferBegin;
            PtrEnd = BufferEnd;
        }
        _Line = 1;
        Encoding = Encoding.Unicode;
    }

    internal CharStream(string chars, char* pChars, char* begin, int length) {
        Debug.Assert((chars == null ? pChars == null
                                    : pChars <= begin && length >= 0 && (int)Buffer.PositiveDistance(pChars, begin) <= chars.Length - length)
                     && (begin == null ? length == 0
                                       : length >= 0 && begin < unchecked(begin + length + 1)));

        BufferString = chars;
        BufferStringPointer = pChars;
        if (length != 0) {
            BufferBegin = begin;
            BufferEnd = begin + length;
            Ptr = begin;
            PtrBegin = begin;
            PtrEnd = BufferEnd;
        }
        _Line = 1;
        Encoding = Encoding.Unicode;
    }

    public CharStream(string path, Encoding encoding)
           : this(path, encoding, true,
                  DefaultBlockSize, DefaultBlockSize/3, DefaultByteBufferLength) { }

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(path, encoding, detectEncodingFromByteOrderMarks,
                  DefaultBlockSize, DefaultBlockSize/3, DefaultByteBufferLength) { }

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks,
                      int blockSize, int blockOverlap, int byteBufferLength)
    {
        if (encoding == null) throw new ArgumentNullException("encoding");
        var stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan);
        try {
           StreamConstructorContinue(stream, false, encoding, detectEncodingFromByteOrderMarks,
                                     blockSize, blockOverlap, byteBufferLength);
           _Name = path;
        } catch {
            stream.Dispose();
            throw;
        }
    }

    public CharStream(Stream stream, Encoding encoding)
           : this(stream,
                  false, encoding, true,
                  DefaultBlockSize, DefaultBlockSize/3, DefaultByteBufferLength) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding)
           : this(stream,
                  leaveOpen, encoding, true,
                  DefaultBlockSize, DefaultBlockSize/3, DefaultByteBufferLength) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(stream,
                  leaveOpen, encoding, detectEncodingFromByteOrderMarks,
                  DefaultBlockSize, DefaultBlockSize/3, DefaultByteBufferLength) { }

    public CharStream(Stream stream, bool leaveOpen,
                      Encoding encoding, bool detectEncodingFromByteOrderMarks,
                      int blockSize, int blockOverlap, int byteBufferLength)
    {
        if (stream == null) throw new ArgumentNullException("stream");
        if (!stream.CanRead) throw new ArgumentException("stream is not readable");
        if (encoding == null) throw new ArgumentNullException("encoding");
        StreamConstructorContinue(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks,
                                  blockSize, blockOverlap, byteBufferLength);
    }

    /// <summary>we modify this flag via reflection in the unit test</summary>
    private static bool DoNotRoundUpBlockSizeToSimplifyTesting = false;

    private void StreamConstructorContinue(Stream stream, bool leaveOpen,
                                           Encoding encoding, bool detectEncodingFromByteOrderMarks,
                                           int blockSize, int blockOverlap, int byteBufferLength)
    {
        if (byteBufferLength < MinimumByteBufferLength) byteBufferLength = MinimumByteBufferLength;

        int remainingBytesCount = -1;
        long streamPosition;
        long streamLength;
        if (stream.CanSeek) {
            streamPosition = stream.Position;
            streamLength = stream.Length;
            long remainingBytesCount64 = streamLength - streamPosition;
            if (remainingBytesCount64 <= Int32.MaxValue) {
                remainingBytesCount = (int)remainingBytesCount64;
                if (remainingBytesCount < byteBufferLength) byteBufferLength = remainingBytesCount;
            }
        } else {
            streamPosition = 0;
            streamLength = Int64.MaxValue;
        }

        byte[] byteBuffer = new byte[byteBufferLength];
        int byteBufferCount = 0;
        do {
            int n = stream.Read(byteBuffer, byteBufferCount, byteBufferLength - byteBufferCount);
            if (n == 0) {
                remainingBytesCount = byteBufferCount;
                Debug.Assert(!stream.CanSeek || streamPosition + byteBufferCount == streamLength);
                streamLength = streamPosition + byteBufferCount;
                break;
            }
            byteBufferCount += n;
        } while (byteBufferCount < MinimumByteBufferLength);
        streamPosition += byteBufferCount;

        int preambleLength = Text.DetectPreamble(byteBuffer, byteBufferCount, ref encoding, detectEncodingFromByteOrderMarks);
        remainingBytesCount -= preambleLength;

        _Line = 1;
        Encoding = encoding;

        // we allow such small block sizes only to simplify testing
        if (blockSize < 8) blockSize = DefaultBlockSize;

        bool allCharsFitIntoOneBlock = false;
        if (remainingBytesCount >= 0 && remainingBytesCount/4 <= blockSize) {
            if (remainingBytesCount != 0) {
                try {
                    int maxCharCount = Encoding.GetMaxCharCount(remainingBytesCount); // may throw ArgumentOutOfRangeException
                    if (blockSize >= maxCharCount) {
                        allCharsFitIntoOneBlock = true;
                        blockSize = maxCharCount;
                    }
                } catch (ArgumentOutOfRangeException) { }
            } else {
                allCharsFitIntoOneBlock = true;
                blockSize = 0;
            }
        }
        var buffer = StringBuffer.Create(blockSize);
        Debug.Assert(buffer.Length >= blockSize && (blockSize > 0 || buffer.StringPointer == null));
        StringBuffer = buffer;
        BufferString = buffer.String;
        BufferStringPointer = buffer.StringPointer;
        char* bufferBegin = buffer.StringPointer + buffer.Index;
        try {
            Decoder decoder = encoding.GetDecoder();
            if (allCharsFitIntoOneBlock) {
                int bufferCount = preambleLength == byteBufferCount
                                  ? 0
                                  : Text.ReadAllRemainingCharsFromStream(bufferBegin, buffer.Length, byteBuffer, preambleLength, byteBufferCount, stream, streamPosition, decoder, streamPosition == streamLength);
                if (!leaveOpen) stream.Close();
                if (bufferCount != 0) {
                    BufferBegin = bufferBegin;
                    Ptr = bufferBegin;
                    PtrBegin = bufferBegin;
                    BufferEnd = bufferBegin + bufferCount;
                    PtrEnd = BufferEnd;
                }
                Block = 0;
            } else {
                if (!DoNotRoundUpBlockSizeToSimplifyTesting) blockSize = buffer.Length;
                BufferBegin = bufferBegin;
                BufferEnd = bufferBegin;
                var d = new MultiBlockData();
                BlockData = d;
                d.CharStream = this;
                d.Stream = stream;
                d.StreamPosition = streamPosition;
                d.StreamLength = streamLength;
                d.LeaveOpen = leaveOpen;
                d.Decoder = decoder;
                d.DecoderIsSerializable = decoder.GetType().IsSerializable;
                d.ByteBuffer = byteBuffer;
                d.ByteBufferIndex = preambleLength;
                d.ByteBufferCount = byteBufferCount;
                d.MaxCharCountForOneByte = Math.Max(1, Encoding.GetMaxCharCount(1));
                if (d.MaxCharCountForOneByte > 1024) // an arbitrary limit low enough that a char array with this size can be allocated on the stack
                    throw new ArgumentException("The CharStream class does not support Encodings with GetMaxCharCount(1) > 1024.");
                if (blockSize < 3*d.MaxCharCountForOneByte) blockSize = 3*d.MaxCharCountForOneByte;
                // MaxCharCountForOneByte == the maximum number of overhang chars
                if(    Math.Min(blockOverlap, blockSize - 2*blockOverlap) < d.MaxCharCountForOneByte
                    || blockOverlap >= blockSize/2) blockOverlap = blockSize/3;
                d.BlockSize = blockSize;
                d.BlockOverlap = blockOverlap;
                d.RegexSpaceThreshold = bufferBegin + (blockSize - 2*blockOverlap/3);
                d.IndexOfLastCharPlus1 = Int64.MaxValue;
                Block = -2; // special value recognized by ReadBlock
                d.LastBlock = Int32.MaxValue;
                d.Blocks = new List<BlockInfo>();
                // the first block has no overlap with a previous block
                d.Blocks.Add(new BlockInfo(preambleLength, preambleLength, 0, EOS, null, null, null, null));
                d.ReadBlock(0);
                if (d.LastBlock == 0) {
                    if (!d.LeaveOpen) d.Stream.Close();
                    BlockData = null;
                }
            }
        } catch {
            buffer.Dispose();
            throw;
        }
    }

    public void Dispose() {
    #if DEBUG
        lock (SubstreamCount) {
            if (SubstreamCount.Value != 0)
                throw new InvalidOperationException("A CharStream must not be disposed before all of its Substreams have been disposed.");
        }
        if (ParentSubstreamCount != null) {
            lock (ParentSubstreamCount) --ParentSubstreamCount.Value;
        }
    #endif
        if (BufferHandle.IsAllocated) BufferHandle.Free();
        if (StringBuffer != null) StringBuffer.Dispose();
        if (BlockData != null && !BlockData.LeaveOpen) BlockData.Stream.Close();
        Ptr = null;
        PtrBegin = null;
        PtrEnd = null;
        BufferBegin = null;
        BufferEnd = null;
    }

    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:Dispose objects before losing scope", Justification="The CharStream is manually disposed.")]
    public static T ParseString<T,TUserState>(string chars, int index, int length,
                                              FSharpFunc<CharStream<TUserState>,T> parser,
                                              TUserState userState,
                                              string streamName)
    {
        if (index < 0) throw new ArgumentOutOfRangeException("index", "index is negative.");
        if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "length is out of range.");
        fixed (char* pChars = chars) {
            var stream = new CharStream<TUserState>(chars, pChars, pChars + index, length);
            stream.UserState = userState;
            stream._Name = streamName;
            try {
                return parser.Invoke(stream);
            } finally {
            #if DEBUG
                stream.Dispose();
            #else
                // manually dispose stream
                stream.Ptr = null;
                stream.PtrBegin = null;
                stream.PtrEnd = null;
                stream.BufferBegin = null;
                stream.BufferEnd = null;
            #endif
            }
        }
    }

    private partial class MultiBlockData {
        /// <summary>Refills the ByteBuffer if no unused byte is remaining.
        /// Returns the number of unused bytes in the (refilled) ByteBuffer.</summary>
        private int FillByteBuffer() {
            int n = ByteBufferCount - ByteBufferIndex;
            if (n > 0) return n;
            return ClearAndRefillByteBuffer(0);
        }

        /// <summary>Refills the ByteBuffer starting at the given index. If the underlying byte
        /// stream contains enough bytes, the ByteBuffer is filled up to the ByteBuffer.Length.
        /// Returns the number of bytes available for consumption in the refilled ByteBuffer.</summary>
        private int ClearAndRefillByteBuffer(int byteBufferIndex) {
            Debug.Assert(byteBufferIndex >= 0 && byteBufferIndex <= ByteBuffer.Length);
            // Stream.Read is not guaranteed to use all the provided output buffer, so we need
            // to call it in a loop when we want to rely on the buffer being fully filled
            // (unless we reach the end of the stream). Knowing that the buffer always gets
            // completely filled allows us to calculate the buffer utilization after skipping
            // a certain number of input bytes. For most streams there will be only one loop
            // iteration anyway (or two at the end of the stream).
            int i = byteBufferIndex;
            int m = ByteBuffer.Length - byteBufferIndex;
            while (m != 0 && StreamPosition != StreamLength) { // we check the StreamPosition to avoid calling Read after it returned 0 at the end of the stream (see ticket #23)
                int c = Stream.Read(ByteBuffer, i, m);
                if (c != 0) {
                    i += c;
                    m -= c;
                    StreamPosition += c;
                } else {
                    Debug.Assert(!Stream.CanSeek || StreamPosition == StreamLength);
                    StreamLength = StreamPosition;
                    break;
                }
            }
            int n = i - byteBufferIndex;
            ByteBufferIndex = byteBufferIndex;
            ByteBufferCount = byteBufferIndex + n;
            return n;
        }

        /// <summary>Reads up to the given maximum number of chars into the given buffer.
        /// If more than the maximum number of chars have to be read from the stream in order to
        /// fill the buffer (due to	the way the Decoder API works), the overhang chars are
        /// returned through the output parameter.
        /// Returns a pointer to one char after the last char read.</summary>
        private char* ReadCharsFromStream(char* buffer, int maxCount, out string overhangChars) {
            Debug.Assert(maxCount >= 0);
            fixed (byte* byteBuffer = ByteBuffer) {
                overhangChars = null;
                try {
                    while (maxCount >= MaxCharCountForOneByte) {// if maxCount < MaxCharCountForOneByte, Convert could throw
                        int nBytesInByteBuffer = FillByteBuffer();
                        bool flush = nBytesInByteBuffer == 0;
                        int bytesUsed, charsUsed; bool completed = false;
                        Decoder.Convert(byteBuffer + ByteBufferIndex, nBytesInByteBuffer,
                                        buffer, maxCount, flush,
                                        out bytesUsed, out charsUsed, out completed);
                        ByteBufferIndex += bytesUsed; // GetChars consumed bytesUsed bytes from the byte buffer
                        buffer += charsUsed;
                        maxCount -= charsUsed;
                        if (flush && completed) return buffer;
                    }
                    if (maxCount == 0) return buffer;

                    char* cs = stackalloc char[MaxCharCountForOneByte];
                    for (;;) {
                        int nBytesInByteBuffer = FillByteBuffer();
                        bool flush = nBytesInByteBuffer == 0;
                        int bytesUsed, charsUsed; bool completed;
                        Decoder.Convert(byteBuffer + ByteBufferIndex, nBytesInByteBuffer,
                                        cs, MaxCharCountForOneByte, flush,
                                        out bytesUsed, out charsUsed, out completed);
                        ByteBufferIndex += bytesUsed;
                        if (charsUsed > 0) {
                            int i = 0;
                            do {
                                *buffer = cs[i];
                                ++buffer; ++i;
                                if (--maxCount == 0) {
                                    if (i < charsUsed) overhangChars = new string(cs, i, charsUsed - i);
                                    return buffer;
                                }
                            } while (i < charsUsed);
                        }
                        if (flush && completed) return buffer;
                    }
                } catch (DecoderFallbackException e) {
                    e.Data.Add("Stream.Position", ByteIndex + e.Index);
                    throw;
                }
            }
        }

        /// <summary> Reads a block of chars (which must be different from the current block)
        /// into the BufferString. If the current CharStream block is block - 1, this method
        /// seeks the CharStream to the first char after the overlap of the two blocks.
        /// Otherwise it seeks the CharStream to the first char in the block. It returns the
        /// CharStream.Ptr value at the new position (which can be null).</summary>
        internal char* ReadBlock(int block) {
            int prevBlock = CharStream.Block;
            if (block == prevBlock) throw new InvalidOperationException();
            if (!DecoderIsSerializable && block > 0) {
                if (prevBlock > block)
                    throw new NotSupportedException("The CharStream does not support seeking backwards over ranges longer than the block overlap because the Encoding's Decoder is not serializable. The decoder has the type: " + Decoder.GetType().FullName);
                while (prevBlock + 1 < block) ReadBlock(++prevBlock);
            }

            BlockInfo bi = Blocks[block]; // will throw if block is out of range
            int blockSizeMinusOverlap = BlockSize - BlockOverlap;
            long charIndex = Math.BigMul(block, blockSizeMinusOverlap);
            char* bufferBegin = CharStream.BufferBegin;
            char* begin, buffer;
            int nCharsToRead;

            // fill [0 ... BlockOverlap-1] if block > 0
            if (prevBlock == block - 1) {
                Buffer.Copy((byte*)bufferBegin, (byte*)(bufferBegin + blockSizeMinusOverlap),
                                  BlockOverlap*sizeof(char));
                Debug.Assert(bufferBegin[BlockOverlap - 1] == bi.LastCharInOverlap);
                begin = buffer = bufferBegin + BlockOverlap;
            } else if (prevBlock >= 0) {
                Stream.Seek(bi.ByteIndex, SeekOrigin.Begin); // will throw if Stream can't seek
                // now that there was no exception, we can change the state...
                StreamPosition = bi.ByteIndex;
                ClearAndRefillByteBuffer(bi.ByteBufferIndex);
                if (block != 0)
                    Decoder = (Decoder)bi.DecoderImageAtBlockBegin.CreateClone();
                else
                    Decoder.Reset();
                if (prevBlock == block + 1) {
                    // move the overlap into [BlockSize - BlockOverlap, BlockSize - 1] before it gets overwritten
                    Buffer.Copy((byte*)(bufferBegin + blockSizeMinusOverlap), (byte*)bufferBegin,
                                      BlockOverlap*sizeof(char));
                }
                begin = buffer = bufferBegin;
                if (block > 0) {
                    nCharsToRead = BlockOverlap;
                    if (bi.OverhangCharsAtBlockBegin != null) {
                        nCharsToRead -= bi.OverhangCharsAtBlockBegin.Length;
                        for (int i = 0; i < bi.OverhangCharsAtBlockBegin.Length; ++i)
                            *(buffer++) = bi.OverhangCharsAtBlockBegin[i];
                    }
                    string overhangCharsAfterOverlap;
                    buffer = ReadCharsFromStream(buffer, nCharsToRead, out overhangCharsAfterOverlap);
                    if (   buffer != bufferBegin + BlockOverlap
                        || ByteIndex != bi.ByteIndex + bi.NumberOfBytesInOverlap
                        || *(buffer - 1) != bi.LastCharInOverlap
                        || overhangCharsAfterOverlap != bi.OverhangCharsAfterOverlap)
                        throw new IOException("CharStream: stream integrity error");
                }
            } else { // ReadBlock was called from the constructor
                if (block != 0) throw new InvalidOperationException();
                begin = buffer = bufferBegin;
            }

            // fill [0            ... BlockSize-BlockOverlap-1] if block == 0
            // and  [BlockOverlap ... BlockSize-BlockOverlap-1] otherwise
            if (block == 0) {
                nCharsToRead = blockSizeMinusOverlap;
            } else {
                nCharsToRead = blockSizeMinusOverlap - BlockOverlap;
                if (bi.OverhangCharsAfterOverlap != null) {
                    nCharsToRead -= bi.OverhangCharsAfterOverlap.Length;
                    for (int i = 0; i < bi.OverhangCharsAfterOverlap.Length; ++i)
                        *(buffer++) = bi.OverhangCharsAfterOverlap[i];
                }
            }
            string overhangCharsAtNextBlockBegin;
            buffer = ReadCharsFromStream(buffer, nCharsToRead, out overhangCharsAtNextBlockBegin);

            long byteIndexAtNextBlockBegin = ByteIndex;
            int byteBufferIndexAtNextBlockBegin = ByteBufferIndex;

            // fill [BlockSize-BlockOverlap ... BlockSize-1]
            if (block == Blocks.Count - 1) { // next block hasn't yet been read
                Cloner cloner = null;
                CloneImage decoderImageAtNextBlockBegin = null;
                if (DecoderIsSerializable) {
                    cloner = Cloner.Create(Decoder.GetType());
                    decoderImageAtNextBlockBegin = cloner.CaptureImage(Decoder);
                }
                nCharsToRead = BlockOverlap;
                if (overhangCharsAtNextBlockBegin != null) {
                    nCharsToRead -= overhangCharsAtNextBlockBegin.Length;
                    for (int i = 0; i < overhangCharsAtNextBlockBegin.Length; ++i)
                        *(buffer++) = overhangCharsAtNextBlockBegin[i];
                }
                string overhangCharsAfterOverlapWithNextBlock;
                buffer = ReadCharsFromStream(buffer, nCharsToRead, out overhangCharsAfterOverlapWithNextBlock);
                if (LastBlock == Int32.MaxValue) { // last block hasn't yet been detected
                    if (buffer == bufferBegin + BlockSize) {
                        var decoderImageAfterOverlapWithNextBlock =
                            !DecoderIsSerializable ? null : cloner.CaptureImage(Decoder);
                        int nBytesInOverlapWithNextBlock = (int)(ByteIndex - byteIndexAtNextBlockBegin);
                        Blocks.Add(new BlockInfo(byteIndexAtNextBlockBegin, byteBufferIndexAtNextBlockBegin,
                                                 nBytesInOverlapWithNextBlock, *(buffer - 1),
                                                 overhangCharsAtNextBlockBegin, decoderImageAtNextBlockBegin,
                                                 overhangCharsAfterOverlapWithNextBlock, decoderImageAfterOverlapWithNextBlock));
                    } else { // we reached the end of the stream
                        LastBlock = block;
                        IndexOfLastCharPlus1 = CharStream.IndexOfFirstChar + charIndex + (buffer - bufferBegin);
                    }
                } else if (IndexOfLastCharPlus1 != CharStream.IndexOfFirstChar + charIndex + (buffer - bufferBegin)) {
                    throw new IOException("CharStream: stream integrity error");
                }
            } else {
                BlockInfo nbi = Blocks[block + 1];
                if (buffer != bufferBegin + blockSizeMinusOverlap
                    || byteIndexAtNextBlockBegin != nbi.ByteIndex
                    || byteBufferIndexAtNextBlockBegin != nbi.ByteBufferIndex
                    || overhangCharsAtNextBlockBegin != nbi.OverhangCharsAtBlockBegin)
                    throw new IOException("CharStream: stream integrity error");

                if (prevBlock != block + 1 || (block == 0 && !DecoderIsSerializable)) { // jumping back to block 0 is supported even if the decoder is not serializable
                    nCharsToRead = BlockOverlap;
                    if (overhangCharsAtNextBlockBegin != null) {
                        nCharsToRead -= overhangCharsAtNextBlockBegin.Length;
                        for (int i = 0; i < overhangCharsAtNextBlockBegin.Length; ++i)
                            *(buffer++) = overhangCharsAtNextBlockBegin[i];
                    }
                    string overhangCharsAfterOverlapWithNextBlock;
                    buffer = ReadCharsFromStream(buffer, nCharsToRead, out overhangCharsAfterOverlapWithNextBlock);
                    int nBytesInOverlapWithNextBlock = (int)(ByteIndex - byteIndexAtNextBlockBegin);
                    if (buffer != bufferBegin + BlockSize
                        || nBytesInOverlapWithNextBlock != nbi.NumberOfBytesInOverlap
                        || *(buffer - 1) != nbi.LastCharInOverlap
                        || overhangCharsAfterOverlapWithNextBlock != nbi.OverhangCharsAfterOverlap)
                        throw new IOException("CharStream: stream integrity error");
                } else {
                    Debug.Assert(bufferBegin[BlockSize - 1] == nbi.LastCharInOverlap);
                    buffer += BlockOverlap; // we already copied the chars at the beginning of this function
                    int off = nbi.NumberOfBytesInOverlap - (ByteBufferCount - ByteBufferIndex);
                    if (off > 0) {
                        // we wouldn't have gotten here if the Stream didn't support seeking
                        Stream.Seek(off, SeekOrigin.Current);
                        StreamPosition += off;
                        ClearAndRefillByteBuffer(off%ByteBuffer.Length);
                    } else {
                        ByteBufferIndex += nbi.NumberOfBytesInOverlap;
                    }
                    Decoder = (Decoder)nbi.DecoderImageAfterOverlap.CreateClone();
                }
            }

            CharStream.Block = block;
            //CharStream.CharIndex = charIndex;
            CharStream.IndexOfFirstCharInBlock = CharStream.IndexOfFirstChar + charIndex;
            CharStream.BufferEnd = buffer;
            if (begin != buffer) {
                CharStream.Ptr = begin;
                CharStream.PtrEnd = buffer;
                CharStream.PtrBegin = CharStream.BufferBegin;
                return begin;
            } else {
                CharStream.Ptr = null;
                CharStream.PtrEnd = null;
                CharStream.PtrBegin = null;
                return null;
            }
        }
    } // class MultiBlockData


    /// <summary>Returns an iterator pointing to the given index in the stream,
    /// or to the end of the stream if the indexed position lies beyond the last char in the stream.</summary>
    /// <exception cref="ArgumentOutOfRangeException">The index is negative or less than the BeginIndex.</exception>
    /// <exception cref="NotSupportedException">Accessing the char with the given index requires seeking in the underlying byte stream, but the byte stream does not support seeking or the Encoding's Decoder is not serializable.</exception>
    /// <exception cref="IOException">An I/O error occured.</exception>
    /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
    /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
    /// <exception cref="OutOfMemoryException">Can not allocate enough memory for the internal data structure.</exception>
    /// <exception cref="ObjectDisposedException">Method is called after the stream was disposed.</exception>
    public void Seek(long index) {
        ++StateTag;
        // The following comparison is safe in case of an overflow since
        // 0 <= IndexOfFirstCharInBlock < 2^60 + 2^31 * 2^31 and BufferEnd - BufferBegin < 2^31,
        // where 2^31 is an upper bound for both the number of blocks and the number of chars in a block.
        long off = unchecked(index - IndexOfFirstCharInBlock);
        if (0 <= off && off < Buffer.PositiveDistance(BufferBegin, BufferEnd)) {
            Ptr = BufferBegin + (uint)off;
            PtrBegin = BufferBegin;
            PtrEnd   = BufferEnd;
            return;
        }
        if (index < IndexOfFirstChar) {
            --StateTag;
            throw (new ArgumentOutOfRangeException("index", "The index is negative or less than the IndexOfFirstChar."));
        }
        if (BlockData == null || index >= BlockData.IndexOfLastCharPlus1) {
            Ptr = null;
            PtrBegin = null;
            PtrEnd = null;
            return;
        }
        // we never get here for streams with only one block
        index -= IndexOfFirstChar;
        int blockSizeMinusOverlap = BlockData.BlockSize - BlockData.BlockOverlap;
        long idx_;
        long block_ = Math.DivRem(index, blockSizeMinusOverlap, out idx_);
        int block = block_ > Int32.MaxValue ? Int32.MaxValue : (int)block_;
        int idx = (int)idx_;
        Seek(block, idx);
    }

    private void Seek(int block, int indexInBlock) {
        Debug.Assert(block >= 0 && indexInBlock >= 0 && BlockData != null);
        if (block > Block) {
            if (indexInBlock < BlockData.BlockOverlap) {
                --block;
                indexInBlock += BlockData.BlockSize - BlockData.BlockOverlap;
            }
        } else if (block < Block) {
            int blockSizeMinusOverlap = BlockData.BlockSize - BlockData.BlockOverlap;
            if (indexInBlock >= blockSizeMinusOverlap) {
                ++block;
                indexInBlock -= blockSizeMinusOverlap;
            }
        }
        if (block == Block) {
            Debug.Assert(indexInBlock < Buffer.PositiveDistance(BufferBegin, BufferEnd));
            PtrBegin = BufferBegin;
            PtrEnd = BufferEnd;
        } else {
            int last = BlockData.Blocks.Count - 1;
            if (block >= last) {
                BlockData.ReadBlock(last);
                while (Block < block && Block != BlockData.LastBlock)
                    BlockData.ReadBlock(Block + 1);
                if (block != Block || indexInBlock >= Buffer.PositiveDistance(PtrBegin, PtrEnd)) {
                    Ptr = null;
                    PtrBegin = null;
                    PtrEnd = null;
                    return;
                }
            } else {
                BlockData.ReadBlock(block);
                Debug.Assert(indexInBlock < Buffer.PositiveDistance(PtrBegin, PtrEnd));
            }
        }
        Ptr = BufferBegin + indexInBlock;
    }

    internal void Seek(char* ptr, int block) {
        if (ptr != null) {
            if (block != Block) {
                Debug.Assert(BlockData != null && ptr >= BufferBegin && ptr < BufferBegin + BlockData.BlockSize);
                int indexInBlock = (int)Buffer.PositiveDistance(BufferBegin, ptr);
                Seek(block, indexInBlock);
            } else {
                Debug.Assert(ptr >= BufferBegin && ptr < BufferEnd);
                Ptr = ptr;
                PtrBegin = BufferBegin;
                PtrEnd = BufferEnd;
            }
        } else {
            Ptr = null;
            PtrBegin = null;
            PtrEnd = null;
        }
    }

    private void SeekToFirstCharAfterLastCharOfCurrentBlock() {
        if (Ptr != null) {
            if (BlockData != null && Block != BlockData.LastBlock) BlockData.ReadBlock(Block + 1);
            else {
                Ptr = null;
                PtrBegin = null;
                PtrEnd = null;
            }
        }
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public CharStreamIndexToken IndexToken { get {
        return new CharStreamIndexToken(
               #if DEBUG
                   this,
               #endif
                   Ptr,
                   Block
               );
    } }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void ThrowInvalidIndexToken() {
        throw new ArgumentException("The CharStreamIndexToken is invalid.");
    }

    public void Seek(CharStreamIndexToken indexToken) {
        int block = indexToken.Block;
        if (block < 0) ThrowInvalidIndexToken(); // tests for zero-initialized IndexTokens
    #if DEBUG
        Debug.Assert(this == indexToken.CharStream);
    #endif
        if (Ptr != null && indexToken.Ptr != null && block == Block) {
            Ptr = indexToken.Ptr;
            Debug.Assert(Ptr >= BufferBegin && Ptr < BufferEnd);
        } else {
            Seek(indexToken.Ptr, block);
        }
        ++StateTag;
    }

    // Below we split many methods into a default method containing the code
    // for the most frequently used branch and a "...Continue" method containing
    // the code for the remaining branches. This allows the JIT to produce
    // faster code for the main branch and in a few cases even to inline it.

    public string ReadFrom(CharStreamIndexToken indexOfFirstChar) {
        int block = indexOfFirstChar.Block;
        if (block < 0) ThrowInvalidIndexToken(); // tests for zero-initialized IndexTokens
    #if DEBUG
        Debug.Assert(this == indexOfFirstChar.CharStream);
    #endif
        return ReadFrom(indexOfFirstChar.Ptr, block);
    }

    internal string ReadFrom(char* ptr, int block) {
        if (ptr != null && ptr < Ptr && block == Block) {
            Debug.Assert(BufferBegin <= ptr && Ptr < BufferEnd);
            return new string(ptr, 0, (int)Buffer.PositiveDistance(ptr, Ptr));
        }
        return ReadFromContinue(ptr, block);
    }
    private string ReadFromContinue(char* ptr, int block) {
        ulong index1 = (ulong)GetIndex(ptr, block);
        ulong index2 = (ulong)Index;

        if (index1 < index2) {
            ulong length_ = index2 - index1;
            // The maximum theoretical string size is Int32.MaxValue,
            // though on .NET it is actually less than 2^30, since the maximum
            // object size is limited to Int32.MaxValue, even on 64-bit systems.
            if (length_ > Int32.MaxValue) {
                // OutOfMemoryException is the exception the .NET string constructor throws
                // if the the string length is larger than the maximum string length,
                // even if enough memory would be available.
                throw new OutOfMemoryException();
            }
            int length = (int)length_;
            var stateTag = StateTag;
            Seek(ptr, block);
            var str = Read((int)length);
            StateTag = stateTag;
            return str;
        } else if (index1 > index2) throw new ArgumentException("The current position of the stream must not lie before the position corresponding to the given CharStreamIndexToken/CharStreamState.");
        return "";
    }

    public void RegisterNewline() {
        var index = Index;
        Debug.Assert(index != _LineBegin);
        _LineBegin = index;
        ++_Line;
        ++StateTag;
    }

    private void RegisterNewlines(char* lineBegin, uint lineOffset) {
        Debug.Assert(BufferBegin <= lineBegin && lineBegin <= BufferEnd && lineOffset > 0);
        _Line += lineOffset;
        long newLineBegin = Buffer.PositiveDistance(BufferBegin, lineBegin) + IndexOfFirstCharInBlock;
        Debug.Assert(newLineBegin != _LineBegin);
        _LineBegin = newLineBegin;
        ++StateTag;
    }

    public void RegisterNewlines(int lineOffset, int newColumnMinus1) {
        Debug.Assert(lineOffset != 0 && newColumnMinus1 >= 0);
        _Line += lineOffset;
        Debug.Assert(_Line > 0);
        var newLineBegin = Index - newColumnMinus1;
        Debug.Assert(newLineBegin != _LineBegin);
        _LineBegin = Index - newColumnMinus1;
        ++StateTag;
    }

    public void RegisterNewlines(long lineOffset, long newColumnMinus1) {
        Debug.Assert(lineOffset != 0 && newColumnMinus1 >= 0);
        _Line += lineOffset;
        Debug.Assert(_Line > 0);
        var newLineBegin = Index - newColumnMinus1;
        Debug.Assert(newLineBegin != _LineBegin);
        _LineBegin = Index - newColumnMinus1;
        ++StateTag;
    }

    public char Peek() {
        char* ptr = Ptr;
        if (ptr != null) return *ptr;
        return EOS;
    }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public void Skip() {
        char* ptr1 = Ptr + 1;
        if (ptr1 < PtrEnd) {
            Ptr = ptr1;
            ++StateTag;
            return;
        }
        SkipContinue();
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void SkipContinue() { SkipContinue(1u); }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public char Read() {
        char* ptr = Ptr;
        char* ptr1 = ptr + 1;
        if (ptr1 < PtrEnd) {
            char c = *ptr;
            Ptr = ptr1;
            ++StateTag;
            return c;
        }
        return ReadContinue();
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private char ReadContinue() {
        var c = Peek();
        Skip();
        return c;
    }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public char SkipAndPeek() {
        char* ptr = Ptr + 1;
        if (ptr < PtrEnd) {
            Ptr = ptr;
            ++StateTag;
            return *ptr;
        }
        return SkipAndPeekContinue();
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private char SkipAndPeekContinue() { return SkipAndPeekContinue(1u); }

    private static readonly bool IsLittleEndian = BitConverter.IsLittleEndian; // improves inlining and dead code elimination, at least with the .NET JIT

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public TwoChars Peek2() {
        char* ptr = Ptr;
        if (ptr + 1 < PtrEnd) {
            #if UNALIGNED_READS
                if (IsLittleEndian) {
                    return new TwoChars(*((uint*)ptr));
                } else {
                    return new TwoChars(ptr[0], ptr[1]);
                }
            #else
                return new TwoChars(ptr[0], ptr[1]);
            #endif
        }
        return Peek2Continue();
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private TwoChars Peek2Continue() {
        return new TwoChars(Peek(), Peek(1u));
    }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public char Peek(uint utf16Offset) {
        if (utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd))
            return Ptr[utf16Offset];
        return PeekContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private char PeekContinue(uint utf16Offset) {
        if (Ptr == null || BlockData == null || Block == BlockData.LastBlock) return EOS;
        char* ptr = Ptr;
        int block = Block;
        var stateTag = StateTag;
        Seek(Index + utf16Offset);
        char c = Peek();
        Seek(ptr, block); // backtrack
        StateTag = stateTag;
        return c;
    }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public void Skip(uint utf16Offset) {
        if (utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd)) {
            Ptr += utf16Offset;
            ++StateTag;
            return;
        }
        SkipContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void SkipContinue(uint utf16Offset) {
        if (Ptr == null || utf16Offset == 0) return;
        if (BlockData == null || Block == BlockData.LastBlock) {
            Ptr = null;
            PtrBegin = null;
            PtrEnd = null;
            ++StateTag;
            return;
        }
        Seek(Index + utf16Offset);
    }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public char SkipAndPeek(uint utf16Offset) {
        if (utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd)) {
            char* ptr = Ptr + utf16Offset;
            Ptr = ptr;
            ++StateTag;
            return *ptr;
        }
        return SkipAndPeekContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private char SkipAndPeekContinue(uint utf16Offset) {
        SkipContinue(utf16Offset);
        return Peek();
    }

    public char Peek(int utf16Offset) { // don't force inlining, because the .NET JIT doesn't optimize after inlining
        if (utf16Offset >= 0
            ? utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd)
            : unchecked((uint)-utf16Offset) <= Buffer.PositiveDistance(PtrBegin, Ptr))
        {
            return Ptr[utf16Offset];
        }
        return PeekContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private char PeekContinue(int utf16Offset) {
        if (utf16Offset >= 0) return PeekContinue((uint)utf16Offset);
        var newIndex = Index + utf16Offset;
        if (newIndex >= _IndexOfFirstChar) {
            char* ptr = Ptr;
            int block = Block;
            var stateTag = StateTag;
            Seek(Index + utf16Offset);
            char c = Peek();
            Seek(ptr, block);
            StateTag = stateTag;
            return c;
        }
        return EOS;
    }

    public void Skip(int utf16Offset) {
        if (utf16Offset >= 0
            ? utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd)
            : unchecked((uint)-utf16Offset) <= Buffer.PositiveDistance(PtrBegin, Ptr))
        {
            Ptr = unchecked(Ptr + utf16Offset); // see https://connect.microsoft.com/VisualStudio/feedback/details/522944
            ++StateTag;
            return;
        }
        SkipContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void SkipContinue(int utf16Offset) {
        if (utf16Offset >= 0) {
            SkipContinue((uint)utf16Offset);
            return;
        }
        Seek(Index + utf16Offset);
    }

    public void Skip(long utf16Offset) {
        if (utf16Offset >= 0
            ? utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd)
            : unchecked((ulong)-utf16Offset) <= Buffer.PositiveDistance(PtrBegin, Ptr))
        {
            Ptr = unchecked(Ptr + utf16Offset); // see https://connect.microsoft.com/VisualStudio/feedback/details/522944
            ++StateTag;
            return;
        }
        SkipContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void SkipContinue(long utf16Offset) {
        long index = Index;
        Seek(utf16Offset > Int64.MaxValue - index ? Int64.MaxValue : index + utf16Offset);
    }

    public char SkipAndPeek(int utf16Offset) {
        if (utf16Offset >= 0
            ? utf16Offset < Buffer.PositiveDistance(Ptr, PtrEnd)
            : unchecked((uint)-utf16Offset) <= Buffer.PositiveDistance(PtrBegin, Ptr))
        {
            char* ptr = unchecked(Ptr + utf16Offset); // see https://connect.microsoft.com/VisualStudio/feedback/details/522944
            Ptr = ptr;
            ++StateTag;
            return *ptr;
        }
        return SkipAndPeekContinue(utf16Offset);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private char SkipAndPeekContinue(int utf16Offset) {
        if (utf16Offset >= 0) {
            SkipContinue((uint)utf16Offset);
            return Peek();
        }
        var newIndex = Index + utf16Offset;
        if (newIndex >= IndexOfFirstChar) {
            Seek(Index + utf16Offset);
            return Peek();
        } else {
            Seek(_IndexOfFirstChar);
            return EOS;
        }
    }

    public string PeekString(int length) {
        if (unchecked((uint)length) <= Buffer.PositiveDistance(Ptr, PtrEnd))
            return new String(Ptr, 0, length);
        return PeekStringContinue(length);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private string PeekStringContinue(int length) {
        return ReadContinue(length, true);
    }

    public string Read(int length) {
        char* ptr = Ptr;
        if (unchecked((uint)length) < Buffer.PositiveDistance(ptr, PtrEnd)) {
            Ptr += length;
            ++StateTag;
            return new String(ptr, 0, length);
        }
        return ReadContinue(length);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private string ReadContinue(int length) {
        return ReadContinue(length, false);
    }

    private string ReadContinue(int length, bool backtrack) {
        if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
        if (length == 0 || Ptr == null) return "";
        if (BlockData == null) {
            int maxLength = (int)Buffer.PositiveDistance(Ptr, PtrEnd);
            if (length > maxLength)
                length = maxLength;
        } else {
            long maxLength = BlockData.IndexOfLastCharPlus1 - Index;
            if (length > maxLength)
                length = (int)maxLength;
        }
        string str = new String('\u0000', length);
        fixed (char* pStr = str) {
            int cc = ReadContinue(pStr, length, backtrack);
            if (cc == length) return str;
            return new String(pStr, 0, cc);
        }
    }

    public int PeekString(char[] buffer, int bufferIndex, int length) {
        return Read(buffer, bufferIndex, length, true);
    }
    public int Read(char[] buffer, int bufferIndex, int length) {
        return Read(buffer, bufferIndex, length, false);
    }
    private int Read(char[] buffer, int bufferIndex, int length, bool backtrack) {
        if (bufferIndex < 0)
            throw new ArgumentOutOfRangeException("bufferIndex", "bufferIndex is negative.");
        if (length > buffer.Length - bufferIndex) // throws if buffer is null
            throw new ArgumentOutOfRangeException("length", "bufferIndex or length is out of range.");
        // We must exit early for length == 0, because pining an empty array
        // would invoke implementation-defined behaviour.
        if (length <= 0) {
            if (length == 0) return 0;
            throw new ArgumentOutOfRangeException("length", "length is negative.");
        }
        fixed (char* pBuffer = buffer)
        return Read(pBuffer + bufferIndex, length, backtrack);
    }

    public int PeekString(char* buffer, int length) {
        return Read(buffer, length, true);
    }
    public int Read(char* buffer, int length) {
        return Read(buffer, length, false);
    }
    private int Read(char* buffer, int length, bool backtrack) {
        if (unchecked((uint)length) < Buffer.PositiveDistance(Ptr, PtrEnd)) {
            char* ptr = Ptr;
            int len = length;
        #if UNALIGNED_READS
            if ((unchecked((int)buffer) & 2) != 0 && len != 0) { // align buffer pointer
                *buffer = *ptr;
                ++buffer; ++ptr; --len;
            }
            len -= 8;
            while (len >= 0) {
                ((int*)buffer)[0] = ((int*)ptr)[0];
                ((int*)buffer)[1] = ((int*)ptr)[1];
                ((int*)buffer)[2] = ((int*)ptr)[2];
                ((int*)buffer)[3] = ((int*)ptr)[3];
                buffer += 8; ptr += 8; len -= 8;
            }
            if ((len & 4) != 0) {
                ((int*)buffer)[0] = ((int*)ptr)[0];
                ((int*)buffer)[1] = ((int*)ptr)[1];
                buffer += 4; ptr += 4;
            }
            if ((len & 2) != 0) {
                ((int*)buffer)[0] = ((int*)ptr)[0];
                buffer += 2; ptr += 2;
            }
        #else
            len -= 2;
            while (len >= 0) {
                buffer[0] = ptr[0];
                buffer[1] = ptr[1];
                buffer += 2; ptr += 2; len -= 2;
            }
        #endif
            if ((len & 1) != 0) {
                *buffer = *ptr;
                ++ptr;
            }
            if (!backtrack) {
                Ptr = ptr;
                ++StateTag;
            }
            return length;
        }
        return ReadContinue(buffer, length, backtrack);
    }
    private int ReadContinue(char* buffer, int length, bool backtrack) {
        if (length < 0)
            throw new ArgumentOutOfRangeException("length", "length is negative.");

        if (length == 0 || Ptr == null) return 0;

        int oldLength = length;
        int oldBlock = Block;
        char* oldPtr = Ptr;
        char* ptr = Ptr;

        do {
            int len = Math.Min((int)Buffer.PositiveDistance(Ptr, PtrEnd), length);
            Debug.Assert(length > 0 && len > 0);
            length -= len;
        #if UNALIGNED_READS
            if ((unchecked((int)buffer) & 2) != 0) { // align buffer pointer
                *buffer = *ptr;
                ++buffer; ++ptr; --len;
            }
            len -= 8;
            while (len >= 0) {
                ((int*)buffer)[0] = ((int*)ptr)[0];
                ((int*)buffer)[1] = ((int*)ptr)[1];
                ((int*)buffer)[2] = ((int*)ptr)[2];
                ((int*)buffer)[3] = ((int*)ptr)[3];
                buffer += 8; ptr += 8; len -= 8;
            }
            if ((len & 4) != 0) {
                ((int*)buffer)[0] = ((int*)ptr)[0];
                ((int*)buffer)[1] = ((int*)ptr)[1];
                buffer += 4; ptr += 4;
            }
            if ((len & 2) != 0) {
                ((int*)buffer)[0] = ((int*)ptr)[0];
                buffer += 2; ptr += 2;
            }
        #else
            len -= 2;
            while (len >= 0) {
                buffer[0] = ptr[0];
                buffer[1] = ptr[1];
                buffer += 2; ptr += 2; len -= 2;
            }
        #endif
            if ((len & 1) != 0) {
                *buffer = *ptr;
                ++buffer; ++ptr;
            }
        } while (length != 0
                 && BlockData != null
                 && Block != BlockData.LastBlock
                 && (ptr = BlockData.ReadBlock(Block + 1)) != null);
        if (!backtrack) {
            ++StateTag;
            if (ptr != PtrEnd) Ptr = ptr;
            else SeekToFirstCharAfterLastCharOfCurrentBlock();
        } else {
            if (Block != oldBlock) Seek(oldPtr, oldBlock);
        }
        return oldLength - length;
    }

    public bool Match(char ch) {
        char* ptr = Ptr;
        return ptr != null && ch == *ptr;
    }

    public bool MatchCaseFolded(char caseFoldedChar) {
        char* ptr = Ptr;
        return ptr != null && caseFoldedChar == CaseFoldTable.FoldedChars[*ptr];
    }

    public bool Skip(char ch) {
        char* ptr1 = Ptr + 1;
        if (ptr1 < PtrEnd && ch == *Ptr) {
            Ptr = ptr1;
            ++StateTag;
            return true;
        }
        return SkipContinue(ch);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private bool SkipContinue(char ch) {
        if (Match(ch)) {
            Skip();
            return true;
        }
        return false;
    }

    public bool SkipCaseFolded(char caseFoldedChar) {
        char* ptr1 = Ptr + 1;
        if (ptr1 < PtrEnd && caseFoldedChar == CaseFoldTable.FoldedChars[*Ptr]) {
            Ptr = ptr1;
            ++StateTag;
            return true;
        }
        return SkipCaseFoldedContinue(caseFoldedChar);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private bool SkipCaseFoldedContinue(char caseFoldedChar) {
        if (MatchCaseFolded(caseFoldedChar)) {
            Skip();
            return true;
        }
        return false;
    }

#if CLR45
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    public bool Skip(TwoChars twoChars) {
        char* ptr2 = Ptr + 2;
        if (ptr2 < PtrEnd) {
        #if UNALIGNED_READS
            if (IsLittleEndian) {
                if (new TwoChars(*((uint*)Ptr)) == twoChars) {
                    Ptr = ptr2;
                    ++StateTag;
                    return true;
                }
            } else {
                if (twoChars.Char0 == Ptr[0] && twoChars.Char1 == Ptr[1]) {
                    Ptr = ptr2;
                    ++StateTag;
                    return true;
                }
            }
        #else
            if (twoChars.Char0 == Ptr[0] && twoChars.Char1 == Ptr[1]) {
                Ptr = ptr2;
                ++StateTag;
                return true;
            }
        #endif
            return false;
        }
        return SkipContinue(twoChars);
    }
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private bool SkipContinue(TwoChars twoChars) {
        char* cs = stackalloc char[2];
        cs[0] = twoChars.Char0;
        cs[1] = twoChars.Char1;
        return SkipContinue(cs, 2, false);
    }

    public bool Match(string chars) {
        if (chars.Length <= Buffer.PositiveDistance(Ptr, PtrEnd)) {
            for (int i = 0; i < chars.Length; ++i) {
                if (Ptr[i] != chars[i]) goto ReturnFalse;
            }
            return true;
        ReturnFalse:
            return false;
        }
        return SkipContinue(chars, true);
    }

    public bool Skip(string chars) {
        if (chars.Length < Buffer.PositiveDistance(Ptr, PtrEnd)) {
            for (int i = 0; i < chars.Length; ++i) {
                if (Ptr[i] != chars[i]) goto ReturnFalse;
            }
            Ptr += chars.Length;
            ++StateTag;
            return true;
        ReturnFalse:
            return false;
        }
        return SkipContinue(chars, false);
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private bool SkipContinue(string chars, bool backtrackEvenIfCharsMatch) {
        fixed (char* pChars = chars)
        return SkipContinue(pChars, chars.Length, backtrackEvenIfCharsMatch);
    }

    public bool MatchCaseFolded(string caseFoldedChars) {
        if (caseFoldedChars.Length <= Buffer.PositiveDistance(Ptr, PtrEnd)) {
            for (int i = 0; i < caseFoldedChars.Length; ++i) {
                if (CaseFoldTable.FoldedChars[Ptr[i]] != caseFoldedChars[i]) goto ReturnFalse;
            }
            return true;
        ReturnFalse:
            return false;
        }
        return SkipCaseFoldedContinue(caseFoldedChars, true);
    }

    public bool SkipCaseFolded(string caseFoldedChars) {
        if (caseFoldedChars.Length < Buffer.PositiveDistance(Ptr, PtrEnd)) {
            for (int i = 0; i < caseFoldedChars.Length; ++i) {
                if (CaseFoldTable.FoldedChars[Ptr[i]] != caseFoldedChars[i]) goto ReturnFalse;
            }
            Ptr += caseFoldedChars.Length;
            ++StateTag;
            return true;
        ReturnFalse:
            return false;
        }
        return SkipCaseFoldedContinue(caseFoldedChars, false);
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private bool SkipCaseFoldedContinue(string caseFoldedChars, bool backtrackEvenIfCharsMatch) {
        fixed (char* pCaseFoldedChars = caseFoldedChars)
        return SkipCaseFoldedContinue(pCaseFoldedChars, caseFoldedChars.Length, backtrackEvenIfCharsMatch);
    }

    public bool Match(char[] chars, int charsIndex, int length) {
        return Skip(chars, charsIndex, length, true);
    }
    public bool Skip(char[] chars, int charsIndex, int length) {
        return Skip(chars, charsIndex, length, false);
    }
    private bool Skip(char[] chars, int charsIndex, int length, bool backtrack) {
        if (charsIndex < 0)
            throw new ArgumentOutOfRangeException("charsIndex", "charsIndex is negative.");
        if (length > chars.Length - charsIndex) // throws if chars is null
            throw new ArgumentOutOfRangeException("length", "length is out of range.");
        // We must exit early for length == 0, because pining an empty array
        // would invoke implementation-defined behaviour.
        if (length <= 0) {
            if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
            if (!backtrack) ++StateTag;
            return true;
        }
        fixed (char* pChars = chars) return Skip(pChars + charsIndex, length, backtrack);
    }

    public bool Match(char* chars, int length) {
        return Skip(chars, length, true);
    }
    public bool Skip(char* chars, int length) {
        return Skip(chars, length, false);
    }
    private bool Skip(char* chars, int length, bool backtrackEvenIfCharsMatch) {
        if (unchecked((uint)length < Buffer.PositiveDistance(Ptr, PtrEnd))) {
            #if UNALIGNED_READS
                char* ptr = Ptr;
                int len = length - 2;
                while (len >= 0) {
                    if (*((int*)ptr) != *((int*)chars)) goto ReturnFalse;
                    ptr += 2; chars += 2; len -= 2;
                }
                if ((len & 1) != 0) {
                    if (*ptr != *chars) goto ReturnFalse;
                    ++ptr;
                }
            #else
                char* ptr = Ptr;
                int len = length;
                while (len != 0) {
                    if (*ptr != *chars) goto ReturnFalse;
                    ++ptr; ++chars; --len;
                }
            #endif
                if (!backtrackEvenIfCharsMatch) {
                    Ptr = ptr;
                    ++StateTag;
                }
                return true;
        ReturnFalse:
                return false;
        }
        return SkipContinue(chars, length, backtrackEvenIfCharsMatch);
    }

    public bool MatchCaseFolded(char* caseFoldedChars, int length) {
        return SkipCaseFolded(caseFoldedChars, length, true);
    }
    public bool SkipCaseFolded(char* caseFoldedChars, int length) {
        return SkipCaseFolded(caseFoldedChars, length, false);
    }
    private bool SkipCaseFolded(char* caseFoldedChars, int length, bool backtrackEvenIfCharsMatch) {
        if (unchecked((uint)length < Buffer.PositiveDistance(Ptr, PtrEnd))) {
            char* ptr = Ptr;
            int len = length;
            while (len != 0) {
                if (CaseFoldTable.FoldedChars[*ptr] != *caseFoldedChars) goto ReturnFalse;
                ++ptr; ++caseFoldedChars; --len;
            }
            if (!backtrackEvenIfCharsMatch) {
                Ptr = ptr;
                ++StateTag;
            }
            return true;
        ReturnFalse:
            return false;
        }
        return SkipCaseFoldedContinue(caseFoldedChars, length, backtrackEvenIfCharsMatch);
    }

    private bool SkipContinue(char* chars, int length, bool backtrackEvenIfCharsMatch) {
        if (length <= 0) {
            if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
            return true;
        }

        if (Ptr == null) return false;

        int oldBlock = Block;
        char* oldPtr = Ptr;
        char* ptr = Ptr;

        for (;;) {
            Debug.Assert(length > 0);
            int len = (int)Buffer.PositiveDistance(ptr, PtrEnd);
            if (len < length) {
                if (BlockData == null || Block == BlockData.LastBlock) goto ReturnFalse;
                length -= len;
            } else {
                len = length;
                length = 0;
            }
            Debug.Assert(len > 0);
        #if UNALIGNED_READS
            len -= 2;
            while (len >= 0) {
                if (*((int*)ptr) != *((int*)chars)) goto ReturnFalse;
                ptr += 2; chars += 2; len -= 2;
            }
            if ((len & 1) != 0) {
                if (*ptr != *chars) goto ReturnFalse;
                ++ptr; ++chars;
            }
        #else
            do {
                if (*ptr != *chars) goto ReturnFalse;
                ++ptr; ++chars; --len;
            } while (len != 0);
        #endif
            if (length != 0) {
                Debug.Assert(BlockData != null && Block != BlockData.LastBlock);
                ptr = BlockData.ReadBlock(Block + 1);
            } else {
                if (backtrackEvenIfCharsMatch) {
                    if (Block != oldBlock) Seek(oldPtr, oldBlock);
                } else {
                    if (ptr != PtrEnd) Ptr = ptr;
                    else SeekToFirstCharAfterLastCharOfCurrentBlock();
                    ++StateTag;
                }
                return true;
            }
        }
    ReturnFalse:
        if (Block != oldBlock) Seek(oldPtr, oldBlock);
        return false;
    }

    private bool SkipCaseFoldedContinue(char* caseFoldedChars, int length, bool backtrackEvenIfCharsMatch) {
        if (length <= 0) {
            if (length == 0) return true;
            throw new ArgumentOutOfRangeException("length", "length is negative.");
        }

        if (Ptr == null) return false;

        int oldBlock = Block;
        char* oldPtr = Ptr;
        char* ptr = Ptr;

        for (;;) {
            Debug.Assert(length > 0);
            int len = (int)Buffer.PositiveDistance(ptr, PtrEnd);
            if (len < length) {
                if (BlockData == null || Block == BlockData.LastBlock) goto ReturnFalse;
                length -= len;
            } else {
                len = length;
                length = 0;
            }
            Debug.Assert(len > 0);
            do {
                if (CaseFoldTable.FoldedChars[*ptr] != *caseFoldedChars) goto ReturnFalse;
                ++ptr; ++caseFoldedChars; --len;
            } while (len != 0);
            if (length != 0) {
                Debug.Assert(BlockData != null && Block != BlockData.LastBlock);
                ptr = BlockData.ReadBlock(Block + 1);
            } else {
                if (backtrackEvenIfCharsMatch) {
                    if (Block != oldBlock) Seek(oldPtr, oldBlock);
                } else {
                    if (ptr != PtrEnd) Ptr = ptr;
                    else SeekToFirstCharAfterLastCharOfCurrentBlock();
                    ++StateTag;
                }
                return true;
            }
        }
    ReturnFalse:
        if (Block != oldBlock) Seek(oldPtr, oldBlock);
        return false;
    }

    public Match Match(Regex regex) {
        if (BufferString == null) throw new NotSupportedException("CharStream instances constructed from char arrays or char pointers do not support regular expression matching.");
        if (Ptr != null) {
            if (BlockData != null && Ptr > BlockData.RegexSpaceThreshold && Block != BlockData.LastBlock) {
                // BlockOverlap > MinRegexSpace
                char c = *Ptr;
                char* ptr = Ptr;
                BlockData.ReadBlock(Block + 1);
                int blockSizeMinusOverlap = BlockData.BlockSize - BlockData.BlockOverlap;
                Ptr = ptr - blockSizeMinusOverlap;
                PtrBegin = BufferBegin; // might have been set to null by ReadBlock
                PtrEnd = BufferEnd;
                Debug.Assert(*Ptr == c && BufferBegin <= Ptr && Ptr < BufferEnd);
            }
            int index  = (int)Buffer.PositiveDistance(BufferStringPointer, Ptr);
            int length = (int)Buffer.PositiveDistance(Ptr, BufferEnd);
            return regex.Match(BufferString, index, length);
        }
        return regex.Match("");
    }

    public bool SkipWhitespace() {
        char* lineBegin = null;
        uint lineOffset = 0;
        char* ptr = Ptr;
        char* end = unchecked(PtrEnd - 1); // - 1 to guarantee the lookahead for '\r',
        if (ptr + 1 < PtrEnd) { // PtrEnd might be null
            char c = *ptr;
            ++ptr;
            if (c > ' ') goto ReturnFalse;
            if (c == ' ') {
                if (*ptr > ' ') {
                    Ptr = ptr;
                    ++StateTag;
                    return true;
                }
                goto Loop;
            } else {
                if (c == '\r') {
                    if (*ptr == '\n') {
                        ++ptr;
                        if (ptr > end) goto Newline;
                    }
                } else if (c != '\n') goto CheckTab;
                if (*ptr > ' ') {
                    Ptr = ptr;
                    RegisterNewline();
                    return true;
                }
                goto Newline;
            CheckTab:
                if (c != '\t') goto ReturnFalse;
                goto Loop;
            }
        Newline:
            lineBegin = ptr;
            ++lineOffset;
        Loop:
            for (;;) {
                if (ptr >= end) break;
                c = *ptr;
                ++ptr;
                if (c != ' ') {
                    if (c != '\t') {
                        if (c == '\r') {
                            if (*ptr == '\n') ++ptr;
                            goto Newline;
                        }
                        if (c == '\n') goto Newline;
                        --ptr;
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            ++StateTag;
                            return true;
                        } else {
                            RegisterNewlines(lineBegin, lineOffset);
                            return true;
                        }
                    }
                }
            }
        }
        return SkipWhitespaceContinue(ptr, lineBegin, lineOffset);
    ReturnFalse:
        return false;
    }
    private bool SkipWhitespaceContinue(char* ptr, char* lineBegin, uint lineOffset) {
        var stateTag = StateTag;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        if (index == 0) {
            c = Peek();
            if (c == ' ' || c == '\t') c = SkipAndPeek();
            else if (c != '\r' && c != '\n') return false;
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
        }
        for (;;) {
             if (c == ' ' || c == '\t') c = SkipAndPeek();
             else if (c != '\r' && c != '\n') {
                 StateTag = stateTag + 1;
                 return true;
             } else {
                 char c0 = c;
                 c = SkipAndPeek();
                 if (c0 == '\r' && c == '\n') c = SkipAndPeek();
                 RegisterNewline();
             }
        }
    }

    public bool SkipUnicodeWhitespace() {
        char* lineBegin = null;
        uint lineOffset = 0;
        char* end = unchecked(PtrEnd - 1); // - 1 to guarantee the lookahead for '\r'
        char* ptr = Ptr;
        if (ptr + 1 < PtrEnd) { // PtrEnd might be null
            char c = *ptr;
            ++ptr;
            if (c == ' ') goto Loop;
            if (!Text.IsWhitespace(c)) return false;
            if (c <= '\r') {
                if (c == '\r') {
                    if (*ptr == '\n') ++ptr;
                } else if (c != '\n') goto Loop;
            } else {
                if (c < '\u2028' ? c != '\u0085' : c > '\u2029') goto Loop;
            }
        Newline:
            lineBegin = ptr;
            ++lineOffset;
        Loop:
            for (;;) {
                if (ptr >= end) break;
                c = *ptr;
                ++ptr;
                if (c != ' ') {
                    if (Text.IsWhitespace(c)) {
                        if (c <= '\r') {
                            if (c == '\r') {
                                if (*ptr == '\n') ++ptr;
                                goto Newline;
                            }
                            if (c == '\n') goto Newline;
                        } else if (c < '\u2028' ? c == '\u0085' : c <= '\u2029') goto Newline;
                    } else {
                        --ptr;
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            ++StateTag;
                            return true;
                        } else {
                            RegisterNewlines(lineBegin, lineOffset);
                            return true;
                        }
                    }
                }
            }
        }
        return SkipUnicodeWhitespaceContinue(ptr, lineBegin, lineOffset);
    }
    private bool SkipUnicodeWhitespaceContinue(char* ptr, char* lineBegin, uint lineOffset) {
        var stateTag = StateTag;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        if (index == 0) {
            c = Peek();
            if (!Text.IsWhitespace(c)) return false;
            if (c == ' ' || c == '\t') c = SkipAndPeek();
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
        }
        for (;;) {
            if (c == ' ') c = SkipAndPeek();
            else {
                if (!Text.IsWhitespace(c)) break;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 <= '\r') {
                    if (c0 == '\r') {
                        if (c == '\n') c = SkipAndPeek();
                    } else if (c0 != '\n') continue;
                } else if (c0 < '\u2028' ? c0 != '\u0085' : c0 > '\u2029') continue;
                RegisterNewline();
            }
        }
        StateTag = stateTag + 1;
        return true;
    }

    public bool SkipNewline() {
        var ptr = Ptr;
        if (ptr + 2 < PtrEnd) {
            char c = *ptr;
            ++ptr;
            if (c == '\r') {
                if (*ptr == '\n') ++ptr;
            } else if (c != '\n') return false;
            Ptr = ptr;
            RegisterNewline();
            return true;
        } else {
            var stateTag = StateTag;
            char c = Peek();
            if (c == '\r') {
                c = SkipAndPeek();
                if (c == '\n') Skip();
            } else {
                if (c != '\n') return false;
                Skip();
            }
            RegisterNewline();
            StateTag = stateTag + 1;
            return true;
        }
    }

    public bool SkipUnicodeNewline() {
        var ptr = Ptr;
        if (ptr + 2 < PtrEnd) {
            char c = *ptr;
            ++ptr;
            if (c <= '\r') {
                if (c == '\r') {
                    if (*ptr == '\n') ++ptr;
                } else if (c != '\n') goto ReturnFalse;
            } else if (c >= '\u2028' ? c > '\u2029' : c != '\u0085') goto ReturnFalse;
            Ptr = ptr;
            RegisterNewline();
            return true;
        } else {
            char c = Peek();
            uint n = 1;
            if (c <= '\r') {
                if (c == '\r') {
                    if (Peek(1u) == '\n') n = 2;
                } else if (c != '\n') goto ReturnFalse;
            } else if (c >= '\u2028' ? c > '\u2029' : c != '\u0085') goto ReturnFalse;
            Skip(n);
            var stateTag = StateTag;
            RegisterNewline();
            StateTag = stateTag;
            return true;
        }
    ReturnFalse:
        return false;
    }

    public int SkipNewlineThenWhitespace(int powerOf2TabStopDistance, bool allowFormFeed) {
        int tabStopDistanceMinus1 = unchecked(powerOf2TabStopDistance - 1);
        if (powerOf2TabStopDistance <= 0 || (powerOf2TabStopDistance & tabStopDistanceMinus1) != 0)
            throw new ArgumentOutOfRangeException("powerOf2TabStopDistance", "powerOf2TabStopDistance must be a positive power of 2.");

        char* lineBegin = null;
        uint lineOffset = 0;
        int ind = -1;
        char* end = unchecked(PtrEnd - 1); // - 1 to guarantee the lookahead for '\r'
        char* ptr = Ptr;
        if (ptr + 1 < PtrEnd) { // PtrEnd might be null
            char c = *ptr;
            ++ptr;
            if (c == '\r') {
                if (*ptr == '\n') ++ptr;
            } else if (c != '\n') {
                return -1;
            }
        Newline:
            lineBegin = ptr;
            ++lineOffset;
            ind = 0;
            for (;;) {
                if (ptr >= end) break;
                c = *ptr;
                ++ptr;
                if (c == ' ') {
                    ind = unchecked(ind + 1);
                    if (ind >= 0) continue;
                    // indentation has overflown, so put back ' ' and return
                    ind = unchecked(ind - 1);
                } else if (c <= '\r') {
                    if (c == '\r') {
                        if (*ptr == '\n') ++ptr;
                        goto Newline;
                    }
                    if (c == '\n') goto Newline;
                    if (c == '\t') {
                        // ind = ind + tabStopDistance - ind%tabStopDistance
                        int d = tabStopDistanceMinus1 + 1 - (ind & tabStopDistanceMinus1);
                        ind = unchecked(ind + d);
                        if (ind >= 0) continue;
                        // indentation has overflown, so put back '\t' and return
                        ind = unchecked(ind - d);
                    } else if (c == '\f' && allowFormFeed) {
                        ind = 0;
                        continue;
                    }
                }
                --ptr;
                Ptr = ptr;
                RegisterNewlines(lineBegin, lineOffset);
                return ind;
            }
            // end of block
        }
        return SkipNewlineWhitespaceContinue(ptr, lineBegin, lineOffset, ind, tabStopDistanceMinus1, allowFormFeed);
    }
    private int SkipNewlineWhitespaceContinue(char* ptr, char* lineBegin, uint lineOffset, int ind_,
                                              int tabStopDistanceMinus1, bool allowFormFeed)
    {
        var stateTag = StateTag;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        if (index == 0) {
            c = Peek();
            if (!(c == '\r' || c == '\n')) return -1;
        } else {
            RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
        }
        int ind = ind_;
        for (;;) {
            if (c == ' ') {
                ind = unchecked(ind + 1);
                if (ind >= 0) c = SkipAndPeek();
                else {
                    // indentation has overflown, so put back ' ' and return
                    ind = unchecked(ind - 1);
                    break;
                }
            } else if (c == '\r' || c == '\n') {
                ind = 0;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 == '\r' && c == '\n') c = SkipAndPeek();
                RegisterNewline();
            } else if (c == '\t') {
                // ind = ind + tabStopDistance - ind%tabStopDistance
                int d = tabStopDistanceMinus1 + 1 - (ind & tabStopDistanceMinus1);
                ind = unchecked(ind + d);
                if (ind >= 0) c = SkipAndPeek();
                else {
                    // indentation has overflown, so put back '\t' and return
                    ind = unchecked(ind - d);
                    break;
                }
            } else if (c == '\f' && allowFormFeed) {
                ind = 0;
                c = SkipAndPeek();
            } else break;
        }
        StateTag = stateTag + 1;
        return ind;
    }

    public void SkipRestOfLine(bool skipNewline) {
        char* ptr = Ptr;
        char* end = unchecked(PtrEnd - 2); // - 2, so that we can do (*) without further checking
        if (ptr + 2 < PtrEnd) { // PtrEnd might be null
            for (;;) {
                char c = *ptr;
                if (c > '\r') {
                    if (++ptr == end) break;
                } else if (c != '\r' && c != '\n') {
                    if (++ptr == end) break;
                } else {
                    if (!skipNewline) {
                        if (ptr != Ptr) {
                            Ptr = ptr;
                            ++StateTag;
                        }
                        return;
                    } else {
                        ++ptr;
                        if (c == '\r' && *ptr == '\n') ++ptr;
                        Ptr = ptr; // (*)
                        RegisterNewline();
                        return;
                    }
                }
            }
        }
        SkipRestOfLineContinue(ptr, skipNewline);
    }
    private void SkipRestOfLineContinue(char* ptr, bool skipNewline) {
        var stateTag = StateTag;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        if (index == 0) {
            c = Peek();
            if (c == EOS || (!skipNewline && (c == '\r' || c == '\n'))) return;
        } else {
            c = SkipAndPeek(index);
        }
        while (c != EOS) {
            if (c == '\r' || c == '\n') {
                if (skipNewline) SkipNewline();
                break;
            }
            c = SkipAndPeek();
        }
        StateTag = stateTag + 1;
        return;
    }

    public string ReadRestOfLine(bool skipNewline) {
        char* ptr = Ptr;
        char* end = unchecked(PtrEnd - 2); // - 2, so that we can do (*) without further checking
        if (ptr + 2 < PtrEnd) { // PtrEnd might be null
            for (;;) {
                char c = *ptr;
                if (c > '\r') {
                    if (++ptr == end) break;
                } else if (c != '\r' && c != '\n') {
                    if (++ptr == end) break;
                } else {
                    char* ptr0 = Ptr;
                    if (!skipNewline) {
                        if (ptr != ptr0) {
                            Ptr = ptr;
                            ++StateTag;
                            return new string(ptr0, 0, (int)Buffer.PositiveDistance(ptr0, ptr));
                        } else {
                            return "";
                        }
                    } else {
                        var skippedString = ptr == ptr0 ? "" : new string(ptr0, 0, (int)Buffer.PositiveDistance(ptr0, ptr));
                        ++ptr;
                        if (c == '\r' && *ptr == '\n') ++ptr;
                        Ptr = ptr; // (*)
                        RegisterNewline();
                        return skippedString;
                    }
                }
            }
        }
        return ReadRestOfLineContinue(ptr, skipNewline);
    }
    private string ReadRestOfLineContinue(char* ptr, bool skipNewline) {
        var stateTag = StateTag;
        var indexToken = IndexToken;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        if (index == 0) {
            c = Peek();
            if (c == EOS || (!skipNewline && (c == '\r' || c == '\n'))) return "";
        } else {
            c = SkipAndPeek(index);
        }
        while (c != EOS) {
            if (c == '\r' || c == '\n') {
                var skippedString = ReadFrom(indexToken);
                if (skipNewline) SkipNewline();
                StateTag = stateTag + 1;
                return skippedString;
            }
            c = SkipAndPeek();
        }
        StateTag = stateTag + 1;
        return ReadFrom(indexToken);
    }

    public char ReadCharOrNewline() {
        var ptr = Ptr;
        if (ptr + 2 < PtrEnd) {
            char c = *ptr;
            ++ptr;
            if (c != '\r') {
                if (c != '\n') {
                    Ptr = ptr;
                    ++StateTag;
                    return c;
                }
            } else if (*ptr == '\n') ++ptr;
            Ptr = ptr;
            RegisterNewline();
            return '\n';
        } else {
            char c0 = Peek();
            if (c0 != EOS) {
                char c = SkipAndPeek();
                var stateTag = StateTag;
                if (c0 != '\r') {
                    if (c0 != '\n') return c0;
                } else if (c == '\n') Skip();
                RegisterNewline();
                StateTag = stateTag;
                return '\n';
            }
            return EOS;
        }
    }

    public int SkipCharsOrNewlines(int maxCount) {
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        char* lineBegin = null;
        uint lineOffset = 0;
        int nCRLF = 0;
        char* ptr = Ptr;
        if (ptr != null) {
            char* bufferEnd1 = PtrEnd - 1; // - 1 to guarantee the lookahead for '\r'
            char* end2 = unchecked(ptr + maxCount);
            char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
            if (ptr < end) {
                for (;;) {
                    char c = *ptr;
                    ++ptr;
                    if (c > '\r') {
                        if (ptr == end) break;
                    } else {
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                ++nCRLF;
                                if (end < bufferEnd1) ++end;
                            }
                        } else if (c != '\n') goto CheckBound;
                        lineBegin = ptr;
                        ++lineOffset;
                    CheckBound:
                        if (ptr >= end) break;
                    }
                }
                if (end < bufferEnd1) {
                    int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                    Ptr = ptr;
                    if (lineOffset == 0) {
                        ++StateTag;
                        return count;
                    } else {
                        RegisterNewlines(lineBegin, lineOffset);
                        return count;
                    }
                }
            }
        }
        return SkipCharsOrNewlinesContinue(ptr, lineBegin, lineOffset, nCRLF, maxCount);
    }
    private int SkipCharsOrNewlinesContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF,
                    int maxCount)
    {
        var stateTag = StateTag;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        int count;
        if (index == 0) {
            if (maxCount == 0 || (c = Peek()) == EOS) return 0;
            count = 0;
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
            count = (int)index - nCRLF;
        }
        for (;;) {
             if (c == EOS || count == maxCount) break;
             ++count;
             char c0 = c;
             c = SkipAndPeek();
             if (c0 <= '\r') {
                 if (c0 == '\r') {
                     if (c == '\n') c = SkipAndPeek();
                 } else if (c0 != '\n') continue;
                 RegisterNewline();
             }
        }
        StateTag = unchecked(stateTag + 1);
        return count;
    }

    public string ReadCharsOrNewlines(int maxCount, bool normalizeNewlines) {
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        char* lineBegin = null;
        uint lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        char* ptr = Ptr;
        if (ptr != null) {
            char* PtrEnd1 = PtrEnd - 1; // - 1 to guarantee the lookahead for '\r'
            char* end2 = unchecked(ptr + maxCount);
            char* end = end2 >= ptr && end2 <= PtrEnd1 ? end2 : PtrEnd1;
            if (ptr < end) {
                for (;;) {
                    char c = *ptr;
                    ++ptr;
                    if (c > '\r') {
                        if (ptr == end) break;
                    } else {
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                ++nCRLF;
                                if (end < PtrEnd1) ++end;
                            } else {
                                ++nCR;
                            }
                        } else if (c != '\n') goto CheckBound;
                        lineBegin = ptr;
                        ++lineOffset;
                    CheckBound:
                        if (ptr >= end) break;
                    }
                }
                if (end < PtrEnd1) {
                    char* ptr0 = Ptr;
                    Ptr = ptr;
                    int length = (int)Buffer.PositiveDistance(ptr0, ptr);
                    if (lineOffset == 0) {
                        ++StateTag;
                        return new string(ptr0, 0, length);
                    }
                    RegisterNewlines(lineBegin, lineOffset);
                    return   !normalizeNewlines || (nCR | nCRLF) == 0
                           ? new string(ptr0, 0, length)
                           : Text.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                }
            }
        }
        return ReadCharsOrNewlinesContinue(ptr, lineBegin, lineOffset, nCRLF, nCR, maxCount, normalizeNewlines);
    }
    private string ReadCharsOrNewlinesContinue(
                       char* ptr, char* lineBegin, uint lineOffset, int nCRLF, int nCR,
                       int maxCount, bool normalizeNewlines)
    {
        var stateTag = StateTag;
        var indexToken = IndexToken;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        int count;
        if (index == 0) {
            if (maxCount == 0 || (c = Peek()) == EOS) return "";
            count = 0;
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
            count = (int)index - nCRLF;
        }
        for (;;) {
             if (c == EOS || count == maxCount) break;
             ++count;
             char c0 = c;
             c = SkipAndPeek();
             if (c0 <= '\r') {
                 if (c0 == '\r') {
                     if (c == '\n') {
                        ++nCRLF;
                        c = SkipAndPeek();
                     } else {
                         ++nCR;
                     }
                 } else if (c0 != '\n') continue;
                 RegisterNewline();
             }
        }
        StateTag = unchecked(stateTag + 1);
        string str = ReadFrom(indexToken);
        if ((nCR | nCRLF) == 0 || !normalizeNewlines) return str;
        fixed (char* pStr = str)
        return Text.CopyWithNormalizedNewlines(pStr, str.Length, nCRLF, nCR);
    }

    public int SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> predicate) {
        return SkipCharsOrNewlinesWhile(predicate, predicate);
    }
    public int SkipCharsOrNewlinesWhile(FSharpFunc<char,bool> predicateForFirstChar,
                                        FSharpFunc<char,bool> predicate)
    {
        char* lineBegin = null;
        uint lineOffset = 0;
        int nCRLF = 0;
        char* ptr = Ptr;
        char* end = unchecked(PtrEnd - 1); // - 1 to guarantee the lookahead for '\r'
        if (ptr + 1 < PtrEnd) { // PtrEnd might be null
            char c = *ptr;
            ++ptr;
            if (c > '\r') {
                if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!predicateForFirstChar.Invoke('\n')) goto ReturnEmpty;
                if (*ptr == '\n') {
                    ++ptr;
                    ++nCRLF;
                }
                lineBegin = ptr;
                ++lineOffset;
            } else {
                if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                if (c == '\n') {
                    lineBegin = ptr;
                    lineOffset = 1;
                }
            }
            for (;;) {
                if (ptr >= end) goto EndOfBlock;
                c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (!predicate.Invoke(c)) break;
                } else if (c == '\r') {
                    if (!predicate.Invoke('\n')) break;
                    if (*ptr == '\n') {
                        ++ptr;
                        ++nCRLF;
                    }
                    lineBegin = ptr;
                    ++lineOffset;
                } else {
                    if (!predicate.Invoke(c)) break;
                    if (c == '\n') {
                        lineBegin = ptr;
                        ++lineOffset;
                    }
                }
            }
            --ptr;
            int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
            Ptr = ptr;
            if (lineOffset == 0) {
                ++StateTag;
                return count;
            }
            RegisterNewlines(lineBegin, lineOffset);
            return count;
        ReturnEmpty:
            return 0;
        }
    EndOfBlock:
        return SkipCharsOrNewlinesWhileContinue(ptr, lineBegin, lineOffset, nCRLF, predicateForFirstChar, predicate);
    }
    private int SkipCharsOrNewlinesWhileContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF,
                    FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate)
    {
        var stateTag = StateTag;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        int count;
        if (index == 0) {
            c = Peek();
            char cc = c == '\r' ? '\n' : c;
            if (c == EOS || !predicateForFirstChar.Invoke(cc)) return 0;
            count = 1;
            char c0 = c;
            c = SkipAndPeek();
            if (cc == '\n') {
                if (c0 == '\r' && c == '\n') c = SkipAndPeek();
                RegisterNewline();
            }
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
            count = (int)index - nCRLF;
        }
        for (;;) {
            if (c == EOS) break;
            if (c != '\r' && c != '\n') {
                if (!predicate.Invoke(c)) break;
                count = unchecked(count + 1);
                if (count >= 0) c = SkipAndPeek();
                else { // overflow
                    count = unchecked(count - 1);
                    break;
                }
            } else {
                if (!predicate.Invoke('\n')) break;
                count = unchecked(count + 1);
                if (count >= 0) {
                    char c0 = c;
                    c = SkipAndPeek();
                    if (c0 == '\r' && c == '\n') c = SkipAndPeek();
                    RegisterNewline();
                } else {
                    count = unchecked(count - 1);
                    break;
                }
            }
        }
        StateTag = unchecked(stateTag + 1);
        return count;
    }

    public string ReadCharsOrNewlinesWhile(FSharpFunc<char,bool> predicate, bool normalizeNewlines) {
        return ReadCharsOrNewlinesWhile(predicate, predicate, normalizeNewlines);
    }
    public string ReadCharsOrNewlinesWhile(
                      FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate,
                      bool normalizeNewlines)
    {
        char* lineBegin = null;
        uint lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        char* ptr = Ptr;
        char* end = unchecked(PtrEnd - 1); // - 1 to guarantee the lookahead for '\r'
        if (ptr + 1 < PtrEnd) { // PtrEnd might be null
            char c = *ptr;
            ++ptr;
            if (c > '\r') {
                if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!predicateForFirstChar.Invoke('\n')) goto ReturnEmpty;
                if (*ptr == '\n') {
                    ++ptr;
                    ++nCRLF;
                } else {
                    ++nCR;
                }
                lineBegin = ptr;
                ++lineOffset;
            } else {
                if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                if (c == '\n') {
                    lineBegin = ptr;
                    lineOffset = 1;
                }
            }
            for (;;) {
                if (ptr >= end) goto EndOfBlock;
                c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (!predicate.Invoke(c)) break;
                } else if (c == '\r') {
                    if (!predicate.Invoke('\n')) break;
                    if (*ptr == '\n') {
                        ++ptr;
                        ++nCRLF;
                    } else {
                        ++nCR;
                    }
                    lineBegin = ptr;
                    ++lineOffset;
                } else {
                    if (!predicate.Invoke(c)) break;
                    if (c == '\n') {
                        lineBegin = ptr;
                        ++lineOffset;
                    }
                }
            }
            --ptr;
            char* ptr0 = Ptr;
            Ptr = ptr;
            int length = (int)Buffer.PositiveDistance(ptr0, ptr);
            if (lineOffset == 0) {
                ++StateTag;
                return new string(ptr0, 0, length);
            }
            RegisterNewlines(lineBegin, lineOffset);
            return !normalizeNewlines || (nCR | nCRLF) == 0
                   ? new string(ptr0, 0, length)
                   : Text.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
        ReturnEmpty:
            return "";
        }
    EndOfBlock:
        return ReadCharsOrNewlinesWhileContinue(ptr, lineBegin, lineOffset, nCRLF, nCR, predicateForFirstChar, predicate, normalizeNewlines);
    }
    private string ReadCharsOrNewlinesWhileContinue(
                       char* ptr, char* lineBegin, uint lineOffset, int nCRLF, int nCR,
                       FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate,
                       bool normalizeNewlines)
    {
        var stateTag = StateTag;
        var indexToken = IndexToken;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        int count;
        if (index == 0) {
            c = Peek();
            char cc = c == '\r' ? '\n' : c;
            if (c == EOS || !predicateForFirstChar.Invoke(cc)) return "";
            count = 1;
            char c0 = c;
            c = SkipAndPeek();
            if (cc == '\n') {
                if (c0 == '\r') {
                    if (c == '\n') {
                        ++nCRLF;
                        c = SkipAndPeek();
                    } else {
                        ++nCR;
                    }
                }
                RegisterNewline();
            }
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
            count = (int)index - nCRLF;
        }
        for (;;) {
            if (c == EOS) break;
            if (c != '\r' && c != '\n') {
                if (!predicate.Invoke(c)) break;
                count = unchecked(count + 1);
                if (count < 0) break;
                c = SkipAndPeek();
            } else {
                if (!predicate.Invoke('\n')) break;
                count = unchecked(count + 1);
                if (count < 0) break;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 == '\r') {
                    if (c == '\n') {
                        ++nCRLF;
                        c = SkipAndPeek();
                    } else {
                        ++nCR;
                    }
                }
                RegisterNewline();
            }
        }
        StateTag = unchecked(stateTag + 1);
        string str = ReadFrom(indexToken);
        if ((nCR | nCRLF) == 0 || !normalizeNewlines) return str;
        fixed (char* pStr = str)
        return Text.CopyWithNormalizedNewlines(pStr, str.Length, nCRLF, nCR);
    }

    public int SkipCharsOrNewlinesWhile(FSharpFunc<char,bool> predicate, int minCount, int maxCount) {
        return SkipCharsOrNewlinesWhile(predicate, predicate, minCount, maxCount);
    }
    public int SkipCharsOrNewlinesWhile(
                   FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate,
                   int minCount, int maxCount)
    {
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        char* lineBegin = null;
        uint lineOffset = 0;
        int nCRLF = 0;
        char* ptr = Ptr;
        if (ptr != null) {
            char* bufferEnd1 = unchecked(PtrEnd - 1); // - 1 to guarantee the lookahead for '\r'
            char* end2 = unchecked(ptr + maxCount);
            char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
            if (ptr < end) {
                char c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                } else if (c == '\r') {
                    if (!predicateForFirstChar.Invoke('\n')) goto ReturnEmpty;
                    if (*ptr == '\n') {
                        ++ptr;
                        ++nCRLF;
                        if (end < bufferEnd1) ++end;
                    }
                    lineBegin = ptr;
                    ++lineOffset;
                } else {
                    if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                    if (c == '\n') {
                        lineBegin = ptr;
                        ++lineOffset;
                    }
                }
                for (;;) {
                    if (ptr < end) {
                        c = *ptr;
                        ++ptr;
                        if (c > '\r') {
                            if (!predicate.Invoke(c)) break;
                        } else if (c == '\r') {
                            if (!predicate.Invoke('\n')) break;
                            if (*ptr == '\n') {
                                ++ptr;
                                ++nCRLF;
                                if (end < bufferEnd1) ++end;
                            }
                            lineBegin = ptr;
                            ++lineOffset;
                        } else {
                            if (!predicate.Invoke(c)) break;
                            if (c == '\n') {
                                lineBegin = ptr;
                                ++lineOffset;
                            }
                        }
                    } else {
                        if (end >= bufferEnd1) goto EndOfBlock;
                        goto ReturnCount;
                    }
                }
                --ptr;
            ReturnCount:
                int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                if (count >= minCount) {
                    Ptr = ptr;
                    if (lineOffset == 0) {
                        ++StateTag;
                        return count;
                    } else {
                        RegisterNewlines(lineBegin, lineOffset);
                        return count;
                    }
                }
            ReturnEmpty:
                return 0;
            }
        }
    EndOfBlock:
        return SkipCharsOrNewlinesWhileContinue(ptr, lineBegin, lineOffset, nCRLF, predicateForFirstChar, predicate, minCount, maxCount);
    }
    private int SkipCharsOrNewlinesWhileContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF,
                    FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate,
                    int minCount, int maxCount)
    {
        var ptr0 = Ptr;
        var block0 = Block;
        var tag0 = StateTag;
        var line0 = _Line;
        var lineBegin0 = _LineBegin;
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        int count;
        if (index == 0) {
            c = Peek();
            if (c == EOS || maxCount == 0) goto ReturnEmpty;
            if (c != '\r' && c != '\n') {
                if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                count = 1;
                c = SkipAndPeek();
            } else {
                if (!predicateForFirstChar.Invoke('\n')) goto ReturnEmpty;
                count = 1;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 == '\r' && c == '\n') c = SkipAndPeek();
                RegisterNewline();
            }
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
            count = (int)index - nCRLF;
        }
        for (;;) {
            if (c == EOS || count == maxCount) break;
            if (c != '\r' && c != '\n') {
                if (!predicate.Invoke(c)) break;
                ++count;
                c = SkipAndPeek();
            } else {
                if (!predicate.Invoke('\n')) break;
                ++count;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 == '\r' && c == '\n') c = SkipAndPeek();
                RegisterNewline();
            }
        }
        if (count >= minCount) {
            StateTag = unchecked(tag0 + 1);
            return count;
        }
    ReturnEmpty:
        // backtrack
        Seek(ptr0, block0);
        _Line = line0;
        _LineBegin = lineBegin0;
        StateTag = tag0;
        return 0;
    }

    public string ReadCharsOrNewlinesWhile(FSharpFunc<char,bool> predicate, int minCount, int maxCount, bool normalizeNewlines) {
        return ReadCharsOrNewlinesWhile(predicate, predicate, minCount, maxCount, normalizeNewlines);
    }
    public string ReadCharsOrNewlinesWhile(
                      FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate,
                      int minCount, int maxCount, bool normalizeNewlines)
    {
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        char* lineBegin = null;
        uint lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        char* ptr = Ptr;
        if (ptr != null) {
            char* bufferEnd1 = PtrEnd - 1; // - 1 to guarantee the lookahead for '\r'
            char* end2 = unchecked(ptr + maxCount);
            char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
            if (ptr < end) {
                char c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                } else if (c == '\r') {
                    if (!predicateForFirstChar.Invoke('\n')) goto ReturnEmpty;
                    if (*ptr == '\n') {
                        ++ptr;
                        ++nCRLF;
                        if (end < bufferEnd1) ++end;
                    } else {
                        ++nCR;
                    }
                    lineBegin = ptr;
                    lineOffset = 1;
                } else {
                    if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                    if (c == '\n') {
                        lineBegin = ptr;
                        lineOffset = 1;
                    }
                }
                for (;;) {
                    if (ptr < end) {
                        c = *ptr;
                        ++ptr;
                        if (c > '\r') {
                            if (!predicate.Invoke(c)) break;
                        } else if (c == '\r') {
                            if (!predicate.Invoke('\n')) break;
                            if (*ptr == '\n') {
                                ++ptr;
                                ++nCRLF;
                                if (end < bufferEnd1) ++end;
                            } else {
                                ++nCR;
                            }
                            lineBegin = ptr;
                            ++lineOffset;
                        } else {
                            if (!predicate.Invoke(c)) break;
                            if (c == '\n') {
                                lineBegin = ptr;
                                ++lineOffset;
                            }
                        }
                    } else {
                        if (end >= bufferEnd1) goto EndOfBlock;
                        goto ReturnStringInBlock;
                    }
                }
                --ptr;
            ReturnStringInBlock:
                {
                    char* ptr0 = Ptr;
                    int length = (int)Buffer.PositiveDistance(ptr0, ptr);
                    if (length - nCRLF >= minCount) {
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            ++StateTag;
                            return new string(ptr0, 0, length);
                        }
                        RegisterNewlines(lineBegin, lineOffset);
                        return !normalizeNewlines || (nCR | nCRLF) == 0
                               ? new string(ptr0, 0, length)
                               : Text.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                    }
                }
            ReturnEmpty:
                return "";
            }
        }
    EndOfBlock:
        return ReadCharsOrNewlinesWhileContinue(ptr, lineBegin, lineOffset, nCRLF, nCR, predicateForFirstChar, predicate, minCount, maxCount, normalizeNewlines);
    }
    private string ReadCharsOrNewlinesWhileContinue(
                       char* ptr, char* lineBegin, uint lineOffset, int nCRLF, int nCR,
                       FSharpFunc<char,bool> predicateForFirstChar, FSharpFunc<char,bool> predicate,
                       int minCount, int maxCount, bool normalizeNewlines)
    {
        var ptr0 = Ptr;
        var block0 = Block;
        var tag0 = StateTag;
        var line0 = _Line;
        var lineBegin0 = _LineBegin;

        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c;
        int count;
        if (index == 0) {
            c = Peek();
            if (c == EOS || maxCount == 0) goto ReturnEmpty;
            if (c != '\r' && c != '\n') {
                count = 1;
                if (!predicateForFirstChar.Invoke(c)) goto ReturnEmpty;
                c = SkipAndPeek();
            } else {
                if (!predicateForFirstChar.Invoke('\n')) goto ReturnEmpty;
                count = 1;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 == '\r') {
                    if (c == '\n') {
                        ++nCRLF;
                        c = SkipAndPeek();
                    } else {
                        ++nCR;
                    }
                }
                RegisterNewline();
            }
        } else {
            if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
            c = SkipAndPeek(index);
            count = (int)index - nCRLF;
        }
        for (;;) {
            if (c == EOS || count == maxCount) break;
            if (c != '\r' && c != '\n') {
                if (!predicate.Invoke(c)) break;
                ++count;
                c = SkipAndPeek();
            } else {
                if (!predicate.Invoke('\n')) break;
                ++count;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 == '\r') {
                    if (c == '\n') {
                        ++nCRLF;
                        c = SkipAndPeek();
                    } else {
                        ++nCR;
                    }
                }
                RegisterNewline();
            }
        }
        if (count >= minCount) {
            StateTag = unchecked(tag0 + 1);
            string str = ReadFrom(ptr0, block0);
            if ((nCR | nCRLF) == 0 || !normalizeNewlines) return str;
            fixed (char* pStr = str)
            return Text.CopyWithNormalizedNewlines(pStr, str.Length, nCRLF, nCR);
        }
    ReturnEmpty:
         // backtrack
        Seek(ptr0, block0);
        _Line = line0;
        _LineBegin = lineBegin0;
        StateTag = tag0;
        return "";
    }

    private static bool Rest3OfStringEquals(char* str1, char* str2, int length) {
        for (int i = 3; i < length; ++i) {
            if (str1[i] != str2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    private static bool Rest3OfStringEqualsCaseFolded(char* str1, char* cfStr2, int length) {
        char* cftable = CaseFoldTable.FoldedChars;
        for (int i = 3; i < length; ++i) {
            if (cftable[str1[i]] != cfStr2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    public int SkipCharsOrNewlinesUntilString(string str, int maxCount, out bool foundString) {
        int strLength = str.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        char* lineBegin = null;
        fixed (char* pStr = str) {
            uint lineOffset = 0;
            int nCRLF = 0;
            char* ptr = Ptr;
            if (ptr != null) {
                char* bufferEnd = PtrEnd;
                char* end1 = unchecked(bufferEnd - strLength);
                if (end1 >= ptr && end1 < bufferEnd) {
                    char* end2 = unchecked(ptr + maxCount);
                    char* end = end2 < ptr || end1 <= end2 ? end1 : end2;
                    for (;;) {
                        char c = *ptr;
                        if (c != pStr[0]) {
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        } else {
                            Debug.Assert(ptr + strLength <= PtrEnd);
                            if (strLength == 1 || (ptr[1] == pStr[1] &&
                                (strLength == 2 || (ptr[2] == pStr[2] &&
                                 (strLength == 3 || Rest3OfStringEquals(ptr, pStr, strLength))))))
                            {
                                foundString = true;
                                int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                                Ptr = ptr;
                                if (lineOffset == 0) {
                                    if (count != 0) ++StateTag;
                                    return count;
                                } else {
                                    RegisterNewlines(lineBegin, lineOffset);
                                    return count;
                                }
                            }
                            c = *ptr;
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        }
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                lineBegin = ptr;
                                ++lineOffset;
                                ++nCRLF;
                                if (end < end1) ++end;
                                else if (ptr > end) break;
                                continue;
                            }
                        } else if (c != '\n') continue;
                        lineBegin = ptr;
                        ++lineOffset;
                    } // for
                    if (ptr < end1) {
                        foundString = false;
                        int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            if (count != 0) ++StateTag;
                            return count;
                        } else {
                            RegisterNewlines(lineBegin, lineOffset);
                            return count;
                        }
                    }
                }
            }
            return SkipCharsOrNewlinesUntilStringContinue(ptr, lineBegin, lineOffset, nCRLF, pStr, strLength, maxCount, out foundString);
        }
    }
    private int SkipCharsOrNewlinesUntilStringContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF,
                    char* pStr, int strLength, int maxCount, out bool foundString)
    {
        var stateTag = StateTag;
        foundString = false;
        if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char c = SkipAndPeek((uint)index);
        int count = (int)index - nCRLF;
        for (;;) {
            if (c != pStr[0] || !Match(pStr, strLength)) {
                if (c == EOS || count == maxCount) break;
                ++count;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 <= '\r') {
                    if (c0 == '\r') {
                        if (c == '\n') {
                            c = SkipAndPeek();
                        }
                    } else if (c0 != '\n') continue;
                    RegisterNewline();
                }
            } else {
                foundString = true;
                break;
            }
        }
        StateTag = count == 0 ? stateTag : unchecked(stateTag + 1);
        return count;
    }

    public int SkipCharsOrNewlinesUntilString(
                   string str, int maxCount, bool normalizeNewlines,
                   out string skippedCharsIfStringFoundOtherwiseNull)
    {
        int strLength = str.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        fixed (char* pStr = str) {
            char* lineBegin = null;
            uint lineOffset = 0;
            int nCRLF = 0;
            int nCR = 0;
            char* ptr = Ptr;
            if (ptr != null) {
                char* end1 = unchecked(PtrEnd - strLength);
                if (end1 >= ptr && end1 < PtrEnd) {
                    char* end2 = unchecked(ptr + maxCount);
                    char* end = end2 < ptr || end1 <= end2 ? end1 : end2;
                    for (;;) {
                        char c = *ptr;
                        if (c != pStr[0]) {
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        } else {
                            Debug.Assert(ptr + strLength <= PtrEnd);
                            if (strLength == 1 || (ptr[1] == pStr[1] &&
                                (strLength == 2 || (ptr[2] == pStr[2] &&
                                 (strLength == 3 || Rest3OfStringEquals(ptr, pStr, strLength))))))
                            {
                                char* ptr0 = Ptr;
                                if (ptr != ptr0) {
                                    Ptr = ptr;
                                    int length = (int)Buffer.PositiveDistance(ptr0, ptr);
                                    if (lineOffset == 0) {
                                        if (length != 0) ++StateTag;
                                        skippedCharsIfStringFoundOtherwiseNull = new string(ptr0, 0, length);
                                        return length;
                                    } else {
                                        RegisterNewlines(lineBegin, lineOffset);
                                        skippedCharsIfStringFoundOtherwiseNull = !normalizeNewlines || (nCR | nCRLF) == 0
                                                        ? new string(ptr0, 0, length)
                                                        : Text.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                                        return length - nCRLF;
                                    }
                                } else {
                                    skippedCharsIfStringFoundOtherwiseNull = "";
                                    return 0;
                                }
                            }
                            c = *ptr;
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        }
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                lineBegin = ptr;
                                ++lineOffset;
                                ++nCRLF;
                                if (end < end1) ++end;
                                else if (ptr > end) break;
                                continue;
                            } else {
                                ++nCR;
                            }
                        } else if (c != '\n') continue;
                        lineBegin = ptr;
                        ++lineOffset;
                    } // for
                    if (ptr < end1) {
                        skippedCharsIfStringFoundOtherwiseNull = null;
                        int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            if (count != 0) ++StateTag;
                            return count;
                        } else {
                            RegisterNewlines(lineBegin, lineOffset);
                            return count;
                        }
                    }
                }
            }
            return SkipCharsOrNewlinesUntilStringContinue(ptr, lineBegin, lineOffset, nCRLF, nCR, pStr, strLength, maxCount, normalizeNewlines, out skippedCharsIfStringFoundOtherwiseNull);
        }
    }
    private int SkipCharsOrNewlinesUntilStringContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF, int nCR,
                    char* pStr, int strLength, int maxCount, bool normalizeNewlines, out string skippedCharsIfStringFoundOtherwiseNull)
    {
        var stateTag = StateTag;
        var indexToken = IndexToken;
        if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        int count = (int)index - nCRLF;
        char c = SkipAndPeek(index);
        for (;;) {
            if (c != pStr[0] || !Match(pStr, strLength)) {
                if (c == EOS || count == maxCount) break;
                ++count;
                char c0 = c;
                c = SkipAndPeek();
                if (c0 <= '\r') {
                    if (c0 == '\r') {
                        if (c == '\n') {
                            c = SkipAndPeek();
                            ++nCRLF;
                        } else {
                            ++nCR;
                        }
                    } else if (c0 != '\n') continue;
                    RegisterNewline();
                }
            } else { // found string
                if (count != 0) {
                    StateTag = unchecked(stateTag + 1);
                    var s = ReadFrom(indexToken);
                    if (!normalizeNewlines || (nCR | nCRLF) == 0) {
                        skippedCharsIfStringFoundOtherwiseNull = s;
                        return count;
                    } else {
                        fixed (char* ps = s)
                        skippedCharsIfStringFoundOtherwiseNull = Text.CopyWithNormalizedNewlines(ps, s.Length, nCRLF, nCR);
                        return count;
                    }
                } else {
                    StateTag = stateTag;
                    skippedCharsIfStringFoundOtherwiseNull = "";
                    return 0;
                }
            }
        }
        StateTag = count == 0 ? stateTag : unchecked(stateTag + 1);
        skippedCharsIfStringFoundOtherwiseNull = null;
        return count;
    }

    public int SkipCharsOrNewlinesUntilCaseFoldedString(
                   string caseFoldedString, int maxCount,
                   out bool foundString)
    {
        int strLength = caseFoldedString.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        char* lineBegin = null;
        fixed (char* pStr = caseFoldedString) {
            uint lineOffset = 0;
            int nCRLF = 0;
            char* ptr = Ptr;
            if (ptr != null) {
                char* bufferEnd = PtrEnd;
                char* end1 = unchecked(bufferEnd - strLength);
                if (end1 >= ptr && end1 < bufferEnd) {
                    char* end2 = unchecked(ptr + maxCount);
                    char* end = end2 < ptr || end1 <= end2 ? end1 : end2;

                    char* cftable = CaseFoldTable.FoldedChars;
                    for (;;) {
                        char c = cftable[*ptr];
                        if (c != pStr[0]) {
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        } else {
                            Debug.Assert(ptr + strLength <= PtrEnd);
                            if (strLength == 1 || (cftable[ptr[1]] == pStr[1] &&
                                (strLength == 2 || (cftable[ptr[2]] == pStr[2] &&
                                 (strLength == 3 || Rest3OfStringEqualsCaseFolded(ptr, pStr, strLength))))))
                            {
                                foundString = true;
                                int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                                Ptr = ptr;
                                if (lineOffset == 0) {
                                    if (count != 0) ++StateTag;
                                    return count;
                                } else {
                                    RegisterNewlines(lineBegin, lineOffset);
                                    return count;
                                }
                            }
                            c = *ptr; // we don't need to casefold here
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        }
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                lineBegin = ptr;
                                ++lineOffset;
                                ++nCRLF;
                                if (end < end1) ++end;
                                else if (ptr > end) break;
                                continue;
                            }
                        } else if (c != '\n') continue;
                        lineBegin = ptr;
                        ++lineOffset;
                    } // for
                    if (ptr < end1) {
                        foundString = false;
                        int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            if (count != 0) ++StateTag;
                            return count;
                        } else {
                            RegisterNewlines(lineBegin, lineOffset);
                            return count;
                        }
                    }
                }
            }
            return SkipCharsOrNewlinesUntilCaseFoldedStringContinue(ptr, lineBegin, lineOffset, nCRLF, pStr, strLength, maxCount, out foundString);
        }
    }
    private int SkipCharsOrNewlinesUntilCaseFoldedStringContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF,
                    char* pStr, int strLength, int maxCount, out bool foundString)
    {
        var stateTag = StateTag;
        foundString = false;
        if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        char* cftable = CaseFoldTable.FoldedChars;
        char c = cftable[SkipAndPeek((uint)index)];
        int count = (int)index - nCRLF;
        for (;;) {
            if (c != pStr[0] || !MatchCaseFolded(pStr, strLength)) {
                if (c == EOS || count == maxCount) break;
                ++count;
                char c0 = c;
                c = cftable[SkipAndPeek()];
                if (c0 <= '\r') {
                    if (c0 == '\r') {
                        if (c == '\n') {
                            c = cftable[SkipAndPeek()];
                            ++nCRLF;
                        }
                    } else if (c0 != '\n') continue;
                    RegisterNewline();
                }
            } else {
                foundString = true;
                break;
            }
        }
        StateTag = count == 0 ? stateTag : unchecked(stateTag + 1);
        return count;
    }

    public int SkipCharsOrNewlinesUntilCaseFoldedString(
                   string caseFoldedString, int maxCount, bool normalizeNewlines,
                   out string skippedCharsIfStringFoundOtherwiseNull)
    {
        int strLength = caseFoldedString.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCount < 0) throw new ArgumentOutOfRangeException("maxCount", "maxCount is negative.");
        fixed (char* pStr = caseFoldedString) {
            char* lineBegin = null;
            uint lineOffset = 0;
            int nCRLF = 0;
            int nCR = 0;
            char* ptr = Ptr;
            if (ptr != null) {
                char* bufferEnd = PtrEnd;
                char* end1 = unchecked(bufferEnd - strLength);
                if (end1 >= ptr && end1 < bufferEnd) {
                    char* end2 = unchecked(ptr + maxCount);
                    char* end = end2 < ptr || end1 <= end2 ? end1 : end2;
                    char* cftable = CaseFoldTable.FoldedChars;
                    for (;;) {
                        char c = cftable[*ptr];
                        if (c != pStr[0]) {
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        } else {
                            Debug.Assert(ptr + strLength <= PtrEnd);
                            if (strLength == 1 || (cftable[ptr[1]] == pStr[1] &&
                                (strLength == 2 || (cftable[ptr[2]] == pStr[2] &&
                                 (strLength == 3 || Rest3OfStringEqualsCaseFolded(ptr, pStr, strLength))))))
                            {
                                char* ptr0 = Ptr;
                                if (ptr != ptr0) {
                                    Ptr = ptr;
                                    int length = (int)Buffer.PositiveDistance(ptr0, ptr);
                                    if (lineOffset == 0) {
                                        if (length != 0) ++StateTag;
                                        skippedCharsIfStringFoundOtherwiseNull = new string(ptr0, 0, length);
                                        return length;
                                    } else {
                                        RegisterNewlines(lineBegin, lineOffset);
                                        skippedCharsIfStringFoundOtherwiseNull = !normalizeNewlines || (nCR | nCRLF) == 0
                                                        ? new string(ptr0, 0, length)
                                                        : Text.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                                        return length - nCRLF;
                                    }
                                } else {
                                    skippedCharsIfStringFoundOtherwiseNull = "";
                                    return 0;
                                }
                            }
                            c = *ptr; // we don't need to casefold here
                            if (ptr == end) break;
                            ++ptr;
                            if (c > '\r' || c == '\t') continue;
                        }
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                lineBegin = ptr;
                                ++lineOffset;
                                ++nCRLF;
                                if (end < end1) ++end;
                                else if (ptr > end) break;
                                continue;
                            } else {
                                ++nCR;
                            }
                        } else if (c != '\n') continue;
                        lineBegin = ptr;
                        ++lineOffset;
                    } // for
                    if (ptr < end1) {
                        skippedCharsIfStringFoundOtherwiseNull = null;
                        int count = (int)Buffer.PositiveDistance(Ptr, ptr) - nCRLF;
                        Ptr = ptr;
                        if (lineOffset == 0) {
                            if (count != 0) ++StateTag;
                            return count;
                        } else {
                            RegisterNewlines(lineBegin, lineOffset);
                            return count;
                        }
                    }
                }
            }
            return SkipCharsOrNewlinesUntilCaseFoldedStringContinue(ptr, lineBegin, lineOffset, nCRLF, nCR, pStr, strLength, maxCount, normalizeNewlines, out skippedCharsIfStringFoundOtherwiseNull);
        }
    }
    private int SkipCharsOrNewlinesUntilCaseFoldedStringContinue(
                    char* ptr, char* lineBegin, uint lineOffset, int nCRLF, int nCR,
                    char* pStr, int strLength, int maxCount, bool normalizeNewlines, out string skippedCharsIfStringFoundOtherwiseNull)
    {
        var stateTag = StateTag;
        var indexToken = IndexToken;
        if (lineOffset != 0) RegisterNewlines(lineBegin, lineOffset);
        uint index = Buffer.PositiveDistance(Ptr, ptr);
        int count = (int)index - nCRLF;
        char* cftable = CaseFoldTable.FoldedChars;
        char c = cftable[SkipAndPeek(index)];
        for (;;) {
            if (c != pStr[0] || !MatchCaseFolded(pStr, strLength)) {
                if (c == EOS || count == maxCount) break;
                ++count;
                char c0 = c;
                c = cftable[SkipAndPeek()];
                if (c0 <= '\r') {
                    if (c0 == '\r') {
                        if (c == '\n') {
                            c = cftable[SkipAndPeek()];
                            ++nCRLF;
                        } else {
                            ++nCR;
                        }
                    } else if (c0 != '\n') continue;
                    RegisterNewline();
                }
            } else { // found string
                if (count != 0) {
                    StateTag = unchecked(stateTag + 1);
                    var s = ReadFrom(indexToken);
                    if ((nCR | nCRLF) == 0 || !normalizeNewlines) {
                        skippedCharsIfStringFoundOtherwiseNull = s;
                        return count;
                    } else {
                        fixed (char* ps = s)
                        skippedCharsIfStringFoundOtherwiseNull = Text.CopyWithNormalizedNewlines(ps, s.Length, nCRLF, nCR);
                        return count;
                    }
                } else {
                    StateTag = stateTag;
                    skippedCharsIfStringFoundOtherwiseNull = "";
                    return 0;
                }
            }
        }
        StateTag = count == 0 ? stateTag : unchecked(stateTag + 1);
        skippedCharsIfStringFoundOtherwiseNull = null;
        return count;
    }
}

public unsafe struct CharStreamState<TUserState> {
#if DEBUG
    internal readonly CharStream<TUserState> CharStream;
    private long Index { get { return GetIndex(CharStream); } }
#endif
    internal readonly char* Ptr;
    internal readonly int   Block;
#if SMALL_STATETAG
    public   readonly int Tag;
#else
    public   readonly long Tag;
#endif
    public   readonly long  Line;
    public   readonly long  LineBegin;
    public   readonly TUserState UserState;
    public   readonly string Name;

    // Public (though undocumented) as long as the .NET JIT doesn't
    // always inline CharStream<TUserState>.State
    public CharStreamState(CharStream<TUserState> charStream) {
     #if DEBUG
        CharStream = charStream;
     #endif
        Ptr       = charStream.Ptr;
        Block     = charStream.Block;
        Tag       = charStream.StateTag;
        Line      = charStream._Line;
        LineBegin = charStream._LineBegin;
        UserState = charStream._UserState;
        Name      = charStream._Name;
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public CharStreamIndexToken IndexToken { get {
        if (Line <= 0) // tests for a zero-initialized state
            throw new InvalidOperationException("The CharStreamState is invalid.");

        return new CharStreamIndexToken(
               #if DEBUG
                   CharStream,
               #endif
                   Ptr,
                   Block);
    } }

    // On .NET calling an instance method of a generic struct can be more
    // expensive than calling an instance method of a generic class
    // (when the type parameter value is not statically known at the call
    // site and isn't a value type that makes the .NET JIT specialize
    // the code).
    //
    // Moving the actual implementations of the following methods into
    // the CharStream<TUserState> class allows the .NET JIT to inline them,
    // so that we effectively replace struct method calls with cheaper
    // class method calls.

    public long GetIndex(CharStream<TUserState> charStreamFromWhichStateWasRetrieved) {
        return charStreamFromWhichStateWasRetrieved.GetIndex(ref this);
    }

    public Position GetPosition(CharStream<TUserState> charStreamFromWhichStateWasRetrieved) {
        return charStreamFromWhichStateWasRetrieved.GetPosition(ref this);
    }
}

/// <summary>Provides read‐access to a sequence of UTF‐16 chars.</summary>
public unsafe sealed class CharStream<TUserState> : CharStream {
    // we don't have a public constructor that only takes a string to avoid potential confusion with a filepath constructor
    internal CharStream(string chars) : base(chars) { }

    public CharStream(string chars, int index, int length) : base(chars, index, length) {}

    public CharStream(string chars, int index, int length, long streamIndexOffset)
           : base(chars, index, length, streamIndexOffset) { }

    public CharStream(char[] chars, int index, int length) : base(chars, index, length) { }

    public CharStream(char[] chars, int index, int length, long streamIndexOffset)
           : base(chars, index, length, streamIndexOffset) { }

    public CharStream(char* chars, int length) : base(chars, length) { }

    public CharStream(char* chars, int length, long streamIndexOffset)
           : base(chars, length, streamIndexOffset) { }

    internal CharStream(string chars, char* pChars, char* begin, int length)
             : base(chars, pChars, begin, length) { }

    public CharStream(string path, Encoding encoding)
           : base(path, encoding) { }

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : base(path, encoding, detectEncodingFromByteOrderMarks) { }

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks,
                      int blockSize, int blockOverlap, int byteBufferLength)
           : base(path, encoding, detectEncodingFromByteOrderMarks, blockSize, blockOverlap, byteBufferLength) { }

    public CharStream(Stream stream, Encoding encoding)
           : base(stream, encoding) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding)
           : base(stream, leaveOpen, encoding) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : base(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks) { }

    public CharStream(Stream stream, bool leaveOpen,
                      Encoding encoding, bool detectEncodingFromByteOrderMarks,
                      int blockSize, int blockOverlap, int byteBufferLength)
           : base(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks,
                  blockSize, blockOverlap, byteBufferLength) {}

    internal TUserState _UserState;
    public TUserState UserState {
        get { return _UserState; }
        set { _UserState = value; ++StateTag; }
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public CharStreamState<TUserState> State { get {
        return new CharStreamState<TUserState>(this);
    } }


    // GetIndex and GetPosition are helper methods for CharStreamState

    internal long GetIndex(ref CharStreamState<TUserState> state) {
        if (state.Line <= 0) // tests for a zero-initialized state
            throw new InvalidOperationException("The CharStreamState is invalid.");
    #if DEBUG
        Debug.Assert(this == state.CharStream);
    #endif
        return GetIndex(state.Ptr, state.Block);
    }

    internal Position GetPosition(ref CharStreamState<TUserState> state) {
        if (state.Line <= 0) // tests for a zero-initialized state
            throw new InvalidOperationException("The CharStreamState is invalid.");
    #if DEBUG
        Debug.Assert(this == state.CharStream);
    #endif
        long index = GetIndex(state.Ptr, state.Block);
        return new Position(state.Name, index, state.Line, index - state.LineBegin + 1);
    }

    // Passing a large struct by value is suboptimal, so for optimization purposes
    // we define internal overloads that take ref arguments. Unfortunately, C#/F#
    // doesn't have const-refs, so we can't make these overloads public (at least,
    // not without risking heart attacks within certain user demographics of this library).
    // An alternative would be to move the following methods into the CharStreamState class,
    // but IMHO the resulting API would feel less intuitive and be somewhat less disoverable.

    public void BacktrackTo(CharStreamState<TUserState> state) {
        BacktrackTo(ref state);
    }
    internal void BacktrackTo(ref CharStreamState<TUserState> state) {
        if (state.Line <= 0) // tests for zero-initialized states
            throw new ArgumentException("The CharStreamState is invalid.");
    #if DEBUG
        Debug.Assert(this == state.CharStream);
    #endif
        Seek(state.Ptr, state.Block);
        StateTag   = state.Tag;
        _Line      = state.Line;
        _LineBegin = state.LineBegin;
        _UserState = state.UserState;
        _Name      = state.Name;
    }

    public string ReadFrom(CharStreamState<TUserState> stateWhereStringBegins, bool normalizeNewlines) {
        return ReadFrom(ref stateWhereStringBegins, normalizeNewlines);
    }
    internal string ReadFrom(ref CharStreamState<TUserState> stateWhereStringBegins, bool normalizeNewlines) {
        if (stateWhereStringBegins.Line <= 0) // tests for zero-initialized states
            throw new ArgumentException("The CharStreamState is invalid.");
    #if DEBUG
        Debug.Assert(this == stateWhereStringBegins.CharStream);
    #endif
        string str = ReadFrom(stateWhereStringBegins.Ptr, stateWhereStringBegins.Block);
        if (!normalizeNewlines || _Line == stateWhereStringBegins.Line) return str;
        return Text.NormalizeNewlines(str);
    }

    public CharStream<TSubStreamUserState> CreateSubstream<TSubStreamUserState>(CharStreamState<TUserState> stateWhereSubstreamBegins) {
        return CreateSubstream<TSubStreamUserState>(ref stateWhereSubstreamBegins);
    }
    internal CharStream<TSubStreamUserState> CreateSubstream<TSubStreamUserState>(ref CharStreamState<TUserState> stateWhereSubstreamBegins) {
        if (stateWhereSubstreamBegins.Line <= 0) // tests for zero-initialized states
            throw new ArgumentException("The CharStreamState is invalid.");
    #if DEBUG
        Debug.Assert(this == stateWhereSubstreamBegins.CharStream);
    #endif
        CharStream<TSubStreamUserState> subStream;
        if (IsSingleBlockStream) {
            // the CharStream has only one block, so its safe to
            // construct a new CharStream from a pointer into the original buffer
            char* ptr0 = stateWhereSubstreamBegins.Ptr;
            if (ptr0 == null) ptr0 = BufferEnd;
            char* end = Ptr;
            if (end == null) end = BufferEnd;
            if (end < ptr0) throw new ArgumentException("The current position of the stream must not lie before the position corresponding to the given CharStreamState.");
            int length = (int)Buffer.PositiveDistance(ptr0, end);
            subStream = new CharStream<TSubStreamUserState>(BufferString, BufferStringPointer, ptr0, length);
            var indexOfFirstChar = Buffer.PositiveDistance(BufferBegin, ptr0) + _IndexOfFirstChar;
            subStream.IndexOfFirstCharInBlock = indexOfFirstChar;
            subStream._IndexOfFirstChar = indexOfFirstChar;
        } else if (Block == stateWhereSubstreamBegins.Block && Ptr != null && stateWhereSubstreamBegins.Ptr != null) {
            char* ptr0 = stateWhereSubstreamBegins.Ptr;
            char* end = Ptr;
            if (end < ptr0) throw new ArgumentException("The current position of the stream must not lie before the position corresponding to the given CharStreamState.");
            int length = (int)Buffer.PositiveDistance(ptr0, end);
            string subString = new String(ptr0, 0, length);
            subStream = new CharStream<TSubStreamUserState>(subString);
            var indexOfFirstChar = Buffer.PositiveDistance(BufferBegin, ptr0) + _IndexOfFirstChar;
            subStream.IndexOfFirstCharInBlock = indexOfFirstChar;
            subStream._IndexOfFirstChar = indexOfFirstChar;
        } else {
            var subString = ReadFrom(ref stateWhereSubstreamBegins, false);
            subStream = new CharStream<TSubStreamUserState>(subString);
            var indexOfFirstChar = GetIndex(stateWhereSubstreamBegins.Ptr, stateWhereSubstreamBegins.Block);
            subStream.IndexOfFirstCharInBlock = indexOfFirstChar;
            subStream._IndexOfFirstChar = indexOfFirstChar;
        }
        subStream.StateTag = stateWhereSubstreamBegins.Tag;
        subStream._Line = stateWhereSubstreamBegins.Line;
        subStream._LineBegin = stateWhereSubstreamBegins.LineBegin;
        subStream._Name = stateWhereSubstreamBegins.Name;
    #if DEBUG
        ++SubstreamCount.Value;
        subStream.ParentSubstreamCount = SubstreamCount;
    #endif
        return subStream;
    }
}

}

#endif // !LOW_TRUST
