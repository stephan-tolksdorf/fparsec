// Copyright (c) Stephan Tolksdorf 2007-2009
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


namespace FParsec {
/// <summary>Provides access to the char content of a binary Stream (or a String) through
/// an iterator-based interface that is especially well suited for parser applications.</summary>
public unsafe sealed class CharStream : IDisposable {

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

        public DecoderState DecoderStateAtBlockBegin;
        public DecoderState DecoderStateAfterOverlap;


        public BlockInfo(long byteIndex, int byteBufferIndex,
                         int nBytesInOverlapCount, char lastCharInOverlap,
                         string overhangCharsAtBlockBegin, DecoderState decoderStateAtBlockBegin,
                         string overhangCharsAfterOverlap, DecoderState decoderStateAfterOverlap)
        {
            this.ByteIndex = byteIndex;
            this.ByteBufferIndex = byteBufferIndex;
            this.NumberOfBytesInOverlap = nBytesInOverlapCount;
            this.LastCharInOverlap = lastCharInOverlap;
            this.OverhangCharsAtBlockBegin = overhangCharsAtBlockBegin;
            this.OverhangCharsAfterOverlap = overhangCharsAfterOverlap;
            this.DecoderStateAtBlockBegin = decoderStateAtBlockBegin;
            this.DecoderStateAfterOverlap = decoderStateAfterOverlap;
        }
    }

    // Unfortunately the Decoder API has no explicit methods for managing the state,
    // which forces us to abuse the comparatively inefficient serialization API for this purpose.
    // (The absence of explicit state management or at least a deep cloning method in the Decoder interface
    // is almost as puzzling as the absence of such methods in System.Random).

    private static Dictionary<Type, MemberInfo[]> SerializableMemberInfoCache;

    private static MemberInfo[] GetSerializableDecoderMemberInfo(Decoder decoder) {
        Type type = decoder.GetType();
        if (!type.IsSerializable) return null;
        MemberInfo[] smis;
        if (SerializableMemberInfoCache == null) {
            SerializableMemberInfoCache = new Dictionary<Type,MemberInfo[]>(8);
        }
        lock (SerializableMemberInfoCache) {
            if (!SerializableMemberInfoCache.TryGetValue(type, out smis) ) {
                smis = FormatterServices.GetSerializableMembers(type, new StreamingContext(StreamingContextStates.Clone));
                SerializableMemberInfoCache.Add(type, smis);
            }
        }
        return smis;
    }

    private struct DecoderState {
        private object[] DecoderData;

        public DecoderState(Decoder decoder, MemberInfo[] serializableDecoderMembers) {
            DecoderData = serializableDecoderMembers != null
                          ? FormatterServices.GetObjectData(decoder, serializableDecoderMembers)
                          : null;
        }

        public void WriteTo(ref Decoder decoder, MemberInfo[] serializableDecoderMembers) {
            if (DecoderData != null) {
                //Decoder newDecoder = (Decoder) FormatterServices.GetUninitializedObject(decoder.GetType());
                //FormatterServices.PopulateObjectMembers(newDecoder, serializableDecoderMembers, DecoderData);
                //decoder = newDecoder;
                FormatterServices.PopulateObjectMembers(decoder, serializableDecoderMembers, DecoderData);
            } else {
                decoder.Reset();
            }
        }
    }

    private const int DefaultBlockSize = 3*(1 << 16); // 3*2^16 = 200k
    private const int DefaultByteBufferLength = (1 << 12);
    private static int MinimumByteBufferLength = 128; // must be larger than longest detectable preamble (we can only guess here)
    private const char EOS = '\uFFFF';

    // For ease of use, we need the iterators to hold a reference to the CharStream. If we stored
    // a CharStream reference directly in the iterator, the JIT would emit a call to the write barrier
    // thunk for each write to the reference field. As we want to use iterators mainly as immutable values,
    // we need them to be structs for performance reasons, and since structs are constantly copied
    // by design, we would get frequent write barrier calls*.  Redirecting the CharStream
    // access through an "Anchor" allows us to relieve the GC from having to keep track of all the
    // CharStream references in the iterators. The trick is that an Anchor instance does not contain
    // any reference to a managed object, only a GCHandle to the CharStream and other value type members
    // important to the Iterators. Because the Anchor struct only has primitive members, we can take
    // an unmanaged pointer which the GC doesn't need to track. To avoid most GCHandle.Target accesses,
    // the CharStream stores pieces of information important to the iterators directly in the Anchor.
    //
    // * Just to be clear: Write barrier calls are rather cheap (about the cost of a virtual function
    //   call) and overall FParsec performance is only marginally influenced by this optimization.
    //   (Each Reply<_,_> value alone currently triggers 2-3 write barrier calls, even when it is
    //   allocated on the stack and all fields are initialized to 0/null!).

    internal Anchor* anchor; // allocated and assigned during construction,
                             // freed and set to null during disposal

    /// <summary>Represents the link between a CharStream and its Iterators.
    /// May be allocated on the unmanaged heap and holds a GCHandle, hence must be properly freed.</summary>
    internal struct Anchor {
        public int Block;
        /// <summary>The index of the last block of the stream, or Int32.MaxValue if the end of stream has not yet been detected.</summary>
        public int LastBlock;
        public GCHandle StreamHandle;
        /// <summary>Begin of the used part of the char buffer (stays constant). Is null if the CharStream is empty.</summary>
        public char* BufferBegin;
        /// <summary>End of the used part of the char buffer (varies for a multi-block stream). Is null if the CharStream is empty.</summary>
        public char* BufferEnd;
        public long CharIndex;
        public long CharIndexPlusOffset;
        public long CharIndexOffset;
        public long EndIndex;
        public int BlockSizeMinusOverlap;
        public bool NeedToFree;

        public static Anchor* Create(CharStream stream) {
            // We create the anchor instance on the unmanaged heap. An alternative would be to use a
            // pinned pointer, but that would carry the risk of fragmenting the managed heap
            // (because an Anchor is a small object that can be long-lived).
            // (If AllocHGlobal becomes a bottleneck, we could replace it with a pool allocator.)
            Anchor* p = (Anchor*) Marshal.AllocHGlobal(sizeof(Anchor));
            p->NeedToFree = true;
            p->StreamHandle = GCHandle.Alloc(stream, GCHandleType.Normal);
            return p;
        }

        public static void Free(Anchor *p) {
            p->StreamHandle.Free();
            if (p->NeedToFree) Marshal.FreeHGlobal((IntPtr) p);
        }
    }

    /// <summary>The Encoding that is used for decoding the underlying byte stream, or
    /// System.Text.UnicodeEncoding in case the stream was directly constructed
    /// from a string.</summary>
    public  Encoding Encoding { get; private set; }

    // If the CharStream is constructed from a binary stream, we use a managed string as the char
    // buffer. This allows us to apply regular expressions directly to the input.
    // In the case of multi-block CharStreams we thus have to mutate the buffer string through pointers.
    // This is safe as long as we use a newly constructed string and we don't pass a reference
    // to the internal buffer string to the "outside world". (The one instance where we have to pass
    // a reference to the buffer string is regex matching. See the docs for Iterator.Match(regex) for more info.)
    //
    // Apart from Iter.Match(regex) we access the internal buffer only through a pinned pointer.
    // This way we avoid the overhead of redundant bounds checking and can support strings, char arrays
    // and unmanaged char buffers through the same interface. Accessing the buffer through pointers
    // is also a requirement for accessing the CharStream data through an Anchor pointer (see above).
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
    private StringBuffer Buffer;

    private MultiBlockData Data;

    /// <summary>Contains the data and methods needed in case the input byte stream
    /// is large enough to span multiple blocks of the CharStream.</summary>
    private class MultiBlockData {
        public Anchor* anchor;

        public Stream Stream;
        // we keep a seperate record of the Stream.Position, so that we don't need to require Stream.CanSeek
        public long StreamPosition;
        public bool LeaveOpen;

        public int MaxCharCountForOneByte;
        public Decoder Decoder;
        public MemberInfo[] SerializableDecoderMembers;

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
            while (m != 0) {
                int c = Stream.Read(ByteBuffer, i, m);
                if (c == 0) break;
                i += c;
                m -= c;
            }
            int n = i - byteBufferIndex;
            ByteBufferIndex = byteBufferIndex;
            ByteBufferCount = byteBufferIndex + n;
            StreamPosition += n;
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
                                *(buffer++) = cs[i++];
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

        /// <summary> Reads a block of chars (must be different from the current block)
        /// into the BufferString. Returns a pointer to the first char of the new block,
        /// or null if no chars could be read.</summary>
        internal char* ReadBlock(int block) {
            if (block > anchor->LastBlock) return null;
            int prevBlock = anchor->Block;
            if (block == prevBlock) throw new InvalidOperationException();
            if (SerializableDecoderMembers == null && block > 0) {
                if (prevBlock > block)
                    throw new NotSupportedException("The CharStream does not support seeking backwards over ranges longer than the block overlap because the Encoding's Decoder is not serializable.");
                while (prevBlock + 1 < block) ReadBlock(++prevBlock);
            }

            BlockInfo bi = Blocks[block];
            int blockSizeMinusOverlap = BlockSize - BlockOverlap;
            long charIndex = Math.BigMul(block, blockSizeMinusOverlap);
            char* bufferBegin = anchor->BufferBegin;
            char* begin, buffer;
            int nCharsToRead;

            // fill [0 ... BlockOverlap-1] if block > 0
            if (prevBlock == block - 1) {
                MemMove(bufferBegin, bufferBegin + blockSizeMinusOverlap, BlockOverlap*2);
                Debug.Assert(bufferBegin[BlockOverlap - 1] == bi.LastCharInOverlap);
                begin = buffer = bufferBegin + BlockOverlap;
            } else if (prevBlock >= 0) {
                Stream.Seek(bi.ByteIndex, SeekOrigin.Begin); // will throw if Stream can't seek
                // now that there was no exception, we can change the state...
                StreamPosition = bi.ByteIndex;
                ClearAndRefillByteBuffer(bi.ByteBufferIndex);
                bi.DecoderStateAtBlockBegin.WriteTo(ref Decoder, SerializableDecoderMembers); // will reset Decoder if block == 0
                if (prevBlock == block + 1) {
                    // move the overlap into [BlockSize - BlockOverlap, BlockSize - 1] before it gets overwritten
                    MemMove(bufferBegin + blockSizeMinusOverlap, bufferBegin, BlockOverlap*2);
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
                DecoderState decoderStateAtNextBlockBegin = new DecoderState(Decoder, SerializableDecoderMembers);
                nCharsToRead = BlockOverlap;
                if (overhangCharsAtNextBlockBegin != null) {
                    nCharsToRead -= overhangCharsAtNextBlockBegin.Length;
                    for (int i = 0; i < overhangCharsAtNextBlockBegin.Length; ++i)
                        *(buffer++) = overhangCharsAtNextBlockBegin[i];
                }
                string overhangCharsAfterOverlapWithNextBlock;
                buffer = ReadCharsFromStream(buffer, nCharsToRead, out overhangCharsAfterOverlapWithNextBlock);
                if (anchor->LastBlock == Int32.MaxValue) { // last block hasn't yet been detected
                    if (buffer == bufferBegin + BlockSize) {
                        DecoderState decoderStateAfterOverlapWithNextBlock = new DecoderState(Decoder, SerializableDecoderMembers);
                        int nBytesInOverlapWithNextBlock = (int)(ByteIndex - byteIndexAtNextBlockBegin);
                        Blocks.Add(new BlockInfo(byteIndexAtNextBlockBegin, byteBufferIndexAtNextBlockBegin,
                                                 nBytesInOverlapWithNextBlock, *(buffer - 1),
                                                 overhangCharsAtNextBlockBegin, decoderStateAtNextBlockBegin,
                                                 overhangCharsAfterOverlapWithNextBlock, decoderStateAfterOverlapWithNextBlock));
                    } else { // we reached the end of the stream
                        anchor->LastBlock = block;
                        anchor->EndIndex = anchor->CharIndexOffset + charIndex + (buffer - bufferBegin);
                    }
                } else if (anchor->EndIndex != anchor->CharIndexOffset + charIndex + (buffer - bufferBegin)) {
                    throw new IOException("CharStream: stream integrity error");
                }
            } else {
                BlockInfo nbi = Blocks[block + 1];
                if (buffer != bufferBegin + blockSizeMinusOverlap
                    || byteIndexAtNextBlockBegin != nbi.ByteIndex
                    || byteBufferIndexAtNextBlockBegin != nbi.ByteBufferIndex
                    || overhangCharsAtNextBlockBegin != nbi.OverhangCharsAtBlockBegin)
                    throw new IOException("CharStream: stream integrity error");

                if (prevBlock != block + 1 || (block == 0 && SerializableDecoderMembers == null)) { // jumping back to block 0 is supported even if the decoder is not serializable
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
                    nbi.DecoderStateAfterOverlap.WriteTo(ref Decoder, SerializableDecoderMembers);
                }
            }

            anchor->Block = block;
            anchor->CharIndex = charIndex;
            anchor->CharIndexPlusOffset = anchor->CharIndexOffset + charIndex;
            anchor->BufferEnd = buffer;
            return begin == buffer ? null : begin;
        }
    }

    /// <summary>Reads all remaining chars into the given buffer. If the remaining stream
    /// content holds more than the given maximum number of chars, an exception will be thrown.</summary>
    private static int ReadAllRemainingCharsFromStream(char* buffer, int maxCount, byte[] byteBuffer, int byteBufferIndex, int byteBufferCount, Stream stream, long streamPosition, Decoder decoder) {
        Debug.Assert(maxCount > 0 && byteBufferIndex >= 0 && byteBufferIndex < byteBufferCount);
        fixed (byte* pByteBuffer = byteBuffer) {
            bool flush = false;
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

    /// <summary>The current block in BufferString.</summary>
    private int Block { get { return anchor->Block; } }

    /// <summary>The number of chars in BufferString.</summary>
    private int BufferCount { get { return (int)PositiveDistance(anchor->BufferBegin, anchor->BufferEnd); } }

    /// <summary>The index of the first char in the stream, i.e. Begin.Index.
    /// This value is determined by the streamBeginIndex argument of some of the CharStream constructors.
    /// By default this value is 0.</summary>
    public long BeginIndex { get { return anchor->CharIndexOffset; } }

    /// <summary>The index of the last char of the stream plus 1,
    /// or Int64.MaxValue if the end of stream has not yet been detected.</summary>
    public long EndIndex { get { return anchor->EndIndex; } }

    [Obsolete("CharStream.IndexOffset has been renamed to CharStream.BeginIndex.")]
    public long IndexOffset { get { return BeginIndex; } }

    [Obsolete("CharStream.EndOfStream has been renamed to CharStream.EndIndex.")]
    public long EndOfStream { get { return EndIndex; } }

    // we don't have a public constructor that only takes a string to avoid potential confusion with a filepath constructor
    internal CharStream(string chars) {
        Debug.Assert(chars != null);
        BufferString = chars;
        // ByteBufferIndex = 0; // we recycle ByteBufferIndex for BufferStringIndex
        BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
        char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject();
        BufferStringPointer = bufferBegin;
        CharConstructorContinue(bufferBegin, chars.Length, 0);
    }
    /// <summary>Constructs a CharStream from the chars in the string argument between the indices index (inclusive) and index + length (exclusive).</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0 and index + length ≤ chars.Length.</exception>
    public CharStream(string chars, int index, int length) : this(chars, index, length, 0) {}

    /// <summary>Constructs a CharStream from the chars in the string argument between the indices index (inclusive) and index + length (exclusive). The first char in the stream is assigned the index streamBeginIndex.</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0, index + length ≤ chars.Length and 0 ≤ streamBeginIndex &lt; 2^60.</exception>
    public CharStream(string chars, int index, int length, long streamBeginIndex) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (index < 0) throw new ArgumentOutOfRangeException("index", "The index is negative.");
        if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "The length is out of range.");
        if (streamBeginIndex < 0 || streamBeginIndex >= (1L << 60)) throw new ArgumentOutOfRangeException("streamBeginIndex", "streamBeginIndex must be non-negative and less than 2^60.");

        BufferString = chars;
        BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
        char* pBufferString = (char*)BufferHandle.AddrOfPinnedObject();
        BufferStringPointer = pBufferString;
        CharConstructorContinue(pBufferString + index, length, streamBeginIndex);
    }

    /// <summary>Constructs a CharStream from the chars in the char array argument between the indices index (inclusive) and index + length (exclusive).</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0 and index + length ≤ chars.Length.</exception>
    public CharStream(char[] chars, int index, int length) : this(chars, index, length, 0) { }

    /// <summary>Constructs a CharStream from the chars in the char array argument between the indices index (inclusive) and index + length (exclusive). The first char in the stream is assigned the index streamBeginIndex.</summary>
    /// <exception cref="NullReferenceException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0, index + length ≤ chars.Length and 0 ≤ streamBeginIndex &lt; 2^60.</exception>
    public CharStream(char[] chars, int index, int length, long streamBeginIndex) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (index < 0) throw new ArgumentOutOfRangeException("index", "The index is negative.");
        if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "The length is out of range.");
        if (streamBeginIndex < 0 || streamBeginIndex >= (1L << 60)) throw new ArgumentOutOfRangeException("streamBeginIndex", "streamBeginIndex must be non-negative and less than 2^60.");

        BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
        char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject() + index;
        CharConstructorContinue(bufferBegin, length, streamBeginIndex);
    }

    /// <summary>Constructs a CharStream from the length chars at the pointer address.</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
    public CharStream(char* chars, int length) : this(chars, length, 0) {}

    /// <summary>Constructs a CharStream from the length chars at the pointer address. The first char in the stream is assigned the index streamBeginIndex.</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: length ≥ 0 and 0 ≤ streamBeginIndex &lt; 2^60.</exception>
    public CharStream(char* chars, int length, long streamBeginIndex) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (length < 0) throw new ArgumentOutOfRangeException("length", "The length is negative.");
        if (chars > unchecked(chars + length))
            throw new ArgumentOutOfRangeException("length", "The length is out of range.");
        if (streamBeginIndex < 0 || streamBeginIndex >= (1L << 60)) throw new ArgumentOutOfRangeException("streamBeginIndex", "streamBeginIndex must be non-negative and less than 2^60.");

        CharConstructorContinue(chars, length, streamBeginIndex);
    }

    private void CharConstructorContinue(char* bufferBegin, int length, long streamBeginIndex) {
        Debug.Assert((bufferBegin != null || length == 0) && length >= 0 && bufferBegin <= unchecked(bufferBegin + length) && streamBeginIndex >= 0 && streamBeginIndex < (1L << 60));
        Encoding = Encoding.Unicode;
        var anchor = Anchor.Create(this);
        this.anchor = anchor;
        if (length != 0) {
            anchor->BufferBegin = bufferBegin;
            anchor->BufferEnd = bufferBegin + length;
            anchor->BlockSizeMinusOverlap = length;
        } else {
            anchor->BufferBegin = null; // ensure that BufferBegin is null if length is 0
            anchor->BufferEnd = null;
            anchor->BlockSizeMinusOverlap = 0;
        }
        anchor->Block = 0;
        anchor->LastBlock = 0;
        anchor->CharIndex = 0;
        anchor->CharIndexPlusOffset = streamBeginIndex;
        anchor->CharIndexOffset = streamBeginIndex;
        anchor->EndIndex = streamBeginIndex + length;
    }

    internal CharStream(string chars, char* pChars, char* begin, int length, long streamIndexOffset, Anchor* newUninitializedAnchor) {
        Debug.Assert((chars == null ? pChars == null : pChars <= begin)
                     && (begin != null || length == 0) && length >= 0 && begin <= unchecked(begin + length) && streamIndexOffset >= 0 && streamIndexOffset < (1L << 60));
        Debug.Assert(newUninitializedAnchor->NeedToFree == false && !newUninitializedAnchor->StreamHandle.IsAllocated
                     && newUninitializedAnchor->Block == 0 && newUninitializedAnchor->LastBlock == 0 && newUninitializedAnchor->CharIndex == 0);
        BufferString = chars;
        BufferStringPointer = pChars;
        Encoding = Encoding.Unicode;
        var anchor = newUninitializedAnchor;
        this.anchor = anchor;
        if (length != 0) {
            anchor->BufferBegin = begin;
            anchor->BufferEnd = begin + length;
            anchor->BlockSizeMinusOverlap = length;
        } else {
            anchor->BufferBegin = null; // ensure that BufferBegin is null if length is 0
            anchor->BufferEnd = null;
            anchor->BlockSizeMinusOverlap = 0;
        }
        anchor->CharIndexPlusOffset = streamIndexOffset;
        anchor->CharIndexOffset = streamIndexOffset;
        anchor->EndIndex = streamIndexOffset + length;
        anchor->StreamHandle = GCHandle.Alloc(this, GCHandleType.Normal);
    }

    /// <summary>Constructs a CharStream from the file at the given path.<br/>Is equivalent to CharStream(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan), false, encoding, true, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(string path, Encoding encoding)
           : this(path, encoding, true,
                  DefaultBlockSize, DefaultBlockSize/3, ((DefaultBlockSize/3)*2)/3, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from the file at the given path.<br/>Is equivalent to CharStream(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan), false, encoding, detectEncodingFromByteOrderMarks, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(path, encoding, detectEncodingFromByteOrderMarks,
                  DefaultBlockSize, DefaultBlockSize/3, ((DefaultBlockSize/3)*2)/3, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from the file at the given path.<br/>Is equivalent to CharStream(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan), false, encoding, detectEncodingFromByteOrderMarks, blockSize, blockOverlap, minRegexSpace, byteBufferLength).</summary>
    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks,
                      int blockSize, int blockOverlap, int minRegexSpace, int byteBufferLength)
    {
        if (encoding == null) throw new ArgumentNullException("encoding");
        var stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan);
        try {
           StreamConstructorContinue(stream, false, encoding, detectEncodingFromByteOrderMarks,
                                     blockSize, blockOverlap, minRegexSpace, byteBufferLength);
        } catch {
            stream.Dispose();
            throw;
        }
    }

    /// <summary>Constructs a CharStream from a byte Stream.<br/>Is equivalent to CharStream(stream, false, encoding, true, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(Stream stream, Encoding encoding)
           : this(stream,
                  false, encoding, true,
                  DefaultBlockSize, DefaultBlockSize/3, ((DefaultBlockSize/3)*2)/3, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from a byte Stream.<br/>Is equivalent to CharStream(stream, leaveOpen, encoding, true, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(Stream stream, bool leaveOpen, Encoding encoding)
           : this(stream,
                  leaveOpen, encoding, true,
                  DefaultBlockSize, DefaultBlockSize/3, ((DefaultBlockSize/3)*2)/3, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from a byte Stream.<br/>Is equivalent to CharStream(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(stream,
                  leaveOpen, encoding, detectEncodingFromByteOrderMarks,
                  DefaultBlockSize, DefaultBlockSize/3, ((DefaultBlockSize/3)*2)/3, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from a byte Stream.</summary>
    /// <param name="stream">The byte stream providing the input.</param>
    /// <param name="leaveOpen">Indicates whether the byte Stream should be left open when the CharStream has finished reading it.</param>
    /// <param name="encoding">The (default) Encoding used for decoding the byte Stream into chars.</param>
    /// <param name="detectEncodingFromByteOrderMarks">Indicates whether the constructor should detect the encoding from a unicode byte-order mark at the beginning of the stream. An encoding detected from a byte-order mark overrides the default encoding.</param>
    /// <param name="blockSize">The number of chars per block. The default is 3×2^16 ≈ 200k.</param>
    /// <param name="blockOverlap">The number of chars at the end of a block that are preserved when reading the next block into the char buffer. It must be less than blockSize/2, but not less than encoding.GetMaxCharCount(1). The default is blockSize/3.</param>
    /// <param name="minRegexSpace">The number of chars that are guaranteed to be visible to a regular expression when it is matched on the stream (assuming there are enough chars remaining in the stream). Must not be greater than blockOverlap. The default is 2/3 of blockOverlap.</param>
    /// <param name="byteBufferLength">The size of the byte buffer used for decoding purposes. The default is 2^12 = 4KB.</param>
    public CharStream(Stream stream, bool leaveOpen,
                      Encoding encoding, bool detectEncodingFromByteOrderMarks,
                      int blockSize, int blockOverlap, int minRegexSpace, int byteBufferLength)
    {
        if (stream == null) throw new ArgumentNullException("stream");
        if (!stream.CanRead) throw new ArgumentException("stream is not readable");
        if (encoding == null) throw new ArgumentNullException("encoding");
        StreamConstructorContinue(stream, false, encoding, detectEncodingFromByteOrderMarks,
                                  blockSize, blockOverlap, minRegexSpace, byteBufferLength);
    }

    /// <summary>we modify this flag via reflection in the unit test</summary>
    private static bool DoNotRoundUpBlockSizeToSimplifyTesting = false;

    private void StreamConstructorContinue(Stream stream, bool leaveOpen,
                                           Encoding encoding, bool detectEncodingFromByteOrderMarks,
                                           int blockSize, int blockOverlap, int minRegexSpace, int byteBufferLength)
    {
        if (byteBufferLength < MinimumByteBufferLength) byteBufferLength = MinimumByteBufferLength;

        int bytesInStream = -1;
        long streamPosition;
        if (stream.CanSeek) {
            streamPosition = stream.Position;
            long streamLength = stream.Length - streamPosition;
            if (streamLength <= Int32.MaxValue) {
                bytesInStream = (int)streamLength;
                if (bytesInStream < byteBufferLength) byteBufferLength = bytesInStream;
            }
        } else {
            streamPosition = 0;
        }

        byte[] byteBuffer = new byte[byteBufferLength];
        int byteBufferCount = 0;
        do {
            int n = stream.Read(byteBuffer, byteBufferCount, byteBufferLength - byteBufferCount);
            if (n == 0) {
                bytesInStream = byteBufferCount;
                break;
            }
            byteBufferCount += n;
        } while (byteBufferCount < MinimumByteBufferLength);
        streamPosition += byteBufferCount;

        int preambleLength = Helper.DetectPreamble(byteBuffer, byteBufferCount, ref encoding, detectEncodingFromByteOrderMarks);
        bytesInStream -= preambleLength;

        Encoding = encoding;
        Decoder decoder = encoding.GetDecoder();

        // we allow such small block sizes only to simplify testing
        if (blockSize < 8) blockSize = DefaultBlockSize;

        bool allCharsFitIntoOneBlock = false;
        if (bytesInStream >= 0 && bytesInStream/4 <= blockSize) {
            if (bytesInStream != 0) {
                try {
                    int maxCharCount = Encoding.GetMaxCharCount(bytesInStream); // may throw ArgumentOutOfRangeException
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
        Buffer = buffer;
        BufferString = buffer.String;
        BufferStringPointer = buffer.StringPointer;
        char* bufferBegin = buffer.StringPointer + buffer.Index;
        try {
            if (allCharsFitIntoOneBlock) {
                int bufferCount = preambleLength == byteBufferCount
                                  ? 0
                                  : ReadAllRemainingCharsFromStream(bufferBegin, buffer.Length, byteBuffer, preambleLength, byteBufferCount, stream, streamPosition, decoder);
                if (!leaveOpen) stream.Close();
                var anchor = Anchor.Create(this);
                this.anchor = anchor;
                anchor->BlockSizeMinusOverlap = bufferCount;
                anchor->EndIndex = bufferCount;
                if (bufferCount != 0) {
                    anchor->BufferBegin = bufferBegin;
                    anchor->BufferEnd = bufferBegin + bufferCount;
                } else {
                    anchor->BufferBegin = null;
                    anchor->BufferEnd = null;
                }
                anchor->Block = 0;
                anchor->LastBlock = 0;
                anchor->CharIndex = 0;
                anchor->CharIndexOffset = 0;
                anchor->CharIndexPlusOffset = 0;
            } else {
                if (!DoNotRoundUpBlockSizeToSimplifyTesting) blockSize = buffer.Length;
                var d = new MultiBlockData();
                Data = d;
                d.Stream = stream;
                d.StreamPosition = streamPosition;
                d.LeaveOpen = leaveOpen;
                d.Decoder = decoder;
                d.ByteBuffer = byteBuffer;
                d.ByteBufferIndex = preambleLength;
                d.ByteBufferCount = byteBufferCount;
                d.MaxCharCountForOneByte = Math.Max(1, Encoding.GetMaxCharCount(1));
                d.SerializableDecoderMembers = GetSerializableDecoderMemberInfo(decoder);
                if (blockSize < 3*d.MaxCharCountForOneByte) blockSize = 3*d.MaxCharCountForOneByte;
                // MaxCharCountForOneByte == the maximum number of overhang chars
                if(    Math.Min(blockOverlap, blockSize - 2*blockOverlap) < d.MaxCharCountForOneByte
                    || blockOverlap >= blockSize/2) blockOverlap = blockSize/3;
                if (minRegexSpace < 0 || minRegexSpace > blockOverlap) minRegexSpace = 2*blockOverlap/3;
                d.BlockSize     = blockSize;
                d.BlockOverlap  = blockOverlap;
                d.RegexSpaceThreshold = bufferBegin + (blockSize - minRegexSpace);
                var anchor = Anchor.Create(this);
                this.anchor = anchor;
                d.anchor = anchor;
                anchor->BlockSizeMinusOverlap = blockSize - blockOverlap;
                anchor->EndIndex = Int64.MaxValue;
                anchor->BufferBegin = bufferBegin;
                anchor->BufferEnd = bufferBegin;
                anchor->Block = -2; // special value recognized by ReadBlock
                anchor->LastBlock = Int32.MaxValue;
                anchor->CharIndex = 0;
                anchor->CharIndexOffset = 0;
                anchor->CharIndexPlusOffset = 0;
                d.Blocks = new List<BlockInfo>();
                // the first block has no overlap with a previous block
                d.Blocks.Add(new BlockInfo(preambleLength, preambleLength, 0, EOS, null, new DecoderState(), null, new DecoderState()));
                d.ReadBlock(0);
                if (anchor->BufferBegin == anchor->BufferEnd) {
                    Debug.Assert(anchor->EndIndex == anchor->CharIndexOffset);
                    anchor->BufferBegin = null;
                    anchor->BufferEnd = null;
                }
            }
        } catch {
            buffer.Dispose();
            if (anchor != null) Anchor.Free(anchor);
            throw;
        }
    }

    public void Dispose() {
        if (anchor == null) return;
        Anchor.Free(anchor);
        anchor = null;
        if (BufferHandle.IsAllocated) BufferHandle.Free();
        if (Buffer != null) Buffer.Dispose();
        if (Data != null && !Data.LeaveOpen) Data.Stream.Close();
    }

    /// <summary>an optimized version of end - begin, which assumes that 2^31 > end - begin >= 0. </summary>
    internal static uint PositiveDistance(char* begin, char* end) {
        return (uint)((byte*)end - (byte*)begin)/2;
    }

    internal static long PositiveDistance64(char* begin, char* end) {
        return (long)((ulong)((byte*)end - (byte*)begin)/2);
    }

    /// <summary>An iterator pointing to the beginning of the stream (or to the end if the CharStream is empty).</summary>
    public Iterator Begin { get {
        Anchor* anchor = this.anchor;
        if (anchor == null) throw new ObjectDisposedException("CharStream");
        char* bufferBegin = anchor->BufferBegin;
        if (bufferBegin != null) {
            return new Iterator{Anchor = anchor, Ptr = bufferBegin, Block = 0};
        } else {
            return new Iterator{Anchor = anchor, Ptr = null, Block = -1};
        }
    } }

    // do not directly provide an iterator to the end of the stream in order to
    // ensure that such iterators only exists once the end's position has been detected

    /// <summary>Returns an iterator pointing to the given index in the stream,
    /// or to the end of the stream if the indexed position lies beyond the last char in the stream.</summary>
    /// <exception cref="ArgumentOutOfRangeException">The index is negative or less than the BeginIndex.</exception>
    /// <exception cref="NotSupportedException">Accessing the char with the given index requires seeking in the underlying byte stream, but the byte stream does not support seeking or the Encoding's Decoder is not serializable.</exception>
    /// <exception cref="IOException">An I/O error occured.</exception>
    /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
    /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
    /// <exception cref="OutOfMemoryException">Can not allocate enough memory for the internal data structure.</exception>
    /// <exception cref="ObjectDisposedException">Method is called after the stream was disposed.</exception>
    public Iterator Seek(long index) {
        Anchor* anchor = this.anchor;
        if (anchor == null) throw new ObjectDisposedException("CharStream");
        // safe in case of overflow since CharIndexPlusOffset < 2^60 + (2^31/sizeof(IntPtr))*2^31 < 2^61 and BufferEnd - BufferBegin < 2^30 (where 2^31/sizeof(IntPtr) is the approximate maximum number of blocks)
        long off = unchecked(index - anchor->CharIndexPlusOffset);
        if (0 <= off && off < PositiveDistance(anchor->BufferBegin, anchor->BufferEnd))
            return new Iterator{Anchor = anchor, Ptr = anchor->BufferBegin + (int)off, Block = anchor->Block};
        if (index >= anchor->EndIndex) return new Iterator{Anchor = anchor, Ptr = null, Block = -1};
        if (index < anchor->CharIndexOffset) throw (new ArgumentOutOfRangeException("index", "The index is negative or less than the BeginIndex."));
        // we never get here for streams with only one block
        index -= anchor->CharIndexOffset;
        long idx_;
        long block_ = Math.DivRem(index, anchor->BlockSizeMinusOverlap, out idx_);
        int block = block_ > Int32.MaxValue ? Int32.MaxValue : (int)block_;
        int idx = (int)idx_;
        return Seek(block, idx);
    }
    private Iterator Seek(int block, int idx) {
        Anchor* anchor = this.anchor;
        if (anchor->Block < block && idx < Data.BlockOverlap) {
            --block;
            idx += anchor->BlockSizeMinusOverlap;
        }
        int last = Data.Blocks.Count - 1;
        if (block >= last) {
            int b = last;
            while (Data.ReadBlock(b) != null && b < block) ++b;
            if (block != anchor->Block || idx >= PositiveDistance(anchor->BufferBegin, anchor->BufferEnd))
                return new Iterator{Anchor = anchor, Ptr = null, Block = -1};
        } else Data.ReadBlock(block);
        return new Iterator{Anchor = anchor, Ptr = anchor->BufferBegin + idx, Block = block};
    }

    // On platform where unaligned 4-byte reads are fast (e.g. x86 or x64),
    // defining UNALIGNED_READS will increase the speed of longer
    // reading and matching operations. On other platforms it should not be defined.

    /// <summary>The iterator type for CharStreams.</summary>
    public struct Iterator : IEquatable<Iterator>  {
        internal Anchor* Anchor;
        /// <summary>Pointer to the current char in the CharStream's buffer (if the CharStream's current block equals Block).</summary>
        internal char* Ptr;
        /// <summary>The buffer block for which Ptr is valid.</summary>
        internal int Block;

        /// <summary>The CharStream over which the Iterator iterates.</summary>
        public CharStream Stream { get { return (CharStream)Anchor->StreamHandle.Target; } }

        /// <summary>Indicates whether the Iterator points to the beginning of the CharStream.
        /// If the CharStream is empty, this property is always true.</summary>
        public bool IsBeginOfStream { get {
            return Ptr == Anchor->BufferBegin && Block <= 0;
        } }

        /// <summary>Indicates whether the Iterator points to the end of the CharStream,
        /// i.e. whether it points to one char beyond the last char in the CharStream.</summary>
        public bool IsEndOfStream { get { return Block < 0; } }

        /// <summary>The char returned by Read() if the iterator has
        /// reached the end of the stream. The value is '\uFFFF'.</summary>
        public const char EndOfStreamChar = EOS;

        // Trivial variations in the code, such as introducing a temporary variable
        // or changing the order of expressions, can have a significant effect on the
        // performance of the machine code generated by the current JIT(s) on .NET.
        // When the following function implementations sometimes look a bit inconsistent or
        // verbose, it's mostly because they have been optimized for optimal performance
        // on the x86 JIT (.Net 3.5 SP1).

        /// <summary>The index of the stream char pointed to by the Iterator.</summary>
        public long Index { get {
            Anchor* anchor = Anchor;
            int block = Block;
            if (block == anchor->Block) {
                Debug.Assert(anchor->BufferBegin <= Ptr && Ptr < anchor->BufferEnd);
                if (sizeof(System.IntPtr) != 8) // the JIT removes the inactive branch
                    return PositiveDistance(anchor->BufferBegin, Ptr) + anchor->CharIndexPlusOffset;
                else
                    return PositiveDistance64(anchor->BufferBegin, Ptr) + anchor->CharIndexPlusOffset;
            } else if (block < 0) {
                Debug.Assert(block == -1 && Ptr == null);
                // this is safe, as there can only be an end-of-stream iterator
                // once the end of stream has been detected
                return anchor->EndIndex;
            } else {
                Debug.Assert(anchor->BufferBegin <= Ptr && (Ptr < anchor->BufferEnd || anchor->Block == anchor->LastBlock));
                long charIndexPlusOffset = anchor->CharIndexOffset + Math.BigMul(block, anchor->BlockSizeMinusOverlap);
                if (sizeof(System.IntPtr) != 8)
                    return PositiveDistance(anchor->BufferBegin, Ptr) + charIndexPlusOffset;
                else
                    return PositiveDistance64(anchor->BufferBegin, Ptr) + charIndexPlusOffset;
            }
        } }

        /// <summary>Returns an Iterator pointing to the next char in the stream. If the Iterator already
        /// has reached the end of the stream, i.e. if it points to one char beyond
        /// the last char, the same Iterator is returned.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public Iterator Next { get {
            Anchor* anchor = Anchor;
            char* newPtr = Ptr + 1;
            if (Block == anchor->Block && newPtr < anchor->BufferEnd)
                return new Iterator{Anchor = anchor, Ptr = newPtr, Block = Block};
            return NextContinue();
        } }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private Iterator NextContinue() { return AdvanceContinue(1u); }

        /// <summary>Returns an Iterator that is advanced by offset chars. The Iterator can't
        /// move past the end of the stream, i.e. any position beyond the last char
        /// in the stream is interpreted as precisely one char beyond the last char.</summary>
        /// <exception cref="ArgumentOutOfRangeException">The new position would lie before the beginning of the `CharStream`.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public Iterator Advance(int offset) {
            if (offset >= 0) {
                Anchor* anchor = Anchor;
                if (Block == anchor->Block && offset < (int)PositiveDistance(Ptr, anchor->BufferEnd))
                    return new Iterator{Anchor = Anchor, Ptr = Ptr + offset, Block = Block};
                return AdvanceContinue((uint)offset);
            } else {
                if (Block >= 0 && unchecked((uint)-offset) <= PositiveDistance(Anchor->BufferBegin, Ptr))
                    return new Iterator{Anchor = Anchor, Ptr = unchecked(Ptr + offset), Block = Block};
                return AdvanceContinue(offset);
            }
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        internal Iterator AdvanceContinue(int offset) {
            return Stream.Seek(Index + offset);
        }

        /// <summary>Returns an Iterator that is advanced by offset chars. The Iterator can't
        /// move past the end of the stream, i.e. any position beyond the last char
        /// in the stream is interpreted as precisely one char beyond the last char.</summary>
        /// <exception cref="ArgumentOutOfRangeException">The new position would lie before the beginning of the `CharStream`.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        /// <exception cref="OutOfMemoryException">Can not allocate enough memory for the internal data structure.</exception>
        public Iterator Advance(long offset) {
            if (Block == Anchor->Block
                && (offset >= 0 ? offset <   PositiveDistance(Ptr, Anchor->BufferEnd)
                                : offset >= -(int)PositiveDistance(Anchor->BufferBegin, Ptr)))
            {
                int nn = (int)offset;
                char* newPtr = unchecked(Ptr + nn); // we need unchecked here because C# always uses
                                                    // unsigned arithmetic for pointer calculations and
                                                    // otherwise would report an overflow for any negative numberOfChars
                                                    // if overflow checking is activated
                return new Iterator{Anchor = Anchor, Ptr = newPtr, Block = Block};
            }
            long index = Index;
            return Stream.Seek(offset > long.MaxValue - index ? long.MaxValue : index + offset);
        }

        /// <summary>Returns an Iterator that is advanced by offset chars. The Iterator can't
        /// move past the end of the stream, i.e. any position beyond the last char
        /// in the stream is interpreted as precisely one char beyond the last char.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public Iterator Advance(uint offset) {
            Anchor* anchor = Anchor;
            if (Block == anchor->Block && offset < PositiveDistance(Ptr, anchor->BufferEnd))
                return new Iterator{Anchor = Anchor, Ptr = Ptr + offset, Block = Block};
            return AdvanceContinue(offset);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private Iterator AdvanceContinue(uint offset) {
            Debug.Assert(offset != 0 || Block != Anchor->Block);
            if (Anchor->LastBlock == 0 || Block < 0 || (Block == Anchor->LastBlock && Block == Anchor->Block))
                return new Iterator{Anchor = Anchor, Ptr = null, Block = -1};
            return Stream.Seek(Index + offset);
        }

        // The following methods with a leading underscore don't belong into the
        // "offical" API, because they mutate the Iterator struct, which otherwise
        // is expected to be immutable. However, users who know what they're doing
        // can use them to implement very efficent parser loops, which is why these
        // methods are declared public.

        /// <summary>Advances the Iterator *in-place* by 1 char and returns the char on the new position.
        ///`c &lt;- iter._Increment()` is equivalent to `iter &lt;- iter.Next; c &lt;- iter.Read()`.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public char _Increment() {
            Anchor* anchor = Anchor;
            char* newPtr = Ptr + 1;
            if (Block == anchor->Block && newPtr < anchor->BufferEnd) {
                Ptr = newPtr;
                return *newPtr;
            }
            return IncrementContinue();
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char IncrementContinue() { return IncrementContinue(1u); }

        /// <summary>Advances the Iterator *in-place* by offset chars and returns the char on the new position.
        /// `c &lt;- iter._Increment(offset)` is an optimized implementation of `iter &lt;- iter.Advance(offset); c &lt;- iter.Read()`.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public char _Increment(uint offset) {
            Anchor* anchor = Anchor;
            char* ptr = Ptr;
            if (Block == anchor->Block && offset < PositiveDistance(ptr, anchor->BufferEnd)) {
                char* newPtr = ptr + offset;
                Ptr = newPtr;
                return *newPtr;
            }
            return IncrementContinue(offset);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        internal char IncrementContinue(uint offset) {
            Debug.Assert(offset != 0 || Block != Anchor->Block);
            if (Anchor->LastBlock == 0 || Block < 0 || (Block == Anchor->LastBlock && Block == Anchor->Block)) {
                Ptr   = null;
                Block = -1;
                return EOS;
            }
            this = Stream.Seek(Index + offset);
            return Read();
        }

        /// <summary>Advances the Iterator *in-place* by -1 char and returns the char on the new position,
        /// except if the Iterator already points to the beginning of the CharStream,
        /// in which case the position does not change and the EndOfStreamChar ('\uFFFF') is returned.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        public char _Decrement() {
            Anchor* anchor = Anchor;
            char* newPtr = unchecked(Ptr - 1);
            if (Block == anchor->Block && newPtr >= anchor->BufferBegin) {
                Ptr = newPtr;
                return *newPtr;
            }
            return DecrementContinue();
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char DecrementContinue() { return DecrementContinue(1u); }

        /// <summary>Advances the Iterator *in-place* by -offset chars and returns the char on the new position,
        /// except if the new position would lie before the beginning of the CharStream,
        /// in which case the Iterator is advanced to the beginning of the stream and the EndOfStreamChar ('\uFFFF') is returned.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        public char _Decrement(uint offset) {
            Anchor* anchor = Anchor;
            if (Block == anchor->Block && offset <= PositiveDistance(anchor->BufferBegin, Ptr)) {
                char* newPtr = Ptr - offset;
                Ptr = newPtr;
                return *newPtr;
            }
            return DecrementContinue(offset);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char DecrementContinue(uint offset) {
            Debug.Assert(offset != 0 || Block != Anchor->Block);
            if (Block == 0 && Anchor->Block == 0) {
                Ptr = Anchor->BufferBegin;
                return EOS;
            }
            long newIndex = Index - offset;
            if (newIndex >= Anchor->CharIndexOffset) {
                this = Stream.Seek(newIndex);
                return Read();
            } else {
                this = Stream.Begin;
                return EOS;
            }
        }

        /// <summary>A helper routine for optimizing State methods</summary>
        internal void _AdvanceInPlace(int offset) { // uses the same logic as Peek(int)
            char* newPtr = unchecked(Ptr + offset);
            if (offset >= 0) {
                if (newPtr >= Ptr && newPtr <  Anchor->BufferEnd   && Anchor->Block == Block)
                    Ptr = newPtr;
                else
                    IncrementContinue((uint)offset);
            } else {
                // we must exclude ptrOff == Ptr here, because Ptr + Int32.MinValue == Ptr
                if (newPtr <  Ptr && newPtr >= Anchor->BufferBegin && Anchor->Block == Block)
                    Ptr = newPtr;
                else
                    this = AdvanceContinue(offset);
            }
        }

        /// <summary>Is an optimized implementation of Next.Read().</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public char Peek() {
            Anchor* anchor = Anchor;
            char* ptr1 = Ptr + 1;
            if (Block == anchor->Block && ptr1 < anchor->BufferEnd) return *ptr1;
            return PeekContinue();
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char PeekContinue() { return PeekContinue(1u); }

        /// <summary>Is an optimized implementation of Advance(offset).Read(),
        /// except that the EndOfStreamChar ('\uFFFF') is returned if Index + offset &lt; 0 (instead of an exception being thrown).</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public char Peek(int offset) {
            char* ptrOff = unchecked(Ptr + offset);
            if (offset < 0) {
                // we must exclude ptrOff == Ptr here, because Ptr + Int32.MinValue == Ptr
                if (ptrOff <  Ptr && ptrOff >= Anchor->BufferBegin && Anchor->Block == Block) return *ptrOff;
                return PeekContinue(offset);
            } else {
                if (ptrOff >= Ptr && ptrOff <  Anchor->BufferEnd   && Anchor->Block == Block) return *ptrOff;
                return PeekContinue((uint)offset);
            }
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char PeekContinue(int offset) {
            Debug.Assert(offset < 0);
            if (Block == 0 && Anchor->Block == 0) return EOS;
            long newIndex = Index + offset;
            return newIndex < Anchor->CharIndexOffset ? EOS : Stream.Seek(newIndex).Read();
        }

        /// <summary>Is an optimized implementation of Advance(offset).Read().</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public char Peek(uint offset) {
            Anchor* anchor = Anchor;
            char* ptr = Ptr;
            if (Block == anchor->Block && offset < PositiveDistance(ptr, anchor->BufferEnd))
                return ptr[offset];
            return PeekContinue(offset);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char PeekContinue(uint offset) {
            Debug.Assert(offset != 0 || Block != Anchor->Block);
            if (Anchor->LastBlock == 0 || Block < 0 || (Block == Anchor->LastBlock && Block == Anchor->Block)) return EOS;
            return Stream.Seek(Index + offset).Read();
        }

        /// <summary>Returns true if and only if the char argument matches the char pointed to by the Iterator.
        /// At the end of the stream Match always returns false.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occurs.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool Match(char ch) {
            if (Block == Anchor->Block) return *Ptr == ch;
            return MatchContinue(ch);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private bool MatchContinue(char ch) {
            if (Block < 0) return false;
            Stream.Data.ReadBlock(Block);
            return *Ptr == ch;
        }

        /// <summary>Returns true if chars matches the chars in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If chars is empty, true is returned.</summary>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool Match(string chars) {
            Anchor* anchor = Anchor;
            if (Block == anchor->Block && (uint)chars.Length <= PositiveDistance(Ptr, anchor->BufferEnd)) {
                for (int i = 0; i < chars.Length; ++i)
                    if (Ptr[i] != chars[i]) goto ReturnFalse;
                return true;
            ReturnFalse:
                return false;
            }
            return MatchContinue(chars);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private bool MatchContinue(string chars) {
            int length = chars.Length; // throws if chars is null
            if (Anchor->LastBlock == 0 || Block < 0 || (Block == Anchor->LastBlock && Block == Anchor->Block))
                return length == 0;
            fixed (char* pChars = chars) return MatchContinue(pChars, length);
        }

        /// <summary>Returns true if caseFoldedChars matches the chars in the stream
        /// beginning with the char pointed to by the Iterator.
        /// The chars in the stream are case-folded before they are matched,
        /// while the chars in the string argument are assumed to already be case-folded.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If caseFoldedChars is empty, true is returned.</summary>
        /// <exception cref="NullReferenceException">caseFoldedChars is null.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool MatchCaseFolded(string caseFoldedChars) {
            Anchor* anchor = Anchor;
            if (Block == anchor->Block && (uint)caseFoldedChars.Length <= PositiveDistance(Ptr, anchor->BufferEnd)) {
                for (int i = 0; i < caseFoldedChars.Length; ++i)
                    if (CaseFoldTable.FoldedChars[Ptr[i]] != caseFoldedChars[i]) goto ReturnFalse;
                return true;
            ReturnFalse:
                return false;
            }
            return MatchCaseFoldedContinue(caseFoldedChars);
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private bool MatchCaseFoldedContinue(string caseFoldedChars) {
            int length = caseFoldedChars.Length; // throws if chars is null
            if (Anchor->LastBlock == 0 || Block < 0 || (Block == Anchor->LastBlock && Block == Anchor->Block))
                return length == 0;
            fixed (char* pCaseFoldedCharsChars = caseFoldedChars)
            return MatchCaseFoldedContinue(pCaseFoldedCharsChars, length);
        }

        /// <summary>Returns true if the chars in chars between the indices charsIndex (inclusive) and
        /// charsIndex + length (exclusive) match the chars in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If length is 0, true is returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">charsIndex is negative, length is negative or charsIndex + length > chars.Length.</exception>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool Match(string chars, int charsIndex, int length) {
            if (charsIndex < 0)
                throw new ArgumentOutOfRangeException("charsIndex", "charsIndex is negative.");
            if (length > chars.Length - charsIndex) // throws if chars is null
                throw new ArgumentOutOfRangeException("length", "Length is out of range.");
            fixed (char* pChars = chars) return Match(pChars + charsIndex, length); // checks length >= 0
        }

        /// <summary>Returns true if the chars in the char array between the indices charsIndex (inclusive) and
        /// charsIndex + length (exclusive) match the chars in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If length is 0, true is returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">charsIndex is negative, length is negative or charsIndex + length > chars.Length.</exception>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool Match(char[] chars, int charsIndex, int length) {
            if (charsIndex < 0)
                throw new ArgumentOutOfRangeException("charsIndex", "charsIndex is negative.");
            if (length > chars.Length - charsIndex) // throws if chars is null
                throw new ArgumentOutOfRangeException("length", "Length is out of range.");
            fixed (char* pChars = chars) return Match(pChars + charsIndex, length); // checks length >= 0
        }

        /// <summary>Returns true if the length chars at the pointer address match the chars
        /// in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream,
        /// false is returned. If length is 0, true is returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool Match(char* chars, int length) {
            Anchor* anchor = Anchor;      // the unsigned comparison will correctly handle negative length values
            if (Block == anchor->Block && unchecked((uint)length <= PositiveDistance(Ptr, anchor->BufferEnd))) {
                #if UNALIGNED_READS
                    int len = length & 0x7ffffffe;
                    for (int i = 0; i < len; i += 2) {
                        if (*((int*)(Ptr + i)) != *((int*)(chars + i))) goto ReturnFalse;
                    }
                    if (len != length) {
                        if (Ptr[len] != chars[len]) goto ReturnFalse;
                    }
                    return true;
                #else
                    for (int i = 0; i < length; ++i) {
                        if (Ptr[i] != chars[i]) goto ReturnFalse;
                    }
                    return true;
                #endif
            ReturnFalse:
                    return false;
            }
            return MatchContinue(chars, length);
        }

        /// <summary>Returns true if the length chars at the pointer address match the chars
        /// in the stream beginning with the char pointed to by the Iterator.
        /// The chars in the stream are case-folded before they are matched,
        /// while the chars at the pointer address are assumed to already be case-folded.
        /// If the chars do not match or if there are not enough chars remaining in the stream,
        /// false is returned. If length is 0, true is returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public bool MatchCaseFolded(char* caseFoldedChars, int length) {
            if (Block == Anchor->Block && unchecked((uint)length) <= PositiveDistance(Ptr, Anchor->BufferEnd)) {
                for (int i = 0; i < length; ++i) {
                    if (CaseFoldTable.FoldedChars[Ptr[i]] != caseFoldedChars[i]) goto ReturnFalse;
                }
                return true;
            ReturnFalse:
                return false;
            }
            return MatchCaseFoldedContinue(caseFoldedChars, length);
        }

        private bool MatchContinue(char* chars, int length) {
            if (length <= 0) {
                if (length == 0) return true;
                throw new ArgumentOutOfRangeException("length", "Length is negative.");
            }

            int block = Block; // local copy that might be modified below
            if (block < 0 || Anchor->LastBlock == 0 || (block == Anchor->LastBlock && block == Anchor->Block)) return false;

            CharStream stream = null;
            if (block != Anchor->Block) {
                stream = Stream;
                stream.Data.ReadBlock(block);
            }

            char* ptr = Ptr;

            // requires length > 0
            for (;;) {
                int len = Math.Min((int)PositiveDistance(ptr, Anchor->BufferEnd), length);
                length -= len;

                #if UNALIGNED_READS
                    while (len >= 2) {
                        if (*((int*)ptr) != *((int*)chars)) goto ReturnFalse;
                        ptr += 2; chars += 2; len -= 2;
                    }
                    if (len != 0) {
                        if (*ptr != *chars) goto ReturnFalse;
                        ++chars;
                    }
                #else
                    do {
                        if (*ptr != *chars) goto ReturnFalse;
                        ++ptr; ++chars; --len;
                    } while (len != 0);
                #endif

                if (length == 0) return true;
                else {
                    if (stream == null) stream = Stream;
                    ptr = stream.Data.ReadBlock(++block);
                    if (ptr == null) return false;
                }
            }
        ReturnFalse:
            return false;
        }

        private bool MatchCaseFoldedContinue(char* caseFoldedChars, int length) {
            if (length <= 0) {
                if (length == 0) return true;
                throw new ArgumentOutOfRangeException("length", "Length is negative.");
            }

            int block = Block; // local copy that might be modified below
            if (block < 0 || Anchor->LastBlock == 0 || (block == Anchor->LastBlock && block == Anchor->Block)) return false;

            CharStream stream = null;
            if (block != Anchor->Block) {
                stream = Stream;
                stream.Data.ReadBlock(block);
            }

            char* ptr = Ptr;

            char* cftable = CaseFoldTable.FoldedChars;

            // requires length > 0
            for (;;) {
                int len = Math.Min((int)PositiveDistance(ptr, Anchor->BufferEnd), length);
                length -= len;

                 do {
                    if (cftable[*ptr] != *caseFoldedChars) goto ReturnFalse;
                    ++ptr; ++caseFoldedChars; --len;
                } while (len != 0);

                if (length == 0) return true;
                else {
                    if (stream == null) stream = Stream;
                    ptr = stream.Data.ReadBlock(++block);
                    if (ptr == null) return false;
                }
            }
        ReturnFalse:
            return false;
        }

        /// <summary>Applies the given regular expression to stream chars beginning with the char pointed to by the Iterator.
        /// Returns the resulting Match object. (Not supported by CharStreams constructed from char arrays or pointers.)</summary>
        /// <remarks><para>For performance reasons you should specify the regular expression
        /// such that it can only match at the beginning of a string,
        /// for example by prepending "\A".</para>
        /// <para>For CharStreams constructed from large binary streams the regular expression is not applied
        /// to a string containing all the remaining chars in the stream. The minRegexSpace parameter
        /// of the CharStream constructors determines the minimum number of chars that are guaranteed
        /// to be visible to the regular expression.</para>
        /// <para>
        /// IMPORTANT:<br/>
        /// If the CharStream has been constructed from a System.IO.Stream or a file path, the regular expression is
        /// applied to an internal mutable buffer. Since the Match object may work lazily, i.e. compute matched strings
        /// not before they are needed, you need to retrieve all the required information from the Match object before
        /// you continue to access the CharStream, otherwise you might get invalid results.</para>
        /// </remarks>
        /// <exception cref="NullReferenceException">regex is null.</exception>
        /// <exception cref="NotSupportedException">Two possible reasons: 1) The CharStream was constructed from a char array or char pointer, in which case it does not support regular expression matching.
        /// 2) Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public Match Match(Regex regex) {
            CharStream stream = Stream;
            if (stream.BufferString == null) throw new NotSupportedException("CharStream instances constructed from char arrays or char pointers do not support regular expression matching.");
            int block = Block;
            if (block >= 0) {
                var data = stream.Data;
                if (data != null) {
                    if (Ptr <= data.RegexSpaceThreshold || block == Anchor->LastBlock) {
                        if (block != Anchor->Block) data.ReadBlock(block);
                    } else {
                        // BlockOverlap > MinRegexSpace
                        if (block + 1 == Anchor->Block || data.ReadBlock(block + 1) != null) {
                            // the char pointed to by the iterator has moved to beginning of the buffer
                            Block = block + 1;
                            Ptr -= Anchor->BlockSizeMinusOverlap;
                            Debug.Assert(Anchor->BufferBegin <= Ptr && Ptr < Anchor->BufferEnd);
                        } else {
                            // block < LastBlock and we failed to read new chars from block + 1,
                            // so we now definitely need to read the current block
                            data.ReadBlock(block);
                        }
                    }
                }
                int index  = (int)PositiveDistance(stream.BufferStringPointer, Ptr);
                int length = (int)PositiveDistance(Ptr, Anchor->BufferEnd);
                return regex.Match(stream.BufferString, index, length);
            }
            return regex.Match("");
        }

        /// <summary>Returns the stream char pointed to by the Iterator,
        /// or the EndOfStreamChar ('\uFFFF') if the Iterator has reached the end of the stream.</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        public char Read() {
            if (Block == Anchor->Block) return *Ptr;
            return ReadContinue();
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private char ReadContinue() {
            if (Block < 0) return EOS;
            Stream.Data.ReadBlock(Block);
            return *Ptr;
        }

        public struct TwoChars : IEquatable<TwoChars> {
            private uint chars;

            internal TwoChars(uint chars) {
                this.chars = chars;
            }
            public TwoChars(char char0, char char1) {
                this.chars = ((uint)char1 << 16) | (uint)char0;
            }

            public char Char0 { get { return unchecked((char)chars); } }
            public char Char1 { get { return (char)(chars >> 16); } }

            public override bool Equals(object obj) { return (obj is TwoChars) && chars == ((TwoChars) obj).chars; }
            public bool Equals(TwoChars other) { return chars == other.chars; }
            public override int GetHashCode()  { return (int)chars; }
            public static bool operator==(TwoChars left, TwoChars right) { return left.chars == right.chars; }
            public static bool operator!=(TwoChars left, TwoChars right) { return left.chars != right.chars; }
        }

        /// <summary>Is an optimized implementation of new TwoChars(Read(), Next.Read()).</summary>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public TwoChars Read2() {
            Anchor* anchor = Anchor;
            char* ptr = Ptr;
            if (Block == anchor->Block && ptr + 1 < anchor->BufferEnd) {
                #if UNALIGNED_READS
                    if (BitConverter.IsLittleEndian) {
                        return new TwoChars(*((uint*)(ptr)));
                    } else {
                        return new TwoChars(ptr[0], ptr[1]);
                    }
                #else
                    return new TwoChars(ptr[0], ptr[1]);
                #endif
            }
            return Read2Continue();
        }
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private TwoChars Read2Continue() {
            if (Block < 0)
                return new TwoChars(EOS, EOS);
            else
                return new TwoChars(Read(), Peek());
        }

        /// <summary>Returns a string with the length stream chars beginning with the char pointed to by the Iterator.
        /// If less than length chars are remaining in the stream, only the remaining chars are returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="OutOfMemoryException">There is not enough memory for the string or the requested string is too large.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public string Read(int length) {
            Anchor* anchor = Anchor;
            if (Block == anchor->Block && unchecked((uint)length) <= PositiveDistance(Ptr, anchor->BufferEnd))
                return new String(Ptr, 0, length);
            return ReadContinue(length, false);
        }

        /// <summary>Returns a string with the length stream chars beginning with the char pointed to by the Iterator.
        /// If less than length chars are remaining in the stream,
        /// only the remaining chars are returned, or an empty string if allOrEmpty is true.</summary>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="OutOfMemoryException">There is not enough memory for the string or the requested string is too large.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public string Read(int length, bool allOrEmpty) {
            Anchor* anchor = Anchor;
            if (Block == anchor->Block && unchecked((uint)length) <= PositiveDistance(Ptr, anchor->BufferEnd))
                return new String(Ptr, 0, length);
            return ReadContinue(length, allOrEmpty);
        }
        private string ReadContinue(int length, bool allOrEmpty) {
            if (length < 0) throw new ArgumentOutOfRangeException("length", "Length is negative.");
            if (length == 0 || Block < 0) return "";
            if (Anchor->LastBlock != Int32.MaxValue) {
                long maxLength = Anchor->EndIndex - Index;
                if (length > maxLength) {
                    if (allOrEmpty) return "";
                    length = (int)maxLength;
                }
            }
            string str = new String('\u0000', length);
            fixed (char* pStr = str) {
                int cc = Read(pStr, length);
                if (cc == length) return str;
                if (allOrEmpty) return "";
                return new String(pStr, 0, cc);
            }
        }


        /// <summary>Copies the length stream chars beginning with the char pointed to by the Iterator into buffer.
        /// The chars are written into buffer beginning at the index bufferIndex.
        /// If less than length chars are remaining in the stream, only the remaining chars are copied.
        /// Returns the actual number of chars copied.</summary>
        /// <exception cref="ArgumentOutOfRangeException">bufferIndex is negative, length is negative or bufferIndex + length > buffer.Length.</exception>
        /// <exception cref="NullReferenceException">buffer is null.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public int Read(char[] buffer, int bufferIndex, int length) {
            if (bufferIndex < 0)
                throw new ArgumentOutOfRangeException("bufferIndex", "bufferIndex is negative.");
            if (length > buffer.Length - bufferIndex)
                throw new ArgumentOutOfRangeException("length", "Length is out of range.");

            fixed (char* pBuffer = buffer)
            return Read(pBuffer + bufferIndex, length); // will check length >= 0
        }

        /// <summary>Copies the length stream chars beginning with the char pointed to by the Iterator into the buffer at the given pointer address.
        /// If less than length chars are remaining in the stream, only the remaining chars are copied.
        /// Returns the actual number of chars copied.</summary>
        /// <exception cref="NullReferenceException">buffer is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public int Read(char* buffer, int length) {
            Anchor* anchor = Anchor;
            char* ptr = Ptr;
            if (Block == anchor->Block && unchecked((uint)length) <= PositiveDistance(ptr, anchor->BufferEnd)) {
                #if UNALIGNED_READS
                    int len = length;
                    if ((unchecked((int)buffer) & 2) != 0) { // align buffer pointer
                        *buffer = *ptr;
                        ++buffer; ++ptr; --len;
                    }
                    while (len >= 8) {
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
                    if ((len & 1) != 0) {
                        *buffer = *ptr;
                    }
                #else
                    int len = length & 0x7ffffffe;
                    for (int i = 0; i < len; i += 2) {
                        buffer[i]     = ptr[i];
                        buffer[i + 1] = ptr[i + 1];
                    }
                    if (len != length) {
                        buffer[len] = ptr[len];
                    }
                #endif
                return length;
            }
            return ReadContinue(buffer, length);
        }
        private int ReadContinue(char* buffer, int length) {
            if (length <= 0) {
                if (length == 0) return 0;
                throw new ArgumentOutOfRangeException("length", "Length is negative.");
            }

            int block = Block; // local copy that might be modified below
            if (block < 0) return 0;

            CharStream stream = null;
            if (block != Anchor->Block) {
                stream = Stream;
                stream.Data.ReadBlock(block);
            }

            char* ptr = Ptr;
            int oldLength = length;

            // requires length > 0
            for (;;) {
                int len = Math.Min((int)PositiveDistance(ptr, Anchor->BufferEnd), length);
                length -= len;

                #if UNALIGNED_READS
                    if ((unchecked((int)buffer) & 2) != 0) { // align buffer pointer
                        *buffer = *ptr;
                        ++buffer; ++ptr; --len;
                    }
                    while (len >= 8) {
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
                    if ((len & 1) != 0) {
                        *buffer = *ptr;
                        ++buffer;
                    }
                #else
                    do {
                        *buffer = *ptr;
                        ++buffer; ++ptr; --len;
                    } while (len != 0);
                #endif

                if (length == 0) return oldLength;
                else {
                    if (stream == null) {
                        if (Anchor->LastBlock == 0) return oldLength - length;
                        stream = Stream;
                    }
                    ptr = stream.Data.ReadBlock(++block);
                    if (ptr == null) return oldLength - length;
                }
            }
        }

        /// <summary>Returns a string with all the chars in the stream between the position of this Iterator (inclusive)
        /// and the position of the Iterator in the argument (exclusive).
        /// If the Iterator argument does not point to a position after the position of this Iterator, the returned string is empty.</summary>
        /// <exception cref="ArgumentOutOfRangeException">iterToCharAfterLastInString belongs to a different CharStream.</exception>
        /// <exception cref="OutOfMemoryException">There is not enough memory for the string or the requested string is too large.</exception>
        /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
        /// <exception cref="IOException">An I/O error occured.</exception>
        /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
        /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
        public string ReadUntil(Iterator iterToCharAfterLastInString) {
            if (Anchor != iterToCharAfterLastInString.Anchor)
                throw new ArgumentOutOfRangeException("iterToCharAfterLastInString", "The Iterator argument belongs to a different CharStream.");
            int block = Block;
            if (block == Anchor->Block && block == iterToCharAfterLastInString.Block) {
                char* ptr = Ptr;
                char* end = iterToCharAfterLastInString.Ptr;
                if (ptr < end)
                    return new String(ptr, 0, (int)PositiveDistance(ptr, end));
                return "";
            }
            return ReadUntilContinue(iterToCharAfterLastInString);
        }
        private string ReadUntilContinue(Iterator iterToCharAfterLastInString) {
            ulong index1 = (ulong)Index;
            ulong index2 = (ulong)iterToCharAfterLastInString.Index;
            if (index2 <= index1) return "";
            ulong length_ = index2 - index1;
            // length >= Int32.MaxValue will trigger an exception anyway (because the string is too large)
            int length = length_ > (uint)System.Int32.MaxValue ? System.Int32.MaxValue : (int)length_;
            string str = new String('\u0000', length);
            fixed (char* pStr = str) ReadContinue(pStr, length);
            return str;
        }

        public override bool Equals(object obj) {
            return (obj is Iterator) && Equals((Iterator) obj);
        }

        public bool Equals(Iterator other) {
            char* ptr = Ptr;
            return    (ptr == other.Ptr && ptr != null && Block == other.Block)
                   || (Anchor == other.Anchor && Index == other.Index);
        }

        public override int GetHashCode() {
            return Index.GetHashCode();
        }

        public static bool operator==(Iterator left, Iterator right) { return  left.Equals(right); }
        public static bool operator!=(Iterator left, Iterator right) { return !left.Equals(right); }
    }

    /// <summary>Returns a case-folded copy of the string argument. All chars are mapped
    /// using the (non-Turkic) 1-to-1 case folding mappings (v. 5.1) for Unicode code
    /// points in the Basic Multilingual Plane, i.e. code points below 0x10000.
    /// If the argument is null, null is returned.</summary>
    static public string FoldCase(string str) {
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


    /// <summary>Returns the given string with all occurrences of "\r\n" and "\r" replaced
    /// by "\n". If the argument is null, null is returned.</summary>
    static public string NormalizeNewlines(string str) {
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
    static internal string CopyWithNormalizedNewlines(char* src, int length, int nCRLF, int nCR) {
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
                    uint len = PositiveDistance(src, end);
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


    // probably for pedagogical reasons there is no Buffer.BlockCopy that takes pointers,
    // hence we are forced to write our own version
    static private void MemMove(void* dst_, void* src_, int n) {
        byte* dst = (byte*)dst_;
        byte* src = (byte*)src_;
        if (n <= 0) return;
        // we assume the pointers are aligned
        if (dst < src) {
            while (n >= 16) {
                ((int*)dst)[0] = ((int*)src)[0];
                ((int*)dst)[1] = ((int*)src)[1];
                ((int*)dst)[2] = ((int*)src)[2];
                ((int*)dst)[3] = ((int*)src)[3];
                src += 16; dst += 16; n -= 16;
            }
            if ((n != 0)) {
                if ((n & 8) != 0) {
                    ((int*)dst)[0] = ((int*)src)[0];
                    ((int*)dst)[1] = ((int*)src)[1];
                    src += 8; dst += 8; n -= 8;
                }
                if ((n & 4) != 0) {
                    ((int*)dst)[0] = ((int*)src)[0];
                    src += 4; dst += 4; n -= 4;
                }
                if ((n & 2) != 0) {
                    ((short*)dst)[0] = ((short*)src)[0];
                    src += 2; dst += 2; n -= 2;
                }
                if ((n & 1) != 0) {
                    ((byte*)dst)[0] = ((byte*)src)[0];
                }
            }
        } else {
            src += n; dst += n;
            if ((n & 0xf) != 0) {
                if ((n & 1) != 0) {
                    src -= 1; dst -= 1; n -= 1;
                    ((byte*)dst)[0] = ((byte*)src)[0];
                }
                if ((n & 2) != 0) {
                    src -= 2; dst -= 2; n -= 2;
                    ((short*)dst)[0] = ((short*)src)[0];
                }
                if ((n & 4) != 0) {
                    src -= 4; dst -= 4; n -= 4;
                    ((int*)dst)[0] = ((int*)src)[0];
                }
                if ((n & 8) != 0) {
                    src -= 8; dst -= 8; n -= 8;
                    ((int*)dst)[1] = ((int*)src)[1];
                    ((int*)dst)[0] = ((int*)src)[0];
                }
            }
            while (n >= 16) {
                src -= 16; dst -= 16; n -= 16;
                ((int*)dst)[3] = ((int*)src)[3];
                ((int*)dst)[2] = ((int*)src)[2];
                ((int*)dst)[1] = ((int*)src)[1];
                ((int*)dst)[0] = ((int*)src)[0];
            }
        }
    }
} // class CharStream

}

#endif // !LOW_TRUST
