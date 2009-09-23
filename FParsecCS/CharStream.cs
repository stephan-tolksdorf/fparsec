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
                this.DecoderData = serializableDecoderMembers != null
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

        /// <summary>Represents the link between a CharStream and its Iterators.
        /// Lives on the unmanaged heap and holds a GCHandle, hence must be properly freed.</summary>
        internal struct Anchor {
            public int Block;
            /// <summary>The index of the last block of the stream, or Int32.MaxValue if the end of stream has not yet been detected.</summary>
            public int LastBlock;
            public GCHandle StreamHandle;
            public char* BufferBegin;
            public char* BufferEnd;
            public long CharIndex;
            public long CharIndexPlusOffset;
            public long CharIndexOffset;
            public long EndOfStream;
            public int BlockSizeMinusOverlap;

            public static Anchor* Create(CharStream stream) {
                // We create the anchor instance on the unmanaged heap. An alternative would be to use a
                // pinned pointer, but that would carry the risk of fragmenting the managed heap
                // (because an Anchor is a small object that can be long-lived).
                // (If AllocHGlobal becomes a bottleneck, we could replace it with a pool allocator.)
                Anchor* p = (Anchor*) Marshal.AllocHGlobal(sizeof(Anchor));
                p->StreamHandle = GCHandle.Alloc(stream, GCHandleType.Normal);
                return p;
            }

            public static void Free(Anchor *p) {
                p->StreamHandle.Free();
                Marshal.FreeHGlobal((IntPtr) p);
            }
        }

        private const int DefaultBlockSize = 3*(1 << 16); // 3*2^16 = 200k
        private const int DefaultByteBufferLength = (1 << 12);
        private const char EOS = '\uFFFF';

        private Stream Stream;
        // we keep a seperate record of the Stream.Position, so that we don't need to require Stream.CanSeek
        private long StreamPosition;
        private bool LeaveOpen;

        /// <summary>The Encoding that is used for decoding the underlying byte stream, or
        /// System.Text.UnicodeEncoding in case the stream was directly constructed
        /// from a string.</summary>
        public  Encoding Encoding { get; private set; }
        private int MaxCharCountForOneByte;
        private Decoder Decoder;
        private MemberInfo[] SerializableDecoderMembers;

        private int BlockSize;
        private int BlockOverlap;
        private int MinRegexSpace;

        private List<BlockInfo> Blocks;

        private byte[] ByteBuffer;
        private int ByteBufferIndex; // stores BufferStringIndex if ByteBuffer == null
        private int ByteBufferCount;

        /// <summary>The byte stream index of the first unused byte in the ByteBuffer.</summary>
        private long ByteIndex { get { return StreamPosition - (ByteBufferCount - ByteBufferIndex); } }

        /// <summary>The string holding the char buffer.</summary>
        internal string BufferString;
        internal int BufferStringIndex { get { return ByteBuffer == null ? ByteBufferIndex : 0; } }
        /// <summary>Pinned pointer handle for BufferString</summary>
        private GCHandle BufferHandle;

        // We use a string as the char buffer so that we can leverage the library support
        // for strings. This way we can apply regular expressions directly to the input
        // stream, for example. Because reference classes can't be allocated on the unmanaged
        // heap, we need to take a pinned pointer. However, this shouldn't be a problem,
        // because either the stream is small and the lifetime short, or the BufferString is
        // so large that it will be allocated on the large object heap and the pinning is a no-op.

        private Anchor* anchor; // allocated and assigned during construction,
                                // freed and set to null during disposal

        /// <summary>The current block in BufferString.</summary>
        private int Block { get { return anchor->Block; } }

        /// <summary>The number of chars in BufferString.</summary>
        private int BufferCount { get { return PositiveDistance(anchor->BufferBegin, anchor->BufferEnd); } }


        /// <summary>The index of the last char of the stream plus 1,
        /// or Int64.MaxValue if the end of stream has not yet been detected.</summary>
        public long EndOfStream { get { return anchor->EndOfStream; } }

        /// <summary>The index of the first char in the stream, i.e. Begin.Index.
        /// This value is determined by the streamIndexOffset argument of some of the CharStream constructors.
        /// By default this value is 0.</summary>
        public long IndexOffset { get { return anchor->CharIndexOffset; } }

        // we don't have a public constructor that only takes a string to avoid potential confusion with a filepath constructor
        internal CharStream(string chars) {
            Debug.Assert(chars != null);
            BufferString = chars;
            ByteBufferIndex = 0; // we recycle ByteBufferIndex for BufferStringIndex
            BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
            char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject();
            CharConstructorContinue(bufferBegin, chars.Length, 0);
        }
        /// <summary>Constructs a CharStream from the chars in the string argument between the indices index (inclusive) and index + length (exclusive).</summary>
        /// <exception cref="ArgumentNullException">chars is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0 and index + length ≤ chars.Length.</exception>
        public CharStream(string chars, int index, int length) : this(chars, index, length, 0) {}

        /// <summary>Constructs a CharStream from the chars in the string argument between the indices index (inclusive) and index + length (exclusive). The first char in the stream is assigned the index streamIndexOffset.</summary>
        /// <exception cref="ArgumentNullException">chars is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0, index + length ≤ chars.Length and 0 ≤ streamIndexOffset &lt; 2^60.</exception>
        public CharStream(string chars, int index, int length, long streamIndexOffset) {
            if (chars == null) throw new ArgumentNullException("chars");
            if (index < 0) throw new ArgumentOutOfRangeException("index", "The index is negative.");
            if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "The length is out of range.");
            if (streamIndexOffset < 0 || streamIndexOffset >= (1L << 60)) throw new ArgumentOutOfRangeException("streamIndexOffset", "The index offset must be non-negative and less than 2^60.");

            BufferString = chars;
            ByteBufferIndex = index; // we recycle ByteBufferIndex for BufferStringIndex
            BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
            char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject() + index;

            CharConstructorContinue(bufferBegin, length, streamIndexOffset);
        }

        internal CharStream(string chars, char* pCharsPlusIndex, int index, int length, long streamIndexOffset) {
            Debug.Assert(index >= 0 && length <= chars.Length - index && pCharsPlusIndex != null && streamIndexOffset >= 0 && streamIndexOffset < (1L << 60));
            BufferString = chars;
            ByteBufferIndex = index; // we recycle ByteBufferIndex for BufferStringIndex
            CharConstructorContinue(pCharsPlusIndex, length, streamIndexOffset);
        }

        /// <summary>Constructs a CharStream from the chars in the char array argument between the indices index (inclusive) and index + length (exclusive).</summary>
        /// <exception cref="ArgumentNullException">chars is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0 and index + length ≤ chars.Length.</exception>
        public CharStream(char[] chars, int index, int length) : this(chars, index, length, 0) { }

        /// <summary>Constructs a CharStream from the chars in the char array argument between the indices index (inclusive) and index + length (exclusive). The first char in the stream is assigned the index streamIndexOffset.</summary>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0, index + length ≤ chars.Length and 0 ≤ streamIndexOffset &lt; 2^60.</exception>
        public CharStream(char[] chars, int index, int length, long streamIndexOffset) {
            if (chars == null) throw new ArgumentNullException("chars");
            if (index < 0) throw new ArgumentOutOfRangeException("index", "The index is negative.");
            if (length < 0 || length > chars.Length - index) throw new ArgumentOutOfRangeException("length", "The length is out of range.");
            if (streamIndexOffset < 0 || streamIndexOffset >= (1L << 60)) throw new ArgumentOutOfRangeException("streamIndexOffset", "The index offset must be non-negative and less than 2^60.");

            BufferHandle = GCHandle.Alloc(chars, GCHandleType.Pinned);
            char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject() + index;

            CharConstructorContinue(bufferBegin, length, streamIndexOffset);
        }

        /// <summary>Constructs a CharStream from the length chars at the pointer address.</summary>
        /// <exception cref="ArgumentNullException">pchars is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        public CharStream(char* pchars, int length) : this(pchars, length, 0) {}

        /// <summary>Constructs a CharStream from the length chars at the pointer address. The first char in the stream is assigned the index streamIndexOffset.</summary>
        /// <exception cref="ArgumentNullException">pchars is null.</exception>
        /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: length ≥ 0 and 0 ≤ streamIndexOffset &lt; 2^60.</exception>
        public CharStream(char* pchars, int length, long streamIndexOffset) {
            if (pchars == null) throw new ArgumentNullException("pchars");
            if (length < 0) throw new ArgumentOutOfRangeException("length", "The length is negative.");
            if (pchars > unchecked(pchars + length))
                throw new ArgumentOutOfRangeException("length", "The length is out of range.");
            if (streamIndexOffset < 0 || streamIndexOffset >= (1L << 60)) throw new ArgumentOutOfRangeException("streamIndexOffset", "The index offset must be non-negative and less than 2^60.");

            CharConstructorContinue(pchars, length, streamIndexOffset);
        }

        internal CharStream(char* pchars, int length, long streamIndexOffset, int dummyArgumentForInternalConstructorWithoutParameterChecking) {
            CharConstructorContinue(pchars, length, streamIndexOffset);
        }

        private void CharConstructorContinue(char* bufferBegin, int length, long streamIndexOffset) {
            Debug.Assert(bufferBegin != null && length >= 0 && bufferBegin <= bufferBegin + length && streamIndexOffset >= 0 && streamIndexOffset < (1L << 60));
            Encoding = Encoding.Unicode;
            BlockSize = length;
            anchor = Anchor.Create(this);
            anchor->BlockSizeMinusOverlap = length;
            anchor->EndOfStream = streamIndexOffset + length;
            anchor->BufferBegin = bufferBegin;
            anchor->BufferEnd = bufferBegin + length;
            anchor->Block = 0;
            anchor->LastBlock = 0;
            anchor->CharIndex = 0;
            anchor->CharIndexPlusOffset = streamIndexOffset;
            anchor->CharIndexOffset = streamIndexOffset;
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
            Stream stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan);
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
            StreamConstructorContinue(stream, leaveOpen,
                                      encoding, detectEncodingFromByteOrderMarks,
                                      blockSize, blockOverlap, minRegexSpace, byteBufferLength);
        }

        private void StreamConstructorContinue(Stream stream, bool leaveOpen,
                                               Encoding encoding, bool detectEncodingFromByteOrderMarks,
                                               int blockSize, int blockOverlap, int minRegexSpace, int byteBufferLength)
        {
            Stream = stream;
            LeaveOpen = leaveOpen;

            // the ByteBuffer must be larger than the longest detectable preamble
            if (byteBufferLength < 16) byteBufferLength = DefaultByteBufferLength;

            int bytesInStream = -1;
            if (Stream.CanSeek) {
                StreamPosition = stream.Position;
                long streamLength = Stream.Length - StreamPosition;
                if (streamLength <= Int32.MaxValue) {
                    bytesInStream = (int)streamLength;
                    if (bytesInStream < byteBufferLength) byteBufferLength = bytesInStream;
                }
            } else {
                StreamPosition = 0;
            }

            ByteBuffer = new byte[byteBufferLength];
            ClearAndRefillByteBuffer(0); // sets ByteBufferIndex and ByteBufferCount
            if (ByteBufferCount < byteBufferLength) bytesInStream = ByteBufferCount;

            int preambleLength = Helper.DetectPreamble(ByteBuffer, ByteBufferCount, ref encoding, detectEncodingFromByteOrderMarks);
            ByteBufferIndex += preambleLength;
            bytesInStream -= preambleLength;

            Encoding = encoding;
            Decoder = encoding.GetDecoder();

            // we allow such small block sizes only to simplify testing
            if (blockSize < 8) blockSize = DefaultBlockSize;

            bool allCharsFitIntoOneBlock = false;
            if (bytesInStream >= 0 && bytesInStream/4 <= blockSize) {
                try {
                    int maxCharCount = Encoding.GetMaxCharCount(bytesInStream); // may throw ArgumentOutOfRangeException
                    if (blockSize >= maxCharCount) {
                        allCharsFitIntoOneBlock = true;
                        blockSize = maxCharCount;
                    }
                } catch (ArgumentOutOfRangeException) { }
            }
            if (allCharsFitIntoOneBlock) {
                blockOverlap  = 0;
                minRegexSpace = 0;
                MaxCharCountForOneByte = -1;
                SerializableDecoderMembers = null;
            } else {
                MaxCharCountForOneByte = Math.Max(1, Encoding.GetMaxCharCount(1));
                SerializableDecoderMembers = GetSerializableDecoderMemberInfo(Decoder);
                if (blockSize < 3*MaxCharCountForOneByte) blockSize = 3*MaxCharCountForOneByte;
                // MaxCharCountForOneByte == the maximum number of overhang chars
                if(    Math.Min(blockOverlap, blockSize - 2*blockOverlap) < MaxCharCountForOneByte
                    || blockOverlap >= blockSize/2) blockOverlap = blockSize/3;
                if (minRegexSpace < 0 || minRegexSpace > blockOverlap) minRegexSpace = 2*blockOverlap/3;
            }

            BlockSize     = blockSize;
            BlockOverlap  = blockOverlap;
            MinRegexSpace = minRegexSpace;

            try {
                BufferString = new String('\u0000', BlockSize);
                BufferHandle = GCHandle.Alloc(BufferString, GCHandleType.Pinned);
                char* bufferBegin = (char*)BufferHandle.AddrOfPinnedObject();

                anchor = Anchor.Create(this);
                anchor->BlockSizeMinusOverlap = blockSize - blockOverlap;
                anchor->BufferBegin = bufferBegin;
                if (allCharsFitIntoOneBlock) {
                    int bufferCount = ByteBufferCount == 0 ? 0 : ReadAllRemainingCharsFromStream(bufferBegin, BlockSize);
                    anchor->Block = 0;
                    anchor->LastBlock = 0;
                    anchor->CharIndex = 0;
                    anchor->CharIndexOffset = 0;
                    anchor->CharIndexPlusOffset = 0;
                    anchor->BufferEnd = bufferBegin + bufferCount;
                    anchor->EndOfStream = bufferCount;
                    ByteBuffer = null; // we don't need the byte buffer anymore
                    if (!leaveOpen) stream.Close();
                    Stream = null;
                } else {
                    anchor->Block = -2; // special value recognized by ReadBlock
                    anchor->LastBlock = Int32.MaxValue;
                    anchor->CharIndex = 0;
                    anchor->CharIndexOffset = 0;
                    anchor->CharIndexPlusOffset = 0;
                    anchor->BufferEnd = bufferBegin;
                    anchor->EndOfStream = Int64.MaxValue;
                    Blocks = new List<BlockInfo>();
                    // the first block has no overlap with a previous block
                    Blocks.Add(new BlockInfo(ByteBufferIndex, ByteBufferIndex, 0, EOS, null, new DecoderState(), null, new DecoderState()));
                    ReadBlock(0);
                }
            } catch {
                if (anchor != null) Anchor.Free(anchor);
                if (BufferHandle.IsAllocated) BufferHandle.Free();
                throw;
            }
        }

        public void Dispose() {
            if (anchor == null) return;
            Anchor.Free(anchor);
            anchor = null;
            Blocks = null;
            ByteBuffer = null;
            BufferString = null;
            if (BufferHandle.IsAllocated) BufferHandle.Free();
            if (Stream != null && !LeaveOpen) {
                Stream.Close();
                Stream = null;
            }
        }

        /// <summary>an optimized version of end - begin, which assumes that 2^31 > end - begin >= 0. </summary>
        internal static int PositiveDistance(char* begin, char* end) {
            return (int)((uint)((byte*)end - (byte*)begin)/2);
        }

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

        /// <summary>Reads all remaining chars into the given buffer. If the remaining stream
        /// content holds more than the given maximum number of chars, an exception will be thrown.</summary>
        private int ReadAllRemainingCharsFromStream(char* buffer, int maxCount) {
            Debug.Assert(maxCount >= 0);
            fixed (byte* byteBuffer = ByteBuffer) {
                int bufferCount = 0;
                bool flush;
                do {
                    int nBytesInByteBuffer = FillByteBuffer();
                    flush = nBytesInByteBuffer == 0;
                    try {
                        bufferCount += Decoder.GetChars(byteBuffer + ByteBufferIndex, nBytesInByteBuffer,
                                                        buffer + bufferCount, maxCount - bufferCount, flush);
                        ByteBufferIndex += nBytesInByteBuffer; // GetChars consumed all bytes in the byte buffer
                    } catch (DecoderFallbackException e) {
                        e.Data.Add("Stream.Position", ByteIndex + e.Index);
                        throw;
                    }
                } while (!flush);
                return bufferCount;
            }
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
                if (block > 0 && SerializableDecoderMembers == null)
                    throw new NotSupportedException("The CharStream does not support seeking backward over ranges longer than the block overlap because the Encoding's Decoder is not serializable.");
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
                        anchor->EndOfStream = anchor->CharIndexOffset + charIndex + (buffer - bufferBegin);
                    }
                } else if (anchor->EndOfStream != anchor->CharIndexOffset + charIndex + (buffer - bufferBegin)) {
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
            return buffer > begin ? begin : null;
        }

        /// <summary>An iterator pointing to the beginning of the stream (or to the end if the CharStream is empty).</summary>
        public Iterator Begin { get {
            Anchor* anchor = this.anchor;
            if (anchor == null) throw new ObjectDisposedException("CharStream");
            if (anchor->EndOfStream != anchor->CharIndexOffset) {
                return new Iterator(){Anchor = anchor, Ptr = anchor->BufferBegin, Block = 0};
            } else {
                return new Iterator(){Anchor = anchor, Ptr = null, Block = -1};
            }
        } }

        // do not directly provide an iterator to the end of the stream in order to
        // ensure that such iterators only exists once the end's position has been detected

        /// <summary>Returns an iterator pointing to the given index in the stream,
        /// or to the end of the stream if the indexed position lies beyond the last char in the stream.</summary>
        /// <exception cref="ArgumentOutOfRangeException">The index is less than 0 (or less than the index offset specified when the CharStream was constructed).</exception>
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
            long off = unchecked (index - anchor->CharIndexPlusOffset);
            if (0 <= off && off < PositiveDistance(anchor->BufferBegin, anchor->BufferEnd))
                return new Iterator(){Anchor = anchor, Ptr = anchor->BufferBegin + (int)off, Block = anchor->Block};
            if (index >= anchor->EndOfStream) return new Iterator() {Anchor = anchor, Ptr = null, Block = -1};
            index -= anchor->CharIndexOffset;
            if (index < 0) throw (new ArgumentOutOfRangeException("index", "The index is negative (or less than the char index offset specified at construction time)."));
            int blockSizeMinusOverlap = anchor->BlockSizeMinusOverlap;
            long idx_;
            long block_ = Math.DivRem(index, blockSizeMinusOverlap, out idx_);
            int block = block_ > Int32.MaxValue ? Int32.MaxValue : (int)block_;
            int idx = (int)idx_;
            return Seek(block, idx);
        }
        private Iterator Seek(int block, int idx) {
            Anchor* anchor = this.anchor;
            int blockSizeMinusOverlap = anchor->BlockSizeMinusOverlap;
            if (anchor->Block < block && idx < BlockOverlap) {
                --block;
                idx += blockSizeMinusOverlap;
            }
            int last = Blocks.Count - 1;
            if (block >= last) {
                int b = last;
                while (ReadBlock(b) != null && b < block) ++b; // we will get an OutOfMemoryException before b overflows
                if (block != anchor->Block || idx >= PositiveDistance(anchor->BufferBegin, anchor->BufferEnd))
                    return new Iterator(){Anchor = anchor, Ptr = null, Block = -1};
            } else ReadBlock(block);
            return new Iterator(){Anchor = anchor, Ptr = anchor->BufferBegin + idx, Block = block};
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

            internal Iterator(Anchor* anchor, int block, char* ptr) {
                Anchor = anchor;
                Ptr = ptr;
                Block = block;
            }

            /// <summary>The CharStream over which the Iterator iterates.</summary>
            public CharStream Stream { get { return (CharStream) Anchor->StreamHandle.Target; } }

            /// <summary>Indicates whether the Iterator has reached the end of the stream,
            /// i.e. whether it points to one char beyond the last char in the stream.</summary>
            public bool IsEndOfStream { get { return Block == -1; } }

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
                    return (uint)PositiveDistance(anchor->BufferBegin, Ptr) + anchor->CharIndexPlusOffset;
                } else if (block == -1) {
                    // this is safe, as there can only be an end-of-stream iterator
                    // once the end of stream has been detected
                    return Anchor->EndOfStream;
                }
                long charIndex = anchor->CharIndexOffset + Math.BigMul(block, anchor->BlockSizeMinusOverlap);
                return (uint)PositiveDistance(anchor->BufferBegin, Ptr) + charIndex;
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
                char* newPtr = this.Ptr + 1;
                int block = Block;
                if (block == anchor->Block && newPtr < anchor->BufferEnd)
                    return new Iterator() {Anchor = anchor, Ptr = newPtr, Block = block};

                return Stream.Seek(Index + 1);
            } }

            /// <summary>Returns an Iterator that is advanced by numberOfChars chars. The Iterator can't
            /// move past the end of the stream, i.e. any position beyond the last char
            /// in the stream is interpreted as precisely one char beyond the last char.</summary>
            /// <exception cref="ArgumentOutOfRangeException">The new index is negative (or less than the index offset specified when the CharStream was constructed).</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public Iterator Advance(int numberOfChars) {
                Anchor* anchor = Anchor;
                char* ptr = this.Ptr;
                char* newPtr = unchecked (ptr + numberOfChars);
                // note that the second case in the following ternary condition must not include newPtr == ptr,
                // so that ptr + Int32.MinValue == ptr on 32-bit platforms is correctly handled
                if (Block == anchor->Block && numberOfChars >= 0 ? newPtr >= ptr && newPtr <  anchor->BufferEnd
                                                                 : newPtr <  ptr && newPtr >= anchor->BufferBegin)
                    return new Iterator() {Anchor = anchor, Ptr = newPtr, Block = Block};

                return Stream.Seek(Index + numberOfChars);

            }

            /// <summary>Returns an Iterator that is advanced by numberOfChars chars. The Iterator can't
            /// move past the end of the stream, i.e. any position beyond the last char
            /// in the stream is interpreted as precisely one char beyond the last char.</summary>
            /// <exception cref="ArgumentOutOfRangeException">The new index is negative (or less than the index offset specified when the CharStream was constructed).</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            /// <exception cref="OutOfMemoryException">Can not allocate enough memory for the internal data structure.</exception>
            public Iterator Advance(long numberOfChars) {
                if (Block == Anchor->Block
                    && (numberOfChars >= 0 ? numberOfChars <   PositiveDistance(Ptr, Anchor->BufferEnd)
                                           : numberOfChars >= -PositiveDistance(Anchor->BufferBegin, Ptr)))
                {
                    int nn = (int)numberOfChars;
                    char* newPtr = unchecked (Ptr + nn); // we need unchecked here because C# always uses
                                                         // unsigned arithmetic for pointer calculations and
                                                         // otherwise would report an overflow for any negative numberOfChars
                                                         // if overflow checking is activated
                    return new Iterator() {Anchor = Anchor, Ptr = newPtr, Block = Block};
                }
                return Stream.Seek(Index + numberOfChars);
            }

            /// <summary>Returns an Iterator that is advanced by numberOfChars chars. The Iterator can't
            /// move past the end of the stream, i.e. any position beyond the last char
            /// in the stream is interpreted as precisely one char beyond the last char.</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public Iterator Advance(uint numberOfChars) {
                Anchor* anchor = Anchor;
                char* ptr = this.Ptr;
                int block = Block;
                if (block == anchor->Block && numberOfChars < (uint)PositiveDistance(ptr, anchor->BufferEnd)) {
                    char* newPtr = ptr + numberOfChars;
                    return new Iterator() {Anchor = Anchor, Ptr = newPtr, Block = Block};
                }

                return Stream.Seek(Index + numberOfChars);
            }

            // The following methods with a leading underscore don't belong into the
            // "offical" API, because they mutate the Iterator struct, which otherwise
            // is expected to be immutable. However, users who know what they're doing
            // can use them to implement very efficent parser loops, which is why these
            // methods are declared public.

            /// <summary>Advances the Iterator *in-place* by 1 char and returns the char on the new position.
            ///`c = iter.Increment()` is equivalent to `iter = iter.Next; c = iter.Read()`.</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public char _Increment() {
                Anchor* anchor = Anchor;
                char* newPtr = this.Ptr + 1;
                if (Block == anchor->Block && newPtr < anchor->BufferEnd) {
                    this.Ptr = newPtr;
                    return *newPtr;
                }
                return IncrementContinue();
            }
            [MethodImplAttribute(MethodImplOptions.NoInlining)]
            private char IncrementContinue() {
                this = Next;
                return Read();
            }

            /// <summary>Advances the Iterator *in-place* by numberOfChars chars and returns the char on the new position.
            /// `c = iter.Increment(numberOfChars)` is an optimized implementation of `iter = iter.Advance(numberOfChars); c = iter.Read()`.</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public char _Increment(uint numberOfChars) {
                Anchor* anchor = Anchor;
                char* ptr = this.Ptr;
                if (Block == anchor->Block && numberOfChars < (uint)PositiveDistance(ptr, anchor->BufferEnd)) {
                    char* newPtr = ptr + numberOfChars;
                    this.Ptr = newPtr;
                    return *newPtr;
                }
                this = Advance(numberOfChars);
                return Read();
            }

            /// <summary>Advances the Iterator *in-place* by -1 char and returns the char on the new position.
            /// `c = iter.Decrement()` is an optimized implementation of `iter = iter.Advance(-1); c = iter.Read()`.</summary>
            /// <exception cref="ArgumentOutOfRangeException">The new index is less than 0 (or less than the index offset specified when the CharStream was constructed).</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            public char _Decrement() {
                Anchor* anchor = Anchor;
                char* newPtr = unchecked (this.Ptr - 1);
                if (Block == anchor->Block && newPtr >= anchor->BufferBegin) {
                    this.Ptr = newPtr;
                    return *newPtr;
                }
                return DecrementContinue(1u);
            }

            /// <summary>Advances the Iterator *in-place* by -numberOfChars chars and returns the char on the new position.
            /// `c = iter.Decrement()` is an optimized implementation of `iter = iter.Advance(-numberOfChars); c = iter.Read()`.</summary>
            /// <exception cref="ArgumentOutOfRangeException">The new index is less than 0 (or less than the index offset specified when the CharStream was constructed).</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            public char _Decrement(uint numberOfChars) {
                Anchor* anchor = Anchor;
                char* ptr = this.Ptr;
                if (Block == anchor->Block && numberOfChars <= (uint)PositiveDistance(anchor->BufferBegin, ptr)) {
                    char* newPtr = ptr - numberOfChars;
                    this.Ptr = newPtr;
                    return *newPtr;
                }
                return DecrementContinue(numberOfChars);
            }
            [MethodImplAttribute(MethodImplOptions.NoInlining)]
            private char DecrementContinue(uint numberOfChars) {
               this = Stream.Seek(Index - numberOfChars);
               return Read();
            }

            /// <summary>Is an optimized implementation of Next.Read().</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public char Peek() {
                Anchor* anchor = Anchor;
                char* ptr1 = this.Ptr + 1;
                if (Block == anchor->Block && ptr1 < anchor->BufferEnd) return *ptr1;
                return PeekContinue(1u);
            }

            /// <summary>Is an optimized implementation of Advance(numberOfChars).Read(),
            /// except that the EndOfStreamChar ('\uFFFF') is returned if Index + numberOfChars &lt; 0 (instead of an exception being thrown).</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public char Peek(int numberOfChars) {
                char* ptrN = unchecked (Ptr + numberOfChars);
                if (Block == Anchor->Block) {
                    if (numberOfChars < 0) {
                        if (ptrN >= Anchor->BufferBegin && ptrN <  Ptr) return *ptrN;
                    } else {
                        if (ptrN <  Anchor->BufferEnd   && ptrN >= Ptr) return *ptrN;
                    }
                }
                return PeekContinue(numberOfChars);
            }
            [MethodImplAttribute(MethodImplOptions.NoInlining)]
            private char PeekContinue(int numberOfChars) {
                long newIndex = Index + numberOfChars;
                return newIndex >= Anchor->CharIndexOffset ? Stream.Seek(newIndex).Read() : EOS;
            }

            /// <summary>Is an optimized implementation of Advance(numberOfChars).Read().</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public char Peek(uint numberOfChars) {
                Anchor* anchor = Anchor;
                char* ptr = this.Ptr;
                if (Block == anchor->Block && numberOfChars < (uint)PositiveDistance(ptr, anchor->BufferEnd))
                    return *(ptr + numberOfChars);
                return PeekContinue(numberOfChars);
            }
            [MethodImplAttribute(MethodImplOptions.NoInlining)]
            private char PeekContinue(uint numberOfChars) {
                if (Block == -1) return EOS;
                return Stream.Seek(Index + numberOfChars).Read();
            }

            /// <summary>Returns true if and only if the char argument matches the char pointed to by the Iterator.
            /// At the end of the stream Match always returns false.</summary>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occurs.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public bool Match(char c) {
                if (Block == Anchor->Block) return *Ptr == c;
                return MatchContinue(c);
            }
            [MethodImplAttribute(MethodImplOptions.NoInlining)]
            private bool MatchContinue(char c) {
                if (Block == -1) return false;
                Stream.ReadBlock(Block);
                return *Ptr == c;
            }

            /// <summary>Returns true if str matches the chars in the stream beginning with the char pointed to by the Iterator.
            /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
            /// If str is empty, true is returned.</summary>
            /// <exception cref="NullReferenceException">str is null.</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public bool Match(string str) {
                Anchor* anchor = Anchor;
                if (Block == anchor->Block && str.Length <= PositiveDistance(Ptr, anchor->BufferEnd)) {
                    for (int i = 0; i < str.Length; ++i) {
                        // The 64-bit JIT doesn't eliminate the bounds checking for strings, *sigh*.
                        // The goto is necessary to improve the code generated by the 32-bit JIT.
                        if (Ptr[i] != str[i]) goto ReturnFalse;
                    }
                    return true;
                ReturnFalse:
                    return false;
                }
                if (Block == -1) return str.Length == 0;
                return Match(str, 0, str.Length);
            }

            /// <summary>Returns true if caseFoldedStr matches the chars in the stream
            /// beginning with the char pointed to by the Iterator.
            /// The chars in the stream are case-folded before they are matched,
            /// while the chars in the string argument are assumed to already be case-folded.
            /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
            /// If caseFoldedStr is empty, true is returned.</summary>
            /// <exception cref="NullReferenceException">caseFoldedStr is null.</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public bool MatchCaseFolded(string caseFoldedStr) {
                string str = caseFoldedStr;
                // The x86 JIT is not able to cope with the additional register pressure due to
                // the cftable lookup. Our only hope for an efficient loop is to pin the string,
                // and use pointer arithmetic.
                fixed (char* pStr_ = str) {
                    char* ptr = Ptr;
                    char* end = unchecked (ptr + str.Length);
                    if (Block == Anchor->Block && end >= ptr && end <= Anchor->BufferEnd) {
                        char* cftable = CaseFoldTable.FoldedChars;
                        if (cftable == null) cftable = CaseFoldTable.Initialize();
                        char* pStr = pStr_;
                        for (; ptr < end; ++ptr, ++pStr) {
                            if (cftable[*ptr] != *pStr) goto ReturnFalse;
                        }
                        return true;
                    ReturnFalse:
                        return false;
                    }
                    return MatchCaseFoldedContinue(pStr_, str.Length);
                }
            }

            /// <summary>Returns true if the chars in str between the indices strIndex (inclusive) and
            /// strIndex + length (exclusive) match the chars in the stream beginning with the char pointed to by the Iterator.
            /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
            /// If length is 0, true is returned.</summary>
            /// <exception cref="ArgumentOutOfRangeException">strIndex is negative, length is negative or strIndex + length > str.Length.</exception>
            /// <exception cref="NullReferenceException">str is null.</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public bool Match(string str, int strIndex, int length) {
                if (strIndex < 0)
                    throw new ArgumentOutOfRangeException("strIndex", "Index is negative.");
                if (length > str.Length - strIndex)
                    throw new ArgumentOutOfRangeException("length", "Length is out of range.");
                fixed (char* pStr = str) return Match(pStr + strIndex, length); // checks length >= 0
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
                    throw new ArgumentOutOfRangeException("charsIndex", "Index is negative.");
                if (length > chars.Length - charsIndex)
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
            public bool Match(char* pStr, int length) {
                char* ptr = Ptr;
                Anchor* anchor = Anchor;      // the unsigned comparison will correctly handle negative length values
                if (Block == anchor->Block && unchecked((uint)length) <= (uint)PositiveDistance(ptr, anchor->BufferEnd)) {
                    #if UNALIGNED_READS
                        int len = length & 0x7ffffffe;
                        for (int i = 0; i < len; i += 2) {
                            if (*((int*)(ptr + i)) != *((int*)(pStr + i))) goto ReturnFalse;
                        }
                        if (len != length) {
                            if (ptr[len] != pStr[len]) goto ReturnFalse;
                        }
                        return true;
                    #else
                        for (int i = 0; i < length; ++i) {
                            if (ptr[i] != pStr[i]) goto ReturnFalse;
                        }
                        return true;
                    #endif
                ReturnFalse:
                        return false;
                }
                return MatchContinue(pStr, length);
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
            public bool MatchCaseFolded(char* pCaseFoldedStr, int length) {
                char* ptr = Ptr;
                char* end = unchecked (ptr + length);
                Anchor* anchor = Anchor; // we don't check length >= 0, so we must require end > ptr, because length could be Int32.MinValue
                if (Block == anchor->Block && end > ptr && end <= anchor->BufferEnd) {
                    char* cftable = CaseFoldTable.FoldedChars;
                    if (cftable == null) cftable = CaseFoldTable.Initialize();
                    char* pStr = pCaseFoldedStr;
                    for (; ptr < end; ++ptr, ++pStr) {
                        if (cftable[*ptr] != *pStr) goto ReturnFalse;
                    }
                    return true;
                ReturnFalse:
                    return false;
                }
                return MatchCaseFoldedContinue(pCaseFoldedStr, length);
            }

            private bool MatchContinue(char* pStr, int length) {
                if (length <= 0) {
                    if (length == 0) return true;
                    throw new ArgumentOutOfRangeException("length", "Length is negative.");
                }

                int block = this.Block; // local copy that might be modified below
                if (block == -1) return false;

                CharStream stream = null;
                if (block != Anchor->Block) {
                    stream = this.Stream;
                    stream.ReadBlock(block);
                }

                char* ptr = Ptr;

                // requires length > 0
                for (;;) {
                    int len = Math.Min(PositiveDistance(ptr, Anchor->BufferEnd), length);
                    length -= len;

                    #if UNALIGNED_READS
                        while (len >= 2) {
                            if (*((int*)ptr) != *((int*)pStr)) goto ReturnFalse;
                            ptr += 2; pStr += 2; len -= 2;
                        }
                        if (len != 0) {
                            if (*ptr != *pStr) goto ReturnFalse;
                            ++pStr;
                        }
                    #else
                        do {
                            if (*ptr != *pStr) goto ReturnFalse;
                            ++ptr; ++pStr; --len;
                        } while (len != 0);
                    #endif

                    if (length == 0) return true;
                    else {
                        if (stream == null) stream = this.Stream;
                        ptr = stream.ReadBlock(++block);
                        if (ptr == null) return false;
                    }
                }
            ReturnFalse:
                return false;
            }

            private bool MatchCaseFoldedContinue(char* pCaseFoldedStr, int length) {
                if (length <= 0) {
                    if (length == 0) return true;
                    throw new ArgumentOutOfRangeException("length", "Length is negative.");
                }

                int block = this.Block; // local copy that might be modified below
                if (block == -1) return false;

                CharStream stream = null;
                if (block != Anchor->Block) {
                    stream = this.Stream;
                    stream.ReadBlock(block);
                }

                char* ptr = Ptr, pStr = pCaseFoldedStr;

                char* cftable = CaseFoldTable.FoldedChars;
                if (cftable == null) cftable = CaseFoldTable.Initialize();

                // requires length > 0
                for (;;) {
                    int len = Math.Min(PositiveDistance(ptr, Anchor->BufferEnd), length);
                    length -= len;

                     do {
                        if (cftable[*ptr] != *pStr) goto ReturnFalse;
                        ++ptr; ++pStr; --len;
                    } while (len != 0);

                    if (length == 0) return true;
                    else {
                        if (stream == null) stream = this.Stream;
                        ptr = stream.ReadBlock(++block);
                        if (ptr == null) return false;
                    }
                }
            ReturnFalse:
                return false;
            }

            /// <summary>Applies the given regular expression to stream chars beginning with the char pointed to by the Iterator.
            /// Returns the resulting Match object.
            /// IMPORTANT: This method is not supported by CharStreams constructed from char arrays or pointers.</summary>
            /// <remarks>For performance reasons you should specifiy the regular expression
            /// such that it can only match at the beginning of a string,
            /// for example by prepending "\A".<br/>
            /// For CharStreams constructed from large binary streams the regular expression is not applied
            /// to a string containing all the remaining chars in the stream. The minRegexSpace parameter
            /// of the CharStream constructors determines the minimum number of chars that are guaranteed
            /// to be visible to the regular expression.</remarks>
            /// <exception cref="NullReferenceException">regex is null.</exception>
            /// <exception cref="NotSupportedException">Two possible reasons: 1) The CharStream was constructed from a char array or char pointer, in which case it does not support regular expression matching.
            /// 2) Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public Match Match(Regex regex) {
                CharStream stream = this.Stream;
                if (stream.BufferString == null) throw new NotSupportedException("CharStreams constructed from char arrays or char pointers do not support regular expression matching.");
                int block = Block;
                if (block >= 0) {
                    int index = PositiveDistance(Anchor->BufferBegin, Ptr);
                    if (index > stream.BlockSize - stream.MinRegexSpace && block < Anchor->LastBlock) {
                        // BlockOverlap > MinRegexSpace
                        if (block + 1 == Anchor->Block || stream.ReadBlock(block + 1) != null) {
                            // index now needs to point to the beginning of the buffer
                            // (where the overlap with the previous block is)
                            index -= Anchor->BlockSizeMinusOverlap;
                        } else {
                            // block < LastBlock and we failed to read new chars from block + 1,
                            // so we now definitely need to read the current block
                            stream.ReadBlock(block);
                        }
                    } else if (block != Anchor->Block) stream.ReadBlock(block);
                    return regex.Match(stream.BufferString, stream.BufferStringIndex + index, PositiveDistance(Anchor->BufferBegin, Anchor->BufferEnd) - index);
                } else {
                    return regex.Match("");
                }
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
                if (Block == -1) return EOS;
                Stream.ReadBlock(Block);
                return *Ptr;
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
                if (Block == anchor->Block && unchecked((uint)length) <= (uint)PositiveDistance(Ptr, anchor->BufferEnd))
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
                if (Block == anchor->Block && unchecked((uint)length) <= (uint)PositiveDistance(Ptr, anchor->BufferEnd))
                    return new String(Ptr, 0, length);
                return ReadContinue(length, allOrEmpty);
            }
            private string ReadContinue(int length, bool allOrEmpty) {
                if (length < 0) throw new ArgumentOutOfRangeException("length", "Length is negative.");
                if (length == 0 || Block == -1) return "";
                if (Anchor->LastBlock != Int32.MaxValue) {
                    long maxLength = Anchor->EndOfStream - Index;
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


            /// <summary>Copies the length stream chars beginning with the char pointed to by the Iterator into dest.
            /// The chars are written into dest beginning at the index destIndex.
            /// If less than length chars are remaining in the stream, only the remaining chars are copied.
            /// Returns the actual number of chars copied.</summary>
            /// <exception cref="ArgumentOutOfRangeException">destIndex is negative, length is negative or destIndex + length > dest.Length.</exception>
            /// <exception cref="NullReferenceException">dest is null.</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public int Read(char[] dest, int destIndex, int length) {
                if (destIndex < 0)
                    throw new ArgumentOutOfRangeException("destIndex", "DestIndex is negative.");
                if (length > dest.Length - destIndex)
                    throw new ArgumentOutOfRangeException("length", "Length is out of range.");

                fixed (char* pDest = dest)
                return Read(pDest + destIndex, length); // will check length >= 0
            }

            /// <summary>Copies the length stream chars beginning with the char pointed to by the Iterator into the buffer at the given pointer address.
            /// If less than length chars are remaining in the stream, only the remaining chars are copied.
            /// Returns the actual number of chars copied.</summary>
            /// <exception cref="NullReferenceException">dest is null.</exception>
            /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
            /// <exception cref="NotSupportedException">Seeking of the underlying byte stream is required, but the byte stream does not support seeking or the Encodings's Decoder is not serializable.</exception>
            /// <exception cref="IOException">An I/O error occured.</exception>
            /// <exception cref="ArgumentException">The input stream contains invalid bytes and the encoding was constructed with the throwOnInvalidBytes option.</exception>
            /// <exception cref="DecoderFallbackException">The input stream contains invalid bytes for which the decoder fallback threw this exception.</exception>
            public int Read(char* dest, int length) {
                Anchor* anchor = Anchor;
                char* ptr = Ptr;
                if (Block == anchor->Block && unchecked((uint)length) <= (uint)PositiveDistance(ptr, anchor->BufferEnd)) {
                    #if UNALIGNED_READS
                        int len = length;
                        if ((unchecked((int)dest) & 2) != 0) { // align dest
                            *dest = *ptr;
                            ++dest; ++ptr; --len;
                        }
                        while (len >= 8) {
                            ((int*)dest)[0] = ((int*)ptr)[0];
                            ((int*)dest)[1] = ((int*)ptr)[1];
                            ((int*)dest)[2] = ((int*)ptr)[2];
                            ((int*)dest)[3] = ((int*)ptr)[3];
                            dest += 8; ptr += 8; len -= 8;
                        }
                        if ((len & 4) != 0) {
                            ((int*)dest)[0] = ((int*)ptr)[0];
                            ((int*)dest)[1] = ((int*)ptr)[1];
                            dest += 4; ptr += 4;
                        }
                        if ((len & 2) != 0) {
                            ((int*)dest)[0] = ((int*)ptr)[0];
                            dest += 2; ptr += 2;
                        }
                        if ((len & 1) != 0) {
                            *dest = *ptr;
                        }
                    #else
                        int len = length & 0x7ffffffe;
                        for (int i = 0; i < len; i += 2) {
                            dest[i]     = ptr[i];
                            dest[i + 1] = ptr[i + 1];
                        }
                        if (len != length) {
                            dest[len] = ptr[len];
                        }
                    #endif
                    return length;
                }
                return ReadContinue(dest, length);
            }
            private int ReadContinue(char* dest, int length) {
                if (length <= 0) {
                    if (length == 0) return 0;
                    throw new ArgumentOutOfRangeException("length", "Length is negative.");
                }

                int block = this.Block; // local copy that might be modified below
                if (block == -1) return 0;

                CharStream stream = null;
                if (block != Anchor->Block) {
                    stream = this.Stream;
                    stream.ReadBlock(block);
                }

                char* ptr = Ptr;
                int oldLength = length;

                // requires length > 0
                for (;;) {
                    int len = Math.Min(PositiveDistance(ptr, Anchor->BufferEnd), length);
                    length -= len;

                    #if UNALIGNED_READS
                        if ((unchecked((int)dest) & 2) != 0) { // align dest
                            *dest = *ptr;
                            ++dest; ++ptr; --len;
                        }
                        while (len >= 8) {
                            ((int*)dest)[0] = ((int*)ptr)[0];
                            ((int*)dest)[1] = ((int*)ptr)[1];
                            ((int*)dest)[2] = ((int*)ptr)[2];
                            ((int*)dest)[3] = ((int*)ptr)[3];
                            dest += 8; ptr += 8; len -= 8;
                        }
                        if ((len & 4) != 0) {
                            ((int*)dest)[0] = ((int*)ptr)[0];
                            ((int*)dest)[1] = ((int*)ptr)[1];
                            dest += 4; ptr += 4;
                        }
                        if ((len & 2) != 0) {
                            ((int*)dest)[0] = ((int*)ptr)[0];
                            dest += 2; ptr += 2;
                        }
                        if ((len & 1) != 0) {
                            *dest = *ptr;
                            ++dest;
                        }
                    #else
                        do {
                            *dest = *ptr;
                            ++dest; ++ptr; --len;
                        } while (len != 0);
                    #endif

                    if (length == 0) return oldLength;
                    else {
                        if (stream == null) stream = this.Stream;
                        ptr = stream.ReadBlock(++block);
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
                char* ptr = Ptr;
                char* end = iterToCharAfterLastInString.Ptr;
                if (block == Anchor->Block && block == iterToCharAfterLastInString.Block) {
                    if (ptr < end) return new String(ptr, 0, PositiveDistance(ptr, end));
                    else return "";
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

            public override bool Equals(object other) {
                if (!(other is Iterator)) return false;
                return Equals((Iterator) other);
            }

            public bool Equals(Iterator other) {
                char* ptr = Ptr;
                return    (ptr == other.Ptr && ptr != null && Block == other.Block)
                       || (Anchor == other.Anchor && Index == other.Index);
            }

            public override int GetHashCode() {
                return Index.GetHashCode();
            }

            public static bool operator==(Iterator i1, Iterator i2) { return  i1.Equals(i2); }
            public static bool operator!=(Iterator i1, Iterator i2) { return !i1.Equals(i2); }
        }

        /// <summary>Returns a case-folded copy of the string argument. All chars are mapped
        /// using the (non-Turkic) 1-to-1 case folding mappings (v. 5.1) for Unicode code
        /// points in the Basic Multilingual Plane, i.e. code points below 0x10000.
        /// If the argument is null, null is returned.</summary>
        static public string FoldCase(string str) {
            char* cftable = CaseFoldTable.FoldedChars;
            if (cftable == null) cftable = CaseFoldTable.Initialize();
            if (str != null) {
                fixed (char* src0 = str) {
                    char* src = src0;
                    char* end = src + str.Length;
                    for (;;) { // src is null-terminated, so we can always read one char
                        char   c = *src;
                        char cfc = cftable[c];
                        if (c == cfc) {
                            if (++src >= end) break;
                        } else {
                            string newString = new String(src0, 0, str.Length);
                            fixed (char* dst0 = newString) {
                                char* dst = dst0 + PositiveDistance(src0, src);
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
                        int len = PositiveDistance(src, end);
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
    }
}

#endif // !LOW_TRUST