// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

#if !LOW_TRUST

using System;
using System.Diagnostics;

namespace FParsec {

public sealed unsafe class State<TUserState> : IEquatable<State<TUserState>> {
    // splitting up the state into a fast changing part, the iterator, and a
    // a slowly changing part, the data member, improves performance
    internal sealed class Data {
        public long       Line;
        public long       LineBegin;
        public TUserState UserState;
        public string     StreamName;

        public Data() {}

        public Data(long line, long lineBegin, TUserState userState, string streamName) {
            Line       = line;
            LineBegin  = lineBegin;
            UserState  = userState;
            StreamName = streamName;
        }
    }

    public CharStream.Iterator Iter;
    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    internal Data data;

    internal State() { }

    public State(CharStream stream, TUserState userState) : this(stream, userState, "") {}

    public State(CharStream stream, TUserState userState, string streamName) {
        // Iter = stream.Begin
        var anchor  = stream.anchor;
        Iter.Anchor = anchor;
        var bufferBegin = anchor->BufferBegin;
        Iter.Ptr    = bufferBegin;
        Iter.Block  = bufferBegin == null ? -1 : 0;
        data = new Data{Line = 1, LineBegin = anchor->CharIndexOffset,
                        UserState = userState, StreamName = streamName};
    }

    public State(CharStream stream, Position position, TUserState userState) {
        Iter = stream.Seek(position.Index); // throws if index smaller then stream.BeginIndex
        if (Iter.Index != position.Index) throw new ArgumentOutOfRangeException("position.Index is too large.", "position");
        data = new Data{Line = position.Line, LineBegin = position.Index - position.Column + 1,
                        UserState = userState, StreamName = position.StreamName};
    }

    public State(CharStream.Iterator iter, long line, long column, TUserState userState)
           : this(iter, line, column, userState, "") {}

    public State(CharStream.Iterator iter, long line, long column, TUserState userState, string streamName) {
        Iter = iter;
        data = new Data{Line = line, LineBegin = iter.Index - column + 1,
                        UserState = userState, StreamName = streamName};
    }

    public CharStream Stream     { get { return Iter.Stream; } }
    public string     StreamName { get { return data.StreamName; } }
    public long       Index      { get { return Iter.Index; } }
    public long       Line       { get { return data.Line; } }
    /// <summary>The stream index of the first char in the line, i.e. Index - Column + 1.</summary>
    public long       LineBegin  { get { return data.LineBegin; } }
    /// <summary>The 1-based index of the UTF16 char in the line, i.e. Index - LineBegin + 1.</summary>
    public long       Column     { get { return Iter.Index - data.LineBegin + 1; } }
    public TUserState UserState  { get { return data.UserState; } }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public State<TUserState> Next { get {
        // manually inline Iter.Next for the common case
        var newState = new State<TUserState>{data = data};
        var anchor = Iter.Anchor;
        newState.Iter.Anchor = anchor;
        int block = Iter.Block;
        newState.Iter.Block = block;
        char* newPtr = Iter.Ptr + 1;
        if (block == anchor->Block && newPtr < anchor->BufferEnd)
            newState.Iter.Ptr = newPtr;
        else {
            newState.Iter.Ptr = newPtr - 1;
            newState.Iter.IncrementContinue(1u);
        }
        return newState;
    } }

    public State<TUserState> Advance(int charOffset) {
        // manually inline Iter.Advance for the common case (the code actually comes from Iterator.Peek(int))
        var newState = new State<TUserState>{data = data};
        var ptr = Iter.Ptr;
        char* newPtr = unchecked(ptr + charOffset);
        if (charOffset >= 0) {
            if (newPtr >= ptr) {
                newState.Iter.Ptr = newPtr;
                var anchor = Iter.Anchor;
                if (newPtr < anchor->BufferEnd) {
                    newState.Iter.Anchor = anchor;
                    int block = Iter.Block;
                    if (block == anchor->Block) {
                        newState.Iter.Block = block;
                        return newState;
                    }
                }
            }
            newState.Iter.Anchor = Iter.Anchor;
            newState.Iter.Ptr    = Iter.Ptr;
            newState.Iter.Block  = Iter.Block;
            newState.Iter.IncrementContinue((uint)charOffset);
        } else { // we must exclude newPtr == ptr here, because ptr + Int32.MinValue == ptr
            if (newPtr < ptr) {
                newState.Iter.Ptr = newPtr;
                var anchor = Iter.Anchor;
                if (newPtr >= anchor->BufferBegin) {
                    newState.Iter.Anchor = anchor;
                    int block = Iter.Block;
                    if (block == anchor->Block) {
                        newState.Iter.Block = block;
                        return newState;
                    }
                }
            }
            newState.Iter = Iter.AdvanceContinue(charOffset);
        }
        return newState;
    }
    public State<TUserState> Advance(int charOffset, TUserState userState) {
        var newData = new Data{Line = data.Line, LineBegin = data.LineBegin,
                               UserState = userState, StreamName = data.StreamName};
        var newState = new State<TUserState>{data = newData};
        if (sizeof(IntPtr) != 8) newState.Iter = Iter;
        else {
            newState.Iter.Anchor = Iter.Anchor;
            newState.Iter.Ptr    = Iter.Ptr;
            newState.Iter.Block  = Iter.Block;
        }
        newState.Iter._AdvanceInPlace(charOffset);
        return newState;
    }
    public State<TUserState> Advance(int charOffset, int lineOffset, int newColumnMinus1) {
        var newData = new Data{Line = data.Line + lineOffset,
                               UserState = data.UserState, StreamName = data.StreamName};
        var newState = new State<TUserState>{data = newData};
        if (sizeof(IntPtr) != 8) newState.Iter = Iter;
        else {
            newState.Iter.Anchor = Iter.Anchor;
            newState.Iter.Ptr    = Iter.Ptr;
            newState.Iter.Block  = Iter.Block;
        }
        newState.Iter._AdvanceInPlace(charOffset);
        newData.LineBegin = newState.Iter.Index - newColumnMinus1;
        return newState;

    }
    public State<TUserState> Advance(int charOffset, int lineOffset, int newColumnMinus1, TUserState userState) {
        var newData = new Data{Line = data.Line + lineOffset,
                               UserState = userState, StreamName = data.StreamName};
        var newState = new State<TUserState>{data = newData};
        if (sizeof(IntPtr) != 8) newState.Iter = Iter;
        else {
            newState.Iter.Anchor = Iter.Anchor;
            newState.Iter.Ptr    = Iter.Ptr;
            newState.Iter.Block  = Iter.Block;
        }
        newState.Iter._AdvanceInPlace(charOffset);
        newData.LineBegin = newState.Iter.Index - newColumnMinus1;
        return newState;
    }

    internal State<TUserState> AdvanceTo(char* ptr) {
        var newState = new State<TUserState>{data = data};
        newState.Iter.Anchor = Iter.Anchor;
        newState.Iter.Ptr    = ptr;
        newState.Iter.Block  = Iter.Block;
        return newState;
    }
    internal State<TUserState> AdvanceTo(char* ptr, char* lineBegin, int lineOffset) {
        var anchor = Iter.Anchor;
        var d = CharStream.PositiveDistance(anchor->BufferBegin, lineBegin);
        long newLineBegin = d + anchor->CharIndexPlusOffset;
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = newLineBegin,
                               UserState = data.UserState, StreamName = data.StreamName};
        var newState = new State<TUserState>{data = newData};
        newState.Iter.Anchor = Iter.Anchor;
        newState.Iter.Ptr    = ptr;
        newState.Iter.Block  = Iter.Block;
        return newState;
    }

    public State<TUserState> AdvanceTo(CharStream.Iterator iter) {
        return new State<TUserState>{data = data, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, TUserState userState) {
        var newData = new Data{Line = data.Line, LineBegin = data.LineBegin,
                               UserState = userState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, int lineOffset, int newColumnMinus1) {
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = iter.Index - newColumnMinus1,
                               UserState = data.UserState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, long lineOffset, long newColumnMinus1) {
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = iter.Index - newColumnMinus1,
                               UserState = data.UserState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, uint lineOffset, uint newColumnMinus1) {
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = iter.Index - newColumnMinus1,
                               UserState = data.UserState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, int lineOffset, int newColumnMinus1, TUserState userState) {
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = iter.Index - newColumnMinus1,
                               UserState = userState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, long lineOffset, long newColumnMinus1, TUserState userState) {
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = iter.Index - newColumnMinus1,
                               UserState = userState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, uint lineOffset, uint newColumnMinus1, TUserState userState) {
        var newData = new Data{Line = data.Line + lineOffset, LineBegin = iter.Index - newColumnMinus1,
                               UserState = userState, StreamName = data.StreamName};
        return new State<TUserState>{data = newData, Iter = iter};
    }

    public State<TUserState> WithUserState(TUserState userState) {
        var newData = new Data{Line = data.Line, LineBegin = data.LineBegin,
                               UserState = userState, StreamName = data.StreamName};
        var newState = new State<TUserState>{data = newData};
        if (sizeof(IntPtr) != 8) newState.Iter = Iter;
        else {
            newState.Iter.Anchor = Iter.Anchor;
            newState.Iter.Ptr    = Iter.Ptr;
            newState.Iter.Block  = Iter.Block;
        }
        return newState;
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public Position Position { get {
        long index = Index;
        return new Position(data.StreamName, index, data.Line, index - data.LineBegin + 1);
    } }

    [Obsolete("FParsec.State.Pos has been renamed to FParsec.State.Position.")]
    public Position Pos { get { return Position; } }

    public override bool Equals(object obj) {
        // most of the time this and obj are equal references ...
        if ((object)this == obj) return true;
        var other = obj as State<TUserState>;
        return    (object)other != null
               // ... or their iterator indices (computed from the the Block and and Ptr) differ
               && (Iter.Ptr == other.Iter.Ptr || Iter.Block != other.Iter.Block)
               && EqualsHelper(other);
    }

    public bool Equals(State<TUserState> other) {
        return    (object)this == (object)other
               || (   (object)other != null
                   && (Iter.Ptr == other.Iter.Ptr || Iter.Block != other.Iter.Block)
                   && EqualsHelper(other));
    }

    public static bool operator==(State<TUserState> left, State<TUserState> right) {
        return    (object)left == (object)right
               || (   (object)left != null && (object)right != null
                   && (left.Iter.Ptr == right.Iter.Ptr || left.Iter.Block != right.Iter.Block)
                   && left.EqualsHelper(right));
    }
    public static bool operator!=(State<TUserState> left, State<TUserState> right) { return !(left == right); }

    private bool EqualsHelper(State<TUserState> other) {
        Data d1 = data, d2 = other.data;
        return    Iter == other.Iter
               && Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityERComparer.Equals(d1.UserState, d2.UserState)
               && d1.Line == d2.Line
               && d1.LineBegin == d2.LineBegin
               && d1.StreamName == d2.StreamName;
    }

    public override int GetHashCode() {  // GetHashCode() is not required to return different hash codes for unequal instances
        return Iter.Index.GetHashCode(); // and any change in the data members usually coincides with an iterator movement
    }

    public string ReadUntil(State<TUserState> stateAfterEndOfStream) {
        string str = Iter.ReadUntil(stateAfterEndOfStream.Iter);
        if ((object)data == (object)stateAfterEndOfStream.data) return str;
        return CharStream.NormalizeNewlines(str);
    }

    public State<TUserState> SkipWhitespace() {
        char* end = unchecked(Iter.Anchor->BufferEnd - 1); // - 1 to guarantee the lookahead for '\r',
        char* ptr = Iter.Ptr;
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            char* lineBegin = null;
            int nLines = 0;
            char c = *ptr;
            ++ptr;
            if (c > ' ') goto ReturnThis;
            if (c == ' ') {
                if (*ptr > ' ') return AdvanceTo(ptr);
            } else {
                if (c == '\r') {
                    if (*ptr == '\n') ++ptr;
                } else if (c != '\n') goto CheckTab;
                if (*ptr > ' ') return AdvanceTo(ptr, ptr, 1);
                lineBegin = ptr;
                ++nLines;
                goto Loop;
            CheckTab:
                if (c != '\t') goto ReturnThis;
            }
        Loop:
            for (;;) {
                c = *ptr;
                ++ptr;
                if (c == ' ') {
                    if (ptr >= end) break;
                } else if (c == '\r') {
                    if (*ptr == '\n') ++ptr;
                    lineBegin = ptr;
                    ++nLines;
                    if (ptr >= end) break;
                } else if (c == '\n') {
                    lineBegin = ptr;
                    ++nLines;
                    if (ptr >= end) break;
                } else if (c == '\t') {
                    if (ptr >= end) break;
                } else {
                    --ptr;
                    if (nLines == 0) return AdvanceTo(ptr);
                    return AdvanceTo(ptr, lineBegin, nLines);
                }
            }
            // reached the end of the current block
            int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
            var newState = nLines == 0 ? Advance(index)
                                       : Advance(index, nLines, (int)CharStream.PositiveDistance(lineBegin, ptr));
            return newState.SkipWhitespaceContinue();
        }
        return SkipWhitespaceContinue();
    ReturnThis:
        return this;
    }
    private State<TUserState> SkipWhitespaceContinue() {
        var state = this;
        for (;;) {
            char c = state.Iter.Read();
            if (c == ' ' || c == '\t') {
                state = state.Next;
            } else if (c == '\r' || c == '\n') {
                state = state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
            } else {
                return state;
            }
        }
    }

    public State<TUserState> SkipNewline() {
        var anchor = Iter.Anchor;
        var ptr = Iter.Ptr;
        if (Iter.Block == anchor->Block && ptr + 2 < anchor->BufferEnd) {
            char c = *ptr;
            ++ptr;
            if (c == '\r') {
                if (*ptr == '\n') ++ptr;
            } else if (c != '\n') goto ReturnThis;
            return AdvanceTo(ptr, ptr, 1);
        } else {
            char c = Iter.Read();
            int i = 1;
            if (c == '\r') {
                if (Iter.Peek() == '\n') i = 2;
            } else if (c != '\n') goto ReturnThis;
            return Advance(i, 1, 0);
        }
    ReturnThis:
        return this;
    }

    public State<TUserState> SkipRestOfLine(bool skipNewline) {
        State<TUserState> state = this;
        char* end = unchecked(Iter.Anchor->BufferEnd - 2); // - 2, so that we can do (*) without further checking
        char* ptr = Iter.Ptr;
        if (Iter.Block == Iter.Anchor->Block && ptr < end) {
            for (;;) {
                char c = *ptr;
                if (c > '\r') {
                    if (++ptr >= end) break;
                } else if (c == '\r' || c == '\n') {
                    if (!skipNewline) {
                        if (ptr != Iter.Ptr) return AdvanceTo(ptr);
                        return this;
                    } else {
                        ++ptr;
                        if (c == '\r' && *ptr == '\n') ++ptr;
                        return AdvanceTo(ptr, ptr, 1);  // (*)
                    }
                } else if (++ptr >= end) break;
            }
            // reached the end of the current block
            if (ptr != Iter.Ptr) {
                int count = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
                state = Advance(count);
            }
        }
        for (;;) {
            char c = state.Iter.Read();
            if (c != CharStream.Iterator.EndOfStreamChar) {
                if (c != '\r' && c != '\n') state = state.Next;
                else {
                    if (!skipNewline) return state;
                    return state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
                }
            } else return state;
        }
    }

    public State<TUserState> SkipRestOfLine(bool skipNewline, out string skippedString) {
        State<TUserState> state = this;
        char* end = unchecked(Iter.Anchor->BufferEnd - 2); // - 2, so that we can do (*) without further checking
        char* ptr = Iter.Ptr;
        if (Iter.Block == Iter.Anchor->Block && ptr < end) {
            for (;;) {
                char c = *ptr;
                if (c > '\r') {
                    if (++ptr >= end) break;
                } else if (c == '\r' || c == '\n') {
                    char* ptr0 = Iter.Ptr;
                    if (!skipNewline) {
                        if (ptr != ptr0) {
                            int length = (int)CharStream.PositiveDistance(ptr0, ptr);
                            skippedString = new string(ptr0, 0, length);
                            return AdvanceTo(ptr);
                        } else {
                            skippedString = "";
                            return this;
                        }
                    } else {
                        if (ptr != ptr0) {
                            int length = (int)CharStream.PositiveDistance(ptr0, ptr);
                            skippedString = new string(ptr0, 0, length);
                        } else {
                            skippedString = "";
                        }
                        ++ptr;
                        if (c == '\r' && *ptr == '\n') ++ptr;
                        return AdvanceTo(ptr, ptr, 1); // (*)
                    }
                } else if (++ptr >= end) break;
            }
            // reached the end of the current block
            if (ptr != Iter.Ptr) {
                int count = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
                state = Advance(count);
            }
        }
        for (;;) {
            char c = state.Iter.Read();
            if (c != CharStream.Iterator.EndOfStreamChar) {
                if (c != '\r' && c != '\n') state = state.Next;
                else {
                    skippedString = (object)state != (object)this ? Iter.ReadUntil(state.Iter) : "";
                    if (!skipNewline) return state;
                    return state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
                }
            } else {
                skippedString = (object)state != (object)this ? Iter.ReadUntil(state.Iter) : "";
                return state;
            }
        }
    }

    public State<TUserState> SkipCharOrNewline() {
        var anchor = Iter.Anchor;
        var ptr = Iter.Ptr;
        if (Iter.Block == anchor->Block && ptr + 2 < anchor->BufferEnd) {
            char c = *ptr;
            ++ptr;
            if (c <= '\r') goto CheckForNewline;
        SkipNormalChar:
            return AdvanceTo(ptr);
        CheckForNewline:
            if (c == '\r') {
                if (*ptr == '\n') ++ptr;
            } else if (c != '\n') goto SkipNormalChar;
      //SkipNewline:
            return AdvanceTo(ptr, ptr, 1);
        } else {
            char c = Iter.Read();
            if (c != CharStream.Iterator.EndOfStreamChar) {
                if (c != '\r' && c != '\n') return Next;
                return Advance(c == '\r' && Iter.Peek() == '\n' ? 2 : 1, 1, 0);
            } else return this;
        }
    }

    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines, out string skippedString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char* lineBegin = null;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        char* ptr = Iter.Ptr;
        var anchor = Iter.Anchor;
        if (Iter.Block == anchor->Block) {
            char* bufferEnd1 = anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r'
            char* end2 = unchecked(ptr + maxCharsOrNewlines);
            char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
            if (ptr < end) {
                for (;;) {
                    char c = *ptr;
                    ++ptr;
                    if (c > '\r') {
                        if (ptr >= end) break;
                    } else {
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                ++nCRLF;
                                if (end < bufferEnd1) ++end;
                            } else {
                                ++nCR;
                            }
                        } else if (c != '\n') goto CheckBound;
                        lineBegin = ptr;
                        ++nLines;
                    CheckBound:
                        if (ptr >= end) break;
                    }
                }
                if (end < bufferEnd1) {
                    char* ptr0 = Iter.Ptr;
                    int index = (int)CharStream.PositiveDistance(ptr0, ptr);
                    if (nLines == 0) {
                        skippedString = new string(ptr0, 0, index);
                        return AdvanceTo(ptr);
                    } else {
                        if ((nCR | nCRLF) == 0)
                            skippedString = new string(ptr0, 0, index);
                        else
                            skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, index, nCRLF, nCR);
                        return AdvanceTo(ptr, lineBegin, nLines);
                    }
                }
            }
        }
        return SkipCharsOrNewlinesContinue(maxCharsOrNewlines, out skippedString, ptr, lineBegin, nLines, nCRLF, nCR);
    }
    private State<TUserState> SkipCharsOrNewlinesContinue(int maxCharsOrNewlines, out string skippedString,
                                                         char* ptr, char* lineBegin, int nLines, int nCRLF, int nCR)
    {
        uint index = CharStream.PositiveDistance(Iter.Ptr, ptr);
        int count = (int)index - nCRLF;
        int lineBeginCount = nLines == 0 ? 0 : count - (int)CharStream.PositiveDistance(lineBegin, ptr);
        CharStream.Iterator iter = Iter;
        char c = iter._Increment(index);
        for (;;) {
             if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
             ++count;
             char c1 = iter._Increment();
             if (c > '\r') c = c1;
             else if (c == '\r') {
                 if (c1 == '\n') {
                     ++nCRLF;
                     c = iter._Increment();
                 } else {
                     ++nCR;
                     c = c1;
                 }
                 lineBeginCount = count;
                 ++nLines;
             } else if (c == '\n') {
                 c = c1;
                 lineBeginCount = count;
                 ++nLines;
             } else c = c1;
        }
        if (count != 0) {
            string s = Iter.ReadUntil(iter);
            if (nLines == 0) {
                skippedString = s;
                return AdvanceTo(iter);
            } else {
                if ((nCR | nCRLF) == 0) skippedString = s;
                else {
                    fixed (char* ps = s)
                        skippedString = CharStream.CopyWithNormalizedNewlines(ps, count + nCRLF, nCRLF, nCR);
                }
                return AdvanceTo(iter, nLines, count - lineBeginCount);
            }
        } else {
            skippedString = "";
            return this;
        }
    }

    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines) {
        int numberOfSkippedCharsOrNewlines;
        return SkipCharsOrNewlines(maxCharsOrNewlines, out numberOfSkippedCharsOrNewlines);
    }
    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines, out int numberOfSkippedCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char* lineBegin = null;
        int nLines = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        var anchor = Iter.Anchor;
        if (Iter.Block == anchor->Block) {
            char* bufferEnd1 = anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r'
            char* end2 = unchecked(ptr + maxCharsOrNewlines);
            char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
            if (ptr < end) {
                for (;;) {
                    char c = *ptr;
                    ++ptr;
                    if (c > '\r') {
                        if (ptr >= end) break;
                    } else {
                        if (c == '\r') {
                            if (*ptr == '\n') {
                                ++ptr;
                                ++nCRLF;
                                if (end < bufferEnd1) ++end;
                            }
                        } else if (c != '\n') goto CheckBound;
                        lineBegin = ptr;
                        ++nLines;
                    CheckBound:
                        if (ptr >= end) break;
                    }
                }
                if (end < bufferEnd1) {
                    numberOfSkippedCharsOrNewlines = (int)CharStream.PositiveDistance(Iter.Ptr, ptr) - nCRLF;
                    if (nLines == 0) return AdvanceTo(ptr);
                    return AdvanceTo(ptr, lineBegin, nLines);
                }
            }
        }
        return SkipCharsOrNewlinesContinue(maxCharsOrNewlines, out numberOfSkippedCharsOrNewlines, ptr, lineBegin, nLines, nCRLF);
    }
    private State<TUserState> SkipCharsOrNewlinesContinue(int maxCharsOrNewlines, out int numberOfSkippedCharsOrNewlines,
                                                         char* ptr, char* lineBegin, int nLines, int nCRLF)
    {
        int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
        int count = index - nCRLF;
        int lineBeginCount = nLines == 0 ? 0 : count - (int)CharStream.PositiveDistance(lineBegin, ptr);
        CharStream.Iterator iter = Iter;
        char c = iter._Increment((uint)index);
        for (;;) {
             if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
             ++count;
             char c1 = iter._Increment();
             if (c > '\r') c = c1;
             else if (c == '\r') {
                 if (c1 == '\n') {
                     ++nCRLF;
                     c = iter._Increment();
                 } else {
                     c = c1;
                 }
                 lineBeginCount = count;
                 ++nLines;
             } else if (c == '\n') {
                 c = c1;
                 lineBeginCount = count;
                 ++nLines;
             } else c = c1;
        }
        numberOfSkippedCharsOrNewlines = count;
        if (count != 0) {
            if (nLines == 0) return AdvanceTo(iter);
            return AdvanceTo(iter, nLines, count - lineBeginCount);
        }
        return this;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f) {
        return SkipCharsOrNewlinesWhile(f, f);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f) {
        char* lineBegin = null;
        int nLines = 0;
        char* ptr = Iter.Ptr;
        char* end = unchecked(Iter.Anchor->BufferEnd - 1); // - 1 to guarantee the lookahead for '\r'
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            char c = *ptr;
            ++ptr;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (*ptr == '\n') ++ptr;
                lineBegin = ptr;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = ptr;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            for (;;) {
                c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (f.Invoke(c)) {
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\r') {
                    if (f.Invoke('\n')) {
                        if (*ptr == '\n') ++ptr;
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\n') {
                    if (f.Invoke('\n')) {
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (f.Invoke(c)) {
                    if (ptr >= end) break; else continue;
                }
                --ptr;
                if (nLines == 0) return AdvanceTo(ptr);
                return AdvanceTo(ptr, lineBegin, nLines);
            }
            // reached the end of the current block
            int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
            var state =   nLines == 0
                        ? Advance(index)
                        : Advance(index, nLines, (int)CharStream.PositiveDistance(lineBegin, ptr));
            return SkipCharsOrNewlinesWhileContinue(f /* not f1 */, f, state);
        ReturnEmpty:
            return this;
        }
        return SkipCharsOrNewlinesWhileContinue(f1, f, this);
    }
    private State<TUserState> SkipCharsOrNewlinesWhileContinue(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, State<TUserState> state) {
        var ff = f1;
        for (;;) {
            char c = state.Iter.Read();
            if (c == CharStream.Iterator.EndOfStreamChar) break;
            if (c != '\r' && c != '\n') {
                if (!ff.Invoke(c)) break;
                state = state.Next;
            } else {
                if (!ff.Invoke('\n')) break;
                state = state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
            }
            ff = f;
        }
        return state;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, out string skippedString) {
        return SkipCharsOrNewlinesWhile(f, f, out skippedString);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, out string skippedString) {
        char* lineBegin = null;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        char* ptr = Iter.Ptr;
        char* end = unchecked(Iter.Anchor->BufferEnd - 1); // - 1 to guarantee the lookahead for '\r'
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            char c = *ptr;
            ++ptr;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (*ptr == '\n') {
                    ++ptr;
                    ++nCRLF;
                } else {
                    ++nCR;
                }
                lineBegin = ptr;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = ptr;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            for (;;) {
                c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (f.Invoke(c)) {
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\r') {
                    if (f.Invoke('\n')) {
                        if (*ptr == '\n') {
                            ++ptr;
                            ++nCRLF;
                        } else {
                            ++nCR;
                        }
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\n') {
                    if (f.Invoke('\n')) {
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (f.Invoke(c)) {
                    if (ptr >= end) break; else continue;
                }
                --ptr;
                char* ptr0 = Iter.Ptr;
                int index = (int)CharStream.PositiveDistance(ptr0, ptr);
                if (nLines == 0) {
                    skippedString = new string(ptr0, 0, index);
                    return AdvanceTo(ptr);
                } else {
                    if ((nCR | nCRLF) == 0)
                        skippedString = new string(ptr0, 0, index);
                    else
                        skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, index, nCRLF, nCR);
                    return AdvanceTo(ptr, lineBegin, nLines);
                }
            }
            {
                // reached the end of the current block
                int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
                var state = nLines == 0
                            ? Advance(index)
                            : Advance(index, nLines, (int)CharStream.PositiveDistance(lineBegin, ptr));
                return SkipCharsOrNewlinesWhileContinue(f /* not f1 */, f, out skippedString, state);
            }
        ReturnEmpty:
            skippedString = "";
            return this;
        }
        return SkipCharsOrNewlinesWhileContinue(f1, f, out skippedString, this);
    }
    private State<TUserState> SkipCharsOrNewlinesWhileContinue(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, out string skippedString, State<TUserState> state) {
        var ff = f1;
        for (;;) {
            char c = state.Iter.Read();
            if (c == CharStream.Iterator.EndOfStreamChar) break;
            if (c != '\r' && c != '\n') {
                if (!ff.Invoke(c)) break;
                state = state.Next;
            } else {
                if (!ff.Invoke('\n')) break;
                state = state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
            }
            ff = f;
        }
        if ((object)state != (object)this) {
            skippedString = ReadUntil(state);
            return state;
        } else {
            skippedString = "";
            return this;
        }
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        return SkipCharsOrNewlinesWhile(f, f, minCharsOrNewlines, maxCharsOrNewlines);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char* lineBegin = null;
        int nLines = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        char* bufferEnd1 = unchecked(Iter.Anchor->BufferEnd - 1); // - 1 to guarantee the lookahead for '\r'
        char* end2 = unchecked(ptr + maxCharsOrNewlines);
        char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            char c = *ptr;
            ++ptr;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (*ptr == '\n') {
                    ++ptr;
                    ++nCRLF;
                    if (end < bufferEnd1) ++end;
                }
                lineBegin = ptr;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = ptr;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            for (;;) {
                c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (f.Invoke(c)) {
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\r') {
                    if (f.Invoke('\n')) {
                        if (*ptr == '\n') {
                            ++ptr;
                            ++nCRLF;
                            if (end < bufferEnd1) ++end;
                        }
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\n') {
                    if (f.Invoke('\n')) {
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (f.Invoke(c)) {
                    if (ptr >= end) break; else continue;
                }
                --ptr;
                goto ReturnStringInBlock;
            }
            if (end >= bufferEnd1) goto EndOfBlock;
        ReturnStringInBlock:
            {
                int count = (int)CharStream.PositiveDistance(Iter.Ptr, ptr) - nCRLF;
                if (count >= minCharsOrNewlines) {
                    if (nLines == 0) return AdvanceTo(ptr);
                    return AdvanceTo(ptr, lineBegin, nLines);
                }
            }
        ReturnEmpty:
            return this;
        EndOfBlock:
            {
                int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
                var state = nLines == 0
                            ? Advance(index)
                            : Advance(index, nLines, (int)CharStream.PositiveDistance(lineBegin, ptr));
                int count = index - nCRLF;
                return SkipCharsOrNewlinesWhileContinue(f /* not f1 */, f, minCharsOrNewlines, maxCharsOrNewlines, state, count);
            }
        }
        return SkipCharsOrNewlinesWhileContinue(f1, f, minCharsOrNewlines, maxCharsOrNewlines, this, 0);
    }
    private State<TUserState> SkipCharsOrNewlinesWhileContinue(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, State<TUserState> state, int count) {
        var ff = f1;
        for (; count < maxCharsOrNewlines; ++count) {
            char c = state.Iter.Read();
            if (c == CharStream.Iterator.EndOfStreamChar) break;
            if (c != '\r' && c != '\n') {
                if (!ff.Invoke(c)) break;
                state = state.Next;
            } else {
                if (!ff.Invoke('\n')) break;
                state = state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
            }
            ff = f;
        }
        return count < minCharsOrNewlines ? this : state;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, out string skippedString) {
        return SkipCharsOrNewlinesWhile(f, f, minCharsOrNewlines, maxCharsOrNewlines, out skippedString);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, out string skippedString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char* lineBegin = null;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        char* ptr = Iter.Ptr;
        char* bufferEnd1 = unchecked(Iter.Anchor->BufferEnd - 1); // - 1 to guarantee the lookahead for '\r'
        char* end2 = unchecked(ptr + maxCharsOrNewlines);
        char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            char c = *ptr;
            ++ptr;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (*ptr == '\n') {
                    ++ptr;
                    ++nCRLF;
                    if (end < bufferEnd1) ++end;
                } else {
                    ++nCR;
                }
                lineBegin = ptr;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = ptr;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            for (;;) {
                c = *ptr;
                ++ptr;
                if (c > '\r') {
                    if (f.Invoke(c)) {
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\r') {
                    if (f.Invoke('\n')) {
                        if (*ptr == '\n') {
                            ++ptr;
                            ++nCRLF;
                            if (end < bufferEnd1) ++end;
                        } else {
                            ++nCR;
                        }
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (c == '\n') {
                    if (f.Invoke('\n')) {
                        lineBegin = ptr;
                        ++nLines;
                        if (ptr >= end) break; else continue;
                    }
                } else if (f.Invoke(c)) {
                    if (ptr >= end) break; else continue;
                }
                --ptr;
                goto ReturnStringInBlock;
            }
            if (end >= bufferEnd1) goto EndOfBlock;
        ReturnStringInBlock:
            {
                char* ptr0 = Iter.Ptr;
                int index = (int)CharStream.PositiveDistance(ptr0, ptr);
                int count = index - nCRLF;
                if (count >= minCharsOrNewlines) {
                    if (nLines == 0) {
                        skippedString = new string(ptr0, 0, index);
                        return AdvanceTo(ptr);
                    } else {
                        if ((nCR | nCRLF) == 0)
                            skippedString = new string(ptr0, 0, index);
                        else
                            skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, index, nCRLF, nCR);
                        return AdvanceTo(ptr, lineBegin, nLines);
                    }
                }
            }
        ReturnEmpty:
            skippedString = "";
            return this;
        EndOfBlock:
            {
                int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
                var state = nLines == 0
                            ? Advance(index)
                            : Advance(index, nLines, (int)CharStream.PositiveDistance(lineBegin, ptr));
                int count = index - nCRLF;
                return SkipCharsOrNewlinesWhileContinue(f /* not f1 */, f, minCharsOrNewlines, maxCharsOrNewlines, out skippedString, state, count);
            }
        }
        return SkipCharsOrNewlinesWhileContinue(f1, f, minCharsOrNewlines, maxCharsOrNewlines, out skippedString, this, 0);
    }
    private State<TUserState> SkipCharsOrNewlinesWhileContinue(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, out string skippedString, State<TUserState> state, int count) {
        var ff = f1;
        for (; count < maxCharsOrNewlines; ++count) {
            char c = state.Iter.Read();
            if (c == CharStream.Iterator.EndOfStreamChar) break;
            if (c != '\r' && c != '\n') {
                if (!ff.Invoke(c)) break;
                state = state.Next;
            } else {
                if (!ff.Invoke('\n')) break;
                state = state.Advance((c == '\r' && state.Iter.Peek() == '\n' ? 2 : 1), 1, 0);
            }
            ff = f;
        }
        if (count >= minCharsOrNewlines && count != 0) {
            skippedString = ReadUntil(state);
            return state;
        } else {
            skippedString = "";
            return this;
        }
    }

    private static bool RestOfStringEquals(char* p1, char* p2, int length) {
        for (int i = 1; i < length; ++i) {
            if (p1[i] != p2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }
    private static bool Rest3OfStringEquals(char* p1, char* p2, int length) {
        for (int i = 3; i < length; ++i) {
            if (p1[i] != p2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    private static bool RestOfStringEqualsCI(char* p1, char* p2, int length) {
        char* cftable = CaseFoldTable.FoldedChars;
        for (int i = 1; i < length; ++i) {
            if (cftable[p1[i]] != p2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    public State<TUserState> SkipToString(string str, int maxCharsOrNewlines, out bool foundString) {
        int strLength = str.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        fixed (char* pStr = str) {
            char* lineBegin = null;
            int nLines = 0;
            int nCRLF = 0;
            char first = pStr[0];
            char* ptr = Iter.Ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked(bufferEnd - strLength);
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked(ptr + maxCharsOrNewlines);
                char* end = end1 <= end2 || end2 < ptr ? end1 : end2;
                if (Iter.Block == Iter.Anchor->Block && ptr < end) {
                    for (;;) {
                        char c = *ptr;
                        if (c == first) goto CompareRestOfString;
                    StringsNotEqual:
                        ++ptr;
                        if (c > '\r') {
                            if (ptr >= end) break;
                        } else {
                            if (c == '\r') {
                                if (*ptr == '\n') {
                                    ++ptr;
                                    ++nCRLF;
                                    if (end < end1) ++end;
                                }
                            } else if (c != '\n') goto CheckBound;
                            lineBegin = ptr;
                            ++nLines;
                        CheckBound:
                            if (ptr >= end) break;
                        }
                        continue;
                    CompareRestOfString:
                        if (strLength == 1 || (ptr[1] == pStr[1] &&
                            (strLength == 2 || (ptr[2] == pStr[2] &&
                             (strLength == 3 || Rest3OfStringEquals(ptr, pStr, strLength)))))) goto Found;
                        goto StringsNotEqual;
                    } // for
                    if (*ptr == first && RestOfStringEquals(ptr, pStr, strLength)) goto Found;
                    if (ptr >= end1) goto EndOfBlock;
                    foundString = false;
                    goto ReturnState;
                Found:
                    foundString = true;
                ReturnState:
                    if (ptr != Iter.Ptr) {
                        if (nLines == 0) return AdvanceTo(ptr);
                        return AdvanceTo(ptr, lineBegin, nLines);
                    }
                    return this;
                }
            }
        EndOfBlock:
            return SkipToStringContinue(pStr, strLength, maxCharsOrNewlines, out foundString, ptr, lineBegin, nLines, nCRLF);
        }
    }
    private State<TUserState> SkipToStringContinue(char* pStr, int strLength, int maxCharsOrNewlines, out bool foundString,
                                                   char* ptr, char* lineBegin, int nLines, int nCRLF)
    {
        foundString = false;
        char first = *pStr;
        int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
        int count = index - nCRLF;
        int lineBeginCount = nLines == 0 ? 0 : count - (int)CharStream.PositiveDistance(lineBegin, ptr);
        CharStream.Iterator iter = Iter;
        char c = iter._Increment((uint)index);
        for (;;) {
            if (c != first || !iter.Match(pStr, strLength)) {
                if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
                ++count;
                char c1 = iter._Increment();
                if (c > '\r') c = c1;
                else if (c == '\r') {
                    if (c1 == '\n') {
                        ++nCRLF;
                        c = iter._Increment();
                    } else {
                        c = c1;
                    }
                    lineBeginCount = count;
                    ++nLines;
                } else if (c == '\n') {
                    c = c1;
                    lineBeginCount = count;
                    ++nLines;
                } else c = c1;
            } else {
                foundString = true;
                break;
            }
        }
        if (count != 0) {
            if (nLines == 0) return AdvanceTo(iter);
            return AdvanceTo(iter, nLines, count - lineBeginCount);
        }
        return this;
    }

    public State<TUserState> SkipToString(string str, int maxCharsOrNewlines, out string skippedString) {
        int strLength = str.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        fixed (char* pStr = str) {
            char* lineBegin = null;
            int nLines = 0;
            int nCRLF = 0;
            int nCR = 0;
            char first = pStr[0];
            char* ptr = Iter.Ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked(bufferEnd - strLength);
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked(ptr + maxCharsOrNewlines);
                char* end = end1 <= end2 || end2 < ptr ? end1 : end2;
                if (Iter.Block == Iter.Anchor->Block && ptr < end) {
                    for (;;) {
                        char c = *ptr;
                        if (c == first) goto CompareRestOfString;
                    StringsNotEqual:
                        ++ptr;
                        if (c > '\r') {
                            if (ptr >= end) break;
                        } else {
                            if (c == '\r') {
                                if (*ptr == '\n') {
                                    ++ptr;
                                    ++nCRLF;
                                    if (end < end1) ++end;
                                } else {
                                    ++nCR;
                                }
                            } else if (c != '\n') goto CheckBound;
                            lineBegin = ptr;
                            ++nLines;
                        CheckBound:
                            if (ptr >= end) break;
                        }
                        continue;
                    CompareRestOfString:
                        if (strLength == 1 || (ptr[1] == pStr[1] &&
                             (strLength == 2 || (ptr[2] == pStr[2] &&
                              (strLength == 3 || Rest3OfStringEquals(ptr, pStr, strLength)))))) goto Found;
                        goto StringsNotEqual;
                    } // for
                    if (*ptr == first && RestOfStringEquals(ptr, pStr, strLength)) goto Found;
                    if (ptr >= end1) goto EndOfBlock;
                    goto NotFound;
                Found:
                    {
                        char* ptr0 = Iter.Ptr;
                        if (ptr != ptr0) {
                            int length = (int)CharStream.PositiveDistance(ptr0, ptr);
                            if (nLines == 0) {
                                skippedString = new string(ptr0, 0, length);
                                return AdvanceTo(ptr);
                            } else {
                                skippedString = (nCR | nCRLF) == 0
                                                ? new string(ptr0, 0, length)
                                                : CharStream.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                                return AdvanceTo(ptr, lineBegin, nLines);
                            }
                        } else {
                            skippedString = "";
                            return this;
                        }
                    }
                NotFound:
                    {
                        skippedString = null;
                        char* ptr0 = Iter.Ptr;
                        Debug.Assert(ptr != ptr0);
                        if (nLines == 0) return AdvanceTo(ptr);
                        return AdvanceTo(ptr, lineBegin, nLines);
                    }
                }
            }
        EndOfBlock:
            return SkipToStringContinue(pStr, strLength, maxCharsOrNewlines, out skippedString, ptr, lineBegin, nLines, nCRLF, nCR);
        }
    }
    private State<TUserState> SkipToStringContinue(char* pStr, int strLength, int maxCharsOrNewlines, out string skippedString,
                                                   char* ptr, char* lineBegin, int nLines, int nCRLF, int nCR)
    {
        char first = *pStr;
        CharStream.Iterator iter = Iter;
        int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
        int count = index - nCRLF;
        int lineBeginCount = nLines == 0 ? 0 : count - (int)CharStream.PositiveDistance(lineBegin, ptr);
        char c = iter._Increment((uint)index);
        for (;;) {
            if (c != first || !iter.Match(pStr, strLength)) {
                if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
                ++count;
                char c1 = iter._Increment();
                if (c > '\r') c = c1;
                else if (c == '\r') {
                    if (c1 == '\n') {
                        ++nCRLF;
                        c = iter._Increment();
                    } else {
                        ++nCR;
                        c = c1;
                    }
                    lineBeginCount = count;
                    ++nLines;
                } else if (c == '\n') {
                    c = c1;
                    lineBeginCount = count;
                    ++nLines;
                } else c = c1;
            } else { // found string
                if (count != 0) {
                    if (nLines == 0) {
                        skippedString = Iter.ReadUntil(iter);
                        return AdvanceTo(iter);
                    } else {
                        string s = Iter.ReadUntil(iter);
                        if ((nCR | nCRLF) == 0) skippedString = s;
                        else {
                            fixed (char* ptr0 = s)
                                skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, count + nCRLF, nCRLF, nCR);
                        }
                        return AdvanceTo(iter, nLines, count - lineBeginCount);
                    }
                } else {
                    skippedString = "";
                    return this;
                }
            }
        }
        skippedString = null;
        if (count != 0) {
            if (nLines == 0) return AdvanceTo(iter);
            return AdvanceTo(iter, nLines, count - lineBeginCount);
        }
        return this;
    }

    public State<TUserState> SkipToStringCI(string caseFoldedString, int maxCharsOrNewlines, out bool foundString) {
        int strLength = caseFoldedString.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        fixed (char* pStr = caseFoldedString) {
            char* lineBegin = null;
            int nLines = 0;
            int nCRLF = 0;
            char first = pStr[0];
            char* ptr = Iter.Ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked(bufferEnd - strLength);
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked(ptr + maxCharsOrNewlines);
                char* end = end1 <= end2 || end2 < ptr ? end1 : end2;
                if (Iter.Block == Iter.Anchor->Block && ptr < end) {
                    char* cftable = CaseFoldTable.FoldedChars;
                    for (;;) {
                        char c = cftable[*ptr];
                        if (c == first) goto CompareRestOfString;
                    StringsNotEqual:
                        ++ptr;
                        if (c > '\r') {
                            if (ptr >= end) break;
                        } else {
                            if (c == '\r') {
                                if (*ptr == '\n') {
                                    ++ptr;
                                    ++nCRLF;
                                    if (end < end1) ++end;
                                }
                            } else if (c != '\n') goto CheckBound;
                            lineBegin = ptr;
                            ++nLines;
                        CheckBound:
                            if (ptr >= end) break;
                        }
                        continue;
                    CompareRestOfString:
                        if (strLength == 1 || RestOfStringEqualsCI(ptr, pStr, strLength)) goto Found;
                        goto StringsNotEqual;
                    } // for
                    if (cftable[*ptr] == first && RestOfStringEqualsCI(ptr, pStr, strLength)) goto Found;
                    if (ptr >= end1) goto EndOfBlock;
                    foundString = false;
                    goto ReturnState;
                Found:
                    foundString = true;
                ReturnState:
                    if (ptr != Iter.Ptr) {
                        if (nLines == 0) return AdvanceTo(ptr);
                        return AdvanceTo(ptr, lineBegin, nLines);
                    }
                    return this;
                }
            }
        EndOfBlock:
            return SkipToStringCIContinue(pStr, strLength, maxCharsOrNewlines, out foundString, ptr, lineBegin, nLines, nCRLF);
        }
    }
    private State<TUserState> SkipToStringCIContinue(char* pStr, int strLength, int maxCharsOrNewlines, out bool foundString,
                                                     char* ptr, char* lineBegin, int nLines, int nCRLF)
    {
        foundString = false;
        char first = *pStr;
        int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
        int count = index - nCRLF;
        int lineBeginCount = nLines == 0 ? 0 : count - (int)CharStream.PositiveDistance(lineBegin, ptr);
        char* cftable = CaseFoldTable.FoldedChars;
        CharStream.Iterator iter = Iter;
        char c = cftable[iter._Increment((uint)index)];
        for (;;) {
            if (c != first || !iter.MatchCaseFolded(pStr, strLength)) {
                if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
                ++count;
                char c1 = cftable[iter._Increment()];
                if (c > '\r') c = c1;
                else if (c == '\r') {
                    if (c1 == '\n') {
                        ++nCRLF;
                        c = cftable[iter._Increment()];
                    } else {
                        c = c1;
                    }
                    lineBeginCount = count;
                    ++nLines;
                } else if (c == '\n') {
                    c = c1;
                    lineBeginCount = count;
                    ++nLines;
                } else c = c1;
            } else {
                foundString = true;
                break;
            }
        }
        if (count != 0) {
            if (nLines == 0) return AdvanceTo(iter);
            return AdvanceTo(iter, nLines, count - lineBeginCount);
        }
        return this;
    }

    public State<TUserState> SkipToStringCI(string caseFoldedString, int maxCharsOrNewlines, out string skippedString) {
        int strLength = caseFoldedString.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        fixed (char* pStr = caseFoldedString) {
            char* lineBegin = null;
            int nLines = 0;
            int nCRLF = 0;
            int nCR = 0;
            char first = pStr[0];
            char* ptr = Iter.Ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked(bufferEnd - strLength);
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked(ptr + maxCharsOrNewlines);
                char* end = end1 <= end2 || end2 < ptr ? end1 : end2;
                if (Iter.Block == Iter.Anchor->Block && ptr < end) {
                    char* cftable = CaseFoldTable.FoldedChars;
                    for (;;) {
                        char c = cftable[*ptr];
                        if (c == first) goto CompareRestOfString;
                    StringsNotEqual:
                        ++ptr;
                        if (c > '\r') {
                            if (ptr >= end) break;
                        } else {
                            if (c == '\r') {
                                if (*ptr == '\n') {
                                    ++ptr;
                                    ++nCRLF;
                                    if (end < end1) ++end;
                                } else {
                                    ++nCR;
                                }
                            } else if (c != '\n') goto CheckBound;
                            lineBegin = ptr;
                            ++nLines;
                        CheckBound:
                            if (ptr >= end) break;
                        }
                        continue;
                    CompareRestOfString:
                        if (strLength == 1 || RestOfStringEqualsCI(ptr, pStr, strLength)) goto Found;
                        goto StringsNotEqual;
                    } // for
                    if (cftable[*ptr] == first && RestOfStringEqualsCI(ptr, pStr, strLength)) goto Found;
                    if (ptr >= end1) goto EndOfBlock;
                    goto NotFound;
                Found:
                    {
                        char* ptr0 = Iter.Ptr;
                        if (ptr != ptr0) {
                            int length = (int)CharStream.PositiveDistance(ptr0, ptr);
                            if (nLines == 0) {
                                skippedString = new string(ptr0, 0, length);
                                return AdvanceTo(ptr);
                            } else {
                                skippedString = (nCR | nCRLF) == 0
                                                ? new string(ptr0, 0, length)
                                                : CharStream.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                                return AdvanceTo(ptr, lineBegin, nLines);
                            }
                        } else {
                            skippedString = "";
                            return this;
                        }
                    }
                NotFound:
                    {
                        skippedString = null;
                        char* ptr0 = Iter.Ptr;
                        Debug.Assert(ptr != ptr0);
                        if (nLines == 0) return AdvanceTo(ptr);
                        return AdvanceTo(ptr, lineBegin, nLines);
                    }
                }
            }
        EndOfBlock:
            return SkipToStringCIContinue(pStr, strLength, maxCharsOrNewlines, out skippedString, ptr, lineBegin, nLines, nCRLF, nCR);
        }
    }
    private State<TUserState> SkipToStringCIContinue(char* pStr, int strLength, int maxCharsOrNewlines, out string skippedString,
                                                     char* ptr, char* lineBegin, int nLines, int nCRLF, int nCR)
    {
        char first = *pStr;
        CharStream.Iterator iter = Iter;
        int index = (int)CharStream.PositiveDistance(Iter.Ptr, ptr);
        int count = index - nCRLF;
        int lineBeginCount = nLines == 0 ? 0 : count - (int)CharStream.PositiveDistance(lineBegin, ptr);
        char* cftable = CaseFoldTable.FoldedChars;
        char c = cftable[iter._Increment((uint)index)];
        for (;;) {
            if (c != first || !iter.MatchCaseFolded(pStr, strLength)) {
                if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
                ++count;
                char c1 = cftable[iter._Increment()];
                if (c > '\r') c = c1;
                else if (c == '\r') {
                    if (c1 == '\n') {
                        ++nCRLF;
                        c = cftable[iter._Increment()];
                    } else {
                        ++nCR;
                        c = c1;
                    }
                    lineBeginCount = count;
                    ++nLines;
                } else if (c == '\n') {
                    c = c1;
                    lineBeginCount = count;
                    ++nLines;
                } else c = c1;
            } else { // found string
                if (count != 0) {
                    if (nLines == 0) {
                        skippedString = Iter.ReadUntil(iter);
                        return AdvanceTo(iter);
                    } else {
                        string s = Iter.ReadUntil(iter);
                        if ((nCR | nCRLF) == 0) skippedString = s;
                        else {
                            fixed (char* ptr0 = s)
                                skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, count + nCRLF, nCRLF, nCR);
                        }
                        return AdvanceTo(iter, nLines, count - lineBeginCount);
                    }
                } else {
                    skippedString = "";
                    return this;
                }
            }
        }
        skippedString = null;
        if (count != 0) {
            if (nLines == 0) return AdvanceTo(iter);
            return AdvanceTo(iter, nLines, count - lineBeginCount);
        }
        return this;
    }

} // class State

}

#endif // !LOW_TRUST