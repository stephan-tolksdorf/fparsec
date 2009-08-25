// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Diagnostics;

namespace FParsec {

public sealed unsafe class State<TUserState> : IEquatable<State<TUserState>> {

    // splitting up the state into a fast changing part, the iterator, and a
    // a slowly changing part, the data member, improves performance
    private sealed class Data {
        public long       Line;
        public long       LineBegin;
        public TUserState UserState;
        public string     StreamName;

        public Data(long line, long lineBegin, TUserState userState, string streamName) {
            this.Line       = line;
            this.LineBegin  = lineBegin;
            this.UserState  = userState;
            this.StreamName = streamName;
        }
    }

    public CharStream.Iterator Iter;
    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    private Data data;

    private State(CharStream.Iterator iter, Data data) {
        this.Iter = iter;
        this.data = data;
    }

    public State(CharStream stream, TUserState userState) : this(stream, userState, "") { }

    public State(CharStream stream, TUserState userState, string streamName) {
        Iter = stream.Begin;
        data = new Data(1, Iter.Anchor->CharIndexOffset, userState, streamName);
    }

    public State(CharStream stream, Pos pos, TUserState userState) {
        Iter = stream.Seek(pos.Index); // will throw for negative index
        if (Iter.Index != pos.Index) throw new ArgumentOutOfRangeException("Pos.Index", "The index is too large.");
        data = new Data(pos.Line, pos.Index - pos.Column + 1, userState, pos.StreamName);
    }

    public State(CharStream.Iterator iter, long line, long column, TUserState userState)
           : this(iter, line, column, userState, "") { }

    public State(CharStream.Iterator iter, long line, long column, TUserState userState, string streamName) {
        Iter = iter;
        data = new Data(line, iter.Index - column + 1, userState, streamName);
    }

    public CharStream Stream     { get { return Iter.Stream; } }
    public string     StreamName { get { return data.StreamName; } }
    public long       Index      { get { return Iter.Index; } }
    public long       Line       { get { return data.Line; } }
    /// <summary>The 1-based index of the UTF16 char in the line.</summary>
    public long       LineBegin  { get { return data.LineBegin; } }
    /// <summary>The 1-based index of the UTF16 char in the line.</summary>
    public long       Column     { get { return Iter.Index - data.LineBegin + 1; } }
    public TUserState UserState  { get { return data.UserState; } }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public State<TUserState> Next { get {
        // inline Iter.Next for the common case
        var iter = Iter;
        char* newPtr = iter.Ptr + 1;
        if (iter.Block == iter.Anchor->Block && newPtr < iter.Anchor->BufferEnd) {
            iter.Ptr = newPtr;
            return new State<TUserState>(iter, data);
        } else {
            return new State<TUserState>(Iter.Next, data);
        }
    } }

    public State<TUserState> Advance(int nChars) {
        return new State<TUserState>(Iter.Advance(nChars), data);
    }
    public State<TUserState> Advance(int nChars, TUserState userState) {
        return new State<TUserState>(Iter.Advance(nChars),
                                     new Data(data.Line,
                                              data.LineBegin,
                                              userState,
                                              data.StreamName));
    }
    public State<TUserState> Advance(int nChars, int nLines, int nCharsAfterLastNL) {
        if (nLines > 0) {
            long newLineBegin = Iter.Index + nChars - nCharsAfterLastNL;
            return new State<TUserState>(Iter.Advance(nChars),
                                         new Data(data.Line + nLines,
                                                  newLineBegin,
                                                  data.UserState,
                                                  data.StreamName));
        } else return Advance(nChars);
    }
    public State<TUserState> Advance(int nChars, int nLines, int nCharsAfterLastNL, TUserState userState) {
        if (nLines > 0) {
            long newLineBegin = Iter.Index + nChars - nCharsAfterLastNL;
            return new State<TUserState>(Iter.Advance(nChars),
                                         new Data(data.Line + nLines,
                                                  newLineBegin,
                                                  userState,
                                                  data.StreamName));
        } else return Advance(nChars, userState);
    }

    internal State<TUserState> AdvanceWithinBlock(CharStream.Iterator iter, char* lineBegin) {
        return new State<TUserState>(iter, new Data(data.Line + 1,
                                                    iter.Anchor->CharIndexPlusOffset + CharStream.PositiveDistance(iter.Anchor->BufferBegin, lineBegin),
                                                    data.UserState,
                                                    data.StreamName));
    }
    internal State<TUserState> AdvanceWithinBlock(CharStream.Iterator iter, int nLines, char* lineBegin) {
        return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                    iter.Anchor->CharIndexPlusOffset + CharStream.PositiveDistance(iter.Anchor->BufferBegin, lineBegin),
                                                    data.UserState,
                                                    data.StreamName));
    }

    public State<TUserState> AdvanceTo(CharStream.Iterator iter) {
        return new State<TUserState>(iter, data);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, TUserState userState) {
        return new State<TUserState>(iter, new Data(data.Line, data.LineBegin, userState, data.StreamName));
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, int nLines, int nCharsAfterLastNL) {
        if (nLines > 0) {
            return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                        iter.Index - nCharsAfterLastNL,
                                                        data.UserState,
                                                        data.StreamName));
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, int nLines, int nCharsAfterLastNL, TUserState userState) {
        if (nLines > 0) {
            return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                        iter.Index - nCharsAfterLastNL,
                                                        userState,
                                                        data.StreamName));
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, uint nLines, uint nCharsAfterLastNL) {
        return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                    iter.Index - nCharsAfterLastNL,
                                                    data.UserState,
                                                    data.StreamName));
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, uint nLines, uint nCharsAfterLastNL, TUserState userState) {
        return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                    iter.Index - nCharsAfterLastNL,
                                                    userState,
                                                    data.StreamName));
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, long nLines, long nCharsAfterLastNL) {
        if (nLines > 0) {
            return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                        iter.Index - nCharsAfterLastNL,
                                                        data.UserState,
                                                        data.StreamName));
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, long nLines, long nCharsAfterLastNL, TUserState userState) {
        if (nLines > 0) {
            return new State<TUserState>(iter, new Data(data.Line + nLines,
                                                        iter.Index - nCharsAfterLastNL,
                                                        userState,
                                                        data.StreamName));
        } else return AdvanceTo(iter);
    }

    public State<TUserState> WithUserState(TUserState userState) {
        return new State<TUserState>(Iter, new Data(data.Line,
                                                    data.LineBegin,
                                                    userState,
                                                    data.StreamName));
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public Pos Pos { get {
        long index = Index;
        return new Pos(data.StreamName, index, data.Line, index - data.LineBegin + 1);
    } }

    public override bool Equals(object other) {
        // most of the time this and other are equal references ...
        if ((object)this == other) return true;
        var state2 = other as State<TUserState>;
        return    (object)state2 != null
              // ... or their iterator indices (computed from the the Block and and Ptr) differ
               && (Iter.Ptr == state2.Iter.Ptr || Iter.Block != state2.Iter.Block)
               && EqualsHelper(state2);
    }

    public bool Equals(State<TUserState> other) {
        return    (object)this == (object)other
               || (   (object)other != null
                   && (Iter.Ptr == other.Iter.Ptr || Iter.Block != other.Iter.Block)
                   && EqualsHelper(other));
    }

    public static bool operator==(State<TUserState> s1, State<TUserState> s2) {
        return    (object)s1 == (object)s2
               || (   (object)s1 != null && (object)s2 != null
                   && (s1.Iter.Ptr == s2.Iter.Ptr || s1.Iter.Block != s2.Iter.Block)
                   && s1.EqualsHelper(s2));
    }
    public static bool operator!=(State<TUserState> s1, State<TUserState> s2) { return !(s1 == s2); }

    private bool EqualsHelper(State<TUserState> other) {
        Data d1 = data, d2 = other.data;
        return    Iter == other.Iter
               && Object.Equals(d1.UserState, d2.UserState)
               && d1.Line == d2.Line
               && d1.LineBegin == d2.LineBegin
               && d1.StreamName == d2.StreamName;
    }

    public override int GetHashCode() { // GetHashCode() is not required to return different hash codes for unequal instances
        return Iter.GetHashCode();      // and any change in the data members usually coincides with an iterator movement
    }

    /// <summary>
    /// Returns a string with all the chars between the position of this State (inclusive)
    /// and the position of the given State (exclusive).
    /// In the returned string any newline ("\n", "\r\n" or "\r") is normalized to "\n"
    /// (assuming that any newline is properly accounted for in the line count of the given State).
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException">The Iterator of the given State does not belong to the same CharStream as this State's Iterator.</exception>
    /// <exception cref="NullReferenceException">The given State is null.</exception>
    public string ReadUntil(State<TUserState> stateAfterEndOfStream) {
        string str = Iter.ReadUntil(stateAfterEndOfStream.Iter);
        if ((object)data == (object)stateAfterEndOfStream.data) return str;
        return CharStream.NormalizeNewlines(str);
    }

    /// <summary>
    /// Skips over a space (' '), a tab ('\t') or a newline ("\n", "\r\n" or "\r").
    /// Returns a new State positioned after the whitespace, or the old State (this) if there is no whitespace.
    /// </summary>
    public State<TUserState> SkipWhitespace() {
        char* end = Iter.Anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r',
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            // fast path
            int nLines = 0;
            char c = *ptr;
            if (c <= ' ') {
                ++ptr;
                if (c == ' ') {
                    c = *ptr;
                    if (*ptr > ' ') {
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr;
                        return AdvanceTo(newIter);
                    }
                } else {
                    if (c == '\r') {
                        if (*ptr == '\n') ++ptr;
                    } else if (c != '\n') goto CheckTab;
                    lineBegin = ptr;
                    ++nLines;
                    if (*ptr > ' ') {
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr;
                        return AdvanceWithinBlock(newIter, ptr);
                    }
                    goto Loop;
                CheckTab:
                    if (c != '\t') goto ReturnThis;
                }
            } else goto ReturnThis;
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
                    CharStream.Iterator newIter = Iter;
                    newIter.Ptr = ptr - 1;
                    if (nLines == 0) return AdvanceTo(newIter);
                    return AdvanceWithinBlock(newIter, nLines, lineBegin);
                }
            }
            // reached the end of the current block
            int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
            var newState = nLines == 0 ? Advance(index)
                                       : Advance(index, nLines, CharStream.PositiveDistance(lineBegin, ptr));
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

    /// <summary>
    /// Skips over a newline ("\n", "\r\n" or "\r").
    /// Returns a new State positioned after the newline, or the old State (this) if there is no newline.
    /// </summary>
    public State<TUserState> SkipNewline() {
        var iter = Iter;
        if (iter.Block == iter.Anchor->Block && iter.Ptr + 2 < iter.Anchor->BufferEnd) {
            char c = *iter.Ptr;
            ++iter.Ptr;
            if (c == '\r') {
                if (*iter.Ptr == '\n') ++iter.Ptr;
            } else if (c != '\n') goto ReturnThis;
            return AdvanceWithinBlock(iter, iter.Ptr);
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

    /// <summary>
    /// Skips over any chars before the next newline ("\n", "\r\n" or "\r") or the end of the stream.
    /// If the given bool is true and a newline is present, the newline is also skipped.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if no chars were skipped.
    /// </summary>
    public State<TUserState> SkipRestOfLine(bool skipNewline) {
        State<TUserState> state = this;
        char* end = Iter.Anchor->BufferEnd - 2; // - 2, so that we can do (*) without further checking
        char* ptr = Iter.Ptr;
        if (Iter.Block == Iter.Anchor->Block && ptr < end) {
            // fast path
            for (;;) {
                char c = *ptr;
                if (c > '\r') {
                    if (++ptr >= end) break;
                } else if (c == '\r' || c == '\n') {
                    if (!skipNewline) {
                        if (ptr != Iter.Ptr) {
                            CharStream.Iterator newIter = Iter;
                            newIter.Ptr = ptr;
                            return AdvanceTo(newIter);
                        } else return this;
                    } else {
                        ++ptr;
                        if (c == '\r' && *ptr == '\n') ++ptr;
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr; // (*)
                        return AdvanceWithinBlock(newIter, 1, ptr);
                    }
                } else if (++ptr >= end) break;
            }
            // reached the end of the current block
            if (ptr != Iter.Ptr) {
                int count = CharStream.PositiveDistance(Iter.Ptr, ptr);
                state = Advance(count);
            }
        }
        // (slow) backup path
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

    /// <summary>
    /// Skips over any chars before the next newline ("\n", "\r\n" or "\r") or the end of the stream.
    /// If the given bool is true and a newline is present, the newline is also skipped.
    /// Assigns a string with the skipped chars (without a newline) to the output parameter.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if no chars were skipped.
    /// </summary>
    public State<TUserState> SkipRestOfLine(bool skipNewline, out string skippedString) {
        State<TUserState> state = this;
        char* end = Iter.Anchor->BufferEnd - 2; // - 2, so that we can do (*) without further checking
        char* ptr = Iter.Ptr;
        if (Iter.Block == Iter.Anchor->Block && ptr < end) {
            // fast path
            for (;;) {
                char c = *ptr;
                if (c > '\r') {
                    if (++ptr >= end) break;
                } else if (c == '\r' || c == '\n') {
                    char* ptr0 = Iter.Ptr;
                    if (!skipNewline) {
                        if (ptr != ptr0) {
                            int length = CharStream.PositiveDistance(ptr0, ptr);
                            skippedString = new string(ptr0, 0, length);
                            CharStream.Iterator newIter = Iter;
                            newIter.Ptr = ptr;
                            return AdvanceTo(newIter);
                        } else {
                            skippedString = "";
                            return this;
                        }
                    } else {
                        if (ptr != ptr0) {
                            int length = CharStream.PositiveDistance(ptr0, ptr);
                            skippedString = new string(ptr0, 0, length);
                        } else {
                            skippedString = "";
                        }
                        ++ptr;
                        if (c == '\r' && *ptr == '\n') ++ptr;
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr; // (*)
                        return AdvanceWithinBlock(newIter, 1, ptr);
                    }
                } else if (++ptr >= end) break;
            }
            // reached the end of the current block
            if (ptr != Iter.Ptr) {
                int count = CharStream.PositiveDistance(Iter.Ptr, ptr);
                state = Advance(count);
            }
        }
        // (slow) backup path
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

    /// <summary>
    /// Skips over a newline ("\n", "\r\n" or "\r") or any other single char.
    /// Returns a new State positioned after the skipped char(s), or the old State (this) if there are no more chars in the stream.
    ///</summary>
    public State<TUserState> SkipCharOrNewline() {
        var iter = Iter;
        if (iter.Block == iter.Anchor->Block && iter.Ptr + 2 < iter.Anchor->BufferEnd) {
            char c = *iter.Ptr;
            ++iter.Ptr;
            if (c <= '\r') goto CheckForNewline;
        SkipNormalChar:
            return new State<TUserState>(iter, data);
        CheckForNewline:
            if (c == '\r') {
                if (*iter.Ptr == '\n') ++iter.Ptr;
            } else if (c != '\n') goto SkipNormalChar;
      //SkipNewline:
            return AdvanceWithinBlock(iter, iter.Ptr);
        } else {
            char c = Iter.Read();
            if (c != CharStream.Iterator.EndOfStreamChar) {
                if (c != '\r' && c != '\n') return Next;
                else return Advance(c == '\r' && Iter.Peek() == '\n' ? 2 : 1, 1, 0);
            } else return this;
        }
    }

    /// <summary>
    /// Skips over up to maxCharsOrNewlines chars.
    /// Assigns a string with the skipped chars to the output parameter.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if there are no more chars in the stream.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// </summary>
    /// <exception cref="ArgumentException">maxCharsOrNewlines is negative.</exception>
    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines, out string skippedString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        int nLines = 0;
        int nCR = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        char* bufferEnd1 = Iter.Anchor->BufferEnd - 1;
        char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
        char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
        if (Iter.Block == Iter.Anchor->Block && ptr < end) {
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
                int index = CharStream.PositiveDistance(ptr0, ptr);
                CharStream.Iterator newIter = Iter;
                newIter.Ptr = ptr;
                if (nLines == 0) {
                    skippedString = new string(ptr0, 0, index);
                    return AdvanceTo(newIter);
                } else {
                    if ((nCR | nCRLF) == 0)
                        skippedString = new string(ptr0, 0, index);
                    else
                        skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, index, nCRLF, nCR);
                    return AdvanceWithinBlock(newIter, nLines, lineBegin);
                }
            }
        }
        {
            CharStream.Iterator iter = Iter;
            int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
            int count = index - nCRLF;
            int lineBeginCount = count - CharStream.PositiveDistance(lineBegin, ptr);
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
    }

    /// <summary>
    /// Skips over up to maxCharsOrNewlines chars.
    /// Assigns the number of skipped chars to the output parameter.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if there are no more chars in the stream.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.</summary>
    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines, out int numberOfSkippedCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        numberOfSkippedCharsOrNewlines = 0;
        int nLines = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        char* bufferEnd1 = Iter.Anchor->BufferEnd - 1;
        char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
        char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
        if (Iter.Block == Iter.Anchor->Block && ptr < end) {
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
                numberOfSkippedCharsOrNewlines = CharStream.PositiveDistance(Iter.Ptr, ptr) - nCRLF;
                CharStream.Iterator newIter = Iter;
                newIter.Ptr = ptr;
                if (nLines == 0) return AdvanceTo(newIter);
                else return AdvanceWithinBlock(newIter, nLines, lineBegin);
            }
        }
        {
            CharStream.Iterator iter = Iter;
            int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
            int count = index - nCRLF;
            int lineBeginCount = count - CharStream.PositiveDistance(lineBegin, ptr);
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
            if (count != 0) {
                numberOfSkippedCharsOrNewlines = count;
                if (nLines == 0) return AdvanceTo(iter);
                else return AdvanceTo(iter, nLines, count - lineBeginCount);
            } else return this;
        }
    }



    /// <summary>
    /// Skips over the current char if f1 returns true for the char.
    /// If there are no more chars in the stream or if f1 returns false, the old State (this) is returned.
    /// Skips over all the following chars for which f returns true. Returns a new State positioned after the skipped chars.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// </summary>
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FastFunc<char,bool> f1, Microsoft.FSharp.Core.FastFunc<char,bool> f) {
        State<TUserState> state = this;
        int nLines = 0;
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        char* end = Iter.Anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r'
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            // fast path
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
                CharStream.Iterator newIter = Iter;
                newIter.Ptr = ptr;
                if (nLines == 0) return AdvanceTo(newIter);
                else return AdvanceWithinBlock(newIter, nLines, lineBegin);
            }
            // reached the end of the current block
            {
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                if (nLines == 0) state = Advance(index);
                else state = Advance(index, nLines, CharStream.PositiveDistance(lineBegin, ptr));
            }
        }
        // (slow) backup path
        var ff = (object)state == (object)this ? f1 : f;
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
    ReturnEmpty:
        return this;
    }

    /// <summary>
    /// Skips over the current char if f1 returns true for the char.
    /// If there are no more chars in the stream or if f1 returns false,
    /// the old State (this) is returned and an empty string is assigned to the output parameter.
    /// Skips over all the following chars for which f returns true.
    /// Assigns a string with the skipped chars to the output parameter.
    /// Returns a new State positioned after the skipped chars.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'. </summary>
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FastFunc<char,bool> f1, Microsoft.FSharp.Core.FastFunc<char,bool> f, out string skippedString) {
        State<TUserState> state = this;
        int nLines = 0;
        int nCR = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        char* end = Iter.Anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r'
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            // fast path
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
                int index = CharStream.PositiveDistance(ptr0, ptr);
                CharStream.Iterator newIter = Iter;
                newIter.Ptr = ptr;
                if (nLines == 0) {
                    skippedString = new string(ptr0, 0, index);
                    return AdvanceTo(newIter);
                } else {
                    if ((nCR | nCRLF) == 0)
                        skippedString = new string(ptr0, 0, index);
                    else
                        skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, index, nCRLF, nCR);
                    return AdvanceWithinBlock(newIter, nLines, lineBegin);
                }
            }
            // reached the end of the current block
            {
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                if (nLines == 0) state = Advance(index);
                else state = Advance(index, nLines, CharStream.PositiveDistance(lineBegin, ptr));
            }
        }
        // (slow) backup path
        var ff = (object)state == (object)this ? f1 : f;
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
        }
    ReturnEmpty:
        skippedString = "";
        return this;
    }



    /// <summary>
    /// Skips over the current char if maxCharsOrNewlines is greater 0 and f1 returns true for the char.
    /// If if there are no more chars in the stream, if maxCharsOrNewlines is 0 or if f1 returns false, the old State (this) is returned.
    /// Skips over all the following chars for which f returns true, but not more than maxCharsOrNewlines ‐ 1 chars.
    /// If in total not less than minCharsOrNewlines chars were skipped, a new State positioned after the skipped chars is returned,
    /// otherwise the old State (this) is returned.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.</summary>
    /// <exception cref="ArgumentException">maxCharsOrNewlines is negative.</exception>
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FastFunc<char,bool> f1, Microsoft.FSharp.Core.FastFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        State<TUserState> state = this;
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        int nLines = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        char* bufferEnd1 = Iter.Anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r'
        char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
        char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            // fast path
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
                int count = CharStream.PositiveDistance(Iter.Ptr, ptr) - nCRLF;
                if (count < minCharsOrNewlines) goto ReturnEmpty;
                CharStream.Iterator newIter = Iter;
                newIter.Ptr = ptr;
                if (nLines == 0) return AdvanceTo(newIter);
                else return AdvanceWithinBlock(newIter, nLines, lineBegin);
            }
        EndOfBlock:
            // reached the end of the current block
            {
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                if (nLines == 0) state = Advance(index);
                else state = Advance(index, nLines, CharStream.PositiveDistance(lineBegin, ptr));
            }
        }
        {
            // (slow) backup path
            var ff = (object)state == (object)this ? f1 : f;
            int count = CharStream.PositiveDistance(Iter.Ptr, ptr) - nCRLF;
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
            if (count >= minCharsOrNewlines) return state;
        }
    ReturnEmpty:
        return this;
    }

    /// <summary>
    /// Skips over the current char if maxCharsOrNewlines is greater 0 and f1 returns true for the char.
    /// If if there are no more chars in the stream, if maxCharsOrNewlines is 0 or if f1 returns false,
    /// the old State (this) is returned and an empty string is assigned to the output parameter.
    /// Skips over all the following chars for which f returns true, but not more than maxCharsOrNewlines ‐ 1 chars.
    /// If in total not less than minCharsOrNewlines chars were skipped,
    /// a string with the skipped chars is assigned to the output parameter and a new State positioned after the skipped chars is returned,
    /// otherwise the old State (this) is returned and an empty string is assigned to the output parameter.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// </summary>
    /// <exception cref="ArgumentException">maxCharsOrNewlines is negative.</exception>
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FastFunc<char,bool> f1, Microsoft.FSharp.Core.FastFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, out string skippedString) {
        State<TUserState> state = this;
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        int nLines = 0;
        int nCR = 0;
        int nCRLF = 0;
        char* ptr = Iter.Ptr;
        char* lineBegin = ptr;
        char* bufferEnd1 = Iter.Anchor->BufferEnd - 1; // - 1 to guarantee the lookahead for '\r'
        char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
        char* end = end2 >= ptr && end2 <= bufferEnd1 ? end2 : bufferEnd1;
        if (Iter.Block == Iter.Anchor->Block && ptr + 2 < end) { // + 2 so that we don't need to check after the first iteration
            // fast path
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
                int index = CharStream.PositiveDistance(ptr0, ptr);
                int count = index - nCRLF;
                if (count < minCharsOrNewlines) goto ReturnEmpty;
                CharStream.Iterator newIter = Iter;
                newIter.Ptr = ptr;
                if (nLines == 0) {
                    skippedString = new string(ptr0, 0, index);
                    return AdvanceTo(newIter);
                } else {
                    if ((nCR | nCRLF) == 0)
                        skippedString = new string(ptr0, 0, index);
                    else
                        skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, index, nCRLF, nCR);
                    return AdvanceWithinBlock(newIter, nLines, lineBegin);
                }
            }
        EndOfBlock:
            // reached the end of the current block
            {
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                if (nLines == 0) state = Advance(index);
                else state = Advance(index, nLines, CharStream.PositiveDistance(lineBegin, ptr));
            }
        }
        {
            // (slow) backup path
            var ff = (object)state == (object)this ? f1 : f;
            int count = CharStream.PositiveDistance(Iter.Ptr, ptr) - nCRLF;
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
            }
        }
    ReturnEmpty:
        skippedString = "";
        return this;
    }

    /// <summary>
    /// Skips over all the chars before the first occurrence of the given string or the end of the stream,
    /// but not over more than maxCharsOrNewlines chars. The given string must not be empty.
    /// Assigns true to the output parameter if the string was found, otherwise false.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if no chars were skipped.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// However, no newline normalization takes place when the string is matched.
    /// Hence, the given string should either contain no newlines or only in the form it occurs in the stream.</summary>
    /// <exception cref="ArgumentException">The string argument is empty or maxCharsOrNewlines is negative.</exception>
    /// <exception cref="NullReferenceException">The string argument is null.</exception>
    public State<TUserState> SkipToString(string str, int maxCharsOrNewlines, out bool foundString) {
        int strLength = str.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        foundString  = false;
        fixed (char* pstr = str) {
            char first = pstr[0];
            int nLines = 0;
            int nCRLF = 0;
            char* ptr = Iter.Ptr;
            char* lineBegin = ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked (bufferEnd - strLength); // could overflow
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
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
                        for (int i = 1; i != strLength; ++i) {
                            if (ptr[i] != pstr[i]) goto StringsNotEqual;
                        }
                        foundString = true;
                        goto ReturnStringInBlock;
                    } // for
                    if (*ptr == first) {
                        for (int i = 1; i != strLength; ++i) {
                            if (ptr[i] != pstr[i]) goto StringsNotEqual2;
                        }
                        foundString = true;
                        goto ReturnStringInBlock;
                    }
                StringsNotEqual2:
                    if (ptr >= end1) goto EndOfBlock;
                ReturnStringInBlock:
                    char* ptr0 = Iter.Ptr;
                    if (ptr != ptr0) {
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr;
                        if (nLines == 0) return AdvanceTo(newIter);
                        else return AdvanceWithinBlock(newIter, nLines, lineBegin);
                    } else return this;
                }
            }
        EndOfBlock:
            {
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                int count = index - nCRLF;
                int lineBeginCount = count - CharStream.PositiveDistance(lineBegin, ptr);
                CharStream.Iterator iter = Iter;
                char c = iter._Increment((uint)index);
                for (;;) {
                    if (c != first || !iter.Match(pstr, strLength)) {
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
                    else return AdvanceTo(iter, nLines, count - lineBeginCount);
                } else return this;
            }
        }
    }


    /// <summary>
    /// Skips over all the chars before the first occurrence of the given string or the end of the stream,
    /// but not over more than maxCharsOrNewlines chars. The given string must not be empty.
    /// If the given string was found, a string with the skipped chars is assigned to the output parameter,
    /// otherwise null is assigned to the output parameter.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if no chars were skipped.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// However, no newline normalization takes place when the string is matched.
    /// Hence, the given string should either contain no newlines or only in the form it occurs in the stream.
    /// </summary>
    /// <exception cref="ArgumentException">The string argument is empty or maxCharsOrNewlines is negative.</exception>
    /// /// <exception cref="NullReferenceException">The string argument is null.</exception>
    public State<TUserState> SkipToString(string str, int maxCharsOrNewlines, out string skippedString) {
        int strLength = str.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        bool foundString  = false;
        fixed (char* pstr = str) {
            char first = pstr[0];
            int nLines = 0;
            int nCR = 0;
            int nCRLF = 0;
            char* ptr = Iter.Ptr;
            char* lineBegin = ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked (bufferEnd - strLength); // could overflow
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
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
                        for (int i = 1; i != strLength; ++i) {
                            if (ptr[i] != pstr[i]) goto StringsNotEqual;
                        }
                        foundString = true;
                        goto ReturnStringInBlock;
                    } // for
                    if (*ptr == first) {
                        for (int i = 1; i != strLength; ++i) {
                            if (ptr[i] != pstr[i]) goto StringsNotEqual2;
                        }
                        foundString = true;
                        goto ReturnStringInBlock;
                    }
                StringsNotEqual2:
                    if (ptr >= end1) goto EndOfBlock;
                ReturnStringInBlock:
                    char* ptr0 = Iter.Ptr;
                    if (ptr != ptr0) {
                        int length = CharStream.PositiveDistance(ptr0, ptr);
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr;
                        if (nLines == 0) {
                            skippedString = foundString ? new string(ptr0, 0, length) : null;
                            return AdvanceTo(newIter);
                        } else {
                            if (foundString) {
                                skippedString = (nCR | nCRLF) == 0
                                                ? new string(ptr0, 0, length)
                                                : CharStream.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                            } else {
                                skippedString = null;
                            }
                            return AdvanceWithinBlock(newIter, nLines, lineBegin);
                        }
                    } else {
                        skippedString = foundString ? "" : null;
                        return this;
                    }
                }
            }
        EndOfBlock:
            {
                CharStream.Iterator iter = Iter;
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                int count = index - nCRLF;
                int lineBeginCount = count - CharStream.PositiveDistance(lineBegin, ptr);
                char c = iter._Increment((uint)index);
                for (;;) {
                    if (c != first || !iter.Match(pstr, strLength)) {
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
                    } else {
                        foundString = true;
                        break;
                    }
                }
                if (count != 0) {
                    if (nLines == 0) {
                        skippedString = foundString ? Iter.ReadUntil(iter) : null;
                        return AdvanceTo(iter);
                    } else {
                        if (foundString) {
                            string s = Iter.ReadUntil(iter);
                            if ((nCR | nCRLF) == 0) skippedString = s;
                            else {
                                fixed (char* ptr0 = s)
                                    skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, count + nCRLF, nCRLF, nCR);
                            }
                        } else {
                            skippedString = null;
                        }
                        return AdvanceTo(iter, nLines, count - lineBeginCount);
                    }
                } else {
                    skippedString = foundString ? "" : null;
                    return this;
                }
            }
        }
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

    /// <summary>
    /// Skips over all the chars before the first case‐insensitive occurrence of the given string or the end of the stream,
    /// but not over more than maxCharsOrNewlines chars. The given string must be case‐folded and not empty.
    /// Assigns true to the output parameter if the string was found, otherwise false.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if no chars were skipped.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// However, no newline normalization takes place when the string is matched.
    /// Hence, the given string should either contain no newlines or only in the form it occurs in the stream.
    /// </summary>
    /// <exception cref="ArgumentException">The string argument is empty or maxCharsOrNewlines is negative.</exception>
    /// <exception cref="NullReferenceException">The string argument is null.</exception>
    public State<TUserState> SkipToStringCI(string caseFoldedStr, int maxCharsOrNewlines, out bool foundString) {
        int strLength = caseFoldedStr.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        foundString  = false;
        fixed (char* pstr = caseFoldedStr) {
            char* cftable = CaseFoldTable.FoldedChars;
            if (cftable == null) cftable = CaseFoldTable.Initialize();
            char first = pstr[0];
            int nLines = 0;
            int nCRLF = 0;
            char* ptr = Iter.Ptr;
            char* lineBegin = ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked (bufferEnd - strLength); // could overflow
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
                char* end = end1 <= end2 || end2 < ptr ? end1 : end2;
                if (Iter.Block == Iter.Anchor->Block && ptr < end) {
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
                        if (!RestOfStringEqualsCI(ptr, pstr, strLength)) goto StringsNotEqual;
                        foundString = true;
                        goto ReturnStringInBlock;
                    } // for
                    if (cftable[*ptr] == first) {
                        if (!RestOfStringEqualsCI(ptr, pstr, strLength)) goto StringsNotEqual2;
                        foundString = true;
                        goto ReturnStringInBlock;
                    }
                StringsNotEqual2:
                    if (ptr >= end1) goto EndOfBlock;
                ReturnStringInBlock:
                    char* ptr0 = Iter.Ptr;
                    if (ptr != ptr0) {
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr;
                        if (nLines == 0) return AdvanceTo(newIter);
                        return AdvanceWithinBlock(newIter, nLines, lineBegin);
                    } else return this;
                }
            }
        EndOfBlock:
            {
                CharStream.Iterator iter = Iter;
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                int count = index - nCRLF;
                int lineBeginCount = count - CharStream.PositiveDistance(lineBegin, ptr);
                char c = cftable[iter._Increment((uint)index)];
                for (;;) {
                    if (c != first || !iter.MatchCaseFolded(pstr, strLength)) {
                        if (c == CharStream.Iterator.EndOfStreamChar || count == maxCharsOrNewlines) break;
                        ++count;
                        char c1 = cftable[iter._Increment()];
                        if (c > '\r') c = c1;
                        else if (c == '\r') {
                            if (c1 == '\n') {
                                ++nCRLF;
                                c =  cftable[iter._Increment()];
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
                    string s = Iter.ReadUntil(iter);
                    if (nLines == 0) return AdvanceTo(iter);
                    else return AdvanceTo(iter, nLines, count - lineBeginCount);
                } else return this;
            }
        }
    }

    /// <summary>
    /// Skips over all the chars before the first case‐insensitive occurrence of the given string or the end of the stream,
    /// but not over more than maxCharsOrNewlines chars. The given string must be case‐folded and not empty.
    /// If the given string was found, a string with the skipped chars is assigned to the output parameter,
    /// otherwise null is assigned to the output parameter.
    /// Returns a new State positioned after the skipped chars, or the old State (this) if no chars were skipped.
    /// This method treats all newlines ("\n", "\r\n" or "\r") in the input as a single char '\n'.
    /// However, no newline normalization takes place when the string is matched.
    /// Hence, the given string should either contain no newlines or only in the form it occurs in the stream.
    /// </summary>
    /// <exception cref="ArgumentException">The string argument is empty or maxCharsOrNewlines is negative.</exception>
    /// <exception cref="NullReferenceException">The string argument is null.</exception>
    public State<TUserState> SkipToStringCI(string caseFoldedStr, int maxCharsOrNewlines, out string skippedString) {
        int strLength = caseFoldedStr.Length; // throws if str is null
        if (strLength == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentException("maxCharsOrNewlines is negative.");
        bool foundString  = false;
        fixed (char* pstr = caseFoldedStr) {
            char* cftable = CaseFoldTable.FoldedChars;
            if (cftable == null) cftable = CaseFoldTable.Initialize();
            char first = pstr[0];
            int nLines = 0;
            int nCR = 0;
            int nCRLF = 0;
            char* ptr = Iter.Ptr;
            char* lineBegin = ptr;
            char* bufferEnd = Iter.Anchor->BufferEnd;
            char* end1 = unchecked (bufferEnd - strLength); // could overflow
            if (end1 >= ptr && end1 < bufferEnd) {
                char* end2 = unchecked (ptr + maxCharsOrNewlines); // could overflow
                char* end = end1 <= end2 || end2 < ptr ? end1 : end2;
                if (Iter.Block == Iter.Anchor->Block && ptr < end) {
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
                        if (!RestOfStringEqualsCI(ptr, pstr, strLength)) goto StringsNotEqual;
                        foundString = true;
                        goto ReturnStringInBlock;
                    } // for
                    if (cftable[*ptr] == first) {
                        if (!RestOfStringEqualsCI(ptr, pstr, strLength)) goto StringsNotEqual2;
                        foundString = true;
                        goto ReturnStringInBlock;
                    }
                StringsNotEqual2:
                    if (ptr >= end1) goto EndOfBlock;
                ReturnStringInBlock:
                    char* ptr0 = Iter.Ptr;
                    if (ptr != ptr0) {
                        int length = CharStream.PositiveDistance(ptr0, ptr);
                        CharStream.Iterator newIter = Iter;
                        newIter.Ptr = ptr;
                        if (nLines == 0) {
                            skippedString = foundString ? new string(ptr0, 0, length) : null;
                            return AdvanceTo(newIter);
                        } else {
                            if (foundString) {
                                skippedString = (nCR | nCRLF) == 0
                                                ? new string(ptr0, 0, length)
                                                : CharStream.CopyWithNormalizedNewlines(ptr0, length, nCRLF, nCR);
                            } else {
                                skippedString = null;
                            }
                            return AdvanceWithinBlock(newIter, nLines, lineBegin);
                        }
                    } else {
                        skippedString = foundString ? "" : null;
                        return this;
                    }
                }
            }
        EndOfBlock:
            {
                CharStream.Iterator iter = Iter;
                int index = CharStream.PositiveDistance(Iter.Ptr, ptr);
                int count = index - nCRLF;
                int lineBeginCount = count - CharStream.PositiveDistance(lineBegin, ptr);
                char c = cftable[iter._Increment((uint)index)];
                for (;;) {
                    if (c != first || !iter.MatchCaseFolded(pstr, strLength)) {
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
                    } else {
                        foundString = true;
                        break;
                    }
                }
                if (count != 0) {
                    if (nLines == 0) {
                        skippedString = foundString ? Iter.ReadUntil(iter) : null;
                        return AdvanceTo(iter);
                    } else {
                        if (foundString) {
                            string s = Iter.ReadUntil(iter);
                            if ((nCR | nCRLF) == 0) skippedString = s;
                            else {
                                fixed (char* ptr0 = s)
                                    skippedString = CharStream.CopyWithNormalizedNewlines(ptr0, count + nCRLF, nCRLF, nCR);
                            }
                        } else {
                            skippedString = null;
                        }
                        return AdvanceTo(iter, nLines, count - lineBeginCount);
                    }
                } else {
                    skippedString = foundString ? "" : null;
                    return this;
                }
            }
        }
    }

} // class state

}
