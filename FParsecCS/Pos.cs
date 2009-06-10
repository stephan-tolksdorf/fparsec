// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

using System;

namespace FParsec {

public class Pos : IEquatable<Pos>, IComparable, IComparable<Pos> {
    public long   Index      { get; private set;}
    public long   Line       { get; private set;}
    public long   Column     { get; private set;}
    public string StreamName { get; private set;}

    public Pos(string streamName) {
        this.StreamName = streamName; this.Index = 0; this.Line = 1; this.Column = 1;
    }

    public Pos(string streamName, long index, long line, long column) {
        this.StreamName = streamName; this.Index = index; this.Line = line; this.Column = column;
    }

    public override string ToString() {
        return (String.IsNullOrEmpty(StreamName) ? "(Ln: " : "(\"" + StreamName + "\", Ln: ")
               + Line.ToString() + ", Col: " + Column.ToString() + ")";
    }

    public override bool Equals(object other) {
        if (!(other is Pos)) return false;
        return Equals((Pos) other);
    }
    public bool Equals(Pos other) {
        return Index == other.Index && Line == other.Line && Column == other.Column && StreamName == other.StreamName;
    }
    public override int GetHashCode() {
        long i = Index ^ Line ^ Column;
        return StreamName.GetHashCode() ^ ((int) i) ^ (int) (((ulong)i) >> 32);
    }
    public static bool operator==(Pos pos1, Pos pos2) { return  pos1.Equals(pos2); }
    public static bool operator!=(Pos pos1, Pos pos2) { return !pos1.Equals(pos2); }

    public int CompareTo(Pos other) {
        int r = StreamName.CompareTo(other.StreamName);
        if (r != 0) return r;
        r = Line.CompareTo(other.Line);
        if (r != 0) return r;
        r = Column.CompareTo(other.Column);
        if (r != 0) return r;
        return Index.CompareTo(other.Index);
    }

    int IComparable.CompareTo(object other) {
        if (!(other is Pos)) throw new ArgumentException("Pos.CompareTo: the given object is not a Pos");
        return CompareTo((Pos) other);
    }
}

}
